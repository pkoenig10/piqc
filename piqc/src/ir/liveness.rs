use crate::collections::{PrimaryMap, SecondaryMap};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::*;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::ops::{Add, AddAssign};

enum RegisterClass {
    Accumulator,
    A,
    B,
}

enum Register {
    Accumulator(u8),
    A(u8),
    B(u8),
}

// TODO: use a less error-prone default here
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
struct ProgramPoint(u32);

impl ProgramPoint {
    fn fetch_add(&mut self, val: u32) -> ProgramPoint {
        let ret = *self;
        *self += val;
        ret
    }
}

impl Add<u32> for ProgramPoint {
    type Output = ProgramPoint;

    fn add(self, rhs: u32) -> ProgramPoint {
        ProgramPoint(self.0 + rhs)
    }
}

impl AddAssign<u32> for ProgramPoint {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

#[derive(Debug)]
pub struct Order {
    insts: SecondaryMap<Inst, ProgramPoint>,
    blocks: SecondaryMap<Block, ProgramPoint>,
}

impl Order {
    pub fn new() -> Order {
        Order {
            insts: SecondaryMap::new(),
            blocks: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph) {
        let mut point = ProgramPoint(0);

        for block in cfg.blocks() {
            self.blocks[block] = point.fetch_add(2);
            for inst in func.layout.insts(block) {
                self.insts[inst] = point.fetch_add(2);
            }
        }
    }
}

trait OrderIndex {
    fn use_point(self, ordering: &Order) -> ProgramPoint;

    fn def_point(self, ordering: &Order) -> ProgramPoint;
}

impl OrderIndex for Block {
    fn use_point(self, ordering: &Order) -> ProgramPoint {
        ordering.blocks[self]
    }

    fn def_point(self, ordering: &Order) -> ProgramPoint {
        ordering.blocks[self] + 1
    }
}

impl OrderIndex for Inst {
    fn use_point(self, ordering: &Order) -> ProgramPoint {
        ordering.insts[self]
    }

    fn def_point(self, ordering: &Order) -> ProgramPoint {
        ordering.insts[self] + 1
    }
}

id!(IntervalId, "interval");

#[derive(Debug, PartialEq, Eq)]
struct Interval {
    value: Value,
    begin: ProgramPoint,
    end: ProgramPoint,
}

#[derive(Debug)]
pub struct Intervals {
    intervals: PrimaryMap<IntervalId, Interval>,
}

impl Intervals {
    pub fn new() -> Intervals {
        Intervals {
            intervals: PrimaryMap::new(),
        }
    }

    fn get(&self, id: IntervalId) -> &Interval {
        &self.intervals[id]
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph, order: &Order) {
        let mut active = HashMap::new();
        let mut liveins = SecondaryMap::<Block, Vec<Value>>::new();

        for block in cfg.blocks().rev() {
            debug_assert!(active.is_empty());

            let last_point = func
                .layout
                .last_inst(block)
                .expect("Block does not have last inst")
                .def_point(order);

            // TODO: this doesn't quite work for loops because we process the successor before the predecessor
            for succ in cfg.succs(block) {
                for &value in &liveins[succ] {
                    active.entry(value).or_insert(last_point);
                }
            }

            for inst in func.layout.insts(block).rev() {
                for &value in func.data.inst(inst).args() {
                    // In the future we could add uses here
                    active.entry(value).or_insert(inst.use_point(order));
                }

                if let Some(value) = func.data.inst_result(inst) {
                    if let Some(end) = active.remove(&value) {
                        self.intervals.insert(Interval {
                            value,
                            begin: inst.def_point(order),
                            end,
                        });
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.intervals.insert(Interval {
                        value,
                        begin: block.def_point(order),
                        end,
                    });
                }
            }

            for (value, end) in active.drain() {
                self.intervals.insert(Interval {
                    value,
                    begin: block.use_point(order),
                    end,
                });
                liveins[block].push(value);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct UnhandledInterval {
    id: IntervalId,
    begin: ProgramPoint,
}

impl PartialOrd for UnhandledInterval {
    fn partial_cmp(&self, other: &UnhandledInterval) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for UnhandledInterval {
    fn cmp(&self, other: &UnhandledInterval) -> Ordering {
        self.begin.cmp(&other.begin).reverse()
    }
}

struct UnhandledIntervals {
    intervals: BinaryHeap<UnhandledInterval>,
}

impl UnhandledIntervals {
    pub fn new() -> UnhandledIntervals {
        UnhandledIntervals {
            intervals: BinaryHeap::new(),
        }
    }

    pub fn push(&mut self, id: IntervalId, begin: ProgramPoint) {
        self.intervals.push(UnhandledInterval { id, begin });
    }

    pub fn pop(&mut self) -> Option<IntervalId> {
        self.intervals.pop().map(|interval| interval.id)
    }
}

struct ActiveInterval {
    id: IntervalId,
}

#[derive(Debug)]
pub struct RegisterAllocator {}

impl RegisterAllocator {
    pub fn new() -> RegisterAllocator {
        RegisterAllocator {}
    }

    pub fn run(&mut self, intervals: &Intervals) {
        let mut unhandled_intervals = UnhandledIntervals::new();
        for (id, interval) in &intervals.intervals {
            unhandled_intervals.push(id, interval.begin);
        }

        let mut active_intervals: Vec<IntervalId> = Vec::new();

        while let Some(id) = unhandled_intervals.pop() {
            let interval = &intervals.get(id);

            active_intervals
                .retain(|&active_interval| interval.begin <= intervals.get(active_interval).end);

            active_intervals.push(id);

            let values: Vec<Value> = active_intervals
                .iter()
                .map(|&id| intervals.get(id).value)
                .collect();

            println!("{}: {}", interval.begin.0, DisplaySlice(&values));
        }
    }
}
