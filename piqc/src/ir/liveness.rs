use crate::collections::{PrimaryMap, SecondaryMap};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::*;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fmt;

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

type InstIdx = u32;

enum InstPoint {
    Use = 0,
    Def = 1,
}

// We changed from ProgramIndex = u32 because we want to perform linear scan without the func or cfg. In order to do this we need a non-optional program point to represent the end of a live-out interval so we know when to remove those intervals from the active set
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ProgramPoint(u32);

// TODO: this is probably a premature optimization, consider just using a struct
impl ProgramPoint {
    pub fn new_use(idx: InstIdx) -> ProgramPoint {
        ProgramPoint::new(idx, InstPoint::Use)
    }

    pub fn new_def(idx: InstIdx) -> ProgramPoint {
        ProgramPoint::new(idx, InstPoint::Def)
    }

    fn new(idx: u32, point: InstPoint) -> ProgramPoint {
        debug_assert!(idx <= 0x4000_0000);
        ProgramPoint(2 * idx + (point as u32))
    }
}

// TODO: remove this
impl fmt::Display for ProgramPoint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 & 1 {
            0 => write!(f, "{}-u", self.0 >> 1),
            _ => write!(f, "{}-d", self.0 >> 1),
        }
    }
}

#[derive(Debug)]
pub struct Order {
    insts: SecondaryMap<Inst, InstIdx>,
}

impl Order {
    pub fn new() -> Order {
        Order {
            insts: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph) {
        let mut index = 0;

        for block in cfg.blocks() {
            for inst in func.layout.insts(block) {
                self.insts[inst] = index;
                index += 1;
            }
        }
    }
}

trait OrderIndex {
    fn index(self, ordering: &Order) -> InstIdx;
}

impl OrderIndex for Inst {
    fn index(self, ordering: &Order) -> InstIdx {
        ordering.insts[self]
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

            let first_point = ProgramPoint::new_use(
                func.layout
                    .first_inst(block)
                    .expect("Block does not have first inst")
                    .index(order),
            );
            let last_point = ProgramPoint::new_def(
                func.layout
                    .last_inst(block)
                    .expect("Block does not have last inst")
                    .index(order),
            );

            for succ in cfg.succs(block) {
                for &value in &liveins[succ] {
                    active.entry(value).or_insert(last_point);
                }
            }

            for inst in func.layout.insts(block).rev() {
                for &value in func.data.inst(inst).args() {
                    // In the future we could add uses here
                    active
                        .entry(value)
                        .or_insert(ProgramPoint::new_use(inst.index(order)));
                }

                if let Some(value) = func.data.inst_result(inst) {
                    if let Some(end) = active.remove(&value) {
                        self.intervals.insert(Interval {
                            value,
                            begin: ProgramPoint::new_def(inst.index(order)),
                            end,
                        });
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.intervals.insert(Interval {
                        value,
                        begin: first_point,
                        end,
                    });
                }
            }

            for (value, end) in active.drain() {
                self.intervals.insert(Interval {
                    value,
                    begin: first_point,
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

            println!("{}: {}", interval.begin, DisplaySlice(&values));
        }
    }
}
