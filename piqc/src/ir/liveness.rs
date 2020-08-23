use crate::collections::SecondaryMap;
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
pub struct ProgramPoint(pub u32);

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

pub trait OrderIndex {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LiveInterval {
    begin: ProgramPoint,
    end: ProgramPoint,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct LiveRange {
    intervals: Vec<LiveInterval>,
}

impl LiveRange {
    fn push(&mut self, interval: LiveInterval) {
        self.intervals.push(interval);
    }

    fn coalesce(&mut self) {
        self.intervals.sort_unstable();

        let mut len = 1;
        for i in 1..self.intervals.len() {
            let interval = self.intervals[i];

            let previous = &mut self.intervals[len - 1];
            if interval.begin <= previous.end + 1 {
                previous.end = interval.end;
                continue;
            }

            self.intervals[len] = interval;
            len += 1;
        }

        self.intervals.truncate(len);
        self.intervals.shrink_to_fit();
    }
}

#[derive(Debug)]
pub struct Intervals {
    ranges: SecondaryMap<Value, LiveRange>,
}

impl Intervals {
    pub fn new() -> Intervals {
        Intervals {
            ranges: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph, order: &Order) {
        let mut active = HashMap::new();
        let mut liveins = SecondaryMap::new();
        let mut back_edges = HashMap::new();

        for block in cfg.blocks().rev() {
            debug_assert!(active.is_empty());

            let last_point = func
                .layout
                .last_inst(block)
                .expect("Block does not have last inst")
                .def_point(order);

            for succ in cfg.succs(block) {
                match liveins[succ] {
                    Some(ref liveins) => {
                        for &value in liveins {
                            active.entry(value).or_insert(last_point);
                        }
                    }
                    None => {
                        back_edges.insert(succ, block);
                    }
                }
            }

            for inst in func.layout.insts(block).rev() {
                for &value in func.data.inst(inst).args() {
                    // TODO: In the future we could add uses here
                    active.entry(value).or_insert(inst.use_point(order));
                }

                if let Some(value) = func.data.inst_result(inst) {
                    if let Some(end) = active.remove(&value) {
                        self.push(
                            value,
                            LiveInterval {
                                begin: inst.def_point(order),
                                end,
                            },
                        );
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.push(
                        value,
                        LiveInterval {
                            begin: block.def_point(order),
                            end,
                        },
                    );
                }
            }

            let mut block_liveins = Vec::new();
            for (value, end) in active.drain() {
                self.push(
                    value,
                    LiveInterval {
                        begin: block.use_point(order),
                        end,
                    },
                );
                block_liveins.push(value);
            }
            liveins[block] = Some(block_liveins);
        }

        // Handle back edges
        // TODO: Can we clean this up?
        let mut live: Vec<(Value, Block)> = Vec::new();

        for block in cfg.blocks() {
            let last_point = func
                .layout
                .last_inst(block)
                .expect("Block does not have last inst")
                .def_point(order);

            for &(value, _) in &live {
                self.push(
                    value,
                    LiveInterval {
                        begin: block.use_point(order),
                        end: last_point,
                    },
                );
            }

            live.retain(|&(_, end_block)| end_block != block);

            if let Some(end_block) = back_edges.remove(&block) {
                for &value in liveins[block].as_ref().unwrap() {
                    live.push((value, end_block))
                }
            }
        }

        self.coalesce();
    }

    fn push(&mut self, value: Value, interval: LiveInterval) {
        self.ranges[value].push(interval);
    }

    fn coalesce(&mut self) {
        for (_, range) in &mut self.ranges {
            range.coalesce();
        }
    }
}

// TODO: Remove Copy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UnhandledInterval {
    value: Value,
    begin: ProgramPoint,
    end: ProgramPoint,
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

    pub fn push(&mut self, value: Value, interval: LiveInterval) {
        self.intervals.push(UnhandledInterval {
            value,
            begin: interval.begin,
            end: interval.end,
        });
    }

    pub fn pop(&mut self) -> Option<UnhandledInterval> {
        self.intervals.pop()
    }
}

#[derive(Debug)]
pub struct RegisterAllocator {}

impl RegisterAllocator {
    pub fn new() -> RegisterAllocator {
        RegisterAllocator {}
    }

    pub fn run(&mut self, intervals: &Intervals) {
        let mut unhandled_intervals = UnhandledIntervals::new();
        for (value, range) in &intervals.ranges {
            for &interval in &range.intervals {
                unhandled_intervals.push(value, interval);
            }
        }

        let mut active_intervals: Vec<UnhandledInterval> = Vec::new();

        while let Some(interval) = unhandled_intervals.pop() {
            active_intervals.retain(|active_interval| interval.begin <= active_interval.end);

            active_intervals.push(interval);

            print!("{}:", interval.begin.0);
            let mut first = true;
            for interval in &active_intervals {
                if first {
                    print!(" {}", interval.value);
                    first = false;
                } else {
                    print!(", {}", interval.value)
                }
            }
            println!("");
        }
    }
}
