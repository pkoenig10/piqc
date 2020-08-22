use crate::collections::SecondaryMap;
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::*;
use std::cmp::Ordering;
use std::cmp::{max, min};
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

// TODO: Remove Clone
#[derive(Debug, Clone, PartialEq, Eq)]
struct LiveInterval {
    begin: ProgramPoint,
    end: ProgramPoint,
}

impl LiveInterval {
    fn extend_begin(&mut self, begin: ProgramPoint) {
        self.begin = min(self.begin, begin);
    }

    fn extend_end(&mut self, end: ProgramPoint) {
        self.end = max(self.end, end);
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct LiveRange {
    intervals: Vec<LiveInterval>,
}

impl LiveRange {
    fn insert(&mut self, interval: LiveInterval) {
        // TODO: this reverse is not intuitive, it exists because we compute live intervals in post-order so larger block IDs are more likely to come first, leading to fewer copies when inserting.
        let index = self.search(interval.begin);

        let (after, before) = self.intervals.split_at_mut(index);
        let mut previous = before.first_mut();
        let mut next = after.last_mut();

        let mut coalesced = false;
        if let Some(ref mut previous) = previous {
            if previous.end + 1 >= interval.begin {
                previous.extend_end(interval.end);
                coalesced = true;
            }
        }
        if let Some(ref mut next) = next {
            if interval.end + 1 >= next.begin {
                next.extend_begin(interval.begin);
                coalesced = true;
            }
        }

        if let (Some(previous), Some(next)) = (previous, next) {
            if previous.end + 1 >= next.begin {
                next.extend_begin(previous.begin);
                coalesced = true;

                self.intervals.remove(index);
            }
        }

        if !coalesced {
            self.intervals.insert(index, interval);
        }
    }

    fn search(&mut self, point: ProgramPoint) -> usize {
        match self
            .intervals
            .binary_search_by(|interval| interval.begin.cmp(&point).reverse())
        {
            Ok(i) => i,
            Err(i) => i,
        }
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
                        self.insert(value, inst.def_point(order), end);
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.insert(value, block.def_point(order), end);
                }
            }

            let mut block_liveins = Vec::new();
            for (value, end) in active.drain() {
                self.insert(value, block.use_point(order), end);
                block_liveins.push(value);
            }
            liveins[block] = Some(block_liveins);
        }

        // Handle back edges
        let mut live: Vec<(Value, Block)> = Vec::new();

        for block in cfg.blocks() {
            let last_point = func
                .layout
                .last_inst(block)
                .expect("Block does not have last inst")
                .def_point(order);

            for &(value, _) in &live {
                self.insert(value, block.use_point(order), last_point);
            }

            live.retain(|&(_, end_block)| end_block != block);

            if let Some(end_block) = back_edges.remove(&block) {
                for &value in liveins[block].as_ref().unwrap() {
                    live.push((value, end_block))
                }
            }
        }
    }

    fn insert(&mut self, value: Value, begin: ProgramPoint, end: ProgramPoint) {
        self.ranges[value].insert(LiveInterval { begin, end });
    }
}

// TODO: Remove Clone
#[derive(Debug, Clone, PartialEq, Eq)]
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
            for interval in &range.intervals {
                unhandled_intervals.push(value, interval.clone());
            }
        }

        let mut active_intervals: Vec<UnhandledInterval> = Vec::new();

        while let Some(interval) = unhandled_intervals.pop() {
            active_intervals.retain(|active_interval| interval.begin <= active_interval.end);

            active_intervals.push(interval.clone());

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
