use crate::collections::{SecondaryMap, UnionFind};
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::*;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::mem;
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
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
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

impl fmt::Debug for ProgramPoint {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.0)
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
    fn use_point(self, order: &Order) -> ProgramPoint;

    fn def_point(self, order: &Order) -> ProgramPoint;
}

impl OrderIndex for Block {
    fn use_point(self, order: &Order) -> ProgramPoint {
        order.blocks[self]
    }

    fn def_point(self, order: &Order) -> ProgramPoint {
        order.blocks[self] + 1
    }
}

impl OrderIndex for Inst {
    fn use_point(self, order: &Order) -> ProgramPoint {
        order.insts[self]
    }

    fn def_point(self, order: &Order) -> ProgramPoint {
        order.insts[self] + 1
    }
}

impl OrderIndex for ValueDef {
    fn use_point(self, order: &Order) -> ProgramPoint {
        match self {
            ValueDef::Inst(inst) => inst.use_point(order),
            ValueDef::Param(block) => block.use_point(order),
        }
    }

    fn def_point(self, order: &Order) -> ProgramPoint {
        match self {
            ValueDef::Inst(inst) => inst.def_point(order),
            ValueDef::Param(block) => block.def_point(order),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct LiveRange {
    start: ProgramPoint,
    end: ProgramPoint,
}

impl LiveRange {
    fn cmp<T>(&self, value: &T) -> Ordering
    where
        T: LiveIntervalOrd,
    {
        value.ord(&self).reverse()
    }
}

impl fmt::Debug for LiveRange {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}..{:?}", self.start, self.end)
    }
}

trait LiveIntervalOrd {
    fn ord(&self, range: &LiveRange) -> Ordering;
}

impl LiveIntervalOrd for ProgramPoint {
    fn ord(&self, range: &LiveRange) -> Ordering {
        if *self < range.start {
            Ordering::Less
        } else if range.end < *self {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

impl LiveIntervalOrd for LiveRange {
    fn ord(&self, range: &LiveRange) -> Ordering {
        if self.end < range.start {
            Ordering::Less
        } else if range.end < self.start {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct LiveInterval {
    ranges: Vec<LiveRange>,
}

impl LiveInterval {
    fn contains<T>(&self, value: T) -> bool
    where
        T: LiveIntervalOrd,
    {
        self.ranges
            .binary_search_by(|range| range.cmp(&value))
            .is_ok()
    }

    fn push(&mut self, range: LiveRange) {
        self.ranges.push(range);
    }

    fn merge(&mut self) {
        self.ranges.sort_unstable();

        let mut len = 1;
        for i in 1..self.ranges.len() {
            let range = self.ranges[i];

            let previous = &mut self.ranges[len - 1];
            if range.start <= previous.end + 1 {
                previous.end = range.end;
                continue;
            }

            self.ranges[len] = range;
            len += 1;
        }

        self.ranges.truncate(len);
        self.ranges.shrink_to_fit();
    }
}

// impl fmt::Debug for LiveInterval {
//     fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
//         fmt.debug_list().entries(self.ranges.iter()).finish()
//     }
// }

#[derive(Debug)]
pub struct Liveness {
    intervals: SecondaryMap<Value, LiveInterval>,
}

impl Liveness {
    pub fn new() -> Liveness {
        Liveness {
            intervals: SecondaryMap::new(),
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
                            LiveRange {
                                start: inst.def_point(order),
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
                        LiveRange {
                            start: block.def_point(order),
                            end,
                        },
                    );
                }
            }

            let mut block_liveins = Vec::new();
            for (value, end) in active.drain() {
                self.push(
                    value,
                    LiveRange {
                        start: block.use_point(order),
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
                    LiveRange {
                        start: block.use_point(order),
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

        self.merge();
    }

    pub fn apply(&mut self, hints: &RegisterHints) {
        let mut intervals = Vec::new();
        let mut modified = HashSet::new();

        for (value, interval) in &mut self.intervals {
            let hint = hints.get(value);
            if hint != value {
                intervals.push((hint, mem::take(interval)));
                modified.insert(hint);
            }
        }

        for interval in intervals {}

        for value in modified {
            self.intervals[value].merge();
        }
    }

    fn live_at(&self, value: Value, point: ProgramPoint) -> bool {
        self.intervals[value].contains(point)
    }

    fn push(&mut self, value: Value, interval: LiveRange) {
        self.intervals[value].push(interval);
    }

    fn merge(&mut self) {
        for (_, interval) in &mut self.intervals {
            interval.merge();
        }
    }

    pub fn print(&self) {
        for (value, interval) in &self.intervals {
            if interval.ranges.is_empty() {
                continue;
            }

            println!("{}:", value);
            for range in &interval.ranges {
                println!("  ({}, {})", range.start.0, range.end.0);
            }
        }
    }
}

#[derive(Debug)]
pub struct RegisterHints {
    hints: UnionFind<Value>,
}

impl RegisterHints {
    pub fn new() -> RegisterHints {
        RegisterHints {
            hints: UnionFind::new(),
        }
    }

    pub fn get(&self, value: Value) -> Value {
        self.hints.find(value)
    }

    pub fn compute(
        &mut self,
        func: &Func,
        cfg: &ControlFlowGraph,
        order: &Order,
        liveness: &Liveness,
    ) {
        for block in cfg.blocks() {
            let params = func.data.block_params(block);

            for inst in cfg.preds(block) {
                let args = func.data.inst(inst).target_args();

                debug_assert!(params.len() == args.len());

                for (&param, &arg) in params.iter().zip(args) {
                    let param_def_point = func.data.value_def(param).def_point(order);
                    let arg_def_point = func.data.value_def(arg).def_point(order);

                    if liveness.live_at(param, arg_def_point)
                        || liveness.live_at(arg, param_def_point)
                    {
                        continue;
                    }

                    // TODO: this is probably wrong because wwe don't consider the live range of the entire union-find group
                    self.hints.union(param, arg);
                }
            }
        }

        print!("{:#?}", self.hints);

        for leader in self.hints.leaders() {
            print!("{}: ", leader);
            for value in self.hints.elements(leader) {
                print!("{}, ", value);
            }
            println!("");
        }
    }
}

pub struct VirtualRegisters {}

impl VirtualRegisters {
    pub fn compute(func: &Func, cfg: &ControlFlowGraph, order: &Order, liveness: &Liveness) {
        let mut isolated_params = Vec::new();
        let mut isolated_args = Vec::new();

        for block in cfg.blocks() {
            let params = func.data.block_params(block);

            for inst in cfg.preds(block) {
                let args = func.data.inst(inst).target_args();

                debug_assert!(params.len() == args.len());

                for (i, (&param, &arg)) in params.iter().zip(args).enumerate() {
                    let param_def_point = func.data.value_def(param).def_point(order);
                    let arg_def_point = func.data.value_def(arg).def_point(order);

                    if liveness.live_at(param, arg_def_point) {
                        println!("Param {} live at arg {} ({})", param, arg, arg_def_point.0);
                        isolated_params.push((inst, i));
                    }

                    if liveness.live_at(arg, param_def_point) {
                        println!("Arg {} ({}) live at param {}", arg, arg_def_point.0, param);
                        isolated_args.push((block, i));
                    }
                }
            }
        }

        println!("");
    }
}

// TODO: Remove Copy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UnhandledInterval {
    value: Value,
    start: ProgramPoint,
    end: ProgramPoint,
}

impl PartialOrd for UnhandledInterval {
    fn partial_cmp(&self, other: &UnhandledInterval) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for UnhandledInterval {
    fn cmp(&self, other: &UnhandledInterval) -> Ordering {
        self.start.cmp(&other.start).reverse()
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

    pub fn push(&mut self, value: Value, interval: LiveRange) {
        self.intervals.push(UnhandledInterval {
            value,
            start: interval.start,
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

    pub fn run(&mut self, liveness: &Liveness) {
        let mut unhandled_intervals = UnhandledIntervals::new();
        for (value, range) in &liveness.intervals {
            for &interval in &range.ranges {
                unhandled_intervals.push(value, interval);
            }
        }

        let mut active_intervals: Vec<UnhandledInterval> = Vec::new();

        while let Some(interval) = unhandled_intervals.pop() {
            active_intervals.retain(|active_interval| interval.start <= active_interval.end);

            active_intervals.push(interval);

            print!("{}:", interval.start.0);
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
