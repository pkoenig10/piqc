use crate::collections::SecondaryMap;
use crate::ir::cfg::ControlFlowGraph;
use crate::ir::{Block, Func, Inst, Value};
use std::cmp::Ordering;
use std::collections::HashMap;

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

#[derive(Debug, PartialEq, Eq)]
struct Interval {
    value: Value,
    begin: ProgramPoint,
    end: ProgramPoint,
}

// TODO: Implementing this trait for Interval is weird
impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Interval) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Interval {
    fn cmp(&self, other: &Interval) -> Ordering {
        self.begin.cmp(&other.begin)
    }
}

#[derive(Debug)]
pub struct Intervals {
    intervals: Vec<Interval>,
}

impl Intervals {
    pub fn new() -> Intervals {
        Intervals {
            intervals: Vec::new(),
        }
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
                        self.intervals.push(Interval {
                            value,
                            begin: ProgramPoint::new_def(inst.index(order)),
                            end,
                        })
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.intervals.push(Interval {
                        value,
                        begin: first_point,
                        end,
                    })
                }
            }

            for (value, end) in active.drain() {
                self.intervals.push(Interval {
                    value,
                    begin: first_point,
                    end,
                });
                liveins[block].push(value);
            }
        }

        // TODO: this could be reverse instead (but actually this makes things worse if we want to pop in order)
        // TODO2: we don't need this is we're going to put the unhandled intervals in a binary heap. But maybe we aren't because we aren't spilling?
        // self.intervals.sort();
    }
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
        ProgramPoint(idx << 1 | (point as u32))
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

    // fn get<I>(&self, index: I) -> ProgramIndex
    // where
    //     I: OrderIndex,
    // {
    //     index.index(self)
    // }
}

trait OrderIndex {
    fn index(self, ordering: &Order) -> InstIdx;
}

// impl OrderIndex for Block {
//     fn index(self, ordering: &Order) -> InstIdx {
//         ordering.blocks[self]
//     }
// }

impl OrderIndex for Inst {
    fn index(self, ordering: &Order) -> InstIdx {
        ordering.insts[self]
    }
}

struct ActiveInterval {
    value: Value,
}

#[derive(Debug)]
pub struct RegisterAllocator {}

impl RegisterAllocator {
    pub fn new() -> RegisterAllocator {
        RegisterAllocator {}
    }

    pub fn run(&mut self, intervals: &mut Intervals) {
        while let Some(interval) = intervals.intervals.pop() {}
    }
}
