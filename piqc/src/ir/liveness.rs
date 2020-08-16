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
    begin: ProgramIndex,
    end: Option<ProgramIndex>,
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
    liveins: SecondaryMap<Block, Vec<Value>>,
}

impl Intervals {
    pub fn new() -> Intervals {
        Intervals {
            intervals: Vec::new(),
            liveins: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph, order: &Order) {
        let mut active: HashMap<Value, Option<ProgramIndex>> = HashMap::new();

        for block in cfg.blocks().rev() {
            debug_assert!(active.is_empty());

            for succ in cfg.succs(block) {
                for &value in &self.liveins[succ] {
                    active.entry(value).or_insert(None);
                }
            }

            for inst in func.layout.insts(block).rev() {
                for &value in func.data.inst(inst).args() {
                    // In the future we could add uses here
                    active.entry(value).or_insert(Some(inst.index(order)));
                }

                if let Some(value) = func.data.inst_result(inst) {
                    if let Some(end) = active.remove(&value) {
                        self.intervals.push(Interval {
                            value,
                            begin: inst.index(order),
                            end,
                        })
                    }
                }
            }

            for &value in func.data.block_params(block) {
                if let Some(end) = active.remove(&value) {
                    self.intervals.push(Interval {
                        value,
                        begin: block.index(order),
                        end,
                    })
                }
            }

            for (value, end) in active.drain() {
                self.intervals.push(Interval {
                    value,
                    begin: block.index(order),
                    end,
                });
                self.liveins[block].push(value);
            }
        }

        // TODO: this could be reverse instead (but actually this makes things worse if we want to pop in order)
        // TODO2: we don't need this is we're going to put the unhandled intervals in a binary heap. But maybe we aren't because we aren't spilling?
        // self.intervals.sort();
    }
}

// TODO: this default can cause weird behavior, maybe make explicit invalid value?
type ProgramIndex = u32;

#[derive(Debug)]
pub struct Order {
    blocks: SecondaryMap<Block, ProgramIndex>,
    insts: SecondaryMap<Inst, ProgramIndex>,
}

impl Order {
    pub fn new() -> Order {
        Order {
            blocks: SecondaryMap::new(),
            insts: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func, cfg: &ControlFlowGraph) {
        let mut index = 0;

        for block in cfg.blocks() {
            self.blocks[block] = index;
            index += 1;

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
    fn index(self, ordering: &Order) -> ProgramIndex;
}

impl OrderIndex for Block {
    fn index(self, ordering: &Order) -> ProgramIndex {
        ordering.blocks[self]
    }
}

impl OrderIndex for Inst {
    fn index(self, ordering: &Order) -> ProgramIndex {
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
