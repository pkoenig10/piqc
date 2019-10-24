use crate::collections::SecondaryMap;
use crate::ir::{Ebb, Func, Inst};
use std::collections::hash_set;
use std::collections::HashSet;

#[derive(Debug, Default)]
struct Node {
    predecessors: HashSet<Inst>,
    successors: HashSet<Ebb>,
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    data: SecondaryMap<Ebb, Node>,
}

impl ControlFlowGraph {
    fn new() -> ControlFlowGraph {
        ControlFlowGraph {
            data: SecondaryMap::new(),
        }
    }

    pub fn compute(&mut self, func: &Func) {
        for ebb in func.layout.ebbs() {
            for inst in func.layout.insts(ebb) {
                if let Some(target_ebb) = func.data.inst(inst).target() {
                    self.insert_edge(ebb, inst, target_ebb);
                }
            }
        }
    }

    pub fn insert_edge(&mut self, from_ebb: Ebb, from_inst: Inst, to_ebb: Ebb) {
        self.data[from_ebb].successors.insert(to_ebb);
        self.data[to_ebb].predecessors.insert(from_inst);
    }

    pub fn pred_iter(&self, ebb: Ebb) -> PredIter<'_> {
        PredIter {
            iter: self.data[ebb].predecessors.iter(),
        }
    }

    pub fn succ_iter(&self, ebb: Ebb) -> SuccIter<'_> {
        SuccIter {
            iter: self.data[ebb].successors.iter(),
        }
    }
}

pub struct PredIter<'a> {
    iter: hash_set::Iter<'a, Inst>,
}

impl Iterator for PredIter<'_> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        self.iter.next().copied()
    }
}

pub struct SuccIter<'a> {
    iter: hash_set::Iter<'a, Ebb>,
}

impl Iterator for SuccIter<'_> {
    type Item = Ebb;

    fn next(&mut self) -> Option<Ebb> {
        self.iter.next().copied()
    }
}
