use crate::collections::{SecondaryMap, Set};
use crate::ir::{Block, Func};
use std::iter;
use std::slice;

#[derive(Debug, Default)]
struct Node {
    predecessors: Vec<Block>,
    successors: Vec<Block>,
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    nodes: SecondaryMap<Block, Node>,
    rpo: Vec<Block>,
}

// TODO: Add validity checks?
impl ControlFlowGraph {
    pub fn new() -> ControlFlowGraph {
        ControlFlowGraph {
            nodes: SecondaryMap::new(),
            rpo: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
        self.rpo.clear();
    }

    pub fn compute(&mut self, func: &Func) {
        for block in func.layout.blocks() {
            for inst in func.layout.insts(block).rev().take(2) {
                if let Some(target_block) = func.data.inst(inst).target() {
                    self.nodes[block].successors.push(target_block);
                    self.nodes[target_block].predecessors.push(block);
                }
            }
        }

        enum Visit {
            Pre(Block),
            Post(Block),
        }

        let mut stack = Vec::new();
        let mut visited = Set::new();

        if let Some(first_block) = func.layout.first_block() {
            stack.push(Visit::Pre(first_block));
        }

        while let Some(visit) = stack.pop() {
            match visit {
                Visit::Pre(block) => {
                    visited.insert(block);

                    stack.push(Visit::Post(block));

                    for &successor in &self.nodes[block].successors {
                        if !visited.contains(successor) {
                            stack.push(Visit::Pre(successor));
                        }
                    }
                }
                Visit::Post(block) => {
                    self.rpo.push(block);
                }
            }
        }

        self.rpo.reverse();
    }

    pub fn blocks(&self) -> Blocks {
        Blocks(self.rpo.iter().copied())
    }

    pub fn preds(&self, block: Block) -> Preds {
        Preds(self.nodes[block].predecessors.iter().copied())
    }

    pub fn succs(&self, block: Block) -> Succs {
        Succs(self.nodes[block].successors.iter().copied())
    }
}

pub struct Blocks<'a>(iter::Copied<slice::Iter<'a, Block>>);

impl Iterator for Blocks<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        self.0.next()
    }
}

impl DoubleEndedIterator for Blocks<'_> {
    fn next_back(&mut self) -> Option<Block> {
        self.0.next_back()
    }
}

pub struct Preds<'a>(iter::Copied<slice::Iter<'a, Block>>);

impl Iterator for Preds<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        self.0.next()
    }
}

pub struct Succs<'a>(iter::Copied<slice::Iter<'a, Block>>);

impl Iterator for Succs<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        self.0.next()
    }
}
