use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block {
    id: usize,
}

impl Key for Block {
    fn new(id: usize) -> Self {
        Block { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "b{}", self.id)
    }
}

#[derive(Debug, Clone)]
pub struct HeaderBlockData {
    ebb: Ebb,
    predecessors: Vec<(Block, Inst)>,
}

impl HeaderBlockData {
    pub fn new(ebb: Ebb) -> HeaderBlockData {
        HeaderBlockData {
            ebb,
            predecessors: Vec::new(),
        }
    }

    pub fn ebb(&self) -> Ebb {
        self.ebb
    }

    pub fn predecessors(&self) -> &Vec<(Block, Inst)> {
        &self.predecessors
    }

    pub fn insert_predecessor(&mut self, predecessor: (Block, Inst)) {
        self.predecessors.push(predecessor);
    }
}

#[derive(Debug, Clone)]
pub struct BodyBlockData {
    predecessor: Block,
}

impl BodyBlockData {
    pub fn new(predecessor: Block) -> BodyBlockData {
        BodyBlockData { predecessor }
    }

    pub fn predecessor(&self) -> Block {
        self.predecessor
    }
}

#[derive(Debug)]
pub enum BlockData {
    Header(HeaderBlockData),
    Body(BodyBlockData),
}
