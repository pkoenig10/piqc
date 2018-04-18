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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Predecessor {
    block: Block,
    inst: Inst,
}

impl Predecessor {
    pub fn new(block: Block, inst: Inst) -> Predecessor {
        Predecessor { block, inst }
    }

    pub fn block(&self) -> Block {
        self.block
    }

    pub fn inst(&self) -> Inst {
        self.inst
    }
}

#[derive(Debug, Clone)]
pub struct HeaderBlock {
    ebb: Ebb,
    predecessors: Vec<Predecessor>,
}

impl HeaderBlock {
    pub fn new(ebb: Ebb) -> HeaderBlock {
        HeaderBlock {
            ebb,
            predecessors: Vec::new(),
        }
    }

    pub fn ebb(&self) -> Ebb {
        self.ebb
    }

    pub fn predecessors(&self) -> &[Predecessor] {
        &self.predecessors
    }

    pub fn insert_predecessor(&mut self, predecessor: Predecessor) {
        self.predecessors.push(predecessor);
    }
}

#[derive(Debug, Clone)]
pub struct BodyBlock {
    predecessor: Block,
}

impl BodyBlock {
    pub fn new(predecessor: Block) -> BodyBlock {
        BodyBlock { predecessor }
    }

    pub fn predecessor(&self) -> Block {
        self.predecessor
    }
}

#[derive(Debug)]
pub enum BlockData {
    Header(HeaderBlock),
    Body(BodyBlock),
}
