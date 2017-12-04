use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockId {
    id: usize,
}

impl Key for BlockId {
    fn new(id: usize) -> Self {
        BlockId { id }
    }

    fn get(&self) -> usize {
        self.id
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "block{}", self.id)
    }
}

#[derive(Debug)]
pub struct Block {
    params: Params<Value>,
}

impl Block {
    pub fn new() -> Block {
        Block { params: Params::new() }
    }

    pub fn push_param(&mut self, value: Value) {
        self.params.push(value);
    }

    pub fn params(&self) -> &Params<Value> {
        &self.params
    }
}
