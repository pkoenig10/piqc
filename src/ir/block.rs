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

#[derive(Debug)]
pub struct BlockData {
    params: Params<Value>,
}

impl BlockData {
    pub fn new() -> BlockData {
        BlockData { params: Params::new() }
    }

    pub fn push_param(&mut self, value: Value) {
        self.params.push(value);
    }

    pub fn params(&self) -> &Params<Value> {
        &self.params
    }
}
