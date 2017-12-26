use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        write!(f, "b{}", self.id)
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

#[derive(Debug)]
pub struct Target {
    block: BlockId,
    args: Params<Value>,
}

impl Target {
    pub fn new(block: BlockId, args: Params<Value>) -> Target {
        Target {
            block,
            args: args,
        }
    }

    pub fn block(&self) -> BlockId {
        self.block
    }

    pub fn push_arg(&mut self, value: Value) {
        self.args.push(value);
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.block,
            self.args,
        )
    }
}
