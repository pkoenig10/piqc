use std::fmt;

use ir::*;

#[derive(Debug)]
pub struct Target {
    block: Block,
    args: Params<Value>,
}

impl Target {
    pub fn new(block: Block, args: Params<Value>) -> Target {
        Target { block, args: args }
    }

    pub fn block(&self) -> Block {
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
