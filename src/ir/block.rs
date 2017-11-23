use std::collections::HashSet;

use ir::*;

#[derive(Debug)]
pub struct Block {
    id: u32,
    insts: Vec<Inst>,
}

impl Block {
    pub fn new(id: u32) -> Block {
        Block {
            id,
            insts: Vec::new(),
        }
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.insts.push(inst);
    }
}
