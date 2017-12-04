use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug)]
pub struct BlockData {
    block: Block,
    prev_block: Option<BlockId>,
    next_block: Option<BlockId>,
    first_inst: Option<InstId>,
    last_inst: Option<InstId>,
}

impl BlockData {
    pub fn new() -> BlockData {
        BlockData {
            block: Block::new(),
            prev_block: None,
            next_block: None,
            first_inst: None,
            last_inst: None,
        }
    }

    pub fn block(&self) -> &Block {
        &self.block
    }

    pub fn block_mut(&mut self) -> &mut Block {
        &mut self.block
    }

    pub fn prev_block(&self) -> Option<BlockId> {
        self.prev_block
    }

    pub fn set_prev_block(&mut self, prev_block: BlockId) {
        self.prev_block = Some(prev_block);
    }

    pub fn next_block(&self) -> Option<BlockId> {
        self.next_block
    }

    pub fn set_next_block(&mut self, next_block: BlockId) {
        self.next_block = Some(next_block);
    }

    pub fn first_inst(&self) -> Option<InstId> {
        self.first_inst
    }

    pub fn set_first_inst(&mut self, first_inst: InstId) {
        self.first_inst = Some(first_inst);
    }

    pub fn last_inst(&self) -> Option<InstId> {
        self.last_inst
    }

    pub fn set_last_inst(&mut self, last_inst: InstId) {
        self.last_inst = Some(last_inst);
    }
}

#[derive(Debug)]
pub struct InstData {
    inst: Inst,
    prev_inst: Option<InstId>,
    next_inst: Option<InstId>,
}

impl InstData {
    pub fn new(inst: Inst) -> InstData {
        InstData {
            inst,
            prev_inst: None,
            next_inst: None,
        }
    }

    pub fn inst(&self) -> &Inst {
        &self.inst
    }

    pub fn prev_inst(&self) -> Option<InstId> {
        self.prev_inst
    }

    pub fn set_prev_inst(&mut self, prev_inst: InstId) {
        self.prev_inst = Some(prev_inst);
    }

    pub fn next_inst(&self) -> Option<InstId> {
        self.next_inst
    }

    pub fn set_next_inst(&mut self, next_inst: InstId) {
        self.next_inst = Some(next_inst);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValueData {
    type_: Type,
}

impl ValueData {
    pub fn new(type_: Type) -> ValueData {
        ValueData { type_ }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }
}

#[derive(Debug)]
pub struct Func {
    params: Params<Type>,
    blocks: Map<BlockId, BlockData>,
    insts: Map<InstId, InstData>,
    values: Map<Value, ValueData>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl Func {
    pub fn new() -> Func {
        Func {
            params: Params::new(),
            values: Map::new(),
            blocks: Map::new(),
            insts: Map::new(),
            first_block: None,
            last_block: None,
        }
    }

    pub fn push_param(&mut self, type_: Type) {
        self.params.push(type_);
    }

    pub fn push_block_param(&mut self, block_id: BlockId, value: Value) {
        self.blocks.get_mut(block_id).block_mut().push_param(value);
    }

    pub fn create_block(&mut self) -> BlockId {
        self.blocks.create(BlockData::new())
    }

    pub fn create_inst(&mut self, inst: Inst) -> InstId {
        self.insts.create(InstData::new(inst))
    }

    pub fn create_value(&mut self, type_: Type) -> Value {
        self.values.create(ValueData::new(type_))
    }

    pub fn block(&self, block_id: BlockId) -> &BlockData {
        self.blocks.get(block_id)
    }

    pub fn blocks(&self) -> BlockIterator {
        BlockIterator::new(self)
    }

    pub fn inst(&self, inst_id: InstId) -> &InstData {
        self.insts.get(inst_id)
    }

    pub fn insts(&self, block_id: BlockId) -> InstIterator {
        InstIterator::new(self, block_id)
    }

    pub fn value(&self, value: Value) -> &ValueData {
        self.values.get(value)
    }

    pub fn push_block(&mut self, block_id: BlockId) {
        match self.last_block {
            Some(last_block) => {
                self.blocks.get_mut(last_block).set_next_block(block_id);
                self.blocks.get_mut(block_id).set_prev_block(last_block);
            }
            None => {
                self.first_block = Some(block_id);
            }
        }
        self.last_block = Some(block_id);
    }

    pub fn push_inst(&mut self, block_id: BlockId, inst_id: InstId) {
        let block_data = self.blocks.get_mut(block_id);
        match block_data.last_inst() {
            Some(last_inst) => {
                self.insts.get_mut(last_inst).set_next_inst(inst_id);
                self.insts.get_mut(inst_id).set_prev_inst(last_inst);
            }
            None => {
                block_data.set_first_inst(inst_id);
            }
        }
        block_data.set_last_inst(inst_id);
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", self.params)?;
        for block_id in self.blocks() {
            let block = self.blocks.get(block_id).block();
            writeln!(f, "")?;
            writeln!(f, "{}({}):", block_id, block.params())?;
            for inst_id in self.insts(block_id) {
                let inst = self.insts.get(inst_id).inst();
                writeln!(f, "    {}", inst)?;
            }
        }
        Ok(())
    }
}

pub struct BlockIterator<'a> {
    func: &'a Func,
    next: Option<BlockId>,
}

impl<'a> BlockIterator<'a> {
    fn new(func: &Func) -> BlockIterator {
        BlockIterator {
            func,
            next: func.first_block,
        }
    }
}

impl<'a> Iterator for BlockIterator<'a> {
    type Item = BlockId;

    fn next(&mut self) -> Option<BlockId> {
        match self.next {
            Some(block_id) => {
                self.next = self.func.blocks.get(block_id).next_block();
                Some(block_id)
            }
            None => None,
        }
    }
}

pub struct InstIterator<'a> {
    func: &'a Func,
    next: Option<InstId>,
}

impl<'a> InstIterator<'a> {
    fn new(func: &Func, block_id: BlockId) -> InstIterator {
        InstIterator {
            func,
            next: func.blocks.get(block_id).first_inst(),
        }
    }
}

impl<'a> Iterator for InstIterator<'a> {
    type Item = InstId;

    fn next(&mut self) -> Option<InstId> {
        match self.next {
            Some(inst_id) => {
                self.next = self.func.insts.get(inst_id).next_inst();
                Some(inst_id)
            }
            None => None,
        }
    }
}
