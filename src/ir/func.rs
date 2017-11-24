use collections::*;
use ir::*;

pub type BlockId = Id;

#[derive(Debug)]
pub struct BlockData {
    prev_block: Option<BlockId>,
    next_block: Option<BlockId>,
    first_inst: Option<InstId>,
    last_inst: Option<InstId>,
}

impl BlockData {
    pub fn new() -> BlockData {
        BlockData {
            prev_block: None,
            next_block: None,
            first_inst: None,
            last_inst: None,
        }
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

pub type InstId = Id;

#[derive(Debug)]
struct InstData {
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

#[derive(Debug)]
struct ValueData {
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
    params: Vec<Value>,
    blocks: Map<BlockId, BlockData>,
    insts: Map<InstId, InstData>,
    values: Map<Value, ValueData>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl Func {
    pub fn new() -> Func {
        Func {
            params: Vec::new(),
            values: Map::new(),
            blocks: Map::new(),
            insts: Map::new(),
            first_block: None,
            last_block: None,
        }
    }

    pub fn push_param(&mut self, value: Value) {
        self.params.push(value);
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

    pub fn get_value_type(&self, value: Value) -> Type {
        self.values.get(value).type_()
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
