use std::fmt;

use collections::*;
use ir::*;

#[derive(Debug)]
struct BlockNode {
    data: BlockData,
    prev_block: Option<Block>,
    next_block: Option<Block>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

impl BlockNode {
    pub fn new() -> BlockNode {
        BlockNode {
            data: BlockData::new(),
            prev_block: None,
            next_block: None,
            first_inst: None,
            last_inst: None,
        }
    }

    pub fn data(&self) -> &BlockData {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut BlockData {
        &mut self.data
    }

    pub fn prev_block(&self) -> Option<Block> {
        self.prev_block
    }

    pub fn set_prev_block(&mut self, prev_block: Block) {
        self.prev_block = Some(prev_block);
    }

    pub fn next_block(&self) -> Option<Block> {
        self.next_block
    }

    pub fn set_next_block(&mut self, next_block: Block) {
        self.next_block = Some(next_block);
    }

    pub fn first_inst(&self) -> Option<Inst> {
        self.first_inst
    }

    pub fn set_first_inst(&mut self, first_inst: Inst) {
        self.first_inst = Some(first_inst);
    }

    pub fn last_inst(&self) -> Option<Inst> {
        self.last_inst
    }

    pub fn set_last_inst(&mut self, last_inst: Inst) {
        self.last_inst = Some(last_inst);
    }
}

#[derive(Debug)]
struct InstNode {
    data: InstData,
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
}

impl InstNode {
    pub fn new(data: InstData) -> InstNode {
        InstNode {
            data,
            prev_inst: None,
            next_inst: None,
        }
    }

    pub fn data(&self) -> &InstData {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut InstData {
        &mut self.data
    }

    pub fn prev_inst(&self) -> Option<Inst> {
        self.prev_inst
    }

    pub fn set_prev_inst(&mut self, prev_inst: Inst) {
        self.prev_inst = Some(prev_inst);
    }

    pub fn next_inst(&self) -> Option<Inst> {
        self.next_inst
    }

    pub fn set_next_inst(&mut self, next_inst: Inst) {
        self.next_inst = Some(next_inst);
    }
}

#[derive(Debug)]
pub struct Func {
    params: Params<Type>,
    blocks: Map<Block, BlockNode>,
    insts: Map<Inst, InstNode>,
    values: Map<Value, ValueData>,
    first_block: Option<Block>,
    last_block: Option<Block>,
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

    pub fn push_block_param(&mut self, block: Block, value: Value) {
        self.blocks.get_mut(block).data_mut().push_param(value);
    }

    pub fn create_block(&mut self) -> Block {
        self.blocks.create(BlockNode::new())
    }

    pub fn create_inst(&mut self, data: InstData) -> Inst {
        self.insts.create(InstNode::new(data))
    }

    pub fn create_value(&mut self, type_: Option<Type>) -> Value {
        self.values.create(ValueData::new(type_))
    }

    pub fn block(&self, block: Block) -> &BlockData {
        self.blocks.get(block).data()
    }

    pub fn last_inst(&self, block: Block) -> Inst {
        self.blocks.get(block).last_inst().unwrap()
    }

    pub fn blocks(&self) -> BlockIterator {
        BlockIterator::new(self)
    }

    pub fn inst(&self, inst: Inst) -> &InstData {
        self.insts.get(inst).data()
    }

    pub fn inst_mut(&mut self, inst: Inst) -> &mut InstData {
        self.insts.get_mut(inst).data_mut()
    }

    pub fn insts(&self, block: Block) -> InstIterator {
        InstIterator::new(self, block)
    }

    pub fn value(&self, value: Value) -> &ValueData {
        self.values.get(value)
    }

    pub fn value_mut(&mut self, value: Value) -> &mut ValueData {
        self.values.get_mut(value)
    }

    pub fn push_block(&mut self, block: Block) {
        match self.last_block {
            Some(last_block) => {
                self.blocks.get_mut(last_block).set_next_block(block);
                self.blocks.get_mut(block).set_prev_block(last_block);
            }
            None => {
                self.first_block = Some(block);
            }
        }
        self.last_block = Some(block);
    }

    pub fn push_inst(&mut self, block: Block, inst: Inst) {
        let block_data = self.blocks.get_mut(block);
        match block_data.last_inst() {
            Some(last_inst) => {
                self.insts.get_mut(last_inst).set_next_inst(inst);
                self.insts.get_mut(inst).set_prev_inst(last_inst);
            }
            None => {
                block_data.set_first_inst(inst);
            }
        }
        block_data.set_last_inst(inst);
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", self.params)?;
        for block in self.blocks() {
            let block_data = self.block(block);
            writeln!(f, "")?;
            writeln!(f, "{}({}):", block, block_data.params())?;
            for inst in self.insts(block) {
                let inst_data = self.inst(inst);
                writeln!(f, "    {}", inst_data)?;
            }
        }
        Ok(())
    }
}

pub struct BlockIterator<'a> {
    func: &'a Func,
    next: Option<Block>,
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
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        match self.next {
            Some(block) => {
                self.next = self.func.blocks.get(block).next_block();
                Some(block)
            }
            None => None,
        }
    }
}

pub struct InstIterator<'a> {
    func: &'a Func,
    next: Option<Inst>,
}

impl<'a> InstIterator<'a> {
    fn new(func: &Func, block: Block) -> InstIterator {
        InstIterator {
            func,
            next: func.blocks.get(block).first_inst(),
        }
    }
}

impl<'a> Iterator for InstIterator<'a> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        match self.next {
            Some(inst) => {
                self.next = self.func.insts.get(inst).next_inst();
                Some(inst)
            }
            None => None,
        }
    }
}
