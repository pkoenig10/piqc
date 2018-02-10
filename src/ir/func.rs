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

    pub fn set_first_inst(&mut self, first_inst: Option<Inst>) {
        self.first_inst = first_inst;
    }

    pub fn last_inst(&self) -> Option<Inst> {
        self.last_inst
    }

    pub fn set_last_inst(&mut self, last_inst: Option<Inst>) {
        self.last_inst = last_inst;
    }
}

#[derive(Debug)]
struct InstNode {
    data: InstData,
    block: Option<Block>,
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
}

impl InstNode {
    pub fn new(data: InstData) -> InstNode {
        InstNode {
            data,
            block: None,
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

    pub fn block(&self) -> Block {
        self.block.unwrap()
    }

    pub fn set_block(&mut self, block: Block) {
        self.block = Some(block)
    }

    pub fn prev_inst(&self) -> Option<Inst> {
        self.prev_inst
    }

    pub fn set_prev_inst(&mut self, prev_inst: Option<Inst>) {
        self.prev_inst = prev_inst;
    }

    pub fn next_inst(&self) -> Option<Inst> {
        self.next_inst
    }

    pub fn set_next_inst(&mut self, next_inst: Option<Inst>) {
        self.next_inst = next_inst;
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
        self.insts.get_mut(inst).set_block(block);

        let block_node = self.blocks.get_mut(block);
        match block_node.last_inst() {
            Some(last_inst) => {
                self.insts.get_mut(last_inst).set_next_inst(Some(inst));
                self.insts.get_mut(inst).set_prev_inst(Some(last_inst));
            }
            None => {
                block_node.set_first_inst(Some(inst));
            }
        }
        block_node.set_last_inst(Some(inst));
    }

    pub fn remove_inst(&mut self, inst: Inst) {
        let (block, prev_inst, next_inst) = {
            let inst_node = self.insts.get(inst);
            (
                inst_node.block(),
                inst_node.prev_inst(),
                inst_node.next_inst(),
            )
        };

        let (first_inst, last_inst) = {
            let block_node = self.blocks.get(block);
            (block_node.first_inst(), block_node.last_inst())
        };

        match prev_inst {
            Some(prev_inst) => {
                self.insts.get_mut(prev_inst).set_next_inst(next_inst);
            }
            None => {
                self.blocks.get_mut(block).set_first_inst(next_inst);
            }
        }

        match next_inst {
            Some(next_inst) => {
                self.insts.get_mut(next_inst).set_prev_inst(prev_inst);
            }
            None => {
                self.blocks.get_mut(block).set_last_inst(prev_inst);
            }
        }
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", self.params)?;
        for block in self.blocks() {
            let block_node = self.block(block);
            writeln!(f, "")?;
            writeln!(f, "{}({}):", block, block_node.params())?;
            for inst in self.insts(block) {
                let inst_node = self.inst(inst);
                writeln!(f, "    {}", inst_node)?;
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
