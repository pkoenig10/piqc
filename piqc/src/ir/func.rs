use crate::collections::{PrimaryMap, SecondaryMap};
use crate::ir::*;
use std::fmt;

#[derive(Debug)]
struct BlockData {
    params: Vec<Value>,
}

#[derive(Debug)]
enum ValueData {
    Inst(Type, Inst),
    Param(Type, Block, usize),
    Alias(Type, Value),
}

impl PrimaryMap<Value, ValueData> {
    pub fn resolve_alias(&self, value: Value) -> Value {
        let mut current_value = value;
        for _ in 0..=self.len() {
            match self[current_value] {
                ValueData::Inst(..) | ValueData::Param(..) => return current_value,
                ValueData::Alias(_, value) => current_value = value,
            }
        }

        panic!("Alias loop detected for {}", value);
    }
}

#[derive(Debug)]
pub struct FuncData {
    params: Vec<Type>,
    blocks: PrimaryMap<Block, BlockData>,
    insts: PrimaryMap<Inst, InstData>,
    values: PrimaryMap<Value, ValueData>,
    results: SecondaryMap<Inst, Option<Value>>,
}

impl FuncData {
    fn new() -> FuncData {
        FuncData {
            params: Vec::new(),
            blocks: PrimaryMap::new(),
            insts: PrimaryMap::new(),
            values: PrimaryMap::new(),
            results: SecondaryMap::new(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let data = BlockData { params: Vec::new() };
        self.blocks.create(data)
    }

    pub fn create_inst(&mut self, data: InstData) -> Inst {
        self.insts.create(data)
    }

    pub fn create_inst_result(&mut self, inst: Inst, ty: Type) -> Value {
        debug_assert!(
            self.inst_result(inst).is_none(),
            "Instruction {} already has result",
        );

        let value = self.values.create(ValueData::Inst(ty, inst));
        self.results.insert(inst, Some(value));
        value
    }

    pub fn params(&self) -> &[Type] {
        &self.params
    }

    pub fn push_param(&mut self, ty: Type) {
        self.params.push(ty);
    }

    pub fn block_params(&self, block: Block) -> &[Value] {
        &self.blocks[block].params
    }

    pub fn push_block_param(&mut self, block: Block, ty: Type) -> Value {
        let idx = self.blocks[block].params.len();
        let value = self.values.create(ValueData::Param(ty, block, idx));
        self.blocks[block].params.push(value);
        value
    }

    pub fn swap_remove_block_param(&mut self, value: Value) {
        let (block, idx) = match self.values[value] {
            ValueData::Param(_, block, idx) => (block, idx),
            _ => panic!("Value {} is not a block parameter", value),
        };

        let params = &mut self.blocks[block].params;

        params.swap_remove(idx);

        if let Some(&swapped_value) = params.get(idx) {
            match &mut self.values[swapped_value] {
                ValueData::Param(_, _, swapped_idx) => {
                    *swapped_idx = idx;
                }
                _ => panic!("Value {} is not a block parameter", swapped_value),
            }
        }
    }

    pub fn inst(&self, inst: Inst) -> &InstData {
        &self.insts[inst]
    }

    pub fn inst_mut(&mut self, inst: Inst) -> &mut InstData {
        &mut self.insts[inst]
    }

    pub fn inst_result(&self, inst: Inst) -> Option<Value> {
        self.results[inst]
    }

    pub fn value_type(&self, value: Value) -> Type {
        match self.values[value] {
            ValueData::Inst(ty, ..) | ValueData::Param(ty, ..) | ValueData::Alias(ty, ..) => ty,
        }
    }

    pub fn value_to_alias(&mut self, value: Value, dest: Value) {
        debug_assert!(
            !self.is_value_attached(value),
            "Value {} is attached",
            value
        );

        let resolved_dest = self.resolve_alias(dest);
        debug_assert_ne!(
            value, resolved_dest,
            "Aliasing {} for {} would create a loop",
            value, dest,
        );

        let value_type = self.value_type(value);
        let dest_type = self.value_type(resolved_dest);
        debug_assert_eq!(
            value_type, dest_type,
            "Aliasing {} to {} would change type from {} to {}",
            value, dest, value_type, dest_type,
        );

        let ty = self.value_type(value);
        self.values[value] = ValueData::Alias(ty, dest);
    }

    pub fn resolve_alias(&self, value: Value) -> Value {
        self.values.resolve_alias(value)
    }

    pub fn resolve_aliases_in_inst(&mut self, inst: Inst) {
        for arg in self.insts[inst].args_mut() {
            let resolved_arg = self.values.resolve_alias(*arg);
            if resolved_arg != *arg {
                *arg = resolved_arg
            }
        }
    }

    fn is_value_attached(&self, value: Value) -> bool {
        match self.values[value] {
            ValueData::Inst(_, inst) => self.inst_result(inst) == Some(value),
            ValueData::Param(_, block, idx) => self.block_params(block).get(idx) == Some(&value),
            ValueData::Alias(..) => false,
        }
    }
}

#[derive(Debug, Default)]
struct BlockNode {
    prev_block: Option<Block>,
    next_block: Option<Block>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

#[derive(Debug, Default)]
struct InstNode {
    block: Option<Block>,
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
}

#[derive(Debug)]
pub struct FuncLayout {
    blocks: SecondaryMap<Block, BlockNode>,
    insts: SecondaryMap<Inst, InstNode>,
    first_block: Option<Block>,
    last_block: Option<Block>,
}

impl FuncLayout {
    pub fn new() -> FuncLayout {
        FuncLayout {
            blocks: SecondaryMap::new(),
            insts: SecondaryMap::new(),
            first_block: None,
            last_block: None,
        }
    }

    pub fn blocks(&self) -> BlockIter<'_> {
        BlockIter {
            blocks: &self.blocks,
            next: self.first_block,
        }
    }

    pub fn first_block(&self) -> Option<Block> {
        self.first_block
    }

    pub fn last_block(&self) -> Option<Block> {
        self.last_block
    }

    pub fn is_block_inserted(&self, block: Block) -> bool {
        self.first_block == Some(block) || self.blocks[block].prev_block.is_some()
    }

    pub fn push_block(&mut self, block: Block) {
        let block_node = &mut self.blocks[block];

        block_node.prev_block = self.last_block;

        match self.last_block {
            Some(prev_block) => {
                self.blocks[prev_block].next_block = Some(block);
            }
            None => {
                self.first_block = Some(block);
            }
        }
        self.last_block = Some(block);
    }

    pub fn remove_block(&mut self, block: Block) {
        let block_node = &mut self.blocks[block];

        let prev_block = block_node.prev_block;
        let next_block = block_node.next_block;
        block_node.prev_block = None;
        block_node.next_block = None;

        match prev_block {
            Some(prev_block) => {
                self.blocks[prev_block].next_block = next_block;
            }
            None => {
                self.first_block = next_block;
            }
        }
        match next_block {
            Some(next_block) => {
                self.blocks[next_block].prev_block = prev_block;
            }
            None => {
                self.last_block = prev_block;
            }
        }
    }

    pub fn insts(&self, block: Block) -> InstIter<'_> {
        InstIter {
            insts: &self.insts,
            next: self.blocks[block].first_inst,
        }
    }

    pub fn first_inst(&self, block: Block) -> Option<Inst> {
        self.blocks[block].first_inst
    }

    pub fn last_inst(&self, block: Block) -> Option<Inst> {
        self.blocks[block].last_inst
    }

    pub fn inst_block(&self, inst: Inst) -> Option<Block> {
        self.insts[inst].block
    }

    pub fn prev_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].prev_inst
    }

    pub fn next_inst(&self, inst: Inst) -> Option<Inst> {
        self.insts[inst].next_inst
    }

    pub fn push_inst(&mut self, block: Block, inst: Inst) {
        let block_node = &mut self.blocks[block];
        let inst_node = &mut self.insts[inst];

        inst_node.block = Some(block);
        inst_node.prev_inst = block_node.last_inst;

        match block_node.last_inst {
            Some(prev_inst) => {
                self.insts[prev_inst].next_inst = Some(inst);
            }
            None => {
                block_node.first_inst = Some(inst);
            }
        }
        block_node.last_inst = Some(inst);
    }

    pub fn remove_inst(&mut self, inst: Inst) {
        let inst_node = &mut self.insts[inst];

        let block = inst_node.block;
        let prev_inst = inst_node.prev_inst;
        let next_inst = inst_node.next_inst;
        inst_node.block = None;
        inst_node.prev_inst = None;
        inst_node.next_inst = None;

        if let Some(block) = block {
            match prev_inst {
                Some(prev_inst) => {
                    self.insts[prev_inst].next_inst = next_inst;
                }
                None => {
                    self.blocks[block].first_inst = next_inst;
                }
            }

            match next_inst {
                Some(next_inst) => {
                    self.insts[next_inst].prev_inst = prev_inst;
                }
                None => {
                    self.blocks[block].last_inst = prev_inst;
                }
            }
        }
    }
}

pub struct BlockIter<'a> {
    blocks: &'a SecondaryMap<Block, BlockNode>,
    next: Option<Block>,
}

impl Iterator for BlockIter<'_> {
    type Item = Block;

    fn next(&mut self) -> Option<Block> {
        match self.next {
            Some(block) => {
                self.next = self.blocks[block].next_block;
                Some(block)
            }
            None => None,
        }
    }
}

pub struct InstIter<'a> {
    insts: &'a SecondaryMap<Inst, InstNode>,
    next: Option<Inst>,
}

impl Iterator for InstIter<'_> {
    type Item = Inst;

    fn next(&mut self) -> Option<Inst> {
        match self.next {
            Some(inst) => {
                self.next = self.insts[inst].next_inst;
                Some(inst)
            }
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct Func {
    pub data: FuncData,
    pub layout: FuncLayout,
}

impl Func {
    pub fn new() -> Func {
        Func {
            data: FuncData::new(),
            layout: FuncLayout::new(),
        }
    }

    pub fn resolve_aliases(&mut self) {
        for block in self.layout.blocks() {
            for inst in self.layout.insts(block) {
                self.data.resolve_aliases_in_inst(inst);
            }
        }
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "func({}):", DisplaySlice(&self.data.params()))?;
        for block in self.layout.blocks() {
            writeln!(f)?;
            writeln!(
                f,
                "{}({}):",
                block,
                DisplaySlice(self.data.block_params(block))
            )?;
            for inst in self.layout.insts(block) {
                write!(f, "    ")?;
                if let Some(result) = self.data.inst_result(inst) {
                    write!(f, "{} = ", result)?;
                }
                let data = self.data.inst(inst);
                writeln!(f, "{}", data)?;
            }
        }
        Ok(())
    }
}
