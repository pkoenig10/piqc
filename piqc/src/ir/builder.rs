use std::collections::HashMap;
use std::collections::HashSet;

use crate::ir::*;
use crate::util::Map;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Predecessor {
    block: Block,
    inst: Inst,
}

impl Predecessor {
    pub fn new(block: Block, inst: Inst) -> Predecessor {
        Predecessor { block, inst }
    }
}

#[derive(Debug, Clone)]
struct HeaderBlock {
    ebb: Ebb,
    predecessors: Vec<Predecessor>,
}

impl HeaderBlock {
    pub fn new(ebb: Ebb) -> HeaderBlock {
        HeaderBlock {
            ebb,
            predecessors: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct BodyBlock {
    predecessor: Block,
}

impl BodyBlock {
    pub fn new(predecessor: Block) -> BodyBlock {
        BodyBlock { predecessor }
    }
}

#[derive(Debug)]
enum BlockData {
    Header(HeaderBlock),
    Body(BodyBlock),
}

#[derive(Debug)]
struct BlockValues {
    params: HashMap<Value, Variable>,
    values: HashMap<Variable, Value>,
}

impl BlockValues {
    fn new() -> BlockValues {
        BlockValues {
            params: HashMap::new(),
            values: HashMap::new(),
        }
    }

    fn get_param(&self, value: Value) -> Option<Variable> {
        self.params.get(&value).cloned()
    }

    fn insert_param(&mut self, variable: Variable, value: Value) {
        self.params.insert(value, variable);
        self.insert_value(variable, value);
    }

    fn get_value(&self, variable: Variable) -> Option<Value> {
        self.values.get(&variable).cloned()
    }

    fn insert_value(&mut self, variable: Variable, value: Value) {
        self.values.insert(variable, value);
    }
}

#[derive(Debug)]
struct ValueTable {
    values: HashMap<Block, BlockValues>,
}

impl ValueTable {
    fn new() -> ValueTable {
        ValueTable {
            values: HashMap::new(),
        }
    }

    fn insert_param(&mut self, block: Block, variable: Variable, value: Value) {
        self.get_values_mut(block).insert_param(variable, value);
    }

    fn get_param(&self, block: Block, value: Value) -> Option<Variable> {
        self.get_values(block)
            .and_then(|values| values.get_param(value))
    }

    fn insert_value(&mut self, block: Block, variable: Variable, value: Value) {
        self.get_values_mut(block).insert_value(variable, value);
    }

    fn get_value(&self, block: Block, variable: Variable) -> Option<Value> {
        self.get_values(block)
            .and_then(|values| values.get_value(variable))
    }

    fn get_values(&self, block: Block) -> Option<&BlockValues> {
        self.values.get(&block)
    }

    fn get_values_mut(&mut self, block: Block) -> &mut BlockValues {
        self.values.entry(block).or_insert_with(BlockValues::new)
    }
}

#[derive(Clone, Copy)]
struct Position {
    pub ebb: Ebb,
    pub block: Block,
}

impl Position {
    fn new(ebb: Ebb, block: Block) -> Position {
        Position { ebb, block }
    }
}

pub struct FuncBuilder {
    func: Func,
    values: ValueTable,
    blocks: Map<Block, BlockData>,
    header_blocks: HashMap<Ebb, Block>,
    position: Option<Position>,
}

impl FuncBuilder {
    pub fn new() -> FuncBuilder {
        FuncBuilder {
            func: Func::new(),
            values: ValueTable::new(),
            blocks: Map::new(),
            header_blocks: HashMap::new(),
            position: None,
        }
    }

    pub fn build(self) -> Func {
        self.func
    }

    pub fn set_position(&mut self, ebb: Ebb) {
        self.func.push_ebb(ebb);
        let block = self.header_blocks[&ebb];
        self.position = Some(Position::new(ebb, block));
    }

    pub fn is_filled(&self) -> bool {
        let ebb = self.position.unwrap().ebb;
        match self.func.last_inst(ebb) {
            Some(inst) => self.func.inst(inst).is_terminator(),
            None => false,
        }
    }

    pub fn push_param(&mut self, ty: Type) {
        self.func.push_param(ty);
    }

    pub fn push_ebb_param(&mut self, variable: Variable, ebb: Ebb, ty: Type) -> Value {
        let value = self.create_value(ty);
        self.func.push_ebb_param(ebb, value);

        let header_block = self.header_blocks[&ebb];
        self.values.insert_param(header_block, variable, value);

        value
    }

    pub fn create_ebb(&mut self) -> Ebb {
        let ebb = self.func.create_ebb();
        let block = self.blocks.create(BlockData::Header(HeaderBlock::new(ebb)));
        self.header_blocks.insert(ebb, block);
        ebb
    }

    pub fn def_var(&mut self, variable: Variable, value: Value) {
        let block = self.position.unwrap().block;
        self.values.insert_value(block, variable, value);
    }

    pub fn use_var(&mut self, variable: Variable) -> Value {
        let block = self.position.unwrap().block;
        if let Ok(value) = self.use_var_in_ebb(block, variable) {
            return value;
        }

        let mut blocks = Vec::new();
        blocks.push(block);

        let mut ebbs = Vec::new();
        let mut predecessors = HashSet::new();
        let mut var_type = None;

        while let Some(block) = blocks.pop() {
            match self.use_var_in_ebb(block, variable) {
                Ok(value) => {
                    let value_type = self.func.value(value).ty;
                    let var_type = *var_type.get_or_insert(value_type);
                    assert_eq!(
                        var_type, value_type,
                        "Variable defined with multiple types '{}' and '{}'",
                        var_type, value_type
                    );
                }
                Err(data) => {
                    let ebb = data.ebb;
                    if ebbs.contains(&ebb) {
                        continue;
                    }

                    ebbs.push(ebb);

                    for &predecessor in &data.predecessors {
                        predecessors.insert(predecessor);
                        blocks.push(predecessor.block);
                    }
                }
            };
        }

        let var_type = var_type.unwrap();

        for ebb in ebbs {
            self.push_ebb_param(variable, ebb, var_type);
        }
        for predecessor in predecessors {
            let block = predecessor.block;
            let inst = predecessor.inst;
            let value = self.use_var_in_ebb(block, variable).unwrap();
            self.push_target_arg(inst, value);
        }

        self.use_var_in_ebb(block, variable).unwrap()
    }

    fn use_var_in_ebb(&self, block: Block, variable: Variable) -> Result<Value, &HeaderBlock> {
        let mut block = block;
        loop {
            match self.values.get_value(block, variable) {
                Some(value) => return Ok(value),
                None => match self.blocks[block] {
                    BlockData::Header(ref data) => return Err(data),
                    BlockData::Body(ref data) => block = data.predecessor,
                },
            }
        }
    }

    pub fn nop(&mut self) {
        let data = InstData::Nop();
        self.push_inst(data, None);
    }

    pub fn element(&mut self) -> Value {
        let data = InstData::Element();
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn count(&mut self) -> Value {
        let data = InstData::Count();
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn iconst(&mut self, imm: i32) -> Value {
        let data = InstData::Iconst(imm);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fconst(&mut self, imm: f32) -> Value {
        let data = InstData::Fconst(imm);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn bconst(&mut self, imm: bool) -> Value {
        let data = InstData::Bconst(imm);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn ftoi(&mut self, arg: Value) -> Value {
        let data = InstData::Ftoi(arg);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn itof(&mut self, arg: Value) -> Value {
        let data = InstData::Itof(arg);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn not(&mut self, arg: Value) -> Value {
        let data = InstData::Not(arg);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn clz(&mut self, arg: Value) -> Value {
        let data = InstData::Clz(arg);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn add(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Add([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn sub(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Sub([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn shl(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Shl([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn shr(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Shr([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn asr(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Asr([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn ror(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Ror([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn min(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Min([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn max(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Max([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn and(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::And([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn or(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Or([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn xor(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Xor([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fadd(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fadd([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fsub(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fsub([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fmul(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmul([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fmin(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmin([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fmax(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmax([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fminabs(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fminabs([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fmaxabs(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmaxabs([arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn select(&mut self, arg0: Value, arg1: Value, arg2: Value) -> Value {
        let data = InstData::Select([arg0, arg1, arg2]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn icmp(&mut self, cond: Cond, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Icmp(cond, [arg0, arg1]);
        let inst = self.push_inst(data, None);
        self.func.result(inst)
    }

    pub fn fcmp(&mut self, cond: Cond, arg0: Value, arg1: Value) -> Value {
        let inst = self.push_inst(InstData::Fcmp(cond, [arg0, arg1]), None);
        self.func.result(inst)
    }

    pub fn alloc(&mut self, len: u8) -> Value {
        let inst = self.push_inst(InstData::Alloc(len), None);
        self.func.result(inst)
    }

    pub fn fetch(&mut self, ty: Type, arg: Value) -> Value {
        let inst = self.push_inst(InstData::Fetch(arg), Some(ty.kind));
        self.func.result(inst)
    }

    pub fn read(&mut self, ty: Type, arg: Value) -> Value {
        let data = InstData::Read(arg);
        let inst = self.push_inst(data, Some(ty.kind));
        self.func.result(inst)
    }

    pub fn write(&mut self, arg0: Value, arg1: Value) {
        let data = InstData::Write([arg0, arg1]);
        self.push_inst(data, None);
    }

    pub fn load(&mut self, ty: Type, arg0: Value, arg1: Value) {
        let data = InstData::Load([arg0, arg1]);
        self.push_inst(data, Some(ty.kind));
    }

    pub fn store(&mut self, arg0: Value, arg1: Value) {
        let data = InstData::Store([arg0, arg1]);
        self.push_inst(data, None);
    }

    pub fn jump(&mut self, ebb: Ebb) {
        let args = self.create_args(ebb, vec![]);
        let data = InstData::Jump(ebb, args);
        self.push_inst(data, None);
    }

    pub fn brallz(&mut self, arg: Value, ebb: Ebb) {
        let args = self.create_args(ebb, vec![arg]);
        let data = InstData::Brallz(ebb, args);
        self.push_inst(data, None);
    }

    pub fn brallnz(&mut self, arg: Value, ebb: Ebb) {
        let args = self.create_args(ebb, vec![arg]);
        let data = InstData::Brallnz(ebb, args);
        self.push_inst(data, None);
    }

    pub fn branyz(&mut self, arg: Value, ebb: Ebb) {
        let args = self.create_args(ebb, vec![arg]);
        let data = InstData::Branyz(ebb, args);
        self.push_inst(data, None);
    }

    pub fn branynz(&mut self, arg: Value, ebb: Ebb) {
        let args = self.create_args(ebb, vec![arg]);
        let data = InstData::Branynz(ebb, args);
        self.push_inst(data, None);
    }

    pub fn ret(&mut self) {
        let data = InstData::Return();
        self.push_inst(data, None);
    }

    fn push_inst(&mut self, data: InstData, kind: Option<TypeKind>) -> Inst {
        let ebb = self.position.unwrap().ebb;
        let inst = self.func.create_inst(data.clone());
        self.func.push_inst(ebb, inst);

        self.func.create_result(inst, kind);

        if let Some((ebb, _)) = data.target() {
            let block = self.position.unwrap().block;
            let predecessor = Predecessor::new(block, inst);
            self.push_predecessor(ebb, predecessor);

            self.position.as_mut().unwrap().block =
                self.blocks.create(BlockData::Body(BodyBlock::new(block)))
        }

        inst
    }

    fn create_value(&mut self, ty: Type) -> Value {
        self.func.create_value(ValueData::new(ty))
    }

    fn create_args(&mut self, ebb: Ebb, mut args: Vec<Value>) -> Vec<Value> {
        let block = self.header_blocks[&ebb];

        for i in 0..self.func.ebb_params(ebb).len() {
            let param = self.func.ebb_params(ebb)[i];
            let variable = self.values.get_param(block, param).unwrap();
            let value = self.use_var(variable);
            args.push(value);
        }

        args
    }

    fn push_target_arg(&mut self, inst: Inst, value: Value) {
        self.func.inst_mut(inst).push_target_arg(value);
    }

    fn push_predecessor(&mut self, ebb: Ebb, predecessor: Predecessor) {
        let block = self.header_blocks[&ebb];
        match self.blocks[block] {
            BlockData::Header(ref mut data) => data.predecessors.push(predecessor),
            _ => panic!(),
        }
    }
}
