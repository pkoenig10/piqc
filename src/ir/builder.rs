use std::collections::HashMap;
use std::collections::HashSet;

use ir::*;

type Variable<'a> = &'a str;

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
struct BlockValues<'a> {
    params: HashMap<Value, Variable<'a>>,
    values: HashMap<Variable<'a>, Value>,
}

impl<'a> BlockValues<'a> {
    fn new() -> BlockValues<'a> {
        BlockValues {
            params: HashMap::new(),
            values: HashMap::new(),
        }
    }

    fn get_param(&self, value: Value) -> Option<Variable<'a>> {
        self.params.get(&value).cloned()
    }

    fn insert_param(&mut self, variable: Variable<'a>, value: Value) {
        self.params.insert(value, variable);
        self.insert_value(variable, value);
    }

    fn get_value(&self, variable: Variable<'a>) -> Option<Value> {
        self.values.get(&variable).cloned()
    }

    fn insert_value(&mut self, variable: Variable<'a>, value: Value) {
        self.values.insert(variable, value);
    }
}

#[derive(Debug)]
struct ValueTable<'a> {
    values: HashMap<Block, BlockValues<'a>>,
}

impl<'a> ValueTable<'a> {
    fn new() -> ValueTable<'a> {
        ValueTable {
            values: HashMap::new(),
        }
    }

    fn insert_param(&mut self, block: Block, variable: Variable<'a>, value: Value) {
        self.get_values_mut(block).insert_param(variable, value);
    }

    fn get_param(&self, block: Block, value: Value) -> Option<Variable<'a>> {
        self.get_values(block)
            .and_then(|values| values.get_param(value))
    }

    fn insert_value(&mut self, block: Block, variable: Variable<'a>, value: Value) {
        self.get_values_mut(block).insert_value(variable, value);
    }

    fn get_value(&self, block: Block, variable: Variable<'a>) -> Option<Value> {
        self.get_values(block)
            .and_then(|values| values.get_value(variable))
    }

    fn get_values(&self, block: Block) -> Option<&BlockValues<'a>> {
        self.values.get(&block)
    }

    fn get_values_mut(&mut self, block: Block) -> &mut BlockValues<'a> {
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

pub struct FuncBuilder<'input> {
    func: Func,
    values: ValueTable<'input>,
    blocks: Map<Block, BlockData>,
    header_blocks: HashMap<Ebb, Block>,
    position: Option<Position>,
}

impl<'input> FuncBuilder<'input> {
    pub fn new() -> FuncBuilder<'input> {
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
        let inst = self.func.last_inst(ebb);
        self.func.inst(inst).is_terminator()
    }

    pub fn push_param(&mut self, type_: Type) {
        self.func.push_param(type_);
    }

    pub fn push_ebb_param(&mut self, variable: Variable<'input>, ebb: Ebb, type_: Type) -> Value {
        let value = self.create_value(type_);
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

    pub fn def_var(&mut self, variable: Variable<'input>, value: Value) {
        let block = self.position.unwrap().block;
        self.values.insert_value(block, variable, value);
    }

    pub fn use_var(&mut self, variable: Variable<'input>) -> Value {
        let block = self.position.unwrap().block;
        if let Ok(value) = self.use_var_in_ebb(block, variable) {
            return value;
        }

        let mut calls = Vec::new();
        calls.push(block);

        let mut ebbs = Vec::new();
        let mut predecessors = HashSet::new();
        let mut type_ = None;

        while let Some(block) = calls.pop() {
            match self.use_var_in_ebb(block, variable) {
                Ok(value) => {
                    let value_type = self.get_value_type(value);
                    let type_ = *type_.get_or_insert(value_type);
                    assert_eq!(
                        type_, value_type,
                        "Variable defined with multiple types '{}' and '{}'",
                        type_, value_type
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
                        calls.push(predecessor.block);
                    }
                }
            };
        }

        let type_ = type_.unwrap();

        for ebb in ebbs {
            self.push_ebb_param(variable, ebb, type_);
        }
        for predecessor in predecessors {
            let block = predecessor.block;
            let inst = predecessor.inst;
            let value = self.use_var_in_ebb(block, variable).unwrap();
            self.push_target_arg(inst, value);
        }

        self.use_var_in_ebb(block, variable).unwrap()
    }

    fn use_var_in_ebb(
        &self,
        block: Block,
        variable: Variable<'input>,
    ) -> Result<Value, &HeaderBlock> {
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

    pub fn get_value_type(&self, value: Value) -> Type {
        self.func.value(value).type_()
    }

    pub fn push_int_const_inst(&mut self, value: i32) -> Value {
        let type_ = Type::new(TypeQualifier::Uniform, TypeKind::INT);
        let dest = self.create_value(type_);
        let inst = InstData::IntConst(IntConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    pub fn push_float_const_inst(&mut self, value: f32) -> Value {
        let type_ = Type::new(TypeQualifier::Uniform, TypeKind::FLOAT);
        let dest = self.create_value(type_);
        let inst = InstData::FloatConst(FloatConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    pub fn push_bool_const_inst(&mut self, value: bool) -> Value {
        let dest = self.create_value(Type::UNIFORM_BOOL);
        let inst = InstData::BoolConst(BoolConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    pub fn push_index_inst(&mut self) -> Value {
        let dest = self.create_value(Type::VARYING_INT);
        let inst = InstData::Index(IndexInst::new(dest));
        self.push_inst(inst);
        dest
    }

    pub fn push_count_inst(&mut self) -> Value {
        let dest = self.create_value(Type::UNIFORM_INT);
        let inst = InstData::Count(CountInst::new(dest));
        self.push_inst(inst);
        dest
    }

    pub fn push_unary_inst(&mut self, op: UnaryOp, src: Operand) -> Value {
        let type_ = self.get_unary_inst_type(src);
        let dest = self.create_value(type_);
        let inst = InstData::Unary(UnaryInst::new(op, dest, src));
        self.push_inst(inst);
        dest
    }

    pub fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_binary_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::Binary(BinaryInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::IntComp(IntCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::FloatComp(FloatCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_select_inst(&mut self, cond: Value, left: Operand, right: Operand) -> Value {
        let type_ = self.get_select_inst_type(left, right);
        let dest = self.create_value(type_);
        let inst = InstData::Select(SelectInst::new(dest, cond, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_jump_inst(&mut self, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::Jump(JumpInst::new(target));
        self.push_inst(inst);
    }

    pub fn push_branch_inst(&mut self, op: BranchOp, cond: Value, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::Branch(BranchInst::new(op, cond, target));
        self.push_inst(inst);
    }

    pub fn push_return_inst(&mut self) {
        let inst = InstData::Return(ReturnInst::new());
        self.push_inst(inst);
    }

    fn push_inst(&mut self, data: InstData) {
        let ebb = self.position.unwrap().ebb;
        let inst = self.func.create_inst(data.clone());
        self.func.push_inst(ebb, inst);

        if let Some(target) = data.target() {
            let block = self.position.unwrap().block;
            let predecessor = Predecessor::new(block, inst);
            self.push_predecessor(target.ebb(), predecessor);

            self.position.as_mut().unwrap().block =
                self.blocks.create(BlockData::Body(BodyBlock::new(block)))
        }
    }

    fn create_value(&mut self, type_: Type) -> Value {
        self.func.create_value(ValueData::new(type_))
    }

    fn create_target(&mut self, ebb: Ebb) -> Target {
        let block = self.header_blocks[&ebb];
        let mut target = Target::new(ebb);

        for i in 0..self.func.ebb_params(ebb).len() {
            let param = self.func.ebb_params(ebb)[i];
            let variable = self.values.get_param(block, param).unwrap();
            let value = self.use_var(variable);
            target.push_arg(value);
        }

        target
    }

    fn push_target_arg(&mut self, inst: Inst, value: Value) {
        if let Some(target) = self.func.inst_mut(inst).target_mut() {
            target.push_arg(value);
        }
    }

    fn push_predecessor(&mut self, ebb: Ebb, predecessor: Predecessor) {
        let block = self.header_blocks[&ebb];
        match self.blocks[block] {
            BlockData::Header(ref mut data) => data.predecessors.push(predecessor),
            _ => panic!(),
        }
    }

    fn get_unary_inst_type(&self, src: Operand) -> Type {
        self.get_operand_type(src)
    }

    fn get_binary_inst_type(&self, op: BinaryOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid binary instruction '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        );

        let qualifier = TypeQualifier::get(left_type.qualifier, right_type.qualifier);

        Type::new(qualifier, left_type.kind)
    }

    fn get_comp_inst_type(&self, op: CompOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid comparison instruction '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        );

        let qualifier = TypeQualifier::get(left_type.qualifier, right_type.qualifier);

        Type::new(qualifier, TypeKind::BOOL)
    }

    fn get_select_inst_type(&self, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid select instruction with operand types '{}' and '{}'",
            left_type, right_type
        );

        let qualifier = TypeQualifier::get(left_type.qualifier, right_type.qualifier);

        Type::new(qualifier, left_type.kind)
    }

    fn get_operand_type(&self, operand: Operand) -> Type {
        match operand {
            Operand::Int(_) => Type::new(TypeQualifier::Uniform, TypeKind::INT),
            Operand::Float(_) => Type::new(TypeQualifier::Uniform, TypeKind::FLOAT),
            Operand::Bool(_) => Type::new(TypeQualifier::Uniform, TypeKind::BOOL),
            Operand::Value(value) => self.get_value_type(value),
        }
    }
}
