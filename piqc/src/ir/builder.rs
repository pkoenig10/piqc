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
                    let value_type = self.get_value_type(value);
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

    pub fn get_value_type(&self, value: Value) -> Type {
        self.func.value(value).ty()
    }

    pub fn push_int_const_inst(&mut self, value: i32) -> Value {
        let ty = Type::new(Variability::Uniform, TypeKind::Int);
        let dest = self.create_value(ty);
        let inst = IntConstInst::new(dest, value).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_float_const_inst(&mut self, value: f32) -> Value {
        let ty = Type::new(Variability::Uniform, TypeKind::Float);
        let dest = self.create_value(ty);
        let inst = FloatConstInst::new(dest, value).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_bool_const_inst(&mut self, value: bool) -> Value {
        let dest = self.create_value(Type::UNIFORM_BOOL);
        let inst = BoolConstInst::new(dest, value).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_element_inst(&mut self) -> Value {
        let dest = self.create_value(Type::VARYING_INT);
        let inst = ElementInst::new(dest).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_count_inst(&mut self) -> Value {
        let dest = self.create_value(Type::UNIFORM_INT);
        let inst = CountInst::new(dest).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_unary_inst(&mut self, op: UnaryOp, src: Value) -> Value {
        let ty = self.get_unary_inst_type(src);
        let dest = self.create_value(ty);
        let inst = UnaryInst::new(op, dest, src).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_binary_inst(&mut self, op: BinaryOp, left: Value, right: Value) -> Value {
        let ty = self.get_binary_inst_type(op, left, right);
        let dest = self.create_value(ty);
        let inst = BinaryInst::new(op, dest, left, right).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_alloc_inst(&mut self, len: u8) -> Value {
        let dest = self.create_value(Type::UNIFORM_INT);
        let inst = AllocInst::new(dest, len).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_fetch_inst(&mut self, addr: Value, ty: Type) -> Value {
        let ty = self.get_fetch_inst_type(addr, ty);
        let dest = self.create_value(ty);
        let inst = FetchInst::new(dest, addr).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_read_inst(&mut self, index: Value, ty: Type) -> Value {
        let ty = self.get_read_inst_type(index, ty);
        let dest = self.create_value(ty);
        let inst = ReadInst::new(dest, index).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_write_inst(&mut self, cond: Option<Value>, src: Value, index: Value) {
        let inst = WriteInst::new(cond, src, index).into();
        self.push_inst(inst);
    }

    pub fn push_store_inst(&mut self, index: Value, addr: Value) {
        let inst = StoreInst::new(index, addr).into();
        self.push_inst(inst);
    }

    pub fn push_int_cmp_inst(&mut self, op: CmpOp, left: Value, right: Value) -> Value {
        let ty = self.get_cmp_inst_type(op, left, right);
        let dest = self.create_value(ty);
        let inst = IntCmpInst::new(op, dest, left, right).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_float_cmp_inst(&mut self, op: CmpOp, left: Value, right: Value) -> Value {
        let ty = self.get_cmp_inst_type(op, left, right);
        let dest = self.create_value(ty);
        let inst = FloatCmpInst::new(op, dest, left, right).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_select_inst(&mut self, cond: Value, left: Value, right: Value) -> Value {
        let ty = self.get_select_inst_type(left, right);
        let dest = self.create_value(ty);
        let inst = SelectInst::new(dest, cond, left, right).into();
        self.push_inst(inst);
        dest
    }

    pub fn push_jump_inst(&mut self, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = JumpInst::new(target).into();
        self.push_inst(inst);
    }

    pub fn push_branch_inst(&mut self, op: BranchOp, cond: Value, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = BranchInst::new(op, cond, target).into();
        self.push_inst(inst);
    }

    pub fn push_return_inst(&mut self) {
        let inst = ReturnInst::new().into();
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

    fn create_value(&mut self, ty: Type) -> Value {
        self.func.create_value(ValueData::new(ty))
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

    fn get_unary_inst_type(&self, src: Value) -> Type {
        self.get_value_type(src)
    }

    fn get_binary_inst_type(&self, op: BinaryOp, left: Value, right: Value) -> Type {
        let left_type = self.get_value_type(left);
        let right_type = self.get_value_type(right);

        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid binary instruction '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        );

        let variability = variability(left_type.variability, right_type.variability);

        Type::new(variability, left_type.kind)
    }

    fn get_fetch_inst_type(&self, addr: Value, ty: Type) -> Type {
        let addr_type = self.get_value_type(addr);

        assert_eq!(
            addr_type,
            Type::new(ty.variability, TypeKind::Int),
            "Invalid fetch instruction with operand type `{}` and value type {}",
            addr_type,
            ty
        );

        ty
    }

    fn get_read_inst_type(&self, index: Value, ty: Type) -> Type {
        let index_type = self.get_value_type(index);

        assert_eq!(
            index_type,
            Type::new(ty.variability, TypeKind::Int),
            "Invalid read instruction with operand type `{}` and value type {}",
            index_type,
            ty
        );

        ty
    }

    fn get_cmp_inst_type(&self, op: CmpOp, left: Value, right: Value) -> Type {
        let left_type = self.get_value_type(left);
        let right_type = self.get_value_type(right);

        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid cmparison instruction '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        );

        let variability = variability(left_type.variability, right_type.variability);

        Type::new(variability, TypeKind::Bool)
    }

    fn get_select_inst_type(&self, left: Value, right: Value) -> Type {
        let left_type = self.get_value_type(left);
        let right_type = self.get_value_type(right);

        assert_eq!(
            left_type.kind, right_type.kind,
            "Invalid select instruction with operand types '{}' and '{}'",
            left_type, right_type
        );

        let variability = variability(left_type.variability, right_type.variability);

        Type::new(variability, left_type.kind)
    }
}

pub fn variability(variability1: Variability, variability2: Variability) -> Variability {
    match (variability1, variability2) {
        (Variability::Uniform, Variability::Uniform) => Variability::Uniform,
        _ => Variability::Varying,
    }
}
