use crate::collections::SecondaryMap;
use crate::ir::*;
use std::collections::HashMap;
use std::mem;

#[derive(Debug)]
struct Param {
    variable: Variable,
    value: Value,
}

#[derive(Debug, Default)]
struct BlockData {
    sealed: bool,
    filled: bool,
    predecessors: Vec<Inst>,
    params: Vec<Param>,
}

#[derive(Debug)]
struct Values {
    values: HashMap<(Block, Variable), Value>,
}

impl Values {
    fn new() -> Values {
        Values {
            values: HashMap::new(),
        }
    }

    fn insert(&mut self, block: Block, variable: Variable, value: Value) {
        self.values.insert((block, variable), value);
    }

    fn get(&self, block: Block, variable: Variable) -> Option<Value> {
        self.values.get(&(block, variable)).copied()
    }
}

#[derive(Debug)]
enum Call {
    UseVar(Block),
    FinishOnePredecessor(Block),
    FinishMultiplePredecessors(Block, Value),
}

pub struct FuncBuilder {
    func: Func,
    blocks: SecondaryMap<Block, BlockData>,
    values: Values,
    types: HashMap<Variable, Type>,
    position: Option<Block>,
    calls: Vec<Call>,
    results: Vec<Value>,
}

impl FuncBuilder {
    pub fn new() -> FuncBuilder {
        FuncBuilder {
            func: Func::new(),
            blocks: SecondaryMap::new(),
            values: Values::new(),
            types: HashMap::new(),
            position: None,
            calls: Vec::new(),
            results: Vec::new(),
        }
    }

    pub fn finalize(self) -> Func {
        debug_assert!(
            self.func
                .layout
                .blocks()
                .all(|block| self.blocks[block].sealed),
            "All blocks must be sealed before finalize"
        );
        debug_assert!(
            self.func
                .layout
                .blocks()
                .all(|block| self.blocks[block].filled),
            "All blocks must be filled before finalize"
        );

        self.func
    }

    pub fn create_block(&mut self) -> Block {
        self.func.data.create_block()
    }

    pub fn seal_block(&mut self, block: Block) {
        let data = &mut self.blocks[block];
        if data.sealed {
            return;
        }

        let params = mem::replace(&mut data.params, Vec::new());
        for param in params {
            self.start_multiple_predecessors(block, param.value);
            self.run_use_var(param.variable);
        }

        self.blocks[block].sealed = true;
    }

    pub fn switch_to_block(&mut self, block: Block) {
        if let Some(current_block) = self.position {
            debug_assert!(
                self.is_filled(),
                "Switched to block {} before current block {} is filled",
                block,
                current_block
            );
        }

        debug_assert!(
            !self.blocks[block].filled,
            "Switched to block {} which is already filled",
            block
        );

        self.position = Some(block);
    }

    pub fn is_filled(&self) -> bool {
        let block = self.position.unwrap();
        self.blocks[block].filled
    }

    pub fn push_param(&mut self, block: Block, ty: Type) -> Value {
        debug_assert!(
            self.blocks[block].sealed,
            "Parameter added to block {} which is not sealed",
            block
        );
        debug_assert!(
            self.func.layout.first_inst(block).is_none(),
            "Parameter added to block {} which already has instructions",
            block
        );

        self.func.data.push_param(ty);
        self.func.data.push_block_param(block, ty)
    }

    pub fn declare_var(&mut self, variable: Variable, ty: Type) {
        self.types.insert(variable, ty);
    }

    pub fn def_var(&mut self, variable: Variable, value: Value) {
        let block = self.position.unwrap();
        self.def_var_in_block(variable, value, block)
    }

    fn def_var_in_block(&mut self, variable: Variable, value: Value, block: Block) {
        let variable_type = self
            .types
            .get(&variable)
            .unwrap_or_else(|| panic!("Variable {} defined but not declared", variable));
        let value_type = self.func.data.value_type(value);

        debug_assert!(
            variable_type.is_assignable_from(value_type),
            "Variable {} with type {} cannot be defined as value {} with type {}",
            variable,
            variable_type,
            value,
            value_type
        );

        self.values.insert(block, variable, value);
    }

    pub fn use_var(&mut self, variable: Variable) -> Value {
        let block = self.position.unwrap();
        if let Some(value) = self.values.get(block, variable) {
            return value;
        }

        self.calls.push(Call::UseVar(block));
        self.run_use_var(variable)
    }

    fn run_use_var(&mut self, variable: Variable) -> Value {
        let ty = *self
            .types
            .get(&variable)
            .unwrap_or_else(|| panic!("Variable {} used but not declared", variable));

        while let Some(call) = self.calls.pop() {
            match call {
                Call::UseVar(block) => {
                    self.use_var_in_block(variable, ty, block);
                }
                Call::FinishOnePredecessor(block) => {
                    self.finish_one_predecessor(variable, block);
                }
                Call::FinishMultiplePredecessors(block, value) => {
                    self.finish_multiple_predecessors(variable, block, value);
                }
            }
        }

        self.results.pop().unwrap()
    }

    fn use_var_in_block(&mut self, variable: Variable, ty: Type, block: Block) {
        if let Some(value) = self.values.get(block, variable) {
            self.results.push(value);
            return;
        }

        let data = &self.blocks[block];

        match (data.sealed, data.predecessors.len()) {
            (false, _) => {
                let value = self.func.data.push_block_param(block, ty);
                self.def_var_in_block(variable, value, block);

                let param = Param { variable, value };
                self.blocks[block].params.push(param);

                self.results.push(value);
            }
            (true, 1) => {
                let predecessor = data.predecessors[0];
                let pred_block = self.func.layout.inst_block(predecessor).unwrap();

                self.start_one_predecessor(block, pred_block);
            }
            (true, _) => {
                let value = self.func.data.push_block_param(block, ty);
                self.def_var_in_block(variable, value, block);

                self.start_multiple_predecessors(block, value);
            }
        };
    }

    fn start_one_predecessor(&mut self, block: Block, predecessor: Block) {
        self.calls.push(Call::FinishOnePredecessor(block));
        self.calls.push(Call::UseVar(predecessor));
    }

    fn finish_one_predecessor(&mut self, variable: Variable, block: Block) {
        let value = *self.results.last().unwrap();
        self.def_var_in_block(variable, value, block)
    }

    fn start_multiple_predecessors(&mut self, block: Block, param_value: Value) {
        self.calls
            .push(Call::FinishMultiplePredecessors(block, param_value));

        for &predecessor in &self.blocks[block].predecessors {
            let pred_block = self.func.layout.inst_block(predecessor).unwrap();
            self.calls.push(Call::UseVar(pred_block))
        }
    }

    fn finish_multiple_predecessors(
        &mut self,
        variable: Variable,
        block: Block,
        param_value: Value,
    ) {
        let predecessors = &self.blocks[block].predecessors;

        enum Results<T> {
            Zero,
            One(T),
            More,
        }

        let mut results = Results::Zero;

        for _ in 0..predecessors.len() {
            let result = self.results.pop().unwrap();
            let pred_value = self.func.data.resolve_alias(result);
            match results {
                Results::Zero => {
                    if pred_value != param_value {
                        results = Results::One(pred_value);
                    }
                }
                Results::One(value) => {
                    if pred_value != param_value && pred_value != value {
                        results = Results::More;
                    }
                }
                Results::More => {
                    break;
                }
            };
        }

        let result = match results {
            Results::Zero => {
                panic!("Variable {} used but not defined", variable);
            }
            Results::One(value) => {
                self.func.data.swap_remove_block_param(param_value);
                self.func.data.value_to_alias(param_value, value);

                value
            }
            Results::More => {
                for &predecessor in predecessors {
                    let pred_block = self.func.layout.inst_block(predecessor).unwrap();
                    let pred_value = self.values.get(pred_block, variable).unwrap();
                    self.func
                        .data
                        .inst_mut(predecessor)
                        .push_target_arg(pred_value);
                }

                param_value
            }
        };

        self.results.push(result)
    }

    pub fn nop(&mut self) {
        let data = InstData::Nop();
        self.push_inst(data);
    }

    pub fn element(&mut self) -> Value {
        let data = InstData::Element();
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::VARYING_INT)
    }

    pub fn count(&mut self) -> Value {
        let data = InstData::Count();
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::UNIFORM_INT)
    }

    pub fn iconst(&mut self, imm: i32) -> Value {
        let data = InstData::Iconst(imm);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::UNIFORM_INT)
    }

    pub fn fconst(&mut self, imm: f32) -> Value {
        let data = InstData::Fconst(imm);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::UNIFORM_FLOAT)
    }

    pub fn bconst(&mut self, imm: bool) -> Value {
        let data = InstData::Bconst(imm);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::UNIFORM_BOOL)
    }

    pub fn ftoi(&mut self, arg: Value) -> Value {
        let data = InstData::Ftoi(arg);
        let inst = self.push_inst(data);

        let ty = self.result_type_with_kind(arg, TypeKind::Int);
        self.create_inst_result(inst, ty)
    }

    pub fn itof(&mut self, arg: Value) -> Value {
        let data = InstData::Itof(arg);
        let inst = self.push_inst(data);

        let ty = self.result_type_with_kind(arg, TypeKind::Float);
        self.create_inst_result(inst, ty)
    }

    pub fn not(&mut self, arg: Value) -> Value {
        let data = InstData::Not(arg);
        let inst = self.push_inst(data);

        let ty = self.func.data.value_type(arg);
        self.create_inst_result(inst, ty)
    }

    pub fn clz(&mut self, arg: Value) -> Value {
        let data = InstData::Clz(arg);
        let inst = self.push_inst(data);

        let ty = self.result_type_with_kind(arg, TypeKind::Int);
        self.create_inst_result(inst, ty)
    }

    pub fn add(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Add([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn sub(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Sub([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn shl(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Shl([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn shr(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Shr([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn asr(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Asr([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn ror(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Ror([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn min(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Min([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn max(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Max([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn and(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::And([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn or(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Or([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn xor(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Xor([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fadd(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fadd([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fsub(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fsub([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fmul(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmul([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fmin(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmin([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fmax(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmax([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fminabs(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fminabs([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn fmaxabs(&mut self, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fmaxabs([arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg0, arg1);
        self.create_inst_result(inst, ty)
    }

    pub fn select(&mut self, arg0: Value, arg1: Value, arg2: Value) -> Value {
        let data = InstData::Select([arg0, arg1, arg2]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary(arg1, arg2);
        self.create_inst_result(inst, ty)
    }

    pub fn icmp(&mut self, cond: Cond, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Icmp(cond, [arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary_with_kind(arg0, arg1, TypeKind::Bool);
        self.create_inst_result(inst, ty)
    }

    pub fn fcmp(&mut self, cond: Cond, arg0: Value, arg1: Value) -> Value {
        let data = InstData::Fcmp(cond, [arg0, arg1]);
        let inst = self.push_inst(data);

        let ty = self.result_type_binary_with_kind(arg0, arg1, TypeKind::Bool);
        self.create_inst_result(inst, ty)
    }

    pub fn alloc(&mut self, len: u8) -> Value {
        let data = InstData::Alloc(len);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, Type::UNIFORM_INT)
    }

    pub fn fetch(&mut self, ty: Type, arg: Value) -> Value {
        let data = InstData::Fetch(arg);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, ty)
    }

    pub fn read(&mut self, ty: Type, arg: Value) -> Value {
        let data = InstData::Read(arg);
        let inst = self.push_inst(data);

        self.create_inst_result(inst, ty)
    }

    pub fn write(&mut self, arg0: Value, arg1: Value) {
        let data = InstData::Write([arg0, arg1]);
        self.push_inst(data);
    }

    pub fn load(&mut self, arg0: Value, arg1: Value) {
        let data = InstData::Load([arg0, arg1]);
        self.push_inst(data);
    }

    pub fn store(&mut self, arg0: Value, arg1: Value) {
        let data = InstData::Store([arg0, arg1]);
        self.push_inst(data);
    }

    pub fn jump(&mut self, block: Block) {
        let data = InstData::Jump(block, vec![]);
        self.push_inst(data);
    }

    pub fn brallz(&mut self, arg: Value, block: Block) {
        let data = InstData::Brallz(block, vec![arg]);
        self.push_inst(data);
    }

    pub fn brallnz(&mut self, arg: Value, block: Block) {
        let data = InstData::Brallnz(block, vec![arg]);
        self.push_inst(data);
    }

    pub fn branyz(&mut self, arg: Value, block: Block) {
        let data = InstData::Branyz(block, vec![arg]);
        self.push_inst(data);
    }

    pub fn branynz(&mut self, arg: Value, block: Block) {
        let data = InstData::Branynz(block, vec![arg]);
        self.push_inst(data);
    }

    pub fn ret(&mut self) {
        let data = InstData::Return();
        self.push_inst(data);
    }

    fn push_inst(&mut self, data: InstData) -> Inst {
        let block = self.position.unwrap();
        if !self.func.layout.is_block_inserted(block) {
            self.func.layout.push_block(block);
        }

        debug_assert!(
            !self.blocks[block].filled,
            "Instruction added to block which is already filled"
        );

        let target = data.target();
        let is_terminator = data.is_terminator();

        let inst = self.func.data.create_inst(data);
        self.func.layout.push_inst(block, inst);

        if let Some(target) = target {
            self.blocks[target].predecessors.push(inst);
        }

        if is_terminator {
            let block = self.position.unwrap();
            self.blocks[block].filled = true;
        }

        inst
    }

    fn result_type_with_kind(&self, arg: Value, kind: TypeKind) -> Type {
        let ty = self.func.data.value_type(arg);

        Type::new(ty.variability, kind)
    }

    fn result_type_binary(&self, arg0: Value, arg1: Value) -> Type {
        let ty0 = self.func.data.value_type(arg0);
        let ty1 = self.func.data.value_type(arg1);

        debug_assert!(
            ty0.kind == ty1.kind,
            "Invalid argument types '{}' and '{}'",
            ty0,
            ty1
        );

        Type::new(ty0.variability | ty1.variability, ty0.kind)
    }

    fn result_type_binary_with_kind(&self, arg0: Value, arg1: Value, kind: TypeKind) -> Type {
        let ty0 = self.func.data.value_type(arg0);
        let ty1 = self.func.data.value_type(arg1);

        debug_assert!(
            ty0.kind == ty1.kind,
            "Invalid argument types '{}' and '{}'",
            ty0,
            ty1
        );

        Type::new(ty0.variability | ty1.variability, kind)
    }

    fn create_inst_result(&mut self, inst: Inst, ty: Type) -> Value {
        self.func.data.create_inst_result(inst, ty)
    }
}
