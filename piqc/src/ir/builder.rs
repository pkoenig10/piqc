use std::collections::HashMap;
use std::mem;

use crate::collections::PrimaryMap;
use crate::ir::*;

id!(Block, "b");

#[derive(Debug)]
struct Predecessor {
    block: Block,
    inst: Inst,
}

#[derive(Debug)]
struct Param {
    variable: Variable,
    value: Value,
}

#[derive(Debug)]
struct HeaderBlockData {
    ebb: Ebb,
    sealed: bool,
    filled: bool,
    predecessors: Vec<Predecessor>,
    params: Vec<Param>,
}

#[derive(Debug)]
struct BodyBlockData {
    predecessor: Block,
}

#[derive(Debug)]
enum BlockData {
    Header(HeaderBlockData),
    Body(BodyBlockData),
}

impl From<HeaderBlockData> for BlockData {
    fn from(data: HeaderBlockData) -> BlockData {
        BlockData::Header(data)
    }
}

impl From<BodyBlockData> for BlockData {
    fn from(data: BodyBlockData) -> BlockData {
        BlockData::Body(data)
    }
}

#[derive(Debug)]
struct Blocks {
    blocks: PrimaryMap<Block, BlockData>,
    headers: HashMap<Ebb, Block>,
}

impl Blocks {
    fn new() -> Blocks {
        Blocks {
            blocks: PrimaryMap::new(),
            headers: HashMap::new(),
        }
    }

    fn create_header(&mut self, ebb: Ebb) -> Block {
        let data = HeaderBlockData {
            ebb,
            sealed: false,
            filled: false,
            predecessors: Vec::new(),
            params: Vec::new(),
        };
        let block = self.blocks.create(data.into());
        self.headers.insert(ebb, block);
        block
    }

    fn create_body(&mut self, predecessor: Block) -> Block {
        self.blocks.create(BlockData::Body(BodyBlockData {
            predecessor: predecessor,
        }))
    }

    fn header(&self, ebb: Ebb) -> Block {
        self.headers[&ebb]
    }

    fn block(&self, block: Block) -> &BlockData {
        &self.blocks[block]
    }

    fn header_block(&self, ebb: Ebb) -> &HeaderBlockData {
        let block = self.header(ebb);
        match &self.blocks[block] {
            BlockData::Header(data) => data,
            _ => panic!("Header block for {} is not a header block"),
        }
    }

    fn header_block_mut(&mut self, ebb: Ebb) -> &mut HeaderBlockData {
        let block = self.header(ebb);
        match &mut self.blocks[block] {
            BlockData::Header(data) => data,
            _ => panic!("Header block for {} is not a header block"),
        }
    }
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

#[derive(Debug, Default)]
struct Position {
    ebb: Option<Ebb>,
    block: Option<Block>,
}

impl Position {
    fn set(&mut self, ebb: Ebb, block: Block) {
        self.ebb = Some(ebb);
        self.block = Some(block);
    }

    fn set_block(&mut self, block: Block) {
        self.block = Some(block);
    }
}

#[derive(Debug)]
enum Call {
    UseVar(Block),
    FinishOnePredecessor(Block),
    FinishMultiplePredecessors(Ebb, Value),
}

pub struct FuncBuilder {
    func: Func,
    blocks: Blocks,
    values: Values,
    types: HashMap<Variable, Type>,
    position: Position,
    calls: Vec<Call>,
    results: Vec<Value>,
}

impl FuncBuilder {
    pub fn new() -> FuncBuilder {
        FuncBuilder {
            func: Func::new(),
            blocks: Blocks::new(),
            values: Values::new(),
            types: HashMap::new(),
            position: Position::default(),
            calls: Vec::new(),
            results: Vec::new(),
        }
    }

    pub fn finalize(self) -> Func {
        debug_assert!(
            self.blocks
                .headers
                .keys()
                .all(|ebb| self.blocks.header_block(*ebb).sealed),
            "All blocks must be sealed before finalize"
        );
        debug_assert!(
            self.blocks
                .headers
                .keys()
                .all(|ebb| self.blocks.header_block(*ebb).filled),
            "All blocks must be filled before finalize"
        );

        self.func
    }

    pub fn create_ebb(&mut self) -> Ebb {
        let ebb = self.func.data.create_ebb();
        self.blocks.create_header(ebb);
        ebb
    }

    pub fn seal_ebb(&mut self, ebb: Ebb) {
        let data = self.blocks.header_block_mut(ebb);
        if data.sealed {
            return;
        }

        let params = mem::replace(&mut data.params, Vec::new());
        for param in params {
            self.start_multiple_predecessors(ebb, param.value);
            self.run_use_var(param.variable);
        }

        self.blocks.header_block_mut(ebb).sealed = true;
    }

    pub fn switch_to_ebb(&mut self, ebb: Ebb) {
        if let Some(current_ebb) = self.position.ebb {
            debug_assert!(
                self.is_filled(),
                "Switched to block {} before current block {} is filled",
                ebb,
                current_ebb
            );
        }

        debug_assert!(
            !self.blocks.header_block(ebb).filled,
            "Switched to block {} which is already filled",
            ebb
        );

        let block = self.blocks.header(ebb);
        self.position.set(ebb, block);
    }

    pub fn is_filled(&self) -> bool {
        let ebb = self.position.ebb.unwrap();
        self.blocks.header_block(ebb).filled
    }

    pub fn push_param(&mut self, ebb: Ebb, ty: Type) -> Value {
        debug_assert!(
            self.blocks.header_block(ebb).sealed,
            "Parameter added to block {} which is not sealed",
            ebb
        );
        debug_assert!(
            self.func.layout.first_inst(ebb).is_none(),
            "Parameter added to block {} which already has instructions",
            ebb
        );

        self.func.data.push_param(ty);
        self.func.data.push_ebb_param(ebb, ty)
    }

    pub fn declare_var(&mut self, variable: Variable, ty: Type) {
        self.types.insert(variable, ty);
    }

    pub fn def_var(&mut self, variable: Variable, value: Value) {
        let block = self.position.block.unwrap();
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
        let block = self.position.block.unwrap();
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
                Call::FinishMultiplePredecessors(ebb, value) => {
                    self.finish_multiple_predecessors(variable, ebb, value);
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

        enum State {
            Unsealed(Ebb),
            SealedOnePredecessor(Block),
            SealedMultiplePredecessors(Ebb),
        };

        let state = match &self.blocks.block(block) {
            BlockData::Header(data) => match (data.sealed, data.predecessors.len()) {
                (false, _) => State::Unsealed(data.ebb),
                (true, 1) => State::SealedOnePredecessor(data.predecessors[0].block),
                (true, _) => State::SealedMultiplePredecessors(data.ebb),
            },
            BlockData::Body(data) => State::SealedOnePredecessor(data.predecessor),
        };
        match state {
            State::Unsealed(ebb) => {
                let value = self.func.data.push_ebb_param(ebb, ty);
                self.def_var_in_block(variable, value, block);

                let param = Param { variable, value };
                self.blocks.header_block_mut(ebb).params.push(param);

                self.results.push(value);
            }
            State::SealedOnePredecessor(predecessor) => {
                self.start_one_predecessor(block, predecessor);
            }
            State::SealedMultiplePredecessors(ebb) => {
                let value = self.func.data.push_ebb_param(ebb, ty);
                self.def_var_in_block(variable, value, block);

                self.start_multiple_predecessors(ebb, value);
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

    fn start_multiple_predecessors(&mut self, ebb: Ebb, param_value: Value) {
        self.calls
            .push(Call::FinishMultiplePredecessors(ebb, param_value));

        for predecessor in &self.blocks.header_block(ebb).predecessors {
            self.calls.push(Call::UseVar(predecessor.block))
        }
    }

    fn finish_multiple_predecessors(&mut self, variable: Variable, ebb: Ebb, param_value: Value) {
        let predecessors = &self.blocks.header_block(ebb).predecessors;

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
                self.func.data.swap_remove_ebb_param(param_value);
                self.func.data.value_to_alias(param_value, value);

                value
            }
            Results::More => {
                for predecessor in predecessors {
                    let pred_value = self.values.get(predecessor.block, variable).unwrap();
                    self.func
                        .data
                        .inst_mut(predecessor.inst)
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

    pub fn jump(&mut self, ebb: Ebb) {
        let data = InstData::Jump(ebb, vec![]);
        self.push_inst(data);
    }

    pub fn brallz(&mut self, arg: Value, ebb: Ebb) {
        let data = InstData::Brallz(ebb, vec![arg]);
        self.push_inst(data);
    }

    pub fn brallnz(&mut self, arg: Value, ebb: Ebb) {
        let data = InstData::Brallnz(ebb, vec![arg]);
        self.push_inst(data);
    }

    pub fn branyz(&mut self, arg: Value, ebb: Ebb) {
        let data = InstData::Branyz(ebb, vec![arg]);
        self.push_inst(data);
    }

    pub fn branynz(&mut self, arg: Value, ebb: Ebb) {
        let data = InstData::Branynz(ebb, vec![arg]);
        self.push_inst(data);
    }

    pub fn ret(&mut self) {
        let data = InstData::Return();
        self.push_inst(data);
    }

    fn push_inst(&mut self, data: InstData) -> Inst {
        let ebb = self.position.ebb.unwrap();
        if !self.func.layout.is_ebb_inserted(ebb) {
            self.func.layout.push_ebb(ebb);
        }

        debug_assert!(
            !self.blocks.header_block(ebb).filled,
            "Instruction added to block which is already filled"
        );

        let is_terminator = data.is_terminator();
        let target_ebb = data.target();

        let inst = self.func.data.create_inst(data);
        self.func.layout.push_inst(ebb, inst);

        if let Some(ebb) = target_ebb {
            self.declare_successor(inst, ebb);
        }

        if is_terminator {
            self.fill_ebb();
        } else if target_ebb.is_some() {
            self.switch_to_next_block();
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

    fn declare_successor(&mut self, inst: Inst, ebb: Ebb) {
        let block = self.position.block.unwrap();
        let predecessor = Predecessor { block, inst };
        self.blocks
            .header_block_mut(ebb)
            .predecessors
            .push(predecessor);
    }

    fn fill_ebb(&mut self) {
        let ebb = self.position.ebb.unwrap();
        self.blocks.header_block_mut(ebb).filled = true;
    }

    fn switch_to_next_block(&mut self) {
        let predecessor = self.position.block.unwrap();
        let block = self.blocks.create_body(predecessor);
        self.position.set_block(block);
    }
}
