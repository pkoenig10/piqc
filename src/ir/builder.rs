use ir::*;

pub struct IrBuilder {
    func: Func,
    current_block: Option<BlockId>,
}

impl IrBuilder {
    pub fn new(func: Func) -> IrBuilder {
        IrBuilder {
            func,
            current_block: None,
        }
    }

    pub fn func(self) -> Func {
        self.func
    }

    pub fn current_block(&self) -> BlockId {
        self.current_block.unwrap()
    }

    pub fn set_current_block(&mut self, block_id: BlockId) {
        self.current_block = Some(block_id);
    }

    pub fn create_func_param(&mut self, type_: Type) {
        self.func.push_param(type_);
    }

    pub fn create_block_param(&mut self, block_id: BlockId, type_: Type) -> Value {
        let value = self.func.create_value(type_);
        self.func.push_block_param(block_id, value);
        value
    }

    pub fn create_block(&mut self) -> BlockId {
        self.func.create_block()
    }

    pub fn block(&self, block_id: BlockId) -> &BlockData {
        self.func.block(block_id)
    }

    pub fn inst(&self, inst_id: InstId) -> &InstData {
        self.func.inst(inst_id)
    }

    pub fn inst_mut(&mut self, inst_id: InstId) -> &mut InstData {
        self.func.inst_mut(inst_id)
    }

    pub fn value(&self, value: Value) -> &ValueData {
        self.func.value(value)
    }

    pub fn push_block(&mut self, block_id: BlockId) {
        self.func.push_block(block_id);
    }

    pub fn push_int_const_inst(&mut self, immediate: IntImmediate) -> Value {
        let dest = self.func.create_value(Int);
        let inst = Inst::IntConstInst(IntConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    pub fn push_float_const_inst(&mut self, immediate: FloatImmediate) -> Value {
        let dest = self.func.create_value(Float);
        let inst = Inst::FloatConstInst(FloatConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    pub fn push_bool_const_inst(&mut self, immediate: BoolImmediate) -> Value {
        let dest = self.func.create_value(Bool);
        let inst = Inst::BoolConstInst(BoolConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    pub fn push_unary_inst(&mut self, op: UnaryOp, src: Operand) -> Value {
        let type_ = self.get_unary_inst_type(op, src);
        let dest = self.func.create_value(type_);
        let inst = Inst::UnaryInst(UnaryInst::new(op, dest, src));
        self.push_inst(inst);
        dest
    }

    pub fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_binary_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = Inst::BinaryInst(BinaryInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = Inst::IntCompInst(IntCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = Inst::FloatCompInst(FloatCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    pub fn push_jump_inst(&mut self, target: Target) {
        let inst = Inst::JumpInst(JumpInst::new(target));
        self.push_inst(inst);
    }

    pub fn push_branch_inst(&mut self, cond: Value, true_target: Target, false_target: Target) {
        let inst = Inst::BranchInst(BranchInst::new(cond, true_target, false_target));
        self.push_inst(inst);
    }

    pub fn push_return_inst(&mut self) {
        let inst = Inst::ReturnInst(ReturnInst::new());
        self.push_inst(inst);
    }

    fn push_inst(&mut self, inst: Inst) {
        let block_id = self.current_block.unwrap();
        let inst_id = self.func.create_inst(inst);
        self.func.push_inst(block_id, inst_id);
    }

    fn get_unary_inst_type(&self, op: UnaryOp, src: Operand) -> Type {
        let src_type = self.get_operand_type(src);

        match (op, src_type) {
            (Not, Int) => Int,
            (Not, Bool) => Bool,
            _ => {
                panic!(
                    "Invalid unary instruction '{}' with operand type '{}'",
                    op,
                    src_type,
                )
            }
        }
    }

    fn get_binary_inst_type(&self, op: BinaryOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);

        match (op, left_type, right_type) {
            (Add, Int, Int) | (Sub, Int, Int) | (Asr, Int, Int) | (Shl, Int, Int) |
            (And, Int, Int) | (Or, Int, Int) | (Xor, Int, Int) => Int,
            (Fadd, Float, Float) |
            (Fsub, Float, Float) |
            (Fmul, Float, Float) => Float,
            (And, Bool, Bool) |
            (Or, Bool, Bool) |
            (Xor, Bool, Bool) => Bool,
            _ => {
                panic!(
                    "Invalid binary instruction '{}' with operand types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        }
    }

    fn get_comp_inst_type(&self, op: CompOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);

        if left_type != right_type {
            panic!(
                "Invalid comparison instruction '{}' with operand types '{}' and '{}'",
                op,
                left_type,
                right_type
            )
        }

        Bool
    }

    fn get_operand_type(&self, operand: Operand) -> Type {
        match operand {
            Operand::IntImmediate(_) => Int,
            Operand::FloatImmediate(_) => Float,
            Operand::BoolImmediate(_) => Bool,
            Operand::Value(value_id) => self.func.value(value_id).type_(),
        }
    }
}
