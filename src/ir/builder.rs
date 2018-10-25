use std::collections::HashMap;
use std::collections::HashSet;

use ast;
use ir::*;

pub fn generate_ir(prog: &ast::Prog) -> Prog {
    let builder = IrBuilder::new();
    let func = builder.generate_func(&prog.func);
    Prog::new(func)
}

type Variable<'a> = &'a str;

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

struct Position {
    ebb: Ebb,
    block: Block,
}

impl Position {
    fn new(ebb: Ebb, block: Block) -> Position {
        Position { ebb, block }
    }

    fn ebb(&self) -> Ebb {
        self.ebb
    }

    fn block(&self) -> Block {
        self.block
    }
}

struct IrBuilder<'input> {
    func: Func,
    values: ValueTable<'input>,
    blocks: Map<Block, BlockData>,
    header_blocks: HashMap<Ebb, Block>,
    position: Option<Position>,
    predicate: Option<Value>,
    not_returned: Option<Value>,
}

impl<'input> IrBuilder<'input> {
    fn new() -> IrBuilder<'input> {
        IrBuilder {
            func: Func::new(),
            values: ValueTable::new(),
            blocks: Map::new(),
            header_blocks: HashMap::new(),
            position: None,
            predicate: None,
            not_returned: None,
        }
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        let entry_ebb = self.create_ebb();
        self.set_position_ebb(entry_ebb);

        for param in &func.params {
            let variable = param.identifier.name;
            let type_ = match param.type_ {
                ast::Type::Int => Type::new(Uniform, Int),
                ast::Type::Float => Type::new(Uniform, Float),
                ast::Type::Bool => Type::new(Uniform, Bool),
            };
            self.create_func_param(type_);
            self.create_block_param(variable, entry_ebb, type_);
        }

        self.generate_stmt(&func.stmt);

        let ebb = self.position().ebb();
        let inst = self.func.last_inst(ebb);
        if !self.func.inst(inst).is_terminator() {
            self.push_return_inst();
        }

        self.func
    }

    fn generate_stmt(&mut self, stmt: &ast::Stmt<'input>) {
        match *stmt {
            ast::Stmt::Block(ref stmt) => self.generate_block_stmt(stmt),
            ast::Stmt::Decl(ref stmt) => self.generate_decl_stmt(stmt),
            ast::Stmt::Assign(ref stmt) => self.generate_assign_stmt(stmt),
            ast::Stmt::If(ref stmt) => self.generate_if_stmt(stmt),
            ast::Stmt::While(ref stmt) => self.generate_while_stmt(stmt),
            ast::Stmt::Return(ref stmt) => self.generate_return_stmt(stmt),
        }
    }

    fn generate_block_stmt(&mut self, stmt: &ast::BlockStmt<'input>) {
        for stmt in &stmt.stmts {
            self.generate_stmt(stmt);
        }
    }

    fn generate_decl_stmt(&mut self, stmt: &ast::DeclStmt<'input>) {
        let expr_value = self.generate_expr(&stmt.expr);

        let variable = stmt.identifier.name;

        self.insert_value(variable, expr_value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let variable = stmt.identifier.name;

        let expr_value = self.generate_expr(&stmt.expr);

        let value = match self.predicate {
            Some(predicate) => {
                let prev_value = self.get_value(variable);
                self.push_select_inst(
                    predicate,
                    Operand::Value(expr_value),
                    Operand::Value(prev_value),
                )
            }
            None => expr_value,
        };

        self.insert_value(variable, value);
    }

    fn generate_if_stmt(&mut self, stmt: &ast::IfStmt<'input>) {
        let condition_value = self.generate_expr(&stmt.expr);

        let qualifier = self.get_value_type(condition_value).qualifier();
        match qualifier {
            Uniform => {
                let else_ebb = self.create_ebb();
                let merge_ebb = match stmt.else_stmt {
                    Some(_) => self.create_ebb(),
                    None => else_ebb,
                };

                self.push_branch_inst(BranchOp::AllFalse, condition_value, else_ebb);

                self.generate_stmt(&stmt.if_stmt);
                self.push_jump_inst(merge_ebb);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.set_position_ebb(else_ebb);

                    self.generate_stmt(else_stmt);
                    self.push_jump_inst(merge_ebb);
                }

                self.set_position_ebb(merge_ebb);
            }
            Varying => {
                let prev_predicate = self.set_predicate_and(condition_value);

                self.generate_stmt(&stmt.if_stmt);
                self.reset_predicate(prev_predicate);

                if let Some(ref else_stmt) = stmt.else_stmt {
                    self.set_predicate_and_not(condition_value);

                    self.generate_stmt(else_stmt);
                    self.reset_predicate(prev_predicate);
                }
            }
        }
    }

    fn generate_while_stmt(&mut self, stmt: &ast::WhileStmt<'input>) {
        let header_ebb = self.create_ebb();
        let after_ebb = self.create_ebb();

        self.push_jump_inst(header_ebb);
        self.set_position_ebb(header_ebb);

        let condition_value = self.generate_expr(&stmt.expr);

        let qualifier = self.get_value_type(condition_value).qualifier();
        match qualifier {
            Uniform => {
                self.push_branch_inst(BranchOp::AllFalse, condition_value, after_ebb);

                self.generate_stmt(&stmt.stmt);
                self.push_jump_inst(header_ebb);

                self.set_position_ebb(after_ebb);
            }
            Varying => {
                let prev_predicate = self.set_predicate_and(condition_value);

                if let Some(predicate) = self.predicate {
                    self.push_branch_inst(BranchOp::AnyFalse, predicate, after_ebb);
                }

                self.generate_stmt(&stmt.stmt);
                self.push_jump_inst(header_ebb);
                self.set_position_ebb(after_ebb);

                self.reset_predicate(prev_predicate);
            }
        };
    }

    fn generate_return_stmt(&mut self, _stmt: &ast::ReturnStmt) {
        match self.predicate {
            Some(predicate) => {
                let not_predicate = self.push_unary_inst(UnaryOp::Not, Operand::Value(predicate));
                let not_returned = match self.not_returned {
                    Some(not_returned) => self.push_binary_inst(
                        BinaryOp::Add,
                        Operand::Value(not_returned),
                        Operand::Value(not_predicate),
                    ),
                    None => not_predicate,
                };
                self.not_returned = Some(not_returned);
            }
            None => {
                self.push_return_inst();

                let ebb = self.create_ebb();
                self.set_position_ebb(ebb);
            }
        };
    }

    fn generate_expr(&mut self, expr: &ast::Expr<'input>) -> Value {
        match *expr {
            ast::Expr::IntLiteral(ref int_literal) => self.generate_int_literal(int_literal),
            ast::Expr::FloatLiteral(ref float_literal) => {
                self.generate_float_literal(float_literal)
            }
            ast::Expr::BoolLiteral(ref bool_literal) => self.generate_bool_literal(bool_literal),
            ast::Expr::Index(_) => self.generate_index(),
            ast::Expr::Count(_) => self.generate_count(),
            ast::Expr::Identifier(ref identifier) => self.generate_identifier(identifier),
            ast::Expr::Unary(ref expr) => self.generate_unary_expr(expr),
            ast::Expr::Binary(ref expr) => self.generate_binary_expr(expr),
        }
    }

    fn generate_int_literal(&mut self, int_literal: &ast::IntLiteral) -> Value {
        self.push_int_const_inst(int_literal.value)
    }

    fn generate_float_literal(&mut self, float_literal: &ast::FloatLiteral) -> Value {
        self.push_float_const_inst(float_literal.value)
    }

    fn generate_bool_literal(&mut self, bool_literal: &ast::BoolLiteral) -> Value {
        self.push_bool_const_inst(bool_literal.value)
    }

    fn generate_index(&mut self) -> Value {
        self.push_index_inst()
    }

    fn generate_count(&mut self) -> Value {
        self.push_count_inst()
    }

    fn generate_identifier(&mut self, identifier: &ast::Identifier<'input>) -> Value {
        self.get_value(identifier.name)
    }

    fn generate_unary_expr(&mut self, expr: &ast::UnaryExpr<'input>) -> Value {
        let src_value = self.generate_expr(&expr.expr);

        let op = expr.op;
        let src_type = self.get_value_type(src_value);

        let src = Operand::Value(src_value);

        match (op, src_type.type_()) {
            (ast::UnaryOp::Negate, Int) => {
                self.push_binary_inst(BinaryOp::Sub, Operand::Int(0), src)
            }
            (ast::UnaryOp::Negate, Float) => {
                self.push_binary_inst(BinaryOp::Fsub, Operand::Float(0.), src)
            }
            (ast::UnaryOp::BitNot, Int) | (ast::UnaryOp::LogicalNot, Bool) => {
                self.push_unary_inst(UnaryOp::Not, src)
            }
            _ => panic!(
                "Invalid unary expression '{}' with operand type '{}'",
                op, src_type
            ),
        }
    }

    fn generate_binary_expr(&mut self, expr: &ast::BinaryExpr<'input>) -> Value {
        let left_value = self.generate_expr(&expr.left);
        let right_value = self.generate_expr(&expr.right);

        let op = expr.op;
        let left_type = self.get_value_type(left_value);
        let right_type = self.get_value_type(right_value);

        let left = Operand::Value(left_value);
        let right = Operand::Value(right_value);

        if let Some(op) = get_binary_op(op, left_type, right_type) {
            return self.push_binary_inst(op, left, right);
        }

        if let Some(op) = get_int_comp_op(op, left_type, right_type) {
            return self.push_int_comp_inst(op, left, right);
        }

        if let Some(op) = get_float_comp_op(op, left_type, right_type) {
            return self.push_float_comp_inst(op, left, right);
        }

        panic!(
            "Invalid binary expression '{}' with operand types '{}' and '{}'",
            op, left_type, right_type
        )
    }

    fn get_value(&mut self, variable: Variable<'input>) -> Value {
        let block = self.position().block();
        if let Ok(value) = self.get_value_in_ebb(block, variable) {
            return value;
        }

        let mut calls = Vec::new();
        calls.push(block);

        let mut ebbs = Vec::new();
        let mut predecessors = HashSet::new();
        let mut type_ = None;

        while let Some(block) = calls.pop() {
            match self.get_value_in_ebb(block, variable) {
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
                    let ebb = data.ebb();
                    if ebbs.contains(&ebb) {
                        continue;
                    }

                    ebbs.push(ebb);

                    for &predecessor in data.predecessors() {
                        predecessors.insert(predecessor);
                        calls.push(predecessor.block());
                    }
                }
            };
        }

        let type_ = type_.unwrap();

        for ebb in ebbs {
            self.create_block_param(variable, ebb, type_);
        }
        for predecessor in predecessors {
            let block = predecessor.block();
            let inst = predecessor.inst();
            let value = self.get_value_in_ebb(block, variable).unwrap();
            self.push_target_arg(inst, value);
        }

        self.get_value_in_ebb(block, variable).unwrap()
    }

    fn get_value_in_ebb(
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
                    BlockData::Body(ref data) => block = data.predecessor(),
                },
            }
        }
    }

    fn get_value_type(&self, value: Value) -> Type {
        self.func.value(value).type_()
    }

    fn position(&self) -> &Position {
        self.position.as_ref().unwrap()
    }

    fn set_position_ebb(&mut self, ebb: Ebb) {
        self.func.push_ebb(ebb);
        let block = self.header_blocks[&ebb];
        self.position = Some(Position::new(ebb, block));
    }

    fn set_position_block(&mut self, block: Block) {
        let ebb = self.position().ebb();
        self.position = Some(Position::new(ebb, block));
    }

    fn set_predicate_and(&mut self, value: Value) -> Option<Value> {
        let predicate = match self.predicate {
            Some(predicate) => self.push_binary_inst(
                BinaryOp::And,
                Operand::Value(predicate),
                Operand::Value(value),
            ),
            None => value,
        };
        self.set_predicate(predicate)
    }

    fn set_predicate_and_not(&mut self, value: Value) -> Option<Value> {
        let not_value = self.push_unary_inst(UnaryOp::Not, Operand::Value(value));

        let predicate = match self.predicate {
            Some(predicate) => self.push_binary_inst(
                BinaryOp::Add,
                Operand::Value(predicate),
                Operand::Value(not_value),
            ),
            None => not_value,
        };
        self.set_predicate(predicate)
    }

    fn set_predicate(&mut self, predicate: Value) -> Option<Value> {
        let prev_predicate = self.predicate;
        self.predicate = Some(predicate);
        prev_predicate
    }

    fn reset_predicate(&mut self, predicate: Option<Value>) {
        self.predicate = match self.not_returned {
            Some(not_returned) => {
                let predicate = match predicate {
                    Some(predicate) => self.push_binary_inst(
                        BinaryOp::And,
                        Operand::Value(predicate),
                        Operand::Value(not_returned),
                    ),
                    None => not_returned,
                };
                Some(predicate)
            }
            None => predicate,
        };
    }

    fn insert_value(&mut self, variable: Variable<'input>, value: Value) {
        let block = self.position().block();
        self.values.insert_value(block, variable, value);
    }

    fn insert_predecessor(&mut self, ebb: Ebb, predecessor: Predecessor) {
        let block = self.header_blocks[&ebb];
        match self.blocks[block] {
            BlockData::Header(ref mut data) => data.insert_predecessor(predecessor),
            _ => panic!(),
        }
    }

    fn create_func_param(&mut self, type_: Type) {
        self.func.push_param(type_);
    }

    fn create_ebb(&mut self) -> Ebb {
        let ebb = self.func.create_ebb();
        let block = self.blocks.create(BlockData::Header(HeaderBlock::new(ebb)));
        self.header_blocks.insert(ebb, block);
        ebb
    }

    fn create_block(&mut self) -> Block {
        let predecessor = self.position().block();
        self.blocks
            .create(BlockData::Body(BodyBlock::new(predecessor)))
    }

    fn create_block_param(&mut self, variable: Variable<'input>, ebb: Ebb, type_: Type) -> Value {
        let value = self.create_value(type_);
        self.func.push_ebb_param(ebb, value);

        let header_block = self.header_blocks[&ebb];
        self.values.insert_param(header_block, variable, value);

        value
    }

    fn create_target(&mut self, ebb: Ebb) -> Target {
        let block = self.header_blocks[&ebb];
        let mut target = Target::new(ebb);

        for i in 0..self.func.ebb(ebb).params().len() {
            let param = self.func.ebb(ebb).params()[i];
            let variable = self.values.get_param(block, param).unwrap();
            let value = self.get_value(variable);
            target.push_arg(value);
        }

        target
    }

    fn create_value(&mut self, type_: Type) -> Value {
        self.func.create_value(ValueData::new(type_))
    }

    fn push_target_arg(&mut self, inst: Inst, value: Value) {
        if let Some(target) = self.func.inst_mut(inst).target_mut() {
            target.push_arg(value);
        }
    }

    fn push_int_const_inst(&mut self, value: i32) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.create_value(type_);
        let inst = InstData::IntConst(IntConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    fn push_float_const_inst(&mut self, value: f32) -> Value {
        let type_ = Type::new(Uniform, Float);
        let dest = self.create_value(type_);
        let inst = InstData::FloatConst(FloatConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    fn push_bool_const_inst(&mut self, value: bool) -> Value {
        let type_ = Type::new(Uniform, Bool);
        let dest = self.create_value(type_);
        let inst = InstData::BoolConst(BoolConstInst::new(dest, value));
        self.push_inst(inst);
        dest
    }

    fn push_index_inst(&mut self) -> Value {
        let type_ = Type::new(Varying, Int);
        let dest = self.create_value(type_);
        let inst = InstData::Index(IndexInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_count_inst(&mut self) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.create_value(type_);
        let inst = InstData::Count(CountInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_unary_inst(&mut self, op: UnaryOp, src: Operand) -> Value {
        let type_ = self.get_unary_inst_type(src);
        let dest = self.create_value(type_);
        let inst = InstData::Unary(UnaryInst::new(op, dest, src));
        self.push_inst(inst);
        dest
    }

    fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_binary_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::Binary(BinaryInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::IntComp(IntCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.create_value(type_);
        let inst = InstData::FloatComp(FloatCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_select_inst(&mut self, cond: Value, left: Operand, right: Operand) -> Value {
        let type_ = self.get_select_inst_type(left, right);
        let dest = self.create_value(type_);
        let inst = InstData::Select(SelectInst::new(dest, cond, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_jump_inst(&mut self, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::Jump(JumpInst::new(target));
        self.push_inst(inst);
    }

    fn push_branch_inst(&mut self, op: BranchOp, cond: Value, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::Branch(BranchInst::new(op, cond, target));
        self.push_inst(inst);
    }

    fn push_return_inst(&mut self) {
        let inst = InstData::Return(ReturnInst::new());
        self.push_inst(inst);
    }

    fn push_inst(&mut self, data: InstData) {
        let ebb = self.position().ebb();
        let inst = self.func.create_inst(data.clone());
        self.func.push_inst(ebb, inst);

        if let Some(target) = data.target() {
            let block = self.position().block();
            let predecessor = Predecessor::new(block, inst);
            self.insert_predecessor(target.ebb(), predecessor);

            let block = self.create_block();
            self.set_position_block(block);
        }
    }

    fn get_unary_inst_type(&self, src: Operand) -> Type {
        self.get_operand_type(src)
    }

    fn get_binary_inst_type(&self, op: BinaryOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.type_(),
            right_type.type_(),
            "Invalid binary instruction '{}' with operand types '{}' and '{}'",
            op,
            left_type,
            right_type
        );

        let type_ = left_type.type_();
        let qualifier = get_type_qualifier(left_type, right_type);

        Type::new(qualifier, type_)
    }

    fn get_comp_inst_type(&self, op: CompOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.type_(),
            right_type.type_(),
            "Invalid comparison instruction '{}' with operand types '{}' and '{}'",
            op,
            left_type,
            right_type
        );

        let qualifier = get_type_qualifier(left_type, right_type);

        Type::new(qualifier, Bool)
    }

    fn get_select_inst_type(&self, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);
        assert_eq!(
            left_type.type_(),
            right_type.type_(),
            "Invalid select instruction with operand types '{}' and '{}'",
            left_type,
            right_type
        );

        let type_ = left_type.type_();
        let qualifier = get_type_qualifier(left_type, right_type);

        Type::new(qualifier, type_)
    }

    fn get_operand_type(&self, operand: Operand) -> Type {
        match operand {
            Operand::Int(_) => Type::new(Uniform, Int),
            Operand::Float(_) => Type::new(Uniform, Float),
            Operand::Bool(_) => Type::new(Uniform, Bool),
            Operand::Value(value) => self.get_value_type(value),
        }
    }
}

fn get_binary_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<BinaryOp> {
    match (op, left_type.type_(), right_type.type_()) {
        (ast::BinaryOp::Mul, Float, Float) => Some(BinaryOp::Fmul),
        (ast::BinaryOp::Add, Int, Int) => Some(BinaryOp::Add),
        (ast::BinaryOp::Add, Float, Float) => Some(BinaryOp::Fadd),
        (ast::BinaryOp::Sub, Int, Int) => Some(BinaryOp::Sub),
        (ast::BinaryOp::Sub, Float, Float) => Some(BinaryOp::Fsub),
        (ast::BinaryOp::Shl, Int, Int) => Some(BinaryOp::Shl),
        (ast::BinaryOp::Shr, Int, Int) => Some(BinaryOp::Asr),
        (ast::BinaryOp::Min, Int, Int) => Some(BinaryOp::Min),
        (ast::BinaryOp::Min, Float, Float) => Some(BinaryOp::Fmin),
        (ast::BinaryOp::Max, Int, Int) => Some(BinaryOp::Max),
        (ast::BinaryOp::Max, Float, Float) => Some(BinaryOp::Fmax),
        (ast::BinaryOp::BitAnd, Int, Int) | (ast::BinaryOp::LogicalAnd, Bool, Bool) => {
            Some(BinaryOp::And)
        }
        (ast::BinaryOp::BitOr, Int, Int) | (ast::BinaryOp::LogicalOr, Bool, Bool) => {
            Some(BinaryOp::Or)
        }
        (ast::BinaryOp::BitXor, Int, Int) => Some(BinaryOp::Xor),
        _ => None,
    }
}

fn get_int_comp_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<CompOp> {
    match (left_type.type_(), right_type.type_()) {
        (Int, Int) | (Bool, Bool) => get_comp_op(op),
        _ => None,
    }
}

fn get_float_comp_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<CompOp> {
    match (left_type.type_(), right_type.type_()) {
        (Float, Float) => get_comp_op(op),
        _ => None,
    }
}

fn get_comp_op(op: ast::BinaryOp) -> Option<CompOp> {
    match op {
        ast::BinaryOp::Eq => Some(CompOp::Eq),
        ast::BinaryOp::Ne => Some(CompOp::Ne),
        ast::BinaryOp::Lt => Some(CompOp::Lt),
        ast::BinaryOp::Gt => Some(CompOp::Gt),
        ast::BinaryOp::Le => Some(CompOp::Le),
        ast::BinaryOp::Ge => Some(CompOp::Ge),
        _ => None,
    }
}

pub fn get_type_qualifier(left_type: Type, right_type: Type) -> TypeQualifier {
    match (left_type.qualifier(), right_type.qualifier()) {
        (Uniform, Uniform) => Uniform,
        _ => Varying,
    }
}
