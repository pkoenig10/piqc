use std::collections::HashMap;

use ast;
use collections::*;
use ir::*;

pub fn generate_ir(prog: &ast::Prog) -> Prog {
    let builder = IrBuilder::new();
    let func = builder.generate_func(prog.func());
    Prog::new(func)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Variable<'a> {
    Variable(&'a str),
    Predicate,
    Count,
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
        ValueTable { values: HashMap::new() }
    }

    fn insert_param(&mut self, block: Block, variable: Variable<'a>, value: Value) {
        self.get_values_mut(block).insert_param(variable, value);
    }

    fn get_param(&self, block: Block, value: Value) -> Option<Variable<'a>> {
        self.get_values(block).and_then(
            |values| values.get_param(value),
        )
    }

    fn insert_value(&mut self, block: Block, variable: Variable<'a>, value: Value) {
        self.get_values_mut(block).insert_value(variable, value);
    }

    fn get_value(&self, block: Block, variable: Variable<'a>) -> Option<Value> {
        self.get_values(block).and_then(
            |values| values.get_value(variable),
        )
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
}

impl<'input> IrBuilder<'input> {
    fn new() -> IrBuilder<'input> {
        IrBuilder {
            func: Func::new(),
            values: ValueTable::new(),
            blocks: Map::new(),
            header_blocks: HashMap::new(),
            position: None,
        }
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        let entry_ebb = self.create_ebb();
        self.set_position_ebb(entry_ebb);

        for param in func.params() {
            let name = param.identifier().name();
            let variable = Variable::Variable(name);
            let type_ = match param.type_() {
                ast::Int => Type::new(Uniform, Int),
                ast::Float => Type::new(Uniform, Float),
                ast::Bool => Type::new(Uniform, Bool),
            };
            self.create_func_param(type_);
            self.create_block_param(variable, entry_ebb, Some(type_));
        }

        let predicate_value = self.push_bool_const_inst(BoolImmediate::new(true));
        self.insert_value(Variable::Predicate, predicate_value);

        let count_value = self.push_int_const_inst(IntImmediate::new(0));
        self.insert_value(Variable::Count, count_value);

        self.generate_stmt(func.stmt());

        let ebb = self.position().ebb();
        let inst = self.func.last_inst(ebb);
        if !self.func.inst(inst).is_terminator() {
            self.push_return_inst();
        }

        self.func
    }

    fn generate_stmt(&mut self, stmt: &ast::Stmt<'input>) {
        match *stmt {
            ast::Stmt::BlockStmt(ref stmt) => self.generate_block_stmt(stmt),
            ast::Stmt::DeclStmt(ref stmt) => self.generate_decl_stmt(stmt),
            ast::Stmt::AssignStmt(ref stmt) => self.generate_assign_stmt(stmt),
            ast::Stmt::IfStmt(ref stmt) => self.generate_if_stmt(stmt),
            ast::Stmt::WhileStmt(ref stmt) => self.generate_while_stmt(stmt),
            ast::Stmt::ReturnStmt(ref stmt) => self.generate_return_stmt(stmt),
        }
    }

    fn generate_block_stmt(&mut self, stmt: &ast::BlockStmt<'input>) {
        for stmt in stmt.stmts() {
            self.generate_stmt(stmt);
        }
    }

    fn generate_decl_stmt(&mut self, stmt: &ast::DeclStmt<'input>) {
        let expr_value = self.generate_expr(stmt.expr());

        let name = stmt.identifier().name();
        let variable = Variable::Variable(name);

        self.insert_value(variable, expr_value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let expr_value = self.generate_expr(stmt.expr());

        let name = stmt.identifier().name();
        let variable = Variable::Variable(name);

        let predicate_value = self.get_value(Variable::Predicate);
        let prev_value = self.get_value(variable);
        let expr = Operand::Value(expr_value);
        let prev = Operand::Value(prev_value);
        let value = self.push_select_inst(predicate_value, expr, prev);

        self.insert_value(variable, value);
    }

    fn generate_if_stmt(&mut self, stmt: &ast::IfStmt<'input>) {
        let condition_value = self.generate_expr(stmt.expr());

        let qualifier = self.func
            .value(condition_value)
            .type_()
            .unwrap()
            .qualifier();
        let if_stmt = stmt.if_stmt();
        let else_stmt = stmt.else_stmt();

        match (qualifier, else_stmt) {
            (Uniform, None) => {
                self.generate_uniform_if_statement(condition_value, if_stmt);
            }
            (Uniform, Some(else_stmt)) => {
                self.generate_uniform_if_else_statement(condition_value, if_stmt, else_stmt);
            }
            (Varying, None) => {
                self.generate_varying_if_statement(condition_value, if_stmt);
            }
            (Varying, Some(else_stmt)) => {
                self.generate_varying_if_else_statement(condition_value, if_stmt, else_stmt);
            }
        };
    }

    fn generate_uniform_if_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
    ) {
        let merge_ebb = self.create_ebb();

        self.push_branch_inst(AllFalse, condition_value, merge_ebb);

        self.generate_stmt(if_stmt);
        self.push_jump_inst(merge_ebb);
        self.set_position_ebb(merge_ebb);
    }

    fn generate_uniform_if_else_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
        else_stmt: &ast::Stmt<'input>,
    ) {
        let else_ebb = self.create_ebb();
        let merge_ebb = self.create_ebb();

        self.push_branch_inst(AllFalse, condition_value, else_ebb);

        self.generate_stmt(if_stmt);
        self.push_jump_inst(merge_ebb);
        self.set_position_ebb(else_ebb);

        self.generate_stmt(else_stmt);
        self.push_jump_inst(merge_ebb);
        self.set_position_ebb(merge_ebb);
    }

    fn generate_varying_if_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
    ) {
        self.increment_count();
        self.push_predicate(condition_value);

        self.generate_stmt(if_stmt);

        self.pop_predicate();
        self.decrement_count();
    }

    fn generate_varying_if_else_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
        else_stmt: &ast::Stmt<'input>,
    ) {
        self.increment_count();
        self.push_predicate(condition_value);

        self.generate_stmt(if_stmt);

        self.invert_predicate();

        self.generate_stmt(else_stmt);

        self.pop_predicate();
        self.decrement_count();
    }

    fn generate_while_stmt(&mut self, stmt: &ast::WhileStmt<'input>) {
        let header_ebb = self.create_ebb();

        self.increment_count();

        self.push_jump_inst(header_ebb);
        self.set_position_ebb(header_ebb);

        let condition_value = self.generate_expr(stmt.expr());

        let qualifier = self.func
            .value(condition_value)
            .type_()
            .unwrap()
            .qualifier();
        let while_stmt = stmt.stmt();

        match qualifier {
            Uniform => self.generate_uniform_while_statement(condition_value, while_stmt),
            Varying => self.generate_varying_while_statement(condition_value, while_stmt),
        }

        self.decrement_count();
    }

    fn generate_uniform_while_statement(
        &mut self,
        condition_value: Value,
        while_stmt: &ast::Stmt<'input>,
    ) {
        let header_ebb = self.position().ebb();
        let after_ebb = self.create_ebb();

        self.push_branch_inst(AllFalse, condition_value, after_ebb);

        self.generate_stmt(while_stmt);
        self.push_jump_inst(header_ebb);
        self.set_position_ebb(after_ebb);
    }

    fn generate_varying_while_statement(
        &mut self,
        condition_value: Value,
        while_stmt: &ast::Stmt<'input>,
    ) {
        let header_ebb = self.position().ebb();
        let after_ebb = self.create_ebb();

        self.push_predicate(condition_value);

        let predicate_value = self.get_value(Variable::Predicate);
        self.push_branch_inst(AnyFalse, predicate_value, after_ebb);

        self.generate_stmt(while_stmt);
        self.push_jump_inst(header_ebb);
        self.set_position_ebb(after_ebb);

        self.pop_predicate();
    }

    fn generate_return_stmt(&mut self, _stmt: &ast::ReturnStmt) {
        self.push_return_inst();
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
            ast::Expr::UnaryExpr(ref expr) => self.generate_unary_expr(expr),
            ast::Expr::BinaryExpr(ref expr) => self.generate_binary_expr(expr),
        }
    }

    fn generate_int_literal(&mut self, int_literal: &ast::IntLiteral) -> Value {
        let immediate = IntImmediate::new(int_literal.value());
        self.push_int_const_inst(immediate)
    }

    fn generate_float_literal(&mut self, float_literal: &ast::FloatLiteral) -> Value {
        let immediate = FloatImmediate::new(float_literal.value());
        self.push_float_const_inst(immediate)
    }

    fn generate_bool_literal(&mut self, bool_literal: &ast::BoolLiteral) -> Value {
        let immediate = BoolImmediate::new(bool_literal.value());
        self.push_bool_const_inst(immediate)
    }

    fn generate_index(&mut self) -> Value {
        self.push_index_inst()
    }

    fn generate_count(&mut self) -> Value {
        self.push_count_inst()
    }

    fn generate_identifier(&mut self, identifier: &ast::Identifier<'input>) -> Value {
        let name = identifier.name();
        let variable = Variable::Variable(name);
        self.get_value(variable)
    }

    fn generate_unary_expr(&mut self, expr: &ast::UnaryExpr<'input>) -> Value {
        let src_value = self.generate_expr(expr.expr());

        let op = expr.op();
        let src_type = self.func.value(src_value).type_().unwrap();

        let src = Operand::Value(src_value);

        match (op, src_type.type_()) {
            (ast::Negate, Int) => {
                let zero = Operand::IntImmediate(IntImmediate::new(0));
                self.push_binary_inst(Sub, zero, src)
            }
            (ast::Negate, Float) => {
                let zero = Operand::FloatImmediate(FloatImmediate::new(0.));
                self.push_binary_inst(Fsub, zero, src)
            }
            (ast::BitNot, Int) |
            (ast::LogicalNot, Bool) => self.push_unary_inst(Not, src),
            _ => {
                panic!(
                    "Invalid unary expression '{}' with operand type '{}'",
                    op,
                    src_type
                )
            }
        }
    }

    fn generate_binary_expr(&mut self, expr: &ast::BinaryExpr<'input>) -> Value {
        let left_value = self.generate_expr(expr.left());
        let right_value = self.generate_expr(expr.right());

        let op = expr.op();
        let left_type = self.func.value(left_value).type_().unwrap();
        let right_type = self.func.value(right_value).type_().unwrap();

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
            op,
            left_type,
            right_type
        )
    }

    fn get_value(&mut self, variable: Variable<'input>) -> Value {
        let block = self.position().block();
        if let Ok(value) = self.get_value_in_ebb(block, variable) {
            return value;
        }

        let mut calls = Vec::new();
        calls.push((block, None));

        let mut values = Vec::new();
        let mut type_ = None;

        while let Some((block, inst)) = calls.pop() {
            let value = match self.get_value_in_ebb(block, variable) {
                Ok(value) => {
                    let value_type = self.func.value(value).type_().unwrap();
                    let type_ = type_.get_or_insert(value_type);
                    assert_eq!(
                        *type_,
                        value_type,
                        "Variable defined with multiple types '{}' and '{}'",
                        type_,
                        value_type
                    );

                    Some(value)
                }
                Err(block) => {
                    let ebb = match *self.blocks.get(block) {
                        BlockData::Header(ref data) => {
                            for &(block, inst) in data.predecessors() {
                                calls.push((block, Some(inst)));
                            }
                            data.ebb()
                        }
                        _ => panic!(),
                    };

                    let value = self.create_block_param(variable, ebb, None);
                    values.push(value);

                    Some(value)
                }
            };

            if let (Some(inst), Some(value)) = (inst, value) {
                self.push_target_arg(inst, value);
            }
        }

        let type_ = type_.unwrap();
        for value in values {
            self.func.value_mut(value).set_type(type_);
        }

        self.get_value_in_ebb(block, variable).unwrap()
    }

    fn get_value_in_ebb(
        &mut self,
        block: Block,
        variable: Variable<'input>,
    ) -> Result<Value, Block> {
        let mut block = block;
        loop {
            match self.values.get_value(block, variable) {
                Some(value) => return Ok(value),
                None => {
                    match *self.blocks.get(block) {
                        BlockData::Header(_) => return Err(block),
                        BlockData::Body(ref data) => block = data.predecessor(),
                    }
                }
            }
        }
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

    fn insert_value(&mut self, variable: Variable<'input>, value: Value) {
        let block = self.position().block();
        self.values.insert_value(block, variable, value);
    }

    fn insert_predecessor(&mut self, ebb: Ebb, predecessor: (Block, Inst)) {
        let block = self.header_blocks[&ebb];
        match *self.blocks.get_mut(block) {
            BlockData::Header(ref mut data) => data.insert_predecessor(predecessor),
            BlockData::Body(_) => panic!(),
        }
    }

    fn create_func_param(&mut self, type_: Type) {
        self.func.push_param(type_);
    }

    fn create_ebb(&mut self) -> Ebb {
        let ebb = self.func.create_ebb();
        let block = self.blocks.create(
            BlockData::Header(HeaderBlockData::new(ebb)),
        );
        self.header_blocks.insert(ebb, block);
        ebb
    }

    fn create_block(&mut self) -> Block {
        let predecessor = self.position().block();
        self.blocks.create(
            BlockData::Body(BodyBlockData::new(predecessor)),
        )
    }

    fn create_block_param(
        &mut self,
        variable: Variable<'input>,
        ebb: Ebb,
        type_: Option<Type>,
    ) -> Value {
        let value = self.func.create_value(type_);
        self.func.push_ebb_param(ebb, value);

        let header_block = self.header_blocks[&ebb];
        self.values.insert_param(header_block, variable, value);

        value
    }

    fn create_target(&mut self, ebb: Ebb) -> Target {
        let block = self.header_blocks[&ebb];
        let mut args = Args::new();

        for i in 0..self.func.ebb(ebb).params().len() {
            let param = self.func.ebb(ebb).params()[i];
            let variable = self.values.get_param(block, param).unwrap();
            let value = self.get_value(variable);
            args.push(value);
        }

        Target::new(ebb, args)
    }

    fn push_target_arg(&mut self, inst: Inst, value: Value) {
        if let Some(target) = self.func.inst_mut(inst).target_mut() {
            target.push_arg(value);
        }
    }

    fn push_predicate(&mut self, condition_value: Value) {
        let predicate_value = self.get_value(Variable::Predicate);

        let predicate = Operand::Value(predicate_value);
        let condition = Operand::Value(condition_value);
        let predicate_value = self.push_select_inst(predicate_value, condition, predicate);
        self.insert_value(Variable::Predicate, predicate_value);
    }

    fn pop_predicate(&mut self) {
        let count_value = self.get_value(Variable::Count);

        let count = Operand::Value(count_value);
        let zero = Operand::IntImmediate(IntImmediate::new(0));
        let predicate_value = self.push_int_comp_inst(Eq, count, zero);
        self.insert_value(Variable::Predicate, predicate_value);
    }

    fn invert_predicate(&mut self) {
        let count_value = self.get_value(Variable::Count);
        let predicate_value = self.get_value(Variable::Predicate);

        let predicate = Operand::Value(predicate_value);
        let not_predciate_value = self.push_unary_inst(Not, predicate);

        let count = Operand::Value(count_value);
        let zero = Operand::IntImmediate(IntImmediate::new(0));
        let count_zero_value = self.push_int_comp_inst(Eq, count, zero);

        let not_predicate = Operand::Value(not_predciate_value);
        let count_zero = Operand::Value(count_zero_value);
        let predicate_value = self.push_binary_inst(And, not_predicate, count_zero);
        self.insert_value(Variable::Predicate, predicate_value);
    }

    fn increment_count(&mut self) {
        self.update_count(Add);
    }

    fn decrement_count(&mut self) {
        self.update_count(Sub);
    }

    fn update_count(&mut self, op: BinaryOp) {
        let predicate_value = self.get_value(Variable::Predicate);
        let count_value = self.get_value(Variable::Count);

        let count = Operand::Value(count_value);
        let one = Operand::IntImmediate(IntImmediate::new(1));
        let inc_count_value = self.push_binary_inst(op, count, one);

        let inc_count = Operand::Value(inc_count_value);
        let count_value = self.push_select_inst(predicate_value, count, inc_count);
        self.insert_value(Variable::Count, count_value);
    }

    fn push_int_const_inst(&mut self, immediate: IntImmediate) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::IntConstInst(IntConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_float_const_inst(&mut self, immediate: FloatImmediate) -> Value {
        let type_ = Type::new(Uniform, Float);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::FloatConstInst(FloatConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_bool_const_inst(&mut self, immediate: BoolImmediate) -> Value {
        let type_ = Type::new(Uniform, Bool);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::BoolConstInst(BoolConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_index_inst(&mut self) -> Value {
        let type_ = Type::new(Varying, Int);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::IndexInst(IndexInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_count_inst(&mut self) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::CountInst(CountInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_unary_inst(&mut self, op: UnaryOp, src: Operand) -> Value {
        let type_ = self.get_unary_inst_type(src);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::UnaryInst(UnaryInst::new(op, dest, src));
        self.push_inst(inst);
        dest
    }

    fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_binary_inst_type(op, left, right);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::BinaryInst(BinaryInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::IntCompInst(IntCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::FloatCompInst(FloatCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_select_inst(&mut self, cond: Value, left: Operand, right: Operand) -> Value {
        let type_ = self.get_select_inst_type(left, right);
        let dest = self.func.create_value(Some(type_));
        let inst = InstData::SelectInst(SelectInst::new(dest, cond, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_jump_inst(&mut self, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::JumpInst(JumpInst::new(target));
        self.push_inst(inst);
    }

    fn push_branch_inst(&mut self, op: BranchOp, cond: Value, ebb: Ebb) {
        let target = self.create_target(ebb);
        let inst = InstData::BranchInst(BranchInst::new(op, cond, target));
        self.push_inst(inst);
    }

    fn push_return_inst(&mut self) {
        let inst = InstData::ReturnInst(ReturnInst::new());
        self.push_inst(inst);
    }

    fn push_inst(&mut self, data: InstData) {
        let ebb = self.position().ebb();
        let inst = self.func.create_inst(data.clone());
        self.func.push_inst(ebb, inst);

        if let Some(target) = data.target() {
            let block = self.position().block();
            self.insert_predecessor(target.ebb(), (block, inst));

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
        assert!(
            left_type.type_() == right_type.type_(),
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
        assert!(
            left_type.type_() == right_type.type_(),
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
        assert!(
            left_type.type_() == right_type.type_(),
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
            Operand::IntImmediate(_) => Type::new(Uniform, Int),
            Operand::FloatImmediate(_) => Type::new(Uniform, Float),
            Operand::BoolImmediate(_) => Type::new(Uniform, Bool),
            Operand::Value(value) => self.func.value(value).type_().unwrap(),
        }
    }
}

fn get_binary_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<BinaryOp> {
    match (op, left_type.type_(), right_type.type_()) {
        (ast::Mul, Float, Float) => Some(Fmul),
        (ast::Add, Int, Int) => Some(Add),
        (ast::Add, Float, Float) => Some(Fadd),
        (ast::Sub, Int, Int) => Some(Sub),
        (ast::Sub, Float, Float) => Some(Fsub),
        (ast::Shl, Int, Int) => Some(Shl),
        (ast::Shr, Int, Int) => Some(Asr),
        (ast::Min, Int, Int) => Some(Min),
        (ast::Min, Float, Float) => Some(Fmin),
        (ast::Max, Int, Int) => Some(Max),
        (ast::Max, Float, Float) => Some(Fmax),
        (ast::BitAnd, Int, Int) |
        (ast::LogicalAnd, Bool, Bool) => Some(And),
        (ast::BitOr, Int, Int) |
        (ast::LogicalOr, Bool, Bool) => Some(Or),
        (ast::BitXor, Int, Int) => Some(Xor),
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
        ast::Eq => Some(Eq),
        ast::Ne => Some(Ne),
        ast::Lt => Some(Lt),
        ast::Gt => Some(Gt),
        ast::Le => Some(Le),
        ast::Ge => Some(Ge),
        _ => None,
    }
}

pub fn get_type_qualifier(left_type: Type, right_type: Type) -> TypeQualifier {
    match (left_type.qualifier(), right_type.qualifier()) {
        (Uniform, Uniform) => Uniform,
        _ => Varying,
    }
}
