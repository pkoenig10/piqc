use std::collections::HashMap;
use std::collections::HashSet;

use ast;
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

struct IrBuilder<'input> {
    func: Func,
    values: ValueTable<'input>,
    predecessors: HashMap<Block, Vec<Block>>,
    current_block: Option<Block>,
}

impl<'input> IrBuilder<'input> {
    fn new() -> IrBuilder<'input> {
        IrBuilder {
            func: Func::new(),
            values: ValueTable::new(),
            predecessors: HashMap::new(),
            current_block: None,
        }
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        let entry_block = self.create_block();
        self.push_block(entry_block);
        self.set_current_block(entry_block);

        for param in func.params() {
            let name = param.identifier().name();
            let variable = Variable::Variable(name);
            let type_ = match param.type_() {
                ast::Int => Type::new(Uniform, Int),
                ast::Float => Type::new(Uniform, Float),
                ast::Bool => Type::new(Uniform, Bool),
            };
            self.create_func_param(type_);
            self.create_block_param(variable, entry_block, Some(type_));
        }

        let predicate_value = self.push_bool_const_inst(BoolImmediate::new(true));
        self.values.insert_value(
            entry_block,
            Variable::Predicate,
            predicate_value,
        );

        let count_value = self.push_int_const_inst(IntImmediate::new(0));
        self.values.insert_value(
            entry_block,
            Variable::Count,
            count_value,
        );

        self.generate_stmt(func.stmt());

        let block = self.current_block();
        let inst = self.func.last_inst(block);
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

        let block = self.current_block();
        let name = stmt.identifier().name();
        let variable = Variable::Variable(name);

        self.values.insert_value(block, variable, expr_value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let expr_value = self.generate_expr(stmt.expr());

        let block = self.current_block();
        let name = stmt.identifier().name();
        let variable = Variable::Variable(name);

        let predicate_value = self.get_value(Variable::Predicate);
        let prev_value = self.get_value(variable);
        let expr = Operand::Value(expr_value);
        let prev = Operand::Value(prev_value);
        let value = self.push_select_inst(predicate_value, expr, prev);

        self.values.insert_value(block, variable, value);
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
        let if_block = self.create_block();
        let merge_block = self.create_block();

        self.push_branch_inst(All, condition_value, if_block, merge_block);

        self.push_block(if_block);
        self.set_current_block(if_block);
        self.generate_stmt(if_stmt);
        self.push_jump_inst(merge_block);

        self.push_block(merge_block);
        self.set_current_block(merge_block);
    }

    fn generate_uniform_if_else_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
        else_stmt: &ast::Stmt<'input>,
    ) {
        let if_block = self.create_block();
        let else_block = self.create_block();
        let merge_block = self.create_block();

        self.push_branch_inst(All, condition_value, if_block, else_block);

        self.push_block(if_block);
        self.set_current_block(if_block);
        self.generate_stmt(if_stmt);
        self.push_jump_inst(merge_block);

        self.push_block(else_block);
        self.set_current_block(else_block);
        self.generate_stmt(else_stmt);
        self.push_jump_inst(merge_block);

        self.push_block(merge_block);
        self.set_current_block(merge_block);
    }

    fn generate_varying_if_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
    ) {
        self.increment_count();
        self.set_predicate(condition_value);

        self.generate_stmt(if_stmt);

        self.unset_predicate();
        self.decrement_count();
    }

    fn generate_varying_if_else_statement(
        &mut self,
        condition_value: Value,
        if_stmt: &ast::Stmt<'input>,
        else_stmt: &ast::Stmt<'input>,
    ) {
        self.increment_count();
        self.set_predicate(condition_value);

        self.generate_stmt(if_stmt);

        self.invert_predicate();

        self.generate_stmt(else_stmt);

        self.unset_predicate();
        self.decrement_count();
    }



    fn generate_while_stmt(&mut self, stmt: &ast::WhileStmt<'input>) {
        let header_block = self.create_block();

        self.increment_count();

        self.push_jump_inst(header_block);

        self.push_block(header_block);
        self.set_current_block(header_block);
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
        let header_block = self.current_block();

        let loop_block = self.create_block();
        let after_block = self.create_block();

        self.push_branch_inst(All, condition_value, loop_block, after_block);

        self.push_block(loop_block);
        self.set_current_block(loop_block);
        self.generate_stmt(while_stmt);
        self.push_jump_inst(header_block);

        self.push_block(after_block);
        self.set_current_block(after_block);
    }

    fn generate_varying_while_statement(
        &mut self,
        condition_value: Value,
        while_stmt: &ast::Stmt<'input>,
    ) {
        let header_block = self.current_block();

        let loop_block = self.create_block();
        let after_block = self.create_block();

        self.set_predicate(condition_value);

        let predicate_value = self.get_value(Variable::Predicate);
        self.push_branch_inst(Any, predicate_value, loop_block, after_block);

        self.push_block(loop_block);
        self.set_current_block(loop_block);
        self.generate_stmt(while_stmt);
        self.push_jump_inst(header_block);

        self.push_block(after_block);
        self.set_current_block(after_block);

        self.unset_predicate();
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

    fn current_block(&self) -> Block {
        self.current_block.unwrap()
    }

    fn set_current_block(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    fn get_value(&mut self, variable: Variable<'input>) -> Value {
        let block = self.current_block();
        if let Some(value) = self.values.get_value(block, variable) {
            return value;
        }

        let mut blocks = Vec::new();
        blocks.push((block, None));

        let mut values = HashSet::new();
        let mut type_ = None;

        while let Some((block, successor)) = blocks.pop() {
            let value = match self.values.get_value(block, variable) {
                Some(value) => {
                    let value_type = self.func.value(value).type_();
                    match (value_type, type_) {
                        (None, _) => {
                            assert!(values.contains(&value), "Variable defined without type")
                        }
                        (Some(value_type), None) => {
                            type_ = Some(value_type);
                        }
                        (Some(value_type), Some(type_)) => {
                            assert!(
                                value_type == type_,
                                "Variable defined with multiple types '{}' and '{}'",
                                type_,
                                value_type
                            )
                        }
                    };

                    value
                }
                None => {
                    let value = self.create_block_param(variable, block, None);
                    values.insert(value);

                    let predecessors = &self.predecessors[&block];
                    assert!(!predecessors.is_empty(), "Variable not defined");
                    for predecessor in predecessors {
                        blocks.push((*predecessor, Some(block)));
                    }

                    value
                }
            };

            if let Some(successor) = successor {
                let inst = self.func.last_inst(block);
                self.push_target_arg(inst, successor, value);
            }
        }

        let type_ = type_.unwrap();
        for value in values {
            self.func.value_mut(value).set_type(type_);
        }

        self.values.get_value(block, variable).unwrap()
    }

    fn insert_predecessor(&mut self, block: Block, predecessor: Block) {
        self.predecessors
            .entry(block)
            .or_insert_with(Vec::new)
            .push(predecessor);
    }

    fn create_func_param(&mut self, type_: Type) {
        self.func.push_param(type_);
    }

    fn create_block(&mut self) -> Block {
        self.func.create_block()
    }

    fn create_block_param(
        &mut self,
        variable: Variable<'input>,
        block: Block,
        type_: Option<Type>,
    ) -> Value {
        let value = self.func.create_value(type_);
        self.func.push_block_param(block, value);
        self.values.insert_param(block, variable, value);
        value
    }

    fn push_block(&mut self, block: Block) {
        self.func.push_block(block);
    }

    fn create_target(&mut self, block: Block) -> Target {
        let mut args = Args::new();

        for i in 0..self.func.block(block).params().len() {
            let param = self.func.block(block).params()[i];
            let variable = self.values.get_param(block, param).unwrap();
            let value = self.get_value(variable);
            args.push(value);
        }

        Target::new(block, args)
    }

    fn push_target_arg(&mut self, inst: Inst, block: Block, value: Value) {
        let target = self.func.inst_mut(inst).get_target_mut(block);
        target.push_arg(value);
    }

    fn set_predicate(&mut self, condition_value: Value) {
        let current_block = self.current_block();
        let predicate_value = self.get_value(Variable::Predicate);

        let predicate = Operand::Value(predicate_value);
        let condition = Operand::Value(condition_value);
        let predicate_value = self.push_select_inst(predicate_value, condition, predicate);
        self.values.insert_value(
            current_block,
            Variable::Predicate,
            predicate_value,
        );
    }

    fn unset_predicate(&mut self) {
        let current_block = self.current_block();
        let count_value = self.get_value(Variable::Count);

        let count = Operand::Value(count_value);
        let zero = Operand::IntImmediate(IntImmediate::new(0));
        let predicate_value = self.push_int_comp_inst(Eq, count, zero);
        self.values.insert_value(
            current_block,
            Variable::Predicate,
            predicate_value,
        );
    }

    fn invert_predicate(&mut self) {
        let current_block = self.current_block();
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
        self.values.insert_value(
            current_block,
            Variable::Predicate,
            predicate_value,
        );
    }

    fn increment_count(&mut self) {
        self.update_count(Add);
    }

    fn decrement_count(&mut self) {
        self.update_count(Sub);
    }

    fn update_count(&mut self, op: BinaryOp) {
        let current_block = self.current_block();
        let predicate_value = self.get_value(Variable::Predicate);
        let count_value = self.get_value(Variable::Count);

        let count = Operand::Value(count_value);
        let one = Operand::IntImmediate(IntImmediate::new(1));
        let inc_count_value = self.push_binary_inst(op, count, one);

        let inc_count = Operand::Value(inc_count_value);
        let count_value = self.push_select_inst(predicate_value, count, inc_count);
        self.values.insert_value(
            current_block,
            Variable::Count,
            count_value,
        );
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

    fn push_jump_inst(&mut self, block: Block) {
        let target = self.create_target(block);
        let inst = InstData::JumpInst(JumpInst::new(target));
        self.push_inst(inst);

        let current_block = self.current_block();
        self.insert_predecessor(block, current_block);
    }

    fn push_branch_inst(
        &mut self,
        op: BranchOp,
        cond: Value,
        true_block: Block,
        false_block: Block,
    ) {
        let true_target = self.create_target(true_block);
        let false_target = self.create_target(false_block);
        let inst = InstData::BranchInst(BranchInst::new(op, cond, true_target, false_target));
        self.push_inst(inst);

        let current_block = self.current_block();
        self.insert_predecessor(true_block, current_block);
        self.insert_predecessor(false_block, current_block);
    }

    fn push_return_inst(&mut self) {
        let inst = InstData::ReturnInst(ReturnInst::new());
        self.push_inst(inst);
    }

    fn push_inst(&mut self, data: InstData) {
        let block = self.current_block.unwrap();
        let inst = self.func.create_inst(data);
        self.func.push_inst(block, inst);
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
