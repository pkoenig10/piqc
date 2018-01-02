use std::collections::HashMap;
use std::collections::HashSet;

use ast;
use ir::*;

pub fn generate_ir(prog: &ast::Prog) -> Prog {
    let builder = IrBuilder::new();
    let func = builder.generate_func(prog.func());
    Prog::new(func)
}

#[derive(Debug)]
struct BlockValues<'a> {
    params: HashMap<Value, &'a str>,
    values: HashMap<&'a str, Value>,
}

impl<'a> BlockValues<'a> {
    fn new() -> BlockValues<'a> {
        BlockValues {
            params: HashMap::new(),
            values: HashMap::new(),
        }
    }

    fn get_param(&self, value: Value) -> Option<&'a str> {
        self.params.get(&value).cloned()
    }

    fn insert_param(&mut self, name: &'a str, value: Value) {
        self.params.insert(value, name);
        self.insert_value(name, value);
    }

    fn get_value(&self, name: &'a str) -> Option<Value> {
        self.values.get(&name).cloned()
    }

    fn insert_value(&mut self, name: &'a str, value: Value) {
        self.values.insert(name, value);
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

    fn insert_param(&mut self, block: Block, name: &'a str, value: Value) {
        self.get_values_mut(block).insert_param(name, value);
    }

    fn get_param(&self, block: Block, value: Value) -> Option<&'a str> {
        self.get_values(block).and_then(
            |values| values.get_param(value),
        )
    }

    fn insert_value(&mut self, block: Block, name: &'a str, value: Value) {
        self.get_values_mut(block).insert_value(name, value);
    }

    fn get_value(&self, block: Block, name: &'a str) -> Option<Value> {
        self.get_values(block).and_then(
            |values| values.get_value(name),
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
    current_block: Option<Block>,
    values: ValueTable<'input>,
    predecessors: HashMap<Block, Vec<Block>>,
}

impl<'input> IrBuilder<'input> {
    fn new() -> IrBuilder<'input> {
        IrBuilder {
            func: Func::new(),
            current_block: None,
            values: ValueTable::new(),
            predecessors: HashMap::new(),
        }
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        let entry_block = self.create_block();
        self.push_block(entry_block);
        self.set_current_block(entry_block);

        for param in func.params() {
            let name = param.identifier().name();
            let type_ = match param.type_() {
                ast::Int => Type::new(Uniform, Int),
                ast::Float => Type::new(Uniform, Float),
                ast::Bool => Type::new(Uniform, Bool),
            };
            self.create_func_param(type_);
            self.create_block_param(name, entry_block, type_);
        }

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
        let value = self.generate_expr(stmt.expr());

        let block = self.current_block();
        let name = stmt.identifier().name();
        self.values.insert_value(block, name, value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let value = self.generate_expr(stmt.expr());

        let block = self.current_block();
        let name = stmt.identifier().name();
        self.values.insert_value(block, name, value);
    }

    fn generate_if_stmt(&mut self, stmt: &ast::IfStmt<'input>) {
        match stmt.else_stmt() {
            None => {
                let if_block = self.create_block();
                let merge_block = self.create_block();

                let value = self.generate_expr(stmt.expr());
                self.push_branch_inst(value, if_block, merge_block);

                self.push_block(if_block);
                self.set_current_block(if_block);
                self.generate_stmt(stmt.if_stmt());
                self.push_jump_inst(merge_block);

                self.push_block(merge_block);
                self.set_current_block(merge_block);
            },
            Some(else_stmt) => {
                let if_block = self.create_block();
                let else_block = self.create_block();
                let merge_block = self.create_block();

                let value = self.generate_expr(stmt.expr());
                self.push_branch_inst(value, if_block, else_block);

                self.push_block(if_block);
                self.set_current_block(if_block);
                self.generate_stmt(stmt.if_stmt());
                self.push_jump_inst(merge_block);

                self.push_block(else_block);
                self.set_current_block(else_block);
                self.generate_stmt(else_stmt);
                self.push_jump_inst(merge_block);

                self.push_block(merge_block);
                self.set_current_block(merge_block);
            }
        };
    }

    fn generate_while_stmt(&mut self, stmt: &ast::WhileStmt<'input>) {
        let header_block = self.create_block();
        let loop_block = self.create_block();
        let after_block = self.create_block();

        self.push_jump_inst(header_block);

        self.push_block(header_block);
        self.set_current_block(header_block);
        let value = self.generate_expr(stmt.expr());
        self.push_branch_inst(value, loop_block, after_block);

        self.push_block(loop_block);
        self.set_current_block(loop_block);
        self.generate_stmt(stmt.stmt());
        self.push_jump_inst(header_block);

        self.push_block(after_block);
        self.set_current_block(after_block);
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
        self.get_value(identifier.name())
    }

    fn generate_unary_expr(&mut self, expr: &ast::UnaryExpr<'input>) -> Value {
        let src_value = self.generate_expr(expr.expr());

        let op = expr.op();
        let src_type = self.func.value(src_value).type_();

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
        let left_type = self.func.value(left_value).type_();
        let right_type = self.func.value(right_value).type_();

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

    fn get_value(&mut self, name: &'input str) -> Value {
        let block = self.current_block();
        self.get_value_in_block(block, name, &mut HashSet::new())
            .unwrap()
    }

    fn get_value_in_block(
        &mut self,
        block: Block,
        name: &'input str,
        visited: &mut HashSet<Block>,
    ) -> Option<Value> {
        match self.values.get_value(block, name) {
            Some(value) => Some(value),
            None => {
                if !visited.insert(block) {
                    return None;
                }

                let mut type_ = None;
                for predecessor_block in self.predecessors[&block].clone() {
                    let value = self.get_value_in_block(predecessor_block, name, visited);
                    if let Some(value) = value {
                        let value_type = self.func.value(value).type_();
                        match type_ {
                            Some(type_) => {
                                if value_type != type_ {
                                    panic!(
                                        "Variable defined with multiple types `{}` and `{}`",
                                        type_,
                                        value_type
                                    );
                                }
                            }
                            None => {
                                type_ = Some(value_type);
                            }
                        };

                        let inst = self.func.last_inst(predecessor_block);
                        self.push_target_arg(inst, block, value);
                    }
                }

                match type_ {
                    Some(type_) => {
                        let value = self.create_block_param(name, block, type_);
                        Some(value)
                    }
                    None => None,
                }
            }
        }
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

    fn create_block_param(&mut self, name: &'input str, block: Block, type_: Type) -> Value {
        let value = self.func.create_value(type_);
        self.func.push_block_param(block, value);
        self.values.insert_param(block, name, value);
        value
    }

    fn push_block(&mut self, block: Block) {
        self.func.push_block(block);
    }

    fn create_target(&mut self, block: Block) -> Target {
        let mut args = Params::new();

        for param in &(*self.func.block(block).params()).clone() {
            let name = self.values.get_param(block, *param).unwrap();
            let value = self.get_value(name);
            args.push(value);
        }
        Target::new(block, args)
    }

    fn push_target_arg(&mut self, inst: Inst, block: Block, value: Value) {
        let target = self.func.inst_mut(inst).get_target_mut(block);
        target.push_arg(value);
    }

    fn push_int_const_inst(&mut self, immediate: IntImmediate) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.func.create_value(type_);
        let inst = InstData::IntConstInst(IntConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_float_const_inst(&mut self, immediate: FloatImmediate) -> Value {
        let type_ = Type::new(Uniform, Float);
        let dest = self.func.create_value(type_);
        let inst = InstData::FloatConstInst(FloatConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_bool_const_inst(&mut self, immediate: BoolImmediate) -> Value {
        let type_ = Type::new(Uniform, Bool);
        let dest = self.func.create_value(type_);
        let inst = InstData::BoolConstInst(BoolConstInst::new(dest, immediate));
        self.push_inst(inst);
        dest
    }

    fn push_index_inst(&mut self) -> Value {
        let type_ = Type::new(Varying, Int);
        let dest = self.func.create_value(type_);
        let inst = InstData::IndexInst(IndexInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_count_inst(&mut self) -> Value {
        let type_ = Type::new(Uniform, Int);
        let dest = self.func.create_value(type_);
        let inst = InstData::CountInst(CountInst::new(dest));
        self.push_inst(inst);
        dest
    }

    fn push_unary_inst(&mut self, op: UnaryOp, src: Operand) -> Value {
        let type_ = self.get_unary_inst_type(op, src);
        let dest = self.func.create_value(type_);
        let inst = InstData::UnaryInst(UnaryInst::new(op, dest, src));
        self.push_inst(inst);
        dest
    }

    fn push_binary_inst(&mut self, op: BinaryOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_binary_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = InstData::BinaryInst(BinaryInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_int_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = InstData::IntCompInst(IntCompInst::new(op, dest, left, right));
        self.push_inst(inst);
        dest
    }

    fn push_float_comp_inst(&mut self, op: CompOp, left: Operand, right: Operand) -> Value {
        let type_ = self.get_comp_inst_type(op, left, right);
        let dest = self.func.create_value(type_);
        let inst = InstData::FloatCompInst(FloatCompInst::new(op, dest, left, right));
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

    fn push_branch_inst(&mut self, cond: Value, true_block: Block, false_block: Block) {
        let true_target = self.create_target(true_block);
        let false_target = self.create_target(false_block);
        let inst = InstData::BranchInst(BranchInst::new(cond, true_target, false_target));
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

    fn get_unary_inst_type(&self, op: UnaryOp, src: Operand) -> Type {
        self.get_operand_type(src)
    }

    fn get_binary_inst_type(&self, op: BinaryOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);

        if left_type.type_() != right_type.type_() {
            panic!(
                "Invalid binary instruction '{}' with operand types '{}' and '{}'",
                op,
                left_type,
                right_type
            )
        };

        let type_ = left_type.type_();
        let qualifier = get_type_qualifier(left_type, right_type);

        Type::new(qualifier, type_)
    }

    fn get_comp_inst_type(&self, op: CompOp, left: Operand, right: Operand) -> Type {
        let left_type = self.get_operand_type(left);
        let right_type = self.get_operand_type(right);

        if left_type.type_() != right_type.type_() {
            panic!(
                "Invalid comparison instruction '{}' with operand types '{}' and '{}'",
                op,
                left_type,
                right_type
            )
        }

        let qualifier = get_type_qualifier(left_type, right_type);

        Type::new(qualifier, Bool)
    }

    fn get_operand_type(&self, operand: Operand) -> Type {
        match operand {
            Operand::IntImmediate(_) => Type::new(Uniform, Int),
            Operand::FloatImmediate(_) => Type::new(Uniform, Float),
            Operand::BoolImmediate(_) => Type::new(Uniform, Bool),
            Operand::Value(value) => self.func.value(value).type_(),
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
