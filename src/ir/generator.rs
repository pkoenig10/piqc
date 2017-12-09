use std::collections::HashMap;

use ast;
use ir::*;
use ir::BinaryOp::*;
use ir::CompOp::*;
use ir::Type::*;
use ir::builder::IrBuilder;

pub fn generate_ir(prog: &ast::Prog) -> Prog {
    let generator = IrGenerator::new();
    let func = generator.generate_func(prog.func());
    Prog::new(func)
}

#[derive(Debug)]
struct ValueTable<'a> {
    table: HashMap<&'a str, HashMap<BlockId, Value>>,
}

impl<'a> ValueTable<'a> {
    pub fn new() -> ValueTable<'a> {
        ValueTable { table: HashMap::new() }
    }

    pub fn insert(&mut self, name: &'a str, block_id: BlockId, value: Value) {
        self.table.entry(name).or_insert_with(HashMap::new).insert(block_id, value);
    }

    pub fn get(&self, name: &str, block_id: BlockId) -> Option<Value> {
        match self.table.get(name) {
            Some(values) => values.get(&block_id).cloned(),
            None => None,
        }
    }
}

struct IrGenerator<'input> {
    builder: IrBuilder,
    value_table: ValueTable<'input>,
}

impl<'input> IrGenerator<'input> {
    pub fn new() -> IrGenerator<'input> {
        IrGenerator {
            builder: IrBuilder::new(Func::new()),
            value_table: ValueTable::new(),
        }
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        let block_id = self.builder.create_block();
        self.builder.push_block(block_id);
        self.builder.set_insert_block(block_id);

        for param in func.params() {
            let type_ = self.generate_type(param.type_());
            self.builder.create_func_param(type_);
            let value = self.builder.create_block_param(type_);
            self.insert_value(param.identifier(), value);
        }

        self.generate_stmt(func.stmt());

        let block_id = self.builder.insert_block();
        let inst_id = self.builder.block(block_id).last_inst();
        let has_terminator = match inst_id {
            Some(inst_id) => self.builder.inst(inst_id).inst().is_terminator(),
            None => false,
        };
        if !has_terminator {
            self.builder.push_return_inst();
        }

        self.builder.func()
    }

    fn generate_stmt(&mut self, stmt: &ast::Stmt<'input>) {
        match *stmt {
            ast::Stmt::DeclStmt(ref stmt) => self.generate_decl_stmt(stmt),
            ast::Stmt::AssignStmt(ref stmt) => self.generate_assign_stmt(stmt),
            ast::Stmt::ReturnStmt(ref stmt) => self.generate_return_stmt(stmt),
            ast::Stmt::BlockStmt(ref stmt) => self.generate_block_stmt(stmt),
        }
    }

    fn generate_decl_stmt(&mut self, stmt: &ast::DeclStmt<'input>) {
        let value = self.generate_expr(stmt.expr());

        self.insert_value(stmt.identifier(), value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let value = self.generate_expr(stmt.expr());

        self.insert_value(stmt.identifier(), value);
    }

    fn generate_return_stmt(&mut self, _stmt: &ast::ReturnStmt) {
        self.builder.push_return_inst();
    }

    fn generate_block_stmt(&mut self, stmt: &ast::BlockStmt<'input>) {
        for stmt in stmt.stmts() {
            self.generate_stmt(stmt);
        }
    }

    fn generate_expr(&mut self, expr: &ast::Expr) -> Value {
        match *expr {
            ast::Expr::IntLiteral(ref int_literal) => self.generate_int_literal(int_literal),
            ast::Expr::FloatLiteral(ref float_literal) => {
                self.generate_float_literal(float_literal)
            }
            ast::Expr::BoolLiteral(ref bool_literal) => self.generate_bool_literal(bool_literal),
            ast::Expr::Identifier(ref identifier) => self.generate_identifier(identifier),
            ast::Expr::UnaryExpr(ref expr) => self.generate_unary_expr(expr),
            ast::Expr::BinaryExpr(ref expr) => self.generate_binary_expr(expr),
        }
    }

    fn generate_int_literal(&mut self, int_literal: &ast::IntLiteral) -> Value {
        let immediate = IntImmediate::new(int_literal.value());
        self.builder.push_int_const_inst(immediate)
    }

    fn generate_float_literal(&mut self, float_literal: &ast::FloatLiteral) -> Value {
        let immediate = FloatImmediate::new(float_literal.value());
        self.builder.push_float_const_inst(immediate)
    }

    fn generate_bool_literal(&mut self, bool_literal: &ast::BoolLiteral) -> Value {
        let immediate = BoolImmediate::new(bool_literal.value());
        self.builder.push_bool_const_inst(immediate)
    }

    fn generate_identifier(&mut self, identifier: &ast::Identifier) -> Value {
        self.get_value(identifier)
    }

    fn generate_unary_expr(&mut self, expr: &ast::UnaryExpr) -> Value {
        let src_value = self.generate_expr(expr.expr());

        let op = expr.op();
        let src_type = self.builder.value(src_value).type_();

        let src = Operand::Value(src_value);

        match (op, src_type) {
            (ast::Negate, Int) => {
                let zero = Operand::IntImmediate(IntImmediate::new(0));
                self.builder.push_binary_inst(Sub, zero, src)
            }
            (ast::Negate, Float) => {
                let zero = Operand::FloatImmediate(FloatImmediate::new(0.));
                self.builder.push_binary_inst(Fsub, zero, src)
            }
            (ast::BitNot, Int) |
            (ast::LogicalNot, Bool) => self.builder.push_unary_inst(Not, src),
            _ => {
                panic!(
                    "Invalid unary expression '{}' with operand type '{}'",
                    op,
                    src_type
                )
            }
        }
    }

    fn generate_binary_expr(&mut self, expr: &ast::BinaryExpr) -> Value {
        let left_value = self.generate_expr(expr.left());
        let right_value = self.generate_expr(expr.right());

        let op = expr.op();
        let left_type = self.builder.value(left_value).type_();
        let right_type = self.builder.value(right_value).type_();

        let left = Operand::Value(left_value);
        let right = Operand::Value(right_value);

        if let Some(op) = get_binary_op(op, left_type, right_type) {
            return self.builder.push_binary_inst(op, left, right);
        }

        if let Some(op) = get_int_comp_op(op, left_type, right_type) {
            return self.builder.push_int_comp_inst(op, left, right);
        }

        if let Some(op) = get_float_comp_op(op, left_type, right_type) {
            return self.builder.push_float_comp_inst(op, left, right);
        }

        panic!(
            "Invalid binary expression '{}' with operand types '{}' and '{}'",
            op,
            left_type,
            right_type
        )
    }

    fn generate_type(&self, type_: ast::Type) -> Type {
        match type_ {
            ast::Int => Int,
            ast::Float => Float,
            ast::Bool => Bool,
        }
    }

    fn insert_value(&mut self, identifier: &ast::Identifier<'input>, value: Value) {
        let block_id = self.builder.insert_block();
        self.value_table.insert(identifier.name(), block_id, value);
    }

    fn get_value(&self, identifier: &ast::Identifier) -> Value {
        let block_id = self.builder.insert_block();
        self.value_table.get(identifier.name(), block_id).unwrap()
    }
}

fn get_binary_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<BinaryOp> {
    match (op, left_type, right_type) {
        (ast::Mul, Float, Float) => Some(Fmul),
        (ast::Add, Int, Int) => Some(Add),
        (ast::Add, Float, Float) => Some(Fadd),
        (ast::Sub, Int, Int) => Some(Sub),
        (ast::Sub, Float, Float) => Some(Fsub),
        (ast::Shl, Int, Int) => Some(Shl),
        (ast::Shr, Int, Int) => Some(Asr),
        (ast::BitAnd, Int, Int) |
        (ast::LogicalAnd, Bool, Bool) => Some(And),
        (ast::BitOr, Int, Int) |
        (ast::LogicalOr, Bool, Bool) => Some(Or),
        (ast::BitXor, Int, Int) => Some(Xor),
        _ => None,
    }
}

fn get_int_comp_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<CompOp> {
    match (left_type, right_type) {
        (Int, Int) | (Bool, Bool) => get_comp_op(op),
        _ => None,
    }
}

fn get_float_comp_op(op: ast::BinaryOp, left_type: Type, right_type: Type) -> Option<CompOp> {
    match (left_type, right_type) {
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
