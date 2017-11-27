use ast;
use ir::*;
use ir::BinaryOp::*;
use ir::CompOp::*;
use ir::Type::*;
use ir::builder::IrBuilder;
use collections::*;

pub fn generate_ir(prog: &ast::Prog) -> Prog {
    let generator = IrGenerator::new();
    let func = generator.generate_func(prog.func());
    Prog::new(func)
}

struct IrGenerator<'input> {
    builder: IrBuilder,
    symbol_table: SymbolTable<'input, Value>,
}

impl<'input> IrGenerator<'input> {
    pub fn new() -> IrGenerator<'input> {
        IrGenerator {
            builder: IrBuilder::new(Func::new()),
            symbol_table: SymbolTable::new(),
        }
    }

    fn builder(&mut self) -> &mut IrBuilder {
        &mut self.builder
    }

    fn generate_func(mut self, func: &ast::Func<'input>) -> Func {
        self.symbol_table.push_scope();

        for param in func.params() {
            let type_ = self.generate_type(param.type_());
            let value = self.builder().create_param(type_);
            self.insert_symbol(param.identifier(), value);
        }

        let block_id = self.builder().create_block();
        self.builder().push_block(block_id);
        self.builder().set_insert_block(block_id);

        self.generate_stmt(func.stmt());

        self.symbol_table.pop_scope();

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

        self.insert_symbol(stmt.identifier(), value);
    }

    fn generate_assign_stmt(&mut self, stmt: &ast::AssignStmt<'input>) {
        let value = self.generate_expr(stmt.expr());

        self.insert_symbol(stmt.identifier(), value);
    }

    fn generate_return_stmt(&mut self, _stmt: &ast::ReturnStmt) {
        self.builder().push_return_inst();
    }

    fn generate_block_stmt(&mut self, stmt: &ast::BlockStmt<'input>) {
        self.symbol_table.push_scope();

        for stmt in stmt.stmts() {
            self.generate_stmt(stmt);
        }

        self.symbol_table.pop_scope();
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
        let value = IntImmediate::new(int_literal.value());
        self.builder().push_int_const_inst(value)
    }

    fn generate_float_literal(&mut self, float_literal: &ast::FloatLiteral) -> Value {
        let value = FloatImmediate::new(float_literal.value());
        self.builder().push_float_const_inst(value)
    }

    fn generate_bool_literal(&mut self, bool_literal: &ast::BoolLiteral) -> Value {
        let value = BoolImmediate::new(bool_literal.value());
        self.builder().push_bool_const_inst(value)
    }

    fn generate_identifier(&mut self, identifier: &ast::Identifier) -> Value {
        self.get_symbol(identifier)
    }

    fn generate_unary_expr(&mut self, expr: &ast::UnaryExpr) -> Value {
        let src_value = self.generate_expr(expr.expr());

        let op = expr.op();
        let src_type = self.builder().get_value_type(src_value);

        let builder = self.builder();

        let src = Operand::Value(src_value);

        match (op, src_type) {
            (ast::Negate, Int) => {
                builder.push_binary_inst(Sub, Operand::IntImmediate(IntImmediate::new(0)), src)
            }
            (ast::Negate, Float) => {
                builder.push_binary_inst(
                    Fsub,
                    Operand::FloatImmediate(FloatImmediate::new(0.)),
                    src,
                )
            }
            (ast::BitNot, Int) |
            (ast::LogicalNot, Bool) => builder.push_unary_inst(Not, src),
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
        let left_type = self.builder().get_value_type(left_value);
        let right_type = self.builder().get_value_type(right_value);

        let builder = self.builder();

        let left = Operand::Value(left_value);
        let right = Operand::Value(right_value);

        match (op, left_type, right_type) {
            (ast::Mul, Float, Float) => builder.push_binary_inst(Fmul, left, right),
            (ast::Add, Int, Int) => builder.push_binary_inst(Add, left, right),
            (ast::Add, Float, Float) => builder.push_binary_inst(Fadd, left, right),
            (ast::Sub, Int, Int) => builder.push_binary_inst(Sub, left, right),
            (ast::Sub, Float, Float) => builder.push_binary_inst(Fsub, left, right),
            (ast::Shl, Int, Int) => builder.push_binary_inst(Shl, left, right),
            (ast::Shr, Int, Int) => builder.push_binary_inst(Asr, left, right),
            (ast::BitAnd, Int, Int) |
            (ast::LogicalAnd, Bool, Bool) => builder.push_binary_inst(And, left, right),
            (ast::BitOr, Int, Int) |
            (ast::LogicalOr, Bool, Bool) => builder.push_binary_inst(Or, left, right),
            (ast::BitXor, Int, Int) => builder.push_binary_inst(Xor, left, right),
            (ast::Eq, Int, Int) |
            (ast::Eq, Bool, Bool) => builder.push_int_comp_inst(Eq, left, right),
            (ast::Eq, Float, Float) => builder.push_float_comp_inst(Eq, left, right),
            (ast::Ne, Int, Int) |
            (ast::Ne, Bool, Bool) => builder.push_int_comp_inst(Ne, left, right),
            (ast::Ne, Float, Float) => builder.push_float_comp_inst(Ne, left, right),
            (ast::Lt, Int, Int) => builder.push_int_comp_inst(Lt, left, right),
            (ast::Lt, Float, Float) => builder.push_float_comp_inst(Lt, left, right),
            (ast::Gt, Int, Int) => builder.push_int_comp_inst(Gt, left, right),
            (ast::Gt, Float, Float) => builder.push_float_comp_inst(Gt, left, right),
            (ast::Le, Int, Int) => builder.push_int_comp_inst(Le, left, right),
            (ast::Le, Float, Float) => builder.push_float_comp_inst(Le, left, right),
            (ast::Ge, Int, Int) => builder.push_int_comp_inst(Ge, left, right),
            (ast::Ge, Float, Float) => builder.push_float_comp_inst(Ge, left, right),
            _ => {
                panic!(
                    "Invalid binary expression '{}' with operand types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        }
    }

    fn generate_type(&self, type_: ast::Type) -> Type {
        match type_ {
            ast::Int => Int,
            ast::Float => Float,
            ast::Bool => Bool,
        }
    }

    fn insert_symbol(&mut self, identifier: &ast::Identifier<'input>, value: Value) {
        self.symbol_table.insert(identifier.name(), value);
    }

    fn get_symbol(&self, identifier: &ast::Identifier) -> Value {
        *self.symbol_table.get(identifier.name()).unwrap()
    }
}
