use ast::*;
use ast::sym::*;
use ast::Type::*;
use ast::UnaryOp::*;
use ast::BinaryOp::*;
use sym::*;

pub fn type_check(prog: &mut Prog) {
    let mut type_checker = TypeChecker::new();
    type_checker.visit_prog(prog);
}

struct TypeChecker<'input> {
    symbol_table: SymbolTable<'input, Symbol<'input>>,
}

impl<'input> TypeChecker<'input> {
    pub fn new() -> TypeChecker<'input> {
        TypeChecker { symbol_table: SymbolTable::new() }
    }
}

#[allow(unused_variables)]
impl<'input> AstVisitor<'input, (), (), (), ()> for TypeChecker<'input> {
    fn visit_prog(&mut self, prog: &mut Prog<'input>) {
        self.visit_func(prog.func());
    }

    fn visit_func(&mut self, func: &mut Func<'input>) {
        self.symbol_table.push_scope();

        for param in func.params() {
            let symbol = Symbol::new(*param.identifier(), param.type_());
            self.symbol_table.insert(symbol.identifier().name(), symbol);
        }

        self.visit_stmt(func.stmt());

        self.symbol_table.pop_scope();
    }

    fn visit_decl_stmt(&mut self, stmt: &mut DeclStmt<'input>) {
        self.visit_expr(stmt.expr());

        let type_ = stmt.type_();
        let expr_type = stmt.expr().type_().unwrap();
        if type_ != expr_type {
            panic!("Mismatched types '{}' and '{}'", type_, expr_type);
        }

        let symbol = Symbol::new(*stmt.identifier(), type_);
        self.symbol_table.insert(symbol.identifier().name(), symbol);
    }

    fn visit_assign_stmt(&mut self, stmt: &mut AssignStmt<'input>) {
        self.visit_expr(stmt.expr());

        let symbol = self.symbol_table.get(stmt.identifier().name()).unwrap();
        let type_ = symbol.type_();
        let expr_type = stmt.expr().type_().unwrap();
        if type_ != expr_type {
            panic!("Mismatched types '{}' and '{}'", type_, expr_type);
        }
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) {}

    fn visit_block_stmt(&mut self, stmt: &mut BlockStmt<'input>) {
        self.symbol_table.push_scope();

        for stmt in stmt.stmts() {
            self.visit_stmt(stmt);
        }

        self.symbol_table.pop_scope();
    }

    fn visit_int_literal(&mut self, int_literal: &IntLiteral) {}

    fn visit_float_literal(&mut self, float_literal: &FloatLiteral) {}

    fn visit_bool_literal(&mut self, bool_literal: &BoolLiteral) {}

    fn visit_identifier(&mut self, identifier: &mut Identifier) {
        let symbol = self.symbol_table.get(identifier.name()).unwrap();
        identifier.set_type(symbol.type_());
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) {
        self.visit_expr(expr.expr());

        let op = expr.op();
        let expr_type = expr.expr().type_().unwrap();

        let type_ = match (op, expr_type) {
            (Negate, Int) | (BitNot, Int) => Int,
            (Negate, Float) => Float,
            (LogicalNot, Bool) => Bool,
            _ => {
                panic!(
                    "Cannot apply unary operator '{}' to type '{}'",
                    op,
                    expr_type
                )
            }
        };

        expr.set_type(type_);
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) {
        self.visit_expr(expr.left());
        self.visit_expr(expr.right());

        let op = expr.op();
        let left_type = expr.left().type_().unwrap();
        let right_type = expr.right().type_().unwrap();

        let type_ = match (op, left_type, right_type) {
            (Add, Int, Int) |
            (Sub, Int, Int) |
            (Shl, Int, Int) |
            (Shr, Int, Int) |
            (BitAnd, Int, Int) |
            (BitXor, Int, Int) |
            (BitOr, Int, Int) => Int,
            (Mul, Float, Float) |
            (Add, Float, Float) |
            (Sub, Float, Float) => Float,
            (Eq, Int, Int) |
            (Eq, Float, Float) |
            (Eq, Bool, Bool) |
            (Ne, Int, Int) |
            (Ne, Float, Float) |
            (Ne, Bool, Bool) |
            (Lt, Int, Int) |
            (Lt, Float, Float) |
            (Gt, Int, Int) |
            (Gt, Float, Float) |
            (Le, Int, Int) |
            (Le, Float, Float) |
            (Ge, Int, Int) |
            (Ge, Float, Float) |
            (LogicalAnd, Bool, Bool) |
            (LogicalOr, Bool, Bool) => Bool,
            _ => {
                panic!(
                    "Cannot apply binary operator '{}' to types '{}' and '{}'",
                    op,
                    left_type,
                    right_type
                )
            }
        };

        expr.set_type(type_);
    }
}
