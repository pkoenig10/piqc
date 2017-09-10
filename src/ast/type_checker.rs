use super::expr::*;
use super::func::*;
use super::prog::*;
use super::stmt::*;
use super::sym::*;
use super::type_::*;
use super::visitor::*;

use self::Type::*;
use self::UnaryOp::*;
use self::BinaryOp::*;

pub fn type_check(prog: &mut Prog) {
    let mut type_checker = TypeChecker::new();
    type_checker.visit_prog(prog);
}

struct TypeChecker<'input: 'ast, 'ast> {
    symbol_table: SymbolTable<'input, 'ast>,
}

impl<'input, 'ast> TypeChecker<'input, 'ast> {
    pub fn new() -> TypeChecker<'input, 'ast> {
        TypeChecker { symbol_table: SymbolTable::new() }
    }
}

#[allow(unused_variables)]
impl<'input, 'ast> Visitor<'input, 'ast> for TypeChecker<'input, 'ast> {
    fn visit_prog(&mut self, prog: &'ast mut Prog<'input>) {
        for func in prog.funcs() {
            self.visit_func(func);
        }
    }

    fn visit_func(&mut self, func: &'ast mut Func<'input>) {
        self.visit_stmt(func.stmt());
    }

    fn visit_decl_stmt(&mut self, stmt: &'ast mut DeclStmt<'input>) {
        self.visit_expr(stmt.expr());

        let type_ = stmt.type_();
        let expr_type = stmt.expr().type_().unwrap();
        if type_ != expr_type {
            panic!("Mismatched types {} and {}", type_, expr_type);
        }

        let symbol = Symbol::new(stmt.identifier(), type_);
        self.symbol_table.insert_symbol(symbol);
    }

    fn visit_assign_stmt(&mut self, stmt: &'ast mut AssignStmt<'input>) {
        self.visit_expr(stmt.expr());

        let symbol = self.symbol_table
            .get_symbol(stmt.identifier().name())
            .unwrap();
        let type_ = symbol.type_();
        let expr_type = stmt.expr().type_().unwrap();
        if type_ != expr_type {
            panic!("Mismatched types {} and {}", type_, expr_type);
        }
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) {}

    fn visit_block_stmt(&mut self, stmt: &'ast mut BlockStmt<'input>) {
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
        let symbol = self.symbol_table.get_symbol(identifier.name()).unwrap();
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
            (Eq, Bool, Bool) |
            (Ne, Bool, Bool) |
            (Lt, Bool, Bool) |
            (Gt, Bool, Bool) |
            (Le, Bool, Bool) |
            (Ge, Bool, Bool) |
            (LogicalAnd, Bool, Bool) |
            (LogicalOr, Bool, Bool) => Bool,
            (op, left_type, right_type) => {
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
