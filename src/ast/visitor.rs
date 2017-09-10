use ast::expr::*;
use ast::func::*;
use ast::prog::*;
use ast::stmt::*;

pub trait Visitor<'input: 'ast, 'ast> {
    fn visit_int_literal(&mut self, int_literal: &IntLiteral);

    fn visit_float_literal(&mut self, float_literal: &FloatLiteral);

    fn visit_bool_literal(&mut self, bool_literal: &BoolLiteral);

    fn visit_identifier(&mut self, identifier: &mut Identifier);

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr);

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr);

    fn visit_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::IntLiteral(ref int_literal) => self.visit_int_literal(int_literal),
            Expr::FloatLiteral(ref float_literal) => self.visit_float_literal(float_literal),
            Expr::BoolLiteral(ref bool_literal) => self.visit_bool_literal(bool_literal),
            Expr::Identifier(ref mut identifier) => self.visit_identifier(identifier),
            Expr::UnaryExpr(ref mut expr) => self.visit_unary_expr(expr),
            Expr::BinaryExpr(ref mut expr) => self.visit_binary_expr(expr),
        }
    }

    fn visit_decl_stmt(&mut self, stmt: &'ast mut DeclStmt<'input>);

    fn visit_assign_stmt(&mut self, stmt: &'ast mut AssignStmt<'input>);

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt);

    fn visit_block_stmt(&mut self, stmt: &'ast mut BlockStmt<'input>);

    fn visit_stmt(&mut self, stmt: &'ast mut Stmt<'input>) {
        match *stmt {
            Stmt::DeclStmt(ref mut stmt) => self.visit_decl_stmt(stmt),
            Stmt::AssignStmt(ref mut stmt) => self.visit_assign_stmt(stmt),
            Stmt::ReturnStmt(ref stmt) => self.visit_return_stmt(stmt),
            Stmt::BlockStmt(ref mut stmt) => self.visit_block_stmt(stmt),
        }
    }

    fn visit_func(&mut self, func: &'ast mut Func<'input>);

    fn visit_prog(&mut self, prog: &'ast mut Prog<'input>);
}
