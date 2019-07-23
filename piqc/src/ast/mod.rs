pub use self::expr::{
    BinaryExpr, BinaryOp, BoolExpr, CountExpr, ElementExpr, Expr, ExprKind, FloatExpr, Identifier,
    IdentifierExpr, IndexExpr, IntExpr, ParenExpr, Symbol, UnaryExpr, UnaryOp,
};
pub use self::func::{Func, Param};
pub use self::gen::generate_ir;
pub use self::stmt::{
    AssignStmt, BlockStmt, DeclStmt, IfStmt, ReturnStmt, Stmt, StmtKind, WhileStmt,
};
pub use crate::ir::{Type, TypeKind, TypeQualifier};

mod expr;
mod func;
mod gen;
mod stmt;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    left: usize,
    right: usize,
}

impl Span {
    pub fn new(left: usize, right: usize) -> Span {
        Span { left, right }
    }
}
