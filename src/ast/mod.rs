pub use self::builder::generate_ir;
pub use self::expr::{
    BinaryExpr, BinaryOp, BoolLiteral, Count, Element, Expr, FloatLiteral, Identifier, IntLiteral,
    UnaryExpr, UnaryOp,
};
pub use self::func::{Func, Param};
pub use self::stmt::{AssignStmt, BlockStmt, DeclStmt, IfStmt, ReturnStmt, Stmt, WhileStmt};
pub use self::type_checker::type_check;
pub use ir::{Type, TypeKind, TypeQualifier};

mod builder;
mod expr;
mod func;
mod stmt;
mod type_checker;

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
