pub use self::expr::{
    BinaryExpr, BinaryOp, BoolExpr, CountExpr, ElementExpr, Expr, ExprKind, FloatExpr, Identifier,
    IdentifierExpr, IndexExpr, IntExpr, ParenExpr, Symbol, UnaryExpr, UnaryOp,
};
pub use self::func::{Func, Param};
pub use self::gen::generate_ir;
pub use self::stmt::{
    AssignStmt, BlockStmt, DeclStmt, IfStmt, ReturnStmt, Stmt, StmtKind, WhileStmt,
};
pub use self::types::{Primitive, Type, Variability};

macro_rules! fn_block {
    ($expr:expr) => {
        (|| $expr)();
    };
}

macro_rules! unwrap_or_return {
    ($result:expr) => {
        match $result {
            Ok(value) => value,
            Err(_) => return,
        }
    };
}

mod expr;
mod func;
mod gen;
mod stmt;
mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start as u32,
            end: end as u32,
        }
    }
}
