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

#[derive(Debug, Clone, Copy)]
pub struct Span {
    left: u32,
    right: u32,
}

impl Span {
    pub fn new(left: usize, right: usize) -> Span {
        Span {
            left: left as u32,
            right: right as u32,
        }
    }
}
