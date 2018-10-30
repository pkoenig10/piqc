pub use self::expr::*;
pub use self::func::*;
pub use self::prog::*;
pub use self::stmt::*;
pub use self::type_checker::type_check;
pub use ir::type_::*;

mod expr;
mod func;
mod prog;
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
