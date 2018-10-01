pub use self::expr::*;
pub use self::func::*;
pub use self::prog::*;
pub use self::stmt::*;
pub use self::type_::*;
pub use self::type_checker::type_check;

mod expr;
mod func;
mod prog;
mod stmt;
mod type_;
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
