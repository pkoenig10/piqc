mod expr;
mod func;
mod loc;
mod prog;
mod stmt;
mod type_;
mod type_checker;

pub use self::expr::*;
pub use self::func::*;
pub use self::prog::*;
pub use self::stmt::*;
pub use self::type_::*;

pub use self::BinaryOp::*;
pub use self::Type::*;
pub use self::UnaryOp::*;

pub use self::type_checker::type_check;
