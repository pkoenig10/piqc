mod builder;
mod func;
mod inst;
mod generator;
mod operand;
mod prog;
mod type_;
mod verifier;

pub use self::func::*;
pub use self::inst::*;
pub use self::operand::*;
pub use self::prog::*;
pub use self::type_::*;

pub use self::UnaryOp::*;
pub use self::BinaryOp::*;
pub use self::CompOp::*;
pub use self::Type::*;

pub use self::generator::generate_ir;
pub use self::verifier::verify_ir;
