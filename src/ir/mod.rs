mod block;
mod builder;
mod func;
mod inst;
mod operand;
mod params;
mod prog;
mod target;
mod type_;
mod verifier;

pub use self::block::*;
pub use self::func::*;
pub use self::inst::*;
pub use self::operand::*;
pub use self::params::*;
pub use self::prog::*;
pub use self::target::*;
pub use self::type_::*;

pub use self::UnaryOp::*;
pub use self::BinaryOp::*;
pub use self::CompOp::*;
pub use self::Type::*;

pub use self::builder::generate_ir;
pub use self::verifier::verify_ir;
