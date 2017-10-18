mod func;
mod inst;
mod ir_generator;
mod operand;
mod prog;

pub use self::func::*;
pub use self::inst::*;
pub use self::operand::*;
pub use self::prog::*;

pub use self::BinaryOp::*;
pub use self::CompOp::*;

pub use self::ir_generator::generate_ir;
