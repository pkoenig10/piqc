pub use self::builder::FuncBuilder;
pub use self::func::Func;
pub use self::inst::{BinaryOp, BranchOp, CmpOp, UnaryOp};
pub use self::pass::run_dead_code;
pub use self::types::{Type, TypeKind, Variability};
pub use self::verifier::verify_ir;

use std::fmt;

use self::inst::{
    AllocInst, BinaryInst, BoolConstInst, BranchInst, CountInst, ElementInst, FetchInst,
    FloatCmpInst, FloatConstInst, InstData, InstTrait, IntCmpInst, IntConstInst, JumpInst,
    ReadInst, ReturnInst, SelectInst, StoreInst, Target, UnaryInst, WriteInst,
};
use self::value::ValueData;

mod builder;
mod func;
mod inst;
mod pass;
mod types;
mod value;
mod verifier;

id!(pub Ebb, "ebb");

id!(Block, "b");

id!(pub Inst, "i");

id!(pub Value, "%");

id!(pub Variable, "v");

struct DisplayList<'a, T>
where
    T: 'a,
{
    values: &'a [T],
}

impl<'a, T> DisplayList<'a, T> {
    pub fn new(values: &'a [T]) -> DisplayList<T> {
        DisplayList { values }
    }
}

impl<'a, T> fmt::Display for DisplayList<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for value in self.values {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", value)?;
        }
        Ok(())
    }
}
