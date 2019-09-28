pub use self::builder::FuncBuilder;
pub use self::func::Func;
pub use self::inst::Cond;
pub use self::types::{Type, TypeKind, Variability};
pub use self::verifier::verify_ir;

use std::fmt;

use self::inst::InstData;

mod builder;
mod func;
mod inst;
mod types;
mod verifier;

id!(pub Ebb, "ebb");

id!(Block, "b");

id!(pub Inst, "i");

id!(pub Value, "%");

id!(pub Variable, "v");

struct DisplaySlice<'a, T>(&'a [T]);

impl<'a, T> fmt::Display for DisplaySlice<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for value in self.0 {
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
