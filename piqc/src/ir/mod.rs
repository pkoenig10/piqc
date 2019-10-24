mod builder;
mod cfg;
mod func;
mod inst;
mod types;
mod verifier;

pub use self::builder::*;
pub use self::func::*;
pub use self::inst::*;
pub use self::types::*;
pub use self::verifier::verify_ir;

use self::inst::InstData;
use std::fmt;

id!(pub Ebb, "ebb");
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
