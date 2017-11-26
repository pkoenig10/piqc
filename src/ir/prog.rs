use std::fmt;

use ir::*;

#[derive(Debug)]
pub struct Prog {
    func: Func,
}

impl Prog {
    pub fn new(func: Func) -> Prog {
        Prog { func }
    }
}

impl fmt::Display for Prog {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.func)
    }
}
