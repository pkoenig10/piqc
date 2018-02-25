use std::fmt;

use ir::*;

#[derive(Debug, Clone)]
pub struct Target {
    ebb: Ebb,
    args: Args<Value>,
}

impl Target {
    pub fn new(ebb: Ebb, args: Args<Value>) -> Target {
        Target { ebb, args: args }
    }

    pub fn ebb(&self) -> Ebb {
        self.ebb
    }

    pub fn args(&self) -> &Args<Value> {
        &self.args
    }

    pub fn push_arg(&mut self, value: Value) {
        self.args.push(value);
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.ebb,
            self.args,
        )
    }
}
