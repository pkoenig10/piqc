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
