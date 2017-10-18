use ir::*;

#[derive(Debug)]
pub struct Prog {
    funcs: Vec<Func>,
}

impl Prog {
    pub fn new(funcs: Vec<Func>) -> Prog {
        Prog { funcs }
    }
}
