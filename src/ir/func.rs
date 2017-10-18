use ir::*;

#[derive(Debug)]
pub struct Func {
    params: Vec<Register>,
    insts: Vec<Inst>,
}

impl Func {
    pub fn new(params: Vec<Register>, insts: Vec<Inst>) -> Func {
        Func { params, insts }
    }
}
