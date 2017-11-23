use ir::*;

#[derive(Debug)]
pub struct Func {
    params: Vec<Register>,
    blocks: Vec<Block>,
}

impl Func {
    pub fn new(params: Vec<Register>, blocks: Vec<Block>) -> Func {
        Func { params, blocks }
    }
}
