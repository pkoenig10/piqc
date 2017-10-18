use ast::*;

#[derive(Debug)]
pub struct Prog<'input> {
    funcs: Vec<Func<'input>>,
}

impl<'input> Prog<'input> {
    pub fn new(funcs: Vec<Func<'input>>) -> Prog<'input> {
        Prog { funcs }
    }

    pub fn funcs(&mut self) -> &mut Vec<Func<'input>> {
        &mut self.funcs
    }
}
