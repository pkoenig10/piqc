use ast::*;

#[derive(Debug)]
pub struct Prog<'input> {
    func: Func<'input>,
}

impl<'input> Prog<'input> {
    pub fn new(func: Func<'input>) -> Prog<'input> {
        Prog { func }
    }

    pub fn func(&self) -> &Func<'input> {
        &self.func
    }
}
