use ast::*;

#[derive(Debug)]
pub struct Prog<'input> {
    span: Span,
    pub func: Func<'input>,
}

impl<'input> Prog<'input> {
    pub fn new(l: usize, func: Func<'input>, r: usize) -> Prog<'input> {
        Prog {
            span: Span::new(l, r),
            func,
        }
    }
}
