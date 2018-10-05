use ast::*;

#[derive(Debug)]
pub struct Param<'input> {
    span: Span,
    pub type_: Type,
    pub identifier: Identifier<'input>,
}

impl<'input> Param<'input> {
    pub fn new(l: usize, type_: Type, identifier: Identifier<'input>, r: usize) -> Param<'input> {
        Param {
            span: Span::new(l, r),
            type_,
            identifier,
        }
    }
}

#[derive(Debug)]
pub struct Func<'input> {
    span: Span,
    pub identifier: Identifier<'input>,
    pub params: Vec<Param<'input>>,
    pub stmt: Box<Stmt<'input>>,
}

impl<'input> Func<'input> {
    pub fn new(
        l: usize,
        identifier: Identifier<'input>,
        params: Option<Vec<Param<'input>>>,
        stmt: Stmt<'input>,
        r: usize,
    ) -> Func<'input> {
        Func {
            span: Span::new(l, r),
            identifier,
            params: params.unwrap_or_else(Vec::new),
            stmt: Box::new(stmt),
        }
    }
}
