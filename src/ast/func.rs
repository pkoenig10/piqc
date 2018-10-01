use ast::*;

#[derive(Debug)]
pub struct Param<'input> {
    span: Span,
    type_: Type,
    identifier: Identifier<'input>,
}

impl<'input> Param<'input> {
    pub fn new(l: usize, type_: Type, identifier: Identifier<'input>, r: usize) -> Param<'input> {
        Param {
            span: Span::new(l, r),
            type_,
            identifier,
        }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }
}

#[derive(Debug)]
pub struct Func<'input> {
    span: Span,
    identifier: Identifier<'input>,
    params: Vec<Param<'input>>,
    stmt: Box<Stmt<'input>>,
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

    pub fn params(&self) -> &[Param<'input>] {
        &self.params
    }

    pub fn stmt(&self) -> &Stmt<'input> {
        &self.stmt
    }
}
