use crate::ast::*;

#[derive(Debug)]
pub struct Param {
    span: Span,
    pub type_: Type,
    pub identifier: Identifier,
}

impl Param {
    pub fn new(l: usize, type_: Type, identifier: Identifier, r: usize) -> Param {
        Param {
            span: Span::new(l, r),
            type_,
            identifier,
        }
    }
}

#[derive(Debug)]
pub struct Func {
    span: Span,
    pub identifier: Identifier,
    pub params: Vec<Param>,
    pub stmt: Stmt,
}

impl Func {
    pub fn new(
        l: usize,
        identifier: Identifier,
        params: Option<Vec<Param>>,
        stmt: Stmt,
        r: usize,
    ) -> Func {
        Func {
            span: Span::new(l, r),
            identifier,
            params: params.unwrap_or_else(Vec::new),
            stmt,
        }
    }
}
