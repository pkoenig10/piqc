use crate::ast::*;

#[derive(Debug)]
pub struct Param {
    span: Span,
    pub ty: Type,
    pub identifier: Identifier,
}

impl Param {
    pub fn new(span: Span, ty: Type, identifier: Identifier) -> Param {
        Param {
            span,
            ty,
            identifier,
        }
    }
}

#[derive(Debug)]
pub struct Func {
    span: Span,
    pub identifier: Identifier,
    pub params: Vec<Param>,
    pub stmt: Box<Stmt>,
}

impl Func {
    pub fn new(span: Span, identifier: Identifier, params: Vec<Param>, stmt: Stmt) -> Func {
        Func {
            span,
            identifier,
            params,
            stmt: Box::new(stmt),
        }
    }
}
