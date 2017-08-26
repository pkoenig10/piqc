use ast::expr::*;
use ast::stmt::*;
use ast::type_::*;

#[derive(Debug)]
pub struct Param {
    type_: Type,
    identifier: Identifier,
}

impl Param {
    pub fn new(type_: Type, identifier: Identifier) -> Param {
        Param { type_, identifier }
    }
}

#[derive(Debug)]
pub struct Params {
    params: Vec<Param>,
}

impl Params {
    pub fn new(params: Vec<Param>) -> Params {
        Params { params }
    }
}

#[derive(Debug)]
pub struct Func {
    identifier: Identifier,
    params: Params,
    stmt: BlockStmt,
}

impl Func {
    pub fn new(identifier: Identifier, params: Params, stmt: BlockStmt) -> Func {
        Func {
            identifier,
            params,
            stmt,
        }
    }
}
