use ast::expr::*;
use ast::loc::*;
use ast::stmt::*;
use ast::type_::*;

#[derive(Debug)]
pub struct Param {
    location: Location,
    type_: Type,
    identifier: Identifier,
}

impl Param {
    pub fn new(l: usize, type_: Type, identifier: Identifier, r: usize) -> Param {
        Param {
            location: Location::new(l, r),
            type_,
            identifier,
        }
    }
}

#[derive(Debug)]
pub struct Params {
    location: Location,
    params: Vec<Param>,
}

impl Params {
    pub fn new(l: usize, params: Option<Vec<Param>>, r: usize) -> Params {
        Params {
            location: Location::new(l, r),
            params: params.unwrap_or(vec![]),
        }
    }
}

#[derive(Debug)]
pub struct Func {
    location: Location,
    identifier: Identifier,
    params: Params,
    stmt: BlockStmt,
}

impl Func {
    pub fn new(
        l: usize,
        identifier: Identifier,
        params: Params,
        stmt: BlockStmt,
        r: usize,
    ) -> Func {
        Func {
            location: Location::new(l, r),
            identifier,
            params,
            stmt,
        }
    }
}
