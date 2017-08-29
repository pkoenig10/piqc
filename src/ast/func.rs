use ast::expr::*;
use ast::loc::*;
use ast::stmt::*;
use ast::type_::*;

#[derive(Debug)]
pub struct Param<'input> {
    location: Location,
    type_: Type,
    identifier: Identifier<'input>,
}

impl<'input> Param<'input> {
    pub fn new(l: usize, type_: Type, identifier: Identifier<'input>, r: usize) -> Param<'input> {
        Param {
            location: Location::new(l, r),
            type_,
            identifier,
        }
    }
}

#[derive(Debug)]
pub struct Params<'input> {
    location: Location,
    params: Vec<Param<'input>>,
}

impl<'input> Params<'input> {
    pub fn new(l: usize, params: Option<Vec<Param<'input>>>, r: usize) -> Params<'input> {
        Params {
            location: Location::new(l, r),
            params: params.unwrap_or(vec![]),
        }
    }
}

#[derive(Debug)]
pub struct Func<'input> {
    location: Location,
    identifier: Identifier<'input>,
    params: Params<'input>,
    stmt: BlockStmt<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        l: usize,
        identifier: Identifier<'input>,
        params: Params<'input>,
        stmt: BlockStmt<'input>,
        r: usize,
    ) -> Func<'input> {
        Func {
            location: Location::new(l, r),
            identifier,
            params,
            stmt,
        }
    }
}
