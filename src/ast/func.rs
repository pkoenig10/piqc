use std::slice::Iter;

use ast::*;
use ast::loc::*;

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

    pub fn type_(&self) -> Type {
        self.type_
    }

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }
}

#[derive(Debug, Default)]
pub struct Params<'input> {
    params: Vec<Param<'input>>,
}

impl<'input> Params<'input> {
    pub fn new() -> Params<'input> {
        Params { params: Vec::new() }
    }

    pub fn push(&mut self, param: Param<'input>) {
        self.params.push(param);
    }
}

impl<'input, 'a> IntoIterator for &'a Params<'input> {
    type Item = &'a Param<'input>;
    type IntoIter = Iter<'a, Param<'input>>;

    fn into_iter(self) -> Self::IntoIter {
        self.params.iter()
    }
}

#[derive(Debug)]
pub struct Func<'input> {
    location: Location,
    identifier: Identifier<'input>,
    params: Params<'input>,
    stmt: Stmt<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        l: usize,
        identifier: Identifier<'input>,
        params: Option<Params<'input>>,
        stmt: BlockStmt<'input>,
        r: usize,
    ) -> Func<'input> {
        Func {
            location: Location::new(l, r),
            identifier,
            params: params.unwrap_or_else(Params::new),
            stmt: Stmt::BlockStmt(stmt),
        }
    }

    pub fn params(&self) -> &Params<'input> {
        &self.params
    }

    pub fn stmt(&self) -> &Stmt<'input> {
        &self.stmt
    }
}
