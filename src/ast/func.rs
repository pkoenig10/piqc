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

#[derive(Debug)]
pub struct Func<'input> {
    location: Location,
    identifier: Identifier<'input>,
    params: Vec<Param<'input>>,
    stmt: Stmt<'input>,
}

impl<'input> Func<'input> {
    pub fn new(
        l: usize,
        identifier: Identifier<'input>,
        params: Option<Vec<Param<'input>>>,
        stmt: BlockStmt<'input>,
        r: usize,
    ) -> Func<'input> {
        Func {
            location: Location::new(l, r),
            identifier,
            params: params.unwrap_or(Vec::new()),
            stmt: Stmt::BlockStmt(stmt),
        }
    }

    pub fn params(&self) -> &Vec<Param<'input>> {
        &self.params
    }

    pub fn stmt(&mut self) -> &mut Stmt<'input> {
        &mut self.stmt
    }
}
