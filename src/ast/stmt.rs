use super::expr::*;
use super::loc::*;
use super::type_::*;

#[derive(Debug)]
pub struct DeclStmt<'input> {
    location: Location,
    type_: Type,
    identifier: Identifier<'input>,
    expr: Expr<'input>,
}

impl<'input> DeclStmt<'input> {
    pub fn new(
        l: usize,
        type_: Type,
        identifier: Identifier<'input>,
        expr: Expr<'input>,
        r: usize,
    ) -> DeclStmt<'input> {
        DeclStmt {
            location: Location::new(l, r),
            type_,
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt<'input> {
    location: Location,
    identifier: Identifier<'input>,
    expr: Expr<'input>,
}

impl<'input> AssignStmt<'input> {
    pub fn new(
        l: usize,
        identifier: Identifier<'input>,
        expr: Expr<'input>,
        r: usize,
    ) -> AssignStmt<'input> {
        AssignStmt {
            location: Location::new(l, r),
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    location: Location,
}

impl ReturnStmt {
    pub fn new(l: usize, r: usize) -> ReturnStmt {
        ReturnStmt { location: Location::new(l, r) }
    }
}

#[derive(Debug)]
pub struct BlockStmt<'input> {
    location: Location,
    stmts: Vec<Stmt<'input>>,
}

impl<'input> BlockStmt<'input> {
    pub fn new(l: usize, stmts: Vec<Stmt<'input>>, r: usize) -> BlockStmt<'input> {
        BlockStmt {
            location: Location::new(l, r),
            stmts,
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'input> {
    DeclStmt(DeclStmt<'input>),
    AssignStmt(AssignStmt<'input>),
    ReturnStmt(ReturnStmt),
    BlockStmt(BlockStmt<'input>),
}
