use ast::expr::*;
use ast::loc::*;
use ast::type_::*;

#[derive(Debug)]
pub struct DeclStmt {
    location: Location,
    type_: Type,
    identifier: Identifier,
    expr: Expr,
}

impl DeclStmt {
    pub fn new(l: usize, type_: Type, identifier: Identifier, expr: Expr, r: usize) -> DeclStmt {
        DeclStmt {
            location: Location::new(l, r),
            type_,
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    location: Location,
    identifier: Identifier,
    expr: Expr,
}

impl AssignStmt {
    pub fn new(l: usize, identifier: Identifier, expr: Expr, r: usize) -> AssignStmt {
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
pub struct BlockStmt {
    location: Location,
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(l: usize, stmts: Vec<Stmt>, r: usize) -> BlockStmt {
        BlockStmt {
            location: Location::new(l, r),
            stmts,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    DeclStmt(DeclStmt),
    AssignStmt(AssignStmt),
    ReturnStmt(ReturnStmt),
    BlockStmt(BlockStmt),
}
