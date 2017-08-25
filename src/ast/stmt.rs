use ast::expr::*;
use ast::type_::*;

#[derive(Debug)]
pub struct DeclStmt {
    type_: Type,
    identifier: Identifier,
    expr: Expr,
}

impl DeclStmt {
    pub fn new(type_: Type, identifier: Identifier, expr: Expr) -> DeclStmt {
        DeclStmt {
            type_,
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    identifier: Identifier,
    expr: Expr,
}

impl AssignStmt {
    pub fn new(identifier: Identifier, expr: Expr) -> AssignStmt {
        AssignStmt { identifier, expr }
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> BlockStmt {
        BlockStmt { stmts }
    }
}

#[derive(Debug)]
pub enum Stmt {
    DeclStmt(DeclStmt),
    AssignStmt(AssignStmt),
    ReturnStmt,
    BlockStmt(BlockStmt),
}
