use enum_dispatch::enum_dispatch;

use crate::ast::*;

#[derive(Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> BlockStmt {
        BlockStmt { stmts }
    }
}

#[derive(Debug)]
pub struct DeclStmt {
    pub ty: Type,
    pub identifier: Identifier,
    pub expr: Box<Expr>,
}

impl DeclStmt {
    pub fn new(ty: Type, identifier: Identifier, expr: Expr) -> DeclStmt {
        DeclStmt {
            ty,
            identifier,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    pub dest: Box<Expr>,
    pub src: Box<Expr>,
}

impl AssignStmt {
    pub fn new(dest: Expr, src: Expr) -> AssignStmt {
        AssignStmt {
            dest: Box::new(dest),
            src: Box::new(src),
        }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {}

impl ReturnStmt {
    pub fn new() -> ReturnStmt {
        ReturnStmt {}
    }
}

#[derive(Debug)]
pub struct IfStmt {
    pub cond: Box<Expr>,
    pub if_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn new(cond: Expr, if_stmt: Stmt, else_stmt: Option<Stmt>) -> IfStmt {
        IfStmt {
            cond: Box::new(cond),
            if_stmt: Box::new(if_stmt),
            else_stmt: else_stmt.map(Box::new),
        }
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<Expr>,
    pub stmt: Box<Stmt>,
}

impl WhileStmt {
    pub fn new(cond: Expr, stmt: Stmt) -> WhileStmt {
        WhileStmt {
            cond: Box::new(cond),
            stmt: Box::new(stmt),
        }
    }
}

#[enum_dispatch]
#[derive(Debug)]
pub enum StmtKind {
    Block(BlockStmt),
    Decl(DeclStmt),
    Assign(AssignStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
}

#[enum_dispatch(StmtKind)]
trait StmtTrait {}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Stmt {
        Stmt { span, kind }
    }
}
