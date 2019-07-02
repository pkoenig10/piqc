use crate::ast::*;

#[derive(Debug)]
pub struct BlockStmt {
    span: Span,
    pub stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(l: usize, stmts: Vec<Stmt>, r: usize) -> BlockStmt {
        BlockStmt {
            span: Span::new(l, r),
            stmts,
        }
    }
}

#[derive(Debug)]
pub struct DeclStmt {
    span: Span,
    pub type_: Type,
    pub identifier: Identifier,
    pub expr: Expr,
}

impl DeclStmt {
    pub fn new(l: usize, type_: Type, identifier: Identifier, expr: Expr, r: usize) -> DeclStmt {
        DeclStmt {
            span: Span::new(l, r),
            type_,
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    span: Span,
    pub dest: Expr,
    pub src: Expr,
}

impl AssignStmt {
    pub fn new(l: usize, dest: Expr, src: Expr, r: usize) -> AssignStmt {
        AssignStmt {
            span: Span::new(l, r),
            dest,
            src,
        }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    span: Span,
}

impl ReturnStmt {
    pub fn new(l: usize, r: usize) -> ReturnStmt {
        ReturnStmt {
            span: Span::new(l, r),
        }
    }
}

#[derive(Debug)]
pub struct IfStmt {
    span: Span,
    pub expr: Expr,
    pub if_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

impl IfStmt {
    pub fn new(l: usize, expr: Expr, if_stmt: Stmt, else_stmt: Option<Stmt>, r: usize) -> IfStmt {
        IfStmt {
            span: Span::new(l, r),
            expr,
            if_stmt: Box::new(if_stmt),
            else_stmt: else_stmt.map(Box::new),
        }
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    span: Span,
    pub expr: Expr,
    pub stmt: Box<Stmt>,
}

impl WhileStmt {
    pub fn new(l: usize, expr: Expr, stmt: Stmt, r: usize) -> WhileStmt {
        WhileStmt {
            span: Span::new(l, r),
            expr,
            stmt: Box::new(stmt),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Block(BlockStmt),
    Decl(DeclStmt),
    Assign(AssignStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
}
