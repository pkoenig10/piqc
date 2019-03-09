use crate::ast::*;

#[derive(Debug)]
pub struct BlockStmt<'input> {
    span: Span,
    pub stmts: Vec<Stmt<'input>>,
}

impl<'input> BlockStmt<'input> {
    pub fn new(l: usize, stmts: Vec<Stmt<'input>>, r: usize) -> BlockStmt<'input> {
        BlockStmt {
            span: Span::new(l, r),
            stmts,
        }
    }
}

#[derive(Debug)]
pub struct DeclStmt<'input> {
    span: Span,
    pub type_: Type,
    pub identifier: Identifier<'input>,
    pub expr: Expr<'input>,
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
            span: Span::new(l, r),
            type_,
            identifier,
            expr,
        }
    }
}

#[derive(Debug)]
pub struct AssignStmt<'input> {
    span: Span,
    pub dest: Expr<'input>,
    pub src: Expr<'input>,
}

impl<'input> AssignStmt<'input> {
    pub fn new(l: usize, dest: Expr<'input>, src: Expr<'input>, r: usize) -> AssignStmt<'input> {
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
pub struct IfStmt<'input> {
    span: Span,
    pub expr: Expr<'input>,
    pub if_stmt: Box<Stmt<'input>>,
    pub else_stmt: Option<Box<Stmt<'input>>>,
}

impl<'input> IfStmt<'input> {
    pub fn new(
        l: usize,
        expr: Expr<'input>,
        if_stmt: Stmt<'input>,
        else_stmt: Option<Stmt<'input>>,
        r: usize,
    ) -> IfStmt<'input> {
        IfStmt {
            span: Span::new(l, r),
            expr,
            if_stmt: Box::new(if_stmt),
            else_stmt: else_stmt.map(Box::new),
        }
    }
}

#[derive(Debug)]
pub struct WhileStmt<'input> {
    span: Span,
    pub expr: Expr<'input>,
    pub stmt: Box<Stmt<'input>>,
}

impl<'input> WhileStmt<'input> {
    pub fn new(l: usize, expr: Expr<'input>, stmt: Stmt<'input>, r: usize) -> WhileStmt<'input> {
        WhileStmt {
            span: Span::new(l, r),
            expr,
            stmt: Box::new(stmt),
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'input> {
    Block(BlockStmt<'input>),
    Decl(DeclStmt<'input>),
    Assign(AssignStmt<'input>),
    If(IfStmt<'input>),
    While(WhileStmt<'input>),
    Return(ReturnStmt),
}
