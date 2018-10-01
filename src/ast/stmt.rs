use std::ops::Deref;

use ast::*;

#[derive(Debug)]
pub struct BlockStmt<'input> {
    span: Span,
    stmts: Vec<Stmt<'input>>,
}

impl<'input> BlockStmt<'input> {
    pub fn new(l: usize, stmts: Vec<Stmt<'input>>, r: usize) -> BlockStmt<'input> {
        BlockStmt {
            span: Span::new(l, r),
            stmts,
        }
    }

    pub fn stmts(&self) -> &[Stmt<'input>] {
        &self.stmts
    }
}

#[derive(Debug)]
pub struct DeclStmt<'input> {
    span: Span,
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
            span: Span::new(l, r),
            type_,
            identifier,
            expr,
        }
    }

    pub fn type_(&self) -> Type {
        self.type_
    }

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }
}

#[derive(Debug)]
pub struct AssignStmt<'input> {
    span: Span,
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
            span: Span::new(l, r),
            identifier,
            expr,
        }
    }

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
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
    expr: Expr<'input>,
    if_stmt: Box<Stmt<'input>>,
    else_stmt: Option<Box<Stmt<'input>>>,
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

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }

    pub fn if_stmt(&self) -> &Stmt<'input> {
        &self.if_stmt
    }

    pub fn else_stmt(&self) -> Option<&Stmt<'input>> {
        self.else_stmt.as_ref().map(Box::deref)
    }
}

#[derive(Debug)]
pub struct WhileStmt<'input> {
    span: Span,
    expr: Expr<'input>,
    stmt: Box<Stmt<'input>>,
}

impl<'input> WhileStmt<'input> {
    pub fn new(l: usize, expr: Expr<'input>, stmt: Stmt<'input>, r: usize) -> WhileStmt<'input> {
        WhileStmt {
            span: Span::new(l, r),
            expr,
            stmt: Box::new(stmt),
        }
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }

    pub fn stmt(&self) -> &Stmt<'input> {
        &self.stmt
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
