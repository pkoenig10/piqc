use ast::*;
use ast::loc::*;

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

    pub fn stmts(&self) -> &Vec<Stmt<'input>> {
        &self.stmts
    }
}

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

    pub fn identifier(&self) -> &Identifier<'input> {
        &self.identifier
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
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
pub struct IfStmt<'input> {
    location: Location,
    expr: Expr<'input>,
    stmt: BlockStmt<'input>,
}

impl<'input> IfStmt<'input> {
    pub fn new(l: usize, expr: Expr<'input>, stmt: BlockStmt<'input>, r: usize) -> IfStmt<'input> {
        IfStmt {
            location: Location::new(l, r),
            expr,
            stmt,
        }
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }

    pub fn stmt(&self) -> &BlockStmt<'input> {
        &self.stmt
    }
}

#[derive(Debug)]
pub struct WhileStmt<'input> {
    location: Location,
    expr: Expr<'input>,
    stmt: BlockStmt<'input>,
}

impl<'input> WhileStmt<'input> {
    pub fn new(l: usize, expr: Expr<'input>, stmt: BlockStmt<'input>, r: usize) -> WhileStmt<'input> {
        WhileStmt {
            location: Location::new(l, r),
            expr,
            stmt,
        }
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }

    pub fn stmt(&self) -> &BlockStmt<'input> {
        &self.stmt
    }
}

#[derive(Debug)]
pub enum Stmt<'input> {
    BlockStmt(BlockStmt<'input>),
    DeclStmt(DeclStmt<'input>),
    AssignStmt(AssignStmt<'input>),
    IfStmt(IfStmt<'input>),
    WhileStmt(WhileStmt<'input>),
    ReturnStmt(ReturnStmt),
}
