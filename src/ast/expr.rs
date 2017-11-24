use std::fmt;

use ast::*;
use ast::loc::*;

#[derive(Debug, Clone, Copy)]
pub struct IntLiteral {
    location: Location,
    value: i32,
}

impl IntLiteral {
    pub fn new(l: usize, value: i32, r: usize) -> IntLiteral {
        IntLiteral {
            location: Location::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatLiteral {
    location: Location,
    value: f32,
}


impl FloatLiteral {
    pub fn new(l: usize, value: f32, r: usize) -> FloatLiteral {
        FloatLiteral {
            location: Location::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> f32 {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    location: Location,
    value: bool,
}

impl BoolLiteral {
    pub fn new(l: usize, value: bool, r: usize) -> BoolLiteral {
        BoolLiteral {
            location: Location::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> bool {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier<'input> {
    location: Location,
    name: &'input str,
}

impl<'input> Identifier<'input> {
    pub fn new(l: usize, name: &'input str, r: usize) -> Identifier<'input> {
        Identifier {
            location: Location::new(l, r),
            name,
        }
    }

    pub fn name(&self) -> &'input str {
        self.name
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    BitNot,
    LogicalNot,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Negate => "-",
            BitNot => "~",
            LogicalNot => "!",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct UnaryExpr<'input> {
    location: Location,
    op: UnaryOp,
    expr: Box<Expr<'input>>,
}

impl<'input> UnaryExpr<'input> {
    pub fn new(l: usize, op: UnaryOp, expr: Expr<'input>, r: usize) -> Expr<'input> {
        Expr::UnaryExpr(UnaryExpr {
            location: Location::new(l, r),
            op,
            expr: Box::new(expr),
        })
    }

    pub fn op(&self) -> UnaryOp {
        self.op
    }

    pub fn expr(&self) -> &Expr<'input> {
        &self.expr
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Mul,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    LogicalAnd,
    LogicalOr,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match *self {
            Mul => "*",
            Add => "+",
            Sub => "-",
            Shl => "<<",
            Shr => ">>",
            BitAnd => "&",
            BitXor => "^",
            BitOr => "|",
            Eq => "==",
            Ne => "!=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
            LogicalAnd => "&&",
            LogicalOr => "||",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct BinaryExpr<'input> {
    location: Location,
    op: BinaryOp,
    left: Box<Expr<'input>>,
    right: Box<Expr<'input>>,
}

impl<'input> BinaryExpr<'input> {
    pub fn new(
        l: usize,
        left: Expr<'input>,
        op: BinaryOp,
        right: Expr<'input>,
        r: usize,
    ) -> Expr<'input> {
        Expr::BinaryExpr(BinaryExpr {
            location: Location::new(l, r),
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    pub fn op(&self) -> BinaryOp {
        self.op
    }

    pub fn left(&self) -> &Expr<'input> {
        &self.left
    }

    pub fn right(&self) -> &Expr<'input> {
        &self.right
    }
}

#[derive(Debug)]
pub enum Expr<'input> {
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(BoolLiteral),
    Identifier(Identifier<'input>),
    UnaryExpr(UnaryExpr<'input>),
    BinaryExpr(BinaryExpr<'input>),
}
