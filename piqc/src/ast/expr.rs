use std::fmt;

use crate::ast::*;
use crate::ir;
use crate::util::Id;

id!(pub Variable, "v");

impl From<Variable> for ir::Variable {
    fn from(variable: Variable) -> ir::Variable {
        ir::Variable::new(variable.get())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntLiteral {
    span: Span,
    pub value: i32,
}

impl IntLiteral {
    pub fn new(l: usize, value: i32, r: usize) -> IntLiteral {
        IntLiteral {
            span: Span::new(l, r),
            value,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatLiteral {
    span: Span,
    pub value: f32,
}

impl FloatLiteral {
    pub fn new(l: usize, value: f32, r: usize) -> FloatLiteral {
        FloatLiteral {
            span: Span::new(l, r),
            value,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    span: Span,
    pub value: bool,
}

impl BoolLiteral {
    pub fn new(l: usize, value: bool, r: usize) -> BoolLiteral {
        BoolLiteral {
            span: Span::new(l, r),
            value,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Element {
    span: Span,
}

impl Element {
    pub fn new(l: usize, r: usize) -> Element {
        Element {
            span: Span::new(l, r),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Count {
    span: Span,
}

impl Count {
    pub fn new(l: usize, r: usize) -> Count {
        Count {
            span: Span::new(l, r),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    span: Span,
    pub variable: Variable,
}

impl Identifier {
    pub fn new(l: usize, variable: Variable, r: usize) -> Identifier {
        Identifier {
            span: Span::new(l, r),
            variable,
        }
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
        let op = match self {
            UnaryOp::Negate => "-",
            UnaryOp::BitNot => "~",
            UnaryOp::LogicalNot => "!",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    span: Span,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(l: usize, op: UnaryOp, expr: Expr, r: usize) -> UnaryExpr {
        UnaryExpr {
            span: Span::new(l, r),
            op,
            expr: Box::new(expr),
        }
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
    Min,
    Max,
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
        let op = match self {
            BinaryOp::Mul => "*",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitXor => "^",
            BinaryOp::BitOr => "|",
            BinaryOp::Min => "<?",
            BinaryOp::Max => ">?",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Le => "<=",
            BinaryOp::Ge => ">=",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    span: Span,
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(l: usize, left: Expr, op: BinaryOp, right: Expr, r: usize) -> BinaryExpr {
        BinaryExpr {
            span: Span::new(l, r),
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub struct IndexExpr {
    span: Span,
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

impl IndexExpr {
    pub fn new(l: usize, expr: Expr, index: Expr, r: usize) -> IndexExpr {
        IndexExpr {
            span: Span::new(l, r),
            expr: Box::new(expr),
            index: Box::new(index),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(BoolLiteral),
    Element(Element),
    Count(Count),
    Identifier(Identifier),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Index(IndexExpr),
}
