use enum_dispatch::enum_dispatch;
use std::fmt;

use crate::ast::*;

id!(pub Symbol, "s");

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    pub span: Span,
    pub symbol: Symbol,
}

impl Identifier {
    pub fn new(span: Span, symbol: Symbol) -> Identifier {
        Identifier { span, symbol }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntExpr {
    pub value: i32,
}

impl IntExpr {
    pub fn new(value: i32) -> IntExpr {
        IntExpr { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatExpr {
    pub value: f32,
}

impl FloatExpr {
    pub fn new(value: f32) -> FloatExpr {
        FloatExpr { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolExpr {
    pub value: bool,
}

impl BoolExpr {
    pub fn new(value: bool) -> BoolExpr {
        BoolExpr { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ElementExpr;

impl ElementExpr {
    pub fn new() -> ElementExpr {
        ElementExpr {}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CountExpr;

impl CountExpr {
    pub fn new() -> CountExpr {
        CountExpr {}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentifierExpr {
    pub identifier: Identifier,
}

impl IdentifierExpr {
    pub fn new(identifier: Identifier) -> IdentifierExpr {
        IdentifierExpr { identifier }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, expr: Expr) -> UnaryExpr {
        UnaryExpr {
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
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(left: Expr, op: BinaryOp, right: Expr) -> BinaryExpr {
        BinaryExpr {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub struct IndexExpr {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

impl IndexExpr {
    pub fn new(expr: Expr, index: Expr) -> IndexExpr {
        IndexExpr {
            expr: Box::new(expr),
            index: Box::new(index),
        }
    }
}

#[derive(Debug)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
}

impl ParenExpr {
    pub fn new(expr: Expr) -> ParenExpr {
        ParenExpr {
            expr: Box::new(expr),
        }
    }
}

#[enum_dispatch]
#[derive(Debug)]
pub enum ExprKind {
    Int(IntExpr),
    Float(FloatExpr),
    Bool(BoolExpr),
    Element(ElementExpr),
    Count(CountExpr),
    Identifier(IdentifierExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Index(IndexExpr),
    Paren(ParenExpr),
}

#[enum_dispatch(ExprKind)]
trait ExprTrait {}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Expr {
        Expr { span, kind }
    }
}
