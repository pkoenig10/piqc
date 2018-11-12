use std::fmt;

use ast::*;

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
pub struct Identifier<'input> {
    span: Span,
    pub name: &'input str,
}

impl<'input> Identifier<'input> {
    pub fn new(l: usize, name: &'input str, r: usize) -> Identifier<'input> {
        Identifier {
            span: Span::new(l, r),
            name,
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
        let op = match *self {
            UnaryOp::Negate => "-",
            UnaryOp::BitNot => "~",
            UnaryOp::LogicalNot => "!",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub struct UnaryExpr<'input> {
    span: Span,
    pub op: UnaryOp,
    pub expr: Box<Expr<'input>>,
}

impl<'input> UnaryExpr<'input> {
    pub fn new(l: usize, op: UnaryOp, expr: Expr<'input>, r: usize) -> UnaryExpr<'input> {
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
        let op = match *self {
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
pub struct BinaryExpr<'input> {
    span: Span,
    pub op: BinaryOp,
    pub left: Box<Expr<'input>>,
    pub right: Box<Expr<'input>>,
}

impl<'input> BinaryExpr<'input> {
    pub fn new(
        l: usize,
        left: Expr<'input>,
        op: BinaryOp,
        right: Expr<'input>,
        r: usize,
    ) -> BinaryExpr<'input> {
        BinaryExpr {
            span: Span::new(l, r),
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'input> {
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(BoolLiteral),
    Element(Element),
    Count(Count),
    Identifier(Identifier<'input>),
    Unary(UnaryExpr<'input>),
    Binary(BinaryExpr<'input>),
}
