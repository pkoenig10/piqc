use std::fmt;

use ast::*;

#[derive(Debug, Clone, Copy)]
pub struct IntLiteral {
    span: Span,
    value: i32,
}

impl IntLiteral {
    pub fn new(l: usize, value: i32, r: usize) -> IntLiteral {
        IntLiteral {
            span: Span::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> i32 {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatLiteral {
    span: Span,
    value: f32,
}

impl FloatLiteral {
    pub fn new(l: usize, value: f32, r: usize) -> FloatLiteral {
        FloatLiteral {
            span: Span::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> f32 {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLiteral {
    span: Span,
    value: bool,
}

impl BoolLiteral {
    pub fn new(l: usize, value: bool, r: usize) -> BoolLiteral {
        BoolLiteral {
            span: Span::new(l, r),
            value,
        }
    }

    pub fn value(&self) -> bool {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Index {
    span: Span,
}

impl Index {
    pub fn new(l: usize, r: usize) -> Index {
        Index {
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
    name: &'input str,
}

impl<'input> Identifier<'input> {
    pub fn new(l: usize, name: &'input str, r: usize) -> Identifier<'input> {
        Identifier {
            span: Span::new(l, r),
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
    op: UnaryOp,
    expr: Box<Expr<'input>>,
}

impl<'input> UnaryExpr<'input> {
    pub fn new(l: usize, op: UnaryOp, expr: Expr<'input>, r: usize) -> UnaryExpr<'input> {
        UnaryExpr {
            span: Span::new(l, r),
            op,
            expr: Box::new(expr),
        }
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
    ) -> BinaryExpr<'input> {
        BinaryExpr {
            span: Span::new(l, r),
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
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
    Index(Index),
    Count(Count),
    Identifier(Identifier<'input>),
    Unary(UnaryExpr<'input>),
    Binary(BinaryExpr<'input>),
}
