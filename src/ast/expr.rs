use super::loc::*;

#[derive(Debug)]
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
}

#[derive(Debug)]
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
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct Identifier<'input> {
    location: Location,
    name: &'input str,
}

impl<'input> Identifier<'input> {
    pub fn new(l: usize, name: &'input str, r: usize) -> Identifier<'input> {
        Identifier {
            location: Location::new(l, r),
            name: name,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    BitNot,
    LogicalNot,
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
}

#[derive(Debug)]
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
        left_expr: Expr<'input>,
        op: BinaryOp,
        right_expr: Expr<'input>,
        r: usize,
    ) -> Expr<'input> {
        Expr::BinaryExpr(BinaryExpr {
            location: Location::new(l, r),
            op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
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
