use ast::loc::*;

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
pub struct Identifier {
    location: Location,
    name: String,
}

impl Identifier {
    pub fn new(l: usize, name: &str, r: usize) -> Identifier {
        Identifier {
            location: Location::new(l, r),
            name: name.to_string(),
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
pub struct UnaryExpr {
    location: Location,
    op: UnaryOp,
    expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(l: usize, op: UnaryOp, expr: Expr, r: usize) -> Expr {
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
pub struct BinaryExpr {
    location: Location,
    op: BinaryOp,
    left: Box<Expr>,
    right: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(l: usize, left_expr: Expr, op: BinaryOp, right_expr: Expr, r: usize) -> Expr {
        Expr::BinaryExpr(BinaryExpr {
            location: Location::new(l, r),
            op,
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }
}

#[derive(Debug)]
pub enum Expr {
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    BoolLiteral(BoolLiteral),
    Identifier(Identifier),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
}
