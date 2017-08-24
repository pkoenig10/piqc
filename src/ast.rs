#[derive(Debug)]
pub struct IntLiteral {
    value: i32,
}

impl IntLiteral {
    pub fn new(value: i32) -> IntLiteral {
        IntLiteral { value }
    }
}

#[derive(Debug)]
pub struct FloatLiteral {
    value: f32,
}


impl FloatLiteral {
    pub fn new(value: f32) -> FloatLiteral {
        FloatLiteral { value }
    }
}

#[derive(Debug)]
pub struct BoolLiteral {
    value: bool,
}

impl BoolLiteral {
    pub fn new(value: bool) -> BoolLiteral {
        BoolLiteral { value }
    }
}

#[derive(Debug)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: &str) -> Identifier {
        Identifier { name: name.to_string() }
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
    op: UnaryOp,
    expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, expr: Expr) -> UnaryExpr {
        UnaryExpr {
            op,
            expr: Box::new(expr),
        }
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
    op: BinaryOp,
    left: Box<Expr>,
    right: Box<Expr>,
}

impl BinaryExpr {
    pub fn new(op: BinaryOp, left: Expr, right: Expr) -> BinaryExpr {
        BinaryExpr {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
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
