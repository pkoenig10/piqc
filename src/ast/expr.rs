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
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier<'input> {
    location: Location,
    name: &'input str,
    type_: Option<Type>,
}

impl<'input> Identifier<'input> {
    pub fn new(l: usize, name: &'input str, r: usize) -> Identifier<'input> {
        Identifier {
            location: Location::new(l, r),
            name,
            type_: None,
        }
    }

    pub fn name(&self) -> &'input str {
        self.name
    }

    pub fn type_(&self) -> Option<Type> {
        self.type_
    }

    pub fn set_type(&mut self, type_: Type) {
        self.type_ = Some(type_);
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
    type_: Option<Type>,
}

impl<'input> UnaryExpr<'input> {
    pub fn new(l: usize, op: UnaryOp, expr: Expr<'input>, r: usize) -> Expr<'input> {
        Expr::UnaryExpr(UnaryExpr {
            location: Location::new(l, r),
            op,
            expr: Box::new(expr),
            type_: None,
        })
    }

    pub fn op(&self) -> UnaryOp {
        self.op
    }

    pub fn expr(&mut self) -> &mut Expr<'input> {
        &mut self.expr
    }

    pub fn type_(&self) -> Option<Type> {
        self.type_
    }

    pub fn set_type(&mut self, type_: Type) {
        self.type_ = Some(type_);
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
    type_: Option<Type>,
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
            type_: None,
        })
    }

    pub fn op(&self) -> BinaryOp {
        self.op
    }

    pub fn left(&mut self) -> &mut Expr<'input> {
        &mut self.left
    }

    pub fn right(&mut self) -> &mut Expr<'input> {
        &mut self.right
    }

    pub fn type_(&self) -> Option<Type> {
        self.type_
    }

    pub fn set_type(&mut self, type_: Type) {
        self.type_ = Some(type_);
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

impl<'input> Expr<'input> {
    pub fn type_(&self) -> Option<Type> {
        match *self {
            Expr::IntLiteral(_) => Some(Type::Int),
            Expr::FloatLiteral(_) => Some(Type::Float),
            Expr::BoolLiteral(_) => Some(Type::Bool),
            Expr::Identifier(ref identifier) => identifier.type_(),
            Expr::UnaryExpr(ref expr) => expr.type_(),
            Expr::BinaryExpr(ref expr) => expr.type_(),
        }
    }
}
