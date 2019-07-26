use std::fmt;

use crate::ir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variability {
    Uniform,
    Varying,
}

impl fmt::Display for Variability {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let variability = match self {
            Variability::Uniform => "uniform",
            Variability::Varying => "varying",
        };
        write!(f, "{}", variability)
    }
}

impl From<Variability> for ir::Variability {
    fn from(variability: Variability) -> ir::Variability {
        match variability {
            Variability::Uniform => ir::Variability::Uniform,
            Variability::Varying => ir::Variability::Varying,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimType {
    Int,
    Float,
    Bool,
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prim = match self {
            PrimType::Int => "int",
            PrimType::Float => "float",
            PrimType::Bool => "bool",
        };
        write!(f, "{}", prim)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Prim(PrimType),
    Ptr(PrimType),
}

impl TypeKind {
    pub const INT: TypeKind = TypeKind::Prim(PrimType::Int);
    pub const FLOAT: TypeKind = TypeKind::Prim(PrimType::Float);
    pub const BOOL: TypeKind = TypeKind::Prim(PrimType::Bool);
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::Prim(prim) => write!(f, "{}", prim),
            TypeKind::Ptr(prim) => write!(f, "{}*", prim),
        }
    }
}

impl From<TypeKind> for ir::TypeKind {
    fn from(kind: TypeKind) -> ir::TypeKind {
        match kind {
            TypeKind::INT => ir::TypeKind::Int,
            TypeKind::FLOAT => ir::TypeKind::Float,
            TypeKind::BOOL => ir::TypeKind::Bool,
            TypeKind::Ptr(_) => ir::TypeKind::Int,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type {
    pub variability: Variability,
    pub kind: TypeKind,
}

impl Type {
    pub const UNIFORM_INT: Type = Type {
        variability: Variability::Uniform,
        kind: TypeKind::INT,
    };
    pub const UNIFORM_FLOAT: Type = Type {
        variability: Variability::Uniform,
        kind: TypeKind::FLOAT,
    };
    pub const UNIFORM_BOOL: Type = Type {
        variability: Variability::Uniform,
        kind: TypeKind::BOOL,
    };
    pub const VARYING_INT: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::INT,
    };
    pub const VARYING_FLOAT: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::FLOAT,
    };
    pub const VARYING_BOOL: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::BOOL,
    };

    pub fn new(variability: Variability, kind: TypeKind) -> Type {
        Type { variability, kind }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.variability, self.kind)
    }
}

impl From<Type> for ir::Type {
    fn from(ty: Type) -> ir::Type {
        ir::Type::new(ty.variability.into(), ty.kind.into())
    }
}
