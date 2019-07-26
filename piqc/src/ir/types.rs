use std::fmt;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let base = match self {
            TypeKind::Int => "int",
            TypeKind::Float => "float",
            TypeKind::Bool => "bool",
        };
        write!(f, "{}", base)
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
        kind: TypeKind::Int,
    };
    pub const UNIFORM_FLOAT: Type = Type {
        variability: Variability::Uniform,
        kind: TypeKind::Float,
    };
    pub const UNIFORM_BOOL: Type = Type {
        variability: Variability::Uniform,
        kind: TypeKind::Bool,
    };
    pub const VARYING_INT: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::Int,
    };
    pub const VARYING_FLOAT: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::Float,
    };
    pub const VARYING_BOOL: Type = Type {
        variability: Variability::Varying,
        kind: TypeKind::Bool,
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
