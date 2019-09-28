use std::fmt;
use std::ops::BitOr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variability {
    Uniform,
    Varying,
}

impl BitOr for Variability {
    type Output = Self;

    fn bitor(self, variability: Variability) -> Self {
        match (self, variability) {
            (Variability::Uniform, Variability::Uniform) => Variability::Uniform,
            _ => Variability::Varying,
        }
    }
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

    pub fn or_variability(&self, ty: Type) -> Type {
        Type::new(self.variability | ty.variability, self.kind)
    }

    pub fn with_kind(&self, kind: TypeKind) -> Type {
        Type::new(self.variability, kind)
    }

    pub fn is_assignable_from(&self, ty: Type) -> bool {
        if self.kind != ty.kind {
            return false;
        }

        if (self.variability, ty.variability) == (Variability::Uniform, Variability::Varying) {
            return false;
        }

        true
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.variability, self.kind)
    }
}
