use std::fmt;
use std::ops::BitOr;

use crate::ir;

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

impl From<Variability> for ir::Variability {
    fn from(variability: Variability) -> ir::Variability {
        match variability {
            Variability::Uniform => ir::Variability::Uniform,
            Variability::Varying => ir::Variability::Varying,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Primitive {
    Int,
    Float,
    Bool,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let prim = match self {
            Primitive::Int => "int",
            Primitive::Float => "float",
            Primitive::Bool => "bool",
        };
        write!(f, "{}", prim)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Prim(Variability, Primitive),
    Array(Variability, Primitive),
    PrimRef(Variability, Primitive),
    ArrayRef(Variability, Primitive),
}

impl Type {
    pub const UNIFORM_INT: Type = Type::Prim(Variability::Uniform, Primitive::Int);
    pub const UNIFORM_FLOAT: Type = Type::Prim(Variability::Uniform, Primitive::Float);
    pub const UNIFORM_BOOL: Type = Type::Prim(Variability::Uniform, Primitive::Bool);
    pub const VARYING_INT: Type = Type::Prim(Variability::Varying, Primitive::Int);
    pub const VARYING_FLOAT: Type = Type::Prim(Variability::Varying, Primitive::Float);
    pub const VARYING_BOOL: Type = Type::Prim(Variability::Varying, Primitive::Bool);

    pub fn is_assignable_from(&self, ty: Type) -> bool {
        match (*self, ty) {
            (Type::Prim(variability, kind), Type::Prim(ty_variability, ty_kind)) => {
                if kind != ty_kind {
                    return false;
                }

                if (Variability::Uniform, Variability::Varying) == (variability, ty_variability) {
                    return false;
                }

                true
            }
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Prim(variability, prim) => write!(f, "{} {}", variability, prim),
            Type::Array(variability, prim) => write!(f, "[{} {}]", variability, prim),
            Type::PrimRef(variability, prim) => write!(f, "{} &{}", variability, prim),
            Type::ArrayRef(variability, prim) => write!(f, "{} &[{}]", variability, prim),
        }
    }
}

impl From<Type> for ir::Type {
    fn from(ty: Type) -> ir::Type {
        match ty {
            Type::UNIFORM_INT => ir::Type::UNIFORM_INT,
            Type::UNIFORM_FLOAT => ir::Type::UNIFORM_FLOAT,
            Type::UNIFORM_BOOL => ir::Type::UNIFORM_BOOL,
            Type::VARYING_INT => ir::Type::VARYING_INT,
            Type::VARYING_FLOAT => ir::Type::VARYING_FLOAT,
            Type::VARYING_BOOL => ir::Type::VARYING_BOOL,
            Type::Array(_, _) => ir::Type::UNIFORM_INT,
            Type::PrimRef(Variability::Uniform, _) => ir::Type::UNIFORM_INT,
            Type::PrimRef(Variability::Varying, _) => ir::Type::VARYING_INT,
            Type::ArrayRef(Variability::Uniform, _) => ir::Type::UNIFORM_INT,
            Type::ArrayRef(Variability::Varying, _) => ir::Type::VARYING_INT,
        }
    }
}
