use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeQualifier {
    Uniform,
    Varying,
}

impl TypeQualifier {
    pub fn get(
        left_type_qualifier: TypeQualifier,
        right_type_qualifier: TypeQualifier,
    ) -> TypeQualifier {
        match (left_type_qualifier, right_type_qualifier) {
            (TypeQualifier::Uniform, TypeQualifier::Uniform) => TypeQualifier::Uniform,
            _ => TypeQualifier::Varying,
        }
    }
}

impl fmt::Display for TypeQualifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let qualifier = match self {
            TypeQualifier::Uniform => "uniform",
            TypeQualifier::Varying => "varying",
        };
        write!(f, "{}", qualifier)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    Int,
    Float,
    Bool,
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let base = match self {
            BaseType::Int => "int",
            BaseType::Float => "float",
            BaseType::Bool => "bool",
        };
        write!(f, "{}", base)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PtrType {
    base: BaseType,
    level: u8,
}

impl PtrType {
    pub fn new(base: BaseType, level: u8) -> PtrType {
        PtrType { base, level }
    }

    pub fn deref(self) -> TypeKind {
        match self.level {
            0 => TypeKind::Base(self.base),
            _ => TypeKind::Ptr(PtrType::new(self.base, self.level - 1)),
        }
    }
}

impl fmt::Display for PtrType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.base, "*".repeat((self.level + 1) as usize))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Base(BaseType),
    Ptr(PtrType),
}

impl TypeKind {
    pub const INT: TypeKind = TypeKind::Base(BaseType::Int);
    pub const FLOAT: TypeKind = TypeKind::Base(BaseType::Float);
    pub const BOOL: TypeKind = TypeKind::Base(BaseType::Bool);

    pub fn new_ptr(kind: TypeKind) -> TypeKind {
        let ptr = match kind {
            TypeKind::Base(ty) => PtrType::new(ty, 0),
            TypeKind::Ptr(ty) => PtrType::new(ty.base, ty.level + 1),
        };
        TypeKind::Ptr(ptr)
    }
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::Base(ty) => write!(f, "{}", ty),
            TypeKind::Ptr(ty) => write!(f, "{}", ty),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type {
    pub qualifier: TypeQualifier,
    pub kind: TypeKind,
}

impl Type {
    pub const UNIFORM_INT: Type = Type {
        qualifier: TypeQualifier::Uniform,
        kind: TypeKind::INT,
    };
    pub const UNIFORM_FLOAT: Type = Type {
        qualifier: TypeQualifier::Uniform,
        kind: TypeKind::FLOAT,
    };
    pub const UNIFORM_BOOL: Type = Type {
        qualifier: TypeQualifier::Uniform,
        kind: TypeKind::BOOL,
    };
    pub const VARYING_INT: Type = Type {
        qualifier: TypeQualifier::Varying,
        kind: TypeKind::INT,
    };
    pub const VARYING_FLOAT: Type = Type {
        qualifier: TypeQualifier::Varying,
        kind: TypeKind::FLOAT,
    };
    pub const VARYING_BOOL: Type = Type {
        qualifier: TypeQualifier::Varying,
        kind: TypeKind::BOOL,
    };

    pub fn new(qualifier: TypeQualifier, kind: TypeKind) -> Type {
        Type { qualifier, kind }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.qualifier, self.kind)
    }
}
