use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeQualifier {
    Uniform,
    Varying,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    Int,
    Float,
    Bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type {
    qualifier: TypeQualifier,
    type_: BaseType,
}

impl Type {
    pub fn new(qualifier: TypeQualifier, type_: BaseType) -> Type {
        Type { qualifier, type_ }
    }

    pub fn qualifier(&self) -> TypeQualifier {
        self.qualifier
    }

    pub fn type_(&self) -> BaseType {
        self.type_
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_ = match self.type_ {
            BaseType::Int => "int",
            BaseType::Float => "float",
            BaseType::Bool => "bool",
        };
        write!(f, "{}", type_)
    }
}
