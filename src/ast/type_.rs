use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_ = match *self {
            Type::Int => "int",
            Type::Float => "float",
            Type::Bool => "bool",
        };
        write!(f, "{}", type_)
    }
}
