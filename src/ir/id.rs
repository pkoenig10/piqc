use std::fmt;

macro_rules! id {
    ($id:ident, $prefix:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $id {
            id: usize,
        }

        impl $crate::ir::Id for $id {
            fn new(id: usize) -> Self {
                $id { id }
            }

            fn index(self) -> usize {
                self.id
            }
        }

        impl fmt::Display for $id {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, concat!($prefix, "{}"), self.id)
            }
        }
    };
}

id!(Ebb, "ebb");
id!(Block, "b");
id!(Inst, "i");
id!(Value, "%");
