use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub use self::block::*;
pub use self::ebb::*;
pub use self::func::*;
pub use self::inst::*;
pub use self::pass::*;
pub use self::prog::*;
pub use self::type_::*;
pub use self::value::*;

pub use self::BaseType::*;
pub use self::TypeQualifier::*;

pub use self::builder::generate_ir;
pub use self::verifier::verify_ir;

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

mod block;
mod builder;
mod ebb;
mod func;
mod inst;
mod pass;
mod prog;
mod type_;
mod value;
mod verifier;

trait Id: Copy {
    fn new(id: usize) -> Self;

    fn index(self) -> usize;
}

#[derive(Debug)]
struct Map<K, V> {
    values: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K, V> Map<K, V>
where
    K: Id,
{
    pub fn new() -> Map<K, V> {
        Map {
            values: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn create(&mut self, value: V) -> K {
        let key = K::new(self.values.len());
        self.values.push(value);
        key
    }
}

impl<K, V> Index<K> for Map<K, V>
where
    K: Id,
{
    type Output = V;

    fn index(&self, key: K) -> &V {
        &self.values[key.index()]
    }
}

impl<K, V> IndexMut<K> for Map<K, V>
where
    K: Id,
{
    fn index_mut(&mut self, key: K) -> &mut V {
        &mut self.values[key.index()]
    }
}

pub struct DisplayList<'a, T>
where
    T: 'a,
{
    values: &'a [T],
}

impl<'a, T> DisplayList<'a, T> {
    pub fn new(values: &'a [T]) -> DisplayList<T> {
        DisplayList { values }
    }
}

impl<'a, T> fmt::Display for DisplayList<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for value in self.values {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", value)?;
        }
        Ok(())
    }
}
