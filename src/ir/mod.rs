pub use self::builder::FuncBuilder;
pub use self::func::Func;
pub use self::id::{Ebb, Inst, Value};
pub use self::inst::{BinaryOp, BranchOp, CompOp, UnaryOp};
pub use self::pass::run_dead_code;
pub use self::types::{Type, TypeKind, TypeQualifier};
pub use self::verifier::verify_ir;

use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use self::id::Block;
use self::inst::{
    BinaryInst, BoolConstInst, BranchInst, CountInst, ElementInst, FloatCompInst, FloatConstInst,
    InstData, IntCompInst, IntConstInst, JumpInst, ReturnInst, SelectInst, Target, UnaryInst,
};
use self::value::ValueData;

mod builder;
mod func;
mod id;
mod inst;
mod pass;
mod types;
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

struct DisplayList<'a, T>
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
