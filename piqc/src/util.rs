use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub trait Id: Copy {
    fn new(id: usize) -> Self;

    fn get(self) -> usize;
}

#[macro_export]
macro_rules! id {
    ($vis:vis $id:ident, $prefix:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $id(usize);

        impl $crate::util::Id for $id {
            fn new(id: usize) -> Self {
                $id(id)
            }

            fn get(self) -> usize {
                self.0
            }
        }

        impl std::fmt::Display for $id {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, concat!($prefix, "{}"), self.0)
            }
        }
    };
}

#[derive(Debug)]
pub struct Map<K, V> {
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
        let id = K::new(self.values.len());
        self.values.push(value);
        id
    }
}

impl<K, V> Index<K> for Map<K, V>
where
    K: Id,
{
    type Output = V;

    fn index(&self, id: K) -> &V {
        &self.values[id.get()]
    }
}

impl<K, V> IndexMut<K> for Map<K, V>
where
    K: Id,
{
    fn index_mut(&mut self, id: K) -> &mut V {
        &mut self.values[id.get()]
    }
}

#[derive(Debug)]
pub struct InternMap<K, V>
where
    V: Eq + Hash,
{
    values: HashMap<V, K>,
}

impl<K, V> InternMap<K, V>
where
    K: Id,
    V: Eq + Hash,
{
    pub fn new() -> InternMap<K, V> {
        InternMap {
            values: HashMap::new(),
        }
    }

    pub fn intern(&mut self, value: V) -> K {
        let id = K::new(self.values.len());
        *self.values.entry(value).or_insert(id)
    }
}
