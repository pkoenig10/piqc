use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub trait Id: Copy + Eq + Hash {
    fn new(id: usize) -> Self;

    fn index(self) -> usize;
}

macro_rules! id {
    ($vis:vis $id:ident, $prefix:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $id(u32);

        impl $crate::collections::Id for $id {
            fn new(id: usize) -> Self {
                $id(id as u32)
            }

            fn index(self) -> usize {
                self.0 as usize
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
pub struct Generator<T> {
    next: usize,
    phantom: PhantomData<T>,
}

impl<T> Generator<T>
where
    T: Id,
{
    pub fn new() -> Generator<T> {
        Generator {
            next: 0,
            phantom: PhantomData,
        }
    }

    pub fn next(&mut self) -> T {
        let id = T::new(self.next);
        self.next += 1;
        id
    }
}

#[derive(Debug)]
pub struct PrimaryMap<K, V> {
    values: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K, V> PrimaryMap<K, V>
where
    K: Id,
{
    pub fn new() -> PrimaryMap<K, V> {
        PrimaryMap {
            values: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn create(&mut self, value: V) -> K {
        let key = K::new(self.values.len());
        self.values.push(value);
        key
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<K, V> Index<K> for PrimaryMap<K, V>
where
    K: Id,
{
    type Output = V;

    fn index(&self, key: K) -> &V {
        &self.values[key.index()]
    }
}

impl<K, V> IndexMut<K> for PrimaryMap<K, V>
where
    K: Id,
{
    fn index_mut(&mut self, key: K) -> &mut V {
        &mut self.values[key.index()]
    }
}

#[derive(Debug)]
pub struct SecondaryMap<K, V> {
    values: Vec<V>,
    default: V,
    phantom: PhantomData<K>,
}

impl<K, V> SecondaryMap<K, V>
where
    K: Id,
    V: Default,
{
    pub fn new() -> SecondaryMap<K, V> {
        SecondaryMap {
            values: Vec::new(),
            default: Default::default(),
            phantom: PhantomData,
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.extend(key);
        self.values[key.index()] = value;
    }

    fn extend(&mut self, key: K) {
        let index = key.index();
        if index >= self.values.len() {
            self.values.resize_with(index + 1, Default::default);
        }
    }
}

impl<K, V> Index<K> for SecondaryMap<K, V>
where
    K: Id,
    V: Default,
{
    type Output = V;

    fn index(&self, key: K) -> &V {
        self.values.get(key.index()).unwrap_or(&self.default)
    }
}

impl<K, V> IndexMut<K> for SecondaryMap<K, V>
where
    K: Id,
    V: Default,
{
    fn index_mut(&mut self, key: K) -> &mut V {
        self.extend(key);
        &mut self.values[key.index()]
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
        let key = K::new(self.values.len());
        *self.values.entry(value).or_insert(key)
    }
}
