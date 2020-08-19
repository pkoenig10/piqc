use std::collections::HashMap;
use std::hash::Hash;
use std::iter::Enumerate;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::slice;

pub trait Id: Copy + Eq + Hash {
    fn new(id: usize) -> Self;

    fn index(self) -> usize;
}

macro_rules! id {
    ($vis:vis $id:ident, $prefix:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

        impl std::fmt::Debug for $id {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::Display::fmt(self, f)
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
pub struct Set<T>
where
    T: Id,
{
    values: Vec<u32>,
    phantom: PhantomData<T>,
}

impl<T> Set<T>
where
    T: Id,
{
    pub fn new() -> Set<T> {
        Set {
            values: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn contains(&mut self, value: T) -> bool {
        let index = Set::index(value);
        match self.values.get(index) {
            Some(block) => {
                let mask = Set::mask(value);
                (block & mask) != 0
            }
            None => false,
        }
    }

    pub fn insert(&mut self, value: T) -> bool {
        if self.contains(value) {
            return false;
        }

        let index = Set::index(value);
        if index >= self.values.len() {
            self.values.resize(index + 1, 0);
        }

        let mask = Set::mask(value);
        self.values[index] |= mask;

        true
    }

    fn index(value: T) -> usize {
        value.index() / 32
    }

    fn mask(value: T) -> u32 {
        1 << (value.index() % 32)
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

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn insert(&mut self, value: V) -> K {
        let key = K::new(self.values.len());
        self.values.push(value);
        key
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

impl<'a, K, V> IntoIterator for &'a PrimaryMap<K, V>
where
    K: Id,
{
    type Item = (K, &'a V);
    type IntoIter = MapIter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> MapIter<'a, K, V> {
        MapIter::new(&self.values)
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

    pub fn clear(&mut self) {
        self.values.clear()
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

pub struct MapIter<'a, K, V>
where
    K: Id,
{
    values: Enumerate<slice::Iter<'a, V>>,
    phantom: PhantomData<K>,
}

impl<'a, K, V> MapIter<'a, K, V>
where
    K: Id,
{
    pub fn new(values: &'a [V]) -> MapIter<'a, K, V> {
        MapIter {
            values: values.iter().enumerate(),
            phantom: PhantomData,
        }
    }
}

impl<'a, K, V> Iterator for MapIter<'a, K, V>
where
    K: Id,
{
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<(K, &'a V)> {
        self.values.next().map(|(i, v)| (K::new(i), v))
    }
}
