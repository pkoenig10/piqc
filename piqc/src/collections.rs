use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::iter::Enumerate;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::slice;

pub trait Id: Copy + Display + Debug + Eq + Hash {
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

impl<'a, K, V> IntoIterator for &'a mut PrimaryMap<K, V>
where
    K: Id,
{
    type Item = (K, &'a mut V);
    type IntoIter = MapIterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> MapIterMut<'a, K, V> {
        MapIterMut::new(&mut self.values)
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

impl<'a, K, V> IntoIterator for &'a SecondaryMap<K, V>
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

impl<'a, K, V> IntoIterator for &'a mut SecondaryMap<K, V>
where
    K: Id,
{
    type Item = (K, &'a mut V);
    type IntoIter = MapIterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> MapIterMut<'a, K, V> {
        MapIterMut::new(&mut self.values)
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

enum UnionFindEntry<T> {
    Rank(u32),
    Link(T),
}

impl<T> Default for UnionFindEntry<T> {
    fn default() -> UnionFindEntry<T> {
        UnionFindEntry::Rank(0)
    }
}

pub struct UnionFind<T>
where
    T: Id,
{
    values: SecondaryMap<T, UnionFindEntry<T>>,
}

impl<T> UnionFind<T>
where
    T: Id,
{
    pub fn new() -> UnionFind<T> {
        UnionFind {
            values: SecondaryMap::new(),
        }
    }

    pub fn find2(&self, mut value: T) -> T {
        loop {
            match self.values[value] {
                UnionFindEntry::Rank(_) => return value,
                UnionFindEntry::Link(parent) => value = parent,
            };
        }
    }

    pub fn find(&mut self, mut value: T) -> (T, u32) {
        let mut path = Vec::new();

        let (leader, rank) = loop {
            match self.values[value] {
                UnionFindEntry::Rank(rank) => break (value, rank),
                UnionFindEntry::Link(parent) => {
                    path.push(value);
                    value = parent;
                }
            }
        };

        for value in path {
            self.values[value] = UnionFindEntry::Link(leader);
        }

        (leader, rank)
    }

    pub fn union(&mut self, value0: T, value1: T) {
        let (leader0, rank0) = self.find(value0);
        let (leader1, rank1) = self.find(value1);

        if leader0 == leader1 {
            return;
        }

        match rank0.cmp(&rank1) {
            Ordering::Less => {
                self.values[leader0] = UnionFindEntry::Link(leader1);
            }
            Ordering::Greater => {
                self.values[leader1] = UnionFindEntry::Link(leader0);
            }
            Ordering::Equal => {
                self.values[leader0] = UnionFindEntry::Rank(rank0 + 1);
                self.values[leader1] = UnionFindEntry::Link(leader0);
            }
        }
    }

    pub fn print(&mut self) {
        for (value, _) in &self.values {
            println!("{}: {}", value, self.find2(value));
        }
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
    fn new(values: &'a [V]) -> MapIter<'a, K, V> {
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

pub struct MapIterMut<'a, K, V>
where
    K: Id,
{
    values: Enumerate<slice::IterMut<'a, V>>,
    phantom: PhantomData<K>,
}

impl<'a, K, V> MapIterMut<'a, K, V>
where
    K: Id,
{
    fn new(values: &'a mut [V]) -> MapIterMut<'a, K, V> {
        MapIterMut {
            values: values.iter_mut().enumerate(),
            phantom: PhantomData,
        }
    }
}

impl<'a, K, V> Iterator for MapIterMut<'a, K, V>
where
    K: Id,
{
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<(K, &'a mut V)> {
        self.values.next().map(|(i, v)| (K::new(i), v))
    }
}
