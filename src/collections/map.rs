use std::marker::PhantomData;

pub trait Key: Copy {
    fn new(id: usize) -> Self;

    fn get(&self) -> usize;
}

#[derive(Debug)]
pub struct Map<K, V> {
    values: Vec<V>,
    phantom: PhantomData<K>,
}

impl<K, V> Map<K, V>
where
    K: Key,
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

    pub fn get(&self, key: K) -> &V {
        self.values.get(key.get()).unwrap()
    }

    pub fn get_mut(&mut self, key: K) -> &mut V {
        self.values.get_mut(key.get()).unwrap()
    }
}
