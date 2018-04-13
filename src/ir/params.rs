use std::fmt;
use std::ops::Deref;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct Params<T> {
    params: Vec<T>,
}

impl<T> Params<T> {
    pub fn new() -> Params<T> {
        Params { params: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn push(&mut self, param: T) {
        self.params.push(param);
    }
}

impl<T> Deref for Params<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.params
    }
}

impl<'a, T> IntoIterator for &'a Params<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.params.iter()
    }
}

impl<T> fmt::Display for Params<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for param in &self.params {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        Ok(())
    }
}
