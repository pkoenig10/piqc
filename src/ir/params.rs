use std::fmt;
use std::slice::Iter;
use std::vec::IntoIter;


#[derive(Debug, Clone)]
pub struct Params<T> {
    params: Vec<T>,
}

impl<T> Params<T> {
    pub fn new() -> Params<T> {
        Params { params: Vec::new() }
    }

    pub fn push(&mut self, param: T) {
        self.params.push(param);
    }
}

impl<'a, T> IntoIterator for &'a Params<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.params.iter()
    }
}

impl<T> IntoIterator for Params<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
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
