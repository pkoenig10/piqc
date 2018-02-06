use std::fmt;
use std::ops::Index;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct Args<T> {
    args: Vec<T>,
}

impl<T> Args<T> {
    pub fn new() -> Args<T> {
        Args { args: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn push(&mut self, arg: T) {
        self.args.push(arg);
    }
}

impl<T> Index<usize> for Args<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.args.index(index)
    }
}

impl<'a, T> IntoIterator for &'a Args<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.iter()
    }
}

impl<T> fmt::Display for Args<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for arg in &self.args {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}
