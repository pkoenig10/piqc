use std::fmt;

#[derive(Debug)]
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
