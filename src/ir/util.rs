use std::fmt;

#[derive(Debug, Clone)]
pub struct DisplayList<'a, T>
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
