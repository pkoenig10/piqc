#[derive(Debug, Clone, Copy)]
pub struct Location {
    left: usize,
    right: usize,
}

impl Location {
    pub fn new(left: usize, right: usize) -> Location {
        Location { left, right }
    }
}
