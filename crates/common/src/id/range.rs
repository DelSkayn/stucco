#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct IdRange<I> {
    pub start: I,
    pub end: I,
}

impl<I> IdRange<I> {
    pub fn new(start: I, end: I) -> Self {
        IdRange { start, end }
    }
}
