use std::ops::RangeInclusive;

use crate::id::Id;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct IdRange<I: Id> {
    pub start: I,
    pub end: I,
}

impl<I: Id> IdRange<I> {
    pub fn new(start: I, end: I) -> Self {
        IdRange { start, end }
    }

    pub fn to_range(self) -> RangeInclusive<I> {
        self.start..=self.end
    }

    pub fn iter(self) -> IdRangeIter<I> {
        IdRangeIter {
            cur: self.start,
            end: self.end,
            exhausted: self.start.idx() > self.end.idx(),
        }
    }
}

pub struct IdRangeIter<I> {
    pub cur: I,
    pub end: I,
    pub exhausted: bool,
}

impl<I: Id> Iterator for IdRangeIter<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }

        if self.cur == self.end {
            self.exhausted = true;
            return Some(self.cur);
        }
        let res = self.cur;
        self.cur = self.cur.next().unwrap();
        Some(res)
    }
}

impl<I: Id> IntoIterator for IdRange<I> {
    type Item = I;
    type IntoIter = IdRangeIter<I>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
