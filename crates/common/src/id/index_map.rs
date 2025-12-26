use std::{
    cmp::Ordering,
    marker::PhantomData,
    ops::{
        Index, IndexMut, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
    },
};

use crate::id::IdRange;

use super::Id;

/// A collection mapping one, dense index to another type.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexMap<I, T> {
    inner: Vec<T>,
    _marker: PhantomData<I>,
}

impl<I, T> Default for IndexMap<I, T> {
    fn default() -> Self {
        IndexMap::new()
    }
}

impl<I, T> IndexMap<I, T> {
    /// Create a new `IdVec`.
    #[inline]
    pub const fn new() -> Self {
        Self {
            inner: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Create a new `IdVec`.
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<I: Id, T> IndexMap<I, T> {
    /// Returns the id of the next value that would be pushed.
    /// Will return None if the size exceeded the value the index can hold.
    #[inline]
    pub fn next_id(&self) -> Option<I> {
        I::from_idx(self.inner.len())
    }

    #[inline]
    #[track_caller]
    pub fn next_id_expect(&self) -> I {
        I::from_idx(self.inner.len()).expect("Pushed too many entries into a u32::MAX limited map")
    }

    /// Add a new value to the vector returning the id.
    /// Will return None if the size exceeded the value the index can hold.
    #[inline]
    pub fn push(&mut self, value: T) -> Option<I> {
        let idx = I::from_idx(self.inner.len())?;
        self.inner.push(value);
        Some(idx)
    }

    /// Add a new value to the vector returning the id.
    /// Will return None if the size exceeded the value the index can hold.
    #[inline]
    #[track_caller]
    pub fn push_expect(&mut self, value: T) -> I {
        let idx = self.next_id_expect();
        self.inner.push(value);
        idx
    }

    /// Inserts the value the given index.
    ///
    /// If the current length is less then the index of where this value should be inserted all the
    /// other values will be filled with the given callback.
    #[inline]
    pub fn insert_fill<F>(&mut self, at: I, value: T, mut fill: F)
    where
        F: FnMut() -> T,
    {
        let idx = at.idx();
        match idx.cmp(&self.inner.len()) {
            Ordering::Less => {
                self[at] = value;
            }
            Ordering::Equal => {
                self.inner.push(value);
            }
            Ordering::Greater => {
                self.inner.reserve(idx - self.inner.len());
                for _ in self.inner.len()..idx {
                    self.inner.push(fill());
                }
                self.inner.push(value);
            }
        }
    }

    #[inline]
    pub fn get(&self, i: I) -> Option<&T> {
        let idx = i.idx();
        if self.inner.len() <= idx {
            return None;
        }
        Some(&self.inner[idx])
    }

    #[inline]
    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        let idx = i.idx();
        if self.inner.len() <= idx {
            return None;
        }
        Some(&mut self.inner[idx])
    }
}

impl<I: Id, T> IndexMap<I, Option<T>> {
    /// Returns the entry for the given index assuming it is filled.
    /// Meaning it is both inserted and not `None`
    #[inline]
    pub fn get_filled(&self, i: I) -> &T {
        self[i].as_ref().unwrap()
    }

    /// Returns the entry for the given index assuming it is filled.
    /// Meaning it is both inserted and not `None`
    #[inline]
    pub fn get_filled_mut(&mut self, i: I) -> &mut T {
        self[i].as_mut().unwrap()
    }

    /// Returns the entry for the given index.
    /// Returns none if either the entry is None or the index is not within the map
    #[inline]
    pub fn get_flat(&self, i: I) -> Option<&T> {
        self.get(i).and_then(|x| x.as_ref())
    }

    /// Returns the entry for the given index assuming it is filled.
    /// Returns none if either the entry is None or the index is not within the map
    #[inline]
    pub fn get_flat_mut(&mut self, i: I) -> Option<&mut T> {
        self.get_mut(i).and_then(|x| x.as_mut())
    }
}

impl<I: Id, T: Default> IndexMap<I, T> {
    pub fn insert_fill_default(&mut self, at: I, value: T) {
        self.insert_fill(at, value, Default::default);
    }
}

impl<I: Id, T> Index<I> for IndexMap<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        self.inner.index(index.idx())
    }
}

impl<I: Id, T> IndexMut<I> for IndexMap<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.inner.index_mut(index.idx())
    }
}

macro_rules! impl_range {
    ($name:ident$(<$gen:ident>)?, $($beg:ident)? [$t:tt] $($end:ident)?) => {
        impl<I: Id,T> Index<$name $(<$gen>)? > for IndexMap<I, T> {
            type Output = [T];

            fn index(&self, _index: $name$(<$gen>)?) -> &Self::Output {
                self.inner.index($(_index.$beg.idx())? $t $(_index.$end.idx())?)
            }
        }

        impl<I : Id, T> IndexMut<$name$(<$gen>)?> for IndexMap<I, T> {
            fn index_mut(&mut self, _index: $name$(<$gen>)?) -> &mut Self::Output {
                self.inner.index_mut($(_index.$beg.idx())? $t $(_index.$end.idx())?)
            }
        }
    };
}

impl_range!(Range<I>, start[..]end);
impl_range!(RangeFrom<I>, start[..]);
impl_range!(RangeFull, [..]);
impl_range!(RangeTo<I>, [..]end);
impl_range!(RangeToInclusive<I>, [..=]end);

impl<I: Id, T> Index<RangeInclusive<I>> for IndexMap<I, T> {
    type Output = [T];
    fn index(&self, _index: RangeInclusive<I>) -> &Self::Output {
        self.inner.index(_index.start().idx()..=_index.end().idx())
    }
}
impl<I: Id, T> IndexMut<RangeInclusive<I>> for IndexMap<I, T> {
    fn index_mut(&mut self, _index: RangeInclusive<I>) -> &mut Self::Output {
        self.inner
            .index_mut(_index.start().idx()..=_index.end().idx())
    }
}

impl<I: Id, T> Index<IdRange<I>> for IndexMap<I, T> {
    type Output = [T];
    fn index(&self, index: IdRange<I>) -> &Self::Output {
        self.inner.index(index.start.idx()..=index.end.idx())
    }
}
impl<I: Id, T> IndexMut<IdRange<I>> for IndexMap<I, T> {
    fn index_mut(&mut self, index: IdRange<I>) -> &mut Self::Output {
        self.inner.index_mut(index.start.idx()..=index.end.idx())
    }
}

/// A collection mapping one, dense index to another.
/// This map can be build up incrementally, inserting anywhere and filling any possible inbetween
/// indecies with empty values.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PartialIndexMap<I, T>(IndexMap<I, Option<T>>);

impl<I, T> Default for PartialIndexMap<I, T> {
    fn default() -> Self {
        PartialIndexMap::new()
    }
}

impl<I, T> PartialIndexMap<I, T> {
    /// Create a new map with a given capacity.
    pub const fn new() -> Self {
        PartialIndexMap(IndexMap::new())
    }

    /// Create a new map with a given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        PartialIndexMap(IndexMap::with_capacity(cap))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl<I: Id, T> PartialIndexMap<I, T> {
    #[inline]
    pub fn get(&self, i: I) -> Option<&T> {
        self.0.get_flat(i)
    }

    #[inline]
    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        self.0.get_flat_mut(i)
    }

    #[inline]
    pub fn insert_fill(&mut self, at: I, value: T) {
        self.0.insert_fill(at, Some(value), || None)
    }
}

impl<I: Id, T> Index<I> for PartialIndexMap<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        self.0.get_filled(index)
    }
}

impl<I: Id, T> IndexMut<I> for PartialIndexMap<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.0.get_filled_mut(index)
    }
}
