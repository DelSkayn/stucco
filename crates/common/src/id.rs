use std::{
    cmp::Ordering,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

pub trait Id: Sized + Copy {
    fn idx(self) -> usize;

    fn from_idx(idx: usize) -> Option<Self>;
}

#[macro_export]
macro_rules! id {
    ($name:ident $( < $($gen:ident),* $(,)? > )? ) => {
        pub struct $name $( < $( $gen, )* > )?{
            id: ::std::num::NonZeroU32,
            $(
                _marker: PhantomData< $($gen),*>
            )?
        }

        impl$( <$($gen),* > )? $crate::id::Id for $name$( <$($gen),* > )?{
            fn idx(self) -> usize{
                self.into_u32() as usize
            }

            fn from_idx(idx: usize) -> Option<Self>{
                idx.try_into().ok().and_then(Self::from_u32)
            }
        }

        impl$( <$($gen),* > )? $name $( < $($gen),* > )? {
            fn from_u32(index: u32) -> Option<Self> {
                if index > (u32::MAX - 1) {
                    return None;
                }

                unsafe {
                    Some(Self{
                        id: ::std::num::NonZeroU32::new_unchecked(index as u32 ^ u32::MAX),
                        $(
                            _marker: PhantomData::<$($gen),*>,
                        )?
                    })
                }
            }

            fn into_u32(self) -> u32 {
                self.id.get() ^ u32::MAX
            }
        }

        impl$( <$($gen),* > )? Clone for $name $( < $($gen),* > )? {
            fn clone(&self) -> Self {
                *self
            }
        }
        impl$( <$($gen),* > )? Copy for $name $( < $($gen),* > )? { }
        impl$( <$($gen),* > )? PartialEq for $name $( < $($gen),* > )? {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id
            }
        }
        impl$( <$($gen),* > )? Eq for $name $( < $($gen),* > )? { }
        impl$( <$($gen),* > )? ::std::hash::Hash for $name $( < $($gen),* > )? {
            fn hash<H: ::std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state)
            }
        }
        impl$( <$($gen),* > )? ::std::fmt::Debug for $name $( < $($gen),* > )? {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("id", &self.into_u32())
                    .finish()
            }
        }

    };
}

pub struct IdVec<I, T> {
    inner: Vec<T>,
    _marker: PhantomData<I>,
}

impl<I: Id, T> IdVec<I, T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn push(&mut self, value: T) -> Option<I> {
        let idx = I::from_idx(self.inner.len())?;
        self.inner.push(value);
        Some(idx)
    }

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
}

impl<I: Id, T> Index<I> for IdVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        self.inner.index(index.idx())
    }
}

impl<I: Id, T> IndexMut<I> for IdVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.inner.index_mut(index.idx())
    }
}
