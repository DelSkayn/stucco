mod index_map;
mod index_set;

pub use index_map::{IndexMap, PartialIndexMap};
pub use index_set::IdSet;

pub trait Id: Sized + Copy {
    fn idx(self) -> usize;

    fn from_idx(idx: usize) -> Option<Self>;
}

impl Id for u32 {
    fn idx(self) -> usize {
        self as usize
    }

    fn from_idx(idx: usize) -> Option<Self> {
        idx.try_into().ok()
    }
}

#[macro_export]
macro_rules! id {
    ($name:ident $( < $($gen:ident),* $(,)? > )? ) => {
        pub struct $name $( < $( $gen, )* > )?{
            id: ::std::num::NonZeroU32,
            $(
                _marker: ::std::marker::PhantomData< $($gen),*>
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
            const MIN: Self = unsafe{
                Self{
                        id: ::std::num::NonZeroU32::new_unchecked(u32::MAX),
                        $(
                            _marker: ::std::marker::PhantomData::<$($gen),*>,
                        )?
                    }
            };

            const MAX : Self = unsafe{
                Self{
                        id: ::std::num::NonZeroU32::new_unchecked((u32::MAX - 1) ^ u32::MAX),
                        $(
                            _marker: ::std::marker::PhantomData::<$($gen),*>,
                        )?
                    }
            };

            pub const fn from_u32(index: u32) -> Option<Self> {
                if index == u32::MAX {
                    return None;
                }

                unsafe {
                    Some(Self::from_u32_unchecked(index))
                }
            }

            pub const unsafe fn from_u32_unchecked(index: u32) -> Self {
                unsafe {
                    Self{
                        id: ::std::num::NonZeroU32::new_unchecked(index as u32 ^ u32::MAX),
                        $(
                            _marker: ::std::marker::PhantomData::<$($gen),*>,
                        )?
                    }
                }
            }

            pub const fn into_u32(self) -> u32 {
                self.id.get() ^ u32::MAX
            }

            pub fn next(self) -> Option<Self>{
                Self::from_u32(self.into_u32() + 1)
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
