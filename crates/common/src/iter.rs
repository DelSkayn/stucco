use std::{
    array,
    mem::{self, MaybeUninit},
};

pub trait IterExt: Iterator {
    /// Collect but for statically sized collections like arrays or tupples.
    ///
    /// Returns type if it can be constructed from the iterator, otherwise returns None
    fn gather<T>(self) -> Option<T>
    where
        T: Gather<Self::Item>;
}

impl<I: Iterator> IterExt for I {
    fn gather<T>(self) -> Option<T>
    where
        T: Gather<I::Item>,
    {
        T::gather_from(self)
    }
}

pub trait Gather<T>: Sized {
    fn gather_from<I>(iter: I) -> Option<Self>
    where
        I: IterExt<Item = T>;
}

impl<const C: usize, T> Gather<T> for [T; C] {
    fn gather_from<I>(mut iter: I) -> Option<Self>
    where
        I: IterExt<Item = T>,
    {
        let (lower, upper) = iter.size_hint();
        if lower > C {
            return None;
        }
        if let Some(upper) = upper {
            if upper < C {
                return None;
            }
        }
        let mut res = MaybeUninit::<[T; C]>::uninit();
        for i in 0..C {
            let x = iter.next()?;
            // Writing into res is safe as it is allocated on the stack above.
            unsafe {
                res.as_mut_ptr().cast::<T>().add(i).write(x);
            }
        }
        // The array is now fully initialized, so transmuting out of MaybeUninit is safe.
        Some(unsafe { MaybeUninit::assume_init(res) })
    }
}
