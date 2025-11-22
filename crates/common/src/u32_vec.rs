use std::{
    alloc::{self, Layout},
    mem::{self},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
};

/// A vec, but it uses u32 for len and capacity so it is only 2 pointers wide instead of 3.
pub struct U32Vec<T> {
    ptr: NonNull<T>,
    len: u32,
    capacity: u32,
}

impl<T> U32Vec<T> {
    const MIN_ALLOC_SIZE: u32 = if mem::size_of::<T>() == 1 {
        8
    } else if mem::size_of::<T>() <= 1024 {
        4
    } else {
        1
    };

    pub fn new() -> Self {
        U32Vec {
            len: 0,
            capacity: 0,
            ptr: NonNull::dangling(),
        }
    }

    pub fn push(&mut self, value: T) {
        if self.len == u32::MAX {
            panic!("exceeded maximum size");
        }

        if self.capacity - self.len == 0 {
            self.grow_one()
        }

        unsafe {
            self.ptr.add(self.len as usize).write(value);
        }

        self.len += 1;
    }

    pub fn get(&self, index: u32) -> Option<&T> {
        if self.len <= index {
            return None;
        }
        Some(unsafe { self.ptr.add(index as usize).as_ref() })
    }

    pub fn get_mut(&mut self, index: u32) -> Option<&mut T> {
        if self.len <= index {
            return None;
        }
        Some(unsafe { self.ptr.add(index as usize).as_mut() })
    }

    pub fn clear(&mut self) {
        if self.len == 0 {
            return;
        }

        if mem::needs_drop::<T>() {
            let mut cur = self.ptr.as_ptr();
            let end = unsafe { self.ptr.add(self.len as usize).as_ptr() };
            while cur != end {
                unsafe { ptr::drop_in_place(cur) };
                cur = unsafe { cur.add(1) };
            }
        }

        self.len = 0;
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn capacity(&self) -> u32 {
        self.capacity
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe {
            let len = self.len;
            std::slice::from_raw_parts(self.ptr.as_ptr(), len as usize)
        }
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe {
            let len = self.len;
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), len as usize)
        }
    }

    #[cold]
    fn grow_one(&mut self) {
        let current_capacity = self.capacity;
        if current_capacity == 0 {
            let layout = Layout::array::<T>(Self::MIN_ALLOC_SIZE as usize).unwrap();

            let memory = unsafe { alloc::alloc(layout) };
            self.ptr = NonNull::new(memory).expect("failed to allocate").cast();
            self.capacity = Self::MIN_ALLOC_SIZE;
        } else {
            let layout = Layout::array::<T>(current_capacity as usize).unwrap();

            let new_capacity = (current_capacity + 1).next_power_of_two();

            let new_size = Layout::array::<T>(new_capacity as usize).unwrap().size();

            let memory = unsafe { alloc::realloc(self.ptr.as_ptr().cast(), layout, new_size) };
            self.ptr = NonNull::new(memory).expect("reallocation failed").cast();
            self.capacity = new_capacity;
        }
    }
}

impl<T> Drop for U32Vec<T> {
    fn drop(&mut self) {
        self.clear();

        if self.capacity != 0 {
            let layout = Layout::array::<T>(self.capacity as usize).unwrap();
            unsafe { alloc::dealloc(self.ptr.as_ptr().cast(), layout) };
        }
    }
}

impl<T> Deref for U32Vec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for U32Vec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_slice_mut()
    }
}

#[cfg(test)]
mod test {
    use std::cell::Cell;

    use super::U32Vec;

    #[test]
    pub fn basic_usage() {
        let mut vec = U32Vec::new();

        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.push(4);

        assert_eq!(vec[0], 1);
        assert_eq!(vec[1], 2);
        assert_eq!(vec[2], 3);
        assert_eq!(vec[3], 4);

        vec.clear();

        for i in 0..100 {
            vec.push(i);
        }

        for i in 0..100 {
            assert_eq!(vec[i], i);
        }
    }

    #[test]
    pub fn drops_correctly() {
        thread_local! {
            pub static TEST: Cell<usize> = Cell::new(0);
        }

        struct Dropper;

        impl Drop for Dropper {
            fn drop(&mut self) {
                TEST.with(|x| x.set(x.get() + 1));
            }
        }

        {
            let mut vec = U32Vec::new();

            vec.push(Dropper);
            vec.push(Dropper);
        }
        assert_eq!(TEST.with(|x| x.get()), 2);

        let mut vec = U32Vec::new();

        vec.push(Dropper);
        vec.push(Dropper);

        vec.clear();
        assert_eq!(TEST.with(|x| x.get()), 4);
    }
}
