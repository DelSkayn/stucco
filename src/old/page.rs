use std::{error::Error, marker::PhantomData};

use memmap2::{Mmap, MmapOptions};
use object::write::WritableBuffer;

use crate::{FuncInstructions, JitFunc};

pub fn div_up(v: usize, by: usize) -> usize {
    (v + (by - 1)) / by
}

pub struct JitFunction<Fn> {
    mmap: Mmap,
    _marker: PhantomData<Fn>,
}

impl<Fn> JitFunction<Fn> {
    pub unsafe fn from_instructions(bytes: &FuncInstructions) -> Result<Self, Box<dyn Error>> {
        let mut mmap = MmapOptions::new()
            .len(div_up(bytes.0.len(), 4096) * 4096)
            .map_anon()?;

        mmap[..bytes.0.len()].copy_from_slice(&bytes.0);

        let mmap = mmap.make_exec()?;

        Ok(JitFunction {
            mmap,
            _marker: PhantomData,
        })
    }

    pub fn ptr(&self) -> *const () {
        self.mmap.as_ptr() as *const ()
    }
}

impl<Fn> JitFunction<Fn>
where
    Fn: JitFunc,
{
    pub fn call(&self, args: Fn::Args) -> Fn::Output {
        unsafe { Fn::call(self.mmap.as_ptr(), args) }
    }
}
