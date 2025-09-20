use std::{
    ffi::c_void,
    marker::PhantomData,
    ptr::{self, NonNull},
};

use rustix::mm::{MapFlags, MprotectFlags, ProtFlags};

use crate::stencil::Module;

pub struct Bytecode<M> {
    pub(crate) code: Vec<u8>,
    pub(crate) __marker: PhantomData<M>,
}

impl<M: Module> Bytecode<M> {
    pub fn into_mapped_function(&self) -> MappedFunction<M> {
        let len = (self.code.len() + (4096 - 1)) & !(4096 - 1);
        dbg!(len);

        let memory: *mut c_void = unsafe {
            rustix::mm::mmap_anonymous(
                ptr::null_mut(),
                len,
                ProtFlags::WRITE | ProtFlags::READ,
                MapFlags::PRIVATE,
            )
        }
        .unwrap();
        let mem = NonNull::new(memory).unwrap();
        unsafe {
            std::ptr::copy_nonoverlapping(self.code.as_ptr(), mem.as_ptr().cast(), self.code.len());
        }

        unsafe {
            rustix::mm::mprotect(mem.as_ptr(), len, MprotectFlags::READ | MprotectFlags::EXEC)
        }
        .unwrap();

        MappedFunction {
            ptr: mem,
            len,
            __marker: PhantomData,
        }
    }

    #[cfg(feature = "object")]
    pub fn to_object_file_bytes(&self) -> Vec<u8> {
        let mut obj = object::write::Object::new(
            object::BinaryFormat::Elf,
            object::Architecture::X86_64,
            object::Endianness::Little,
        );

        let main_symbol = obj.add_symbol(object::write::Symbol {
            name: b"_start_".into(),
            value: 0,
            size: 0,
            kind: object::SymbolKind::Text,
            scope: object::SymbolScope::Linkage,
            weak: false,
            section: object::write::SymbolSection::Undefined,
            flags: object::SymbolFlags::None,
        });

        let main_section =
            obj.add_subsection(object::write::StandardSection::Text, b"stucco_function");
        let _main_offset = obj.add_symbol_data(main_symbol, main_section, &self.code, 8);
        obj.write().unwrap()
    }
}

pub struct MappedFunction<M> {
    ptr: NonNull<c_void>,
    len: usize,
    __marker: PhantomData<M>,
}

impl<M: Module> MappedFunction<M> {
    pub fn call(&self, args: M::Args) -> M::Result {
        unsafe { M::call(self.ptr.as_ptr().cast(), args) }
    }
}

impl<M> Drop for MappedFunction<M> {
    fn drop(&mut self) {
        unsafe { rustix::mm::munmap(self.ptr.as_ptr(), self.len).unwrap() }
    }
}
