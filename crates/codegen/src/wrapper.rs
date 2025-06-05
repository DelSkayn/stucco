//! Wrapper library used to extend the C api and inkwell with functions only exposed on LLVM C++
//! API.

use inkwell::values::{AsValueRef, GlobalValue};
use llvm_sys::prelude::LLVMValueRef;

pub trait GlobalValueExt {
    fn set_dso_local(&self, is_dso_local: bool);
}

impl GlobalValueExt for GlobalValue<'_> {
    fn set_dso_local(&self, is_dso_local: bool) {
        unsafe { LLVMRustSetDSOLocal(self.as_value_ref(), is_dso_local) };
    }
}

unsafe extern "C" {
    fn LLVMRustSetDSOLocal(value: LLVMValueRef, is_dso_local: bool);
}
