// C++ wrapper functions for API not exposed in inkwell and the C API.
#include "llvm/IR/GlobalValue.h"

extern "C" void LLVMRustSetDSOLocal(LLVMValueRef Global, bool is_dso_local) {
  llvm::unwrap<llvm::GlobalValue>(Global)->setDSOLocal(is_dso_local);
}
