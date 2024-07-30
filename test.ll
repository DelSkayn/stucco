; ModuleID = 'put_constant'
source_filename = "put_constant"

@__global__F = external dso_local global i8

define ghccc void @__stencil__(ptr %0) {
entry:
  %F = alloca { ptr }, align 8
  store ptr @__global__F, ptr %F, align 8
  call void asm sideeffect "", "=*m"(ptr elementtype(ptr) %F)
  %load_global_ptr1 = load i64, ptr %F, align 4
  store i64 %load_global_ptr1, ptr %0, align 4
  tail call void @__become_next(ptr %0)
  ret void
}

declare void @__become_next(ptr %0)
