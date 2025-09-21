//! This is an example of the code that the proc macro generates.

mod bf {
    #![allow(dead_code)]
    pub struct Module;
    unsafe impl ::stucco::stencil::Module for Module {
        type Args = (*mut u64,);
        type Result = ();
        unsafe fn call(ptr: *mut (), args: Self::Args) -> Self::Result {
            unsafe {
                (::core::mem::transmute::<*mut (), fn(u64, u64, u64, u64, u64, u64, u64, *mut u64)>(
                    ptr,
                ))(0, 0, 0, 0, 0, 0, 0, args.0)
            }
        }
        const BYTECODE: &[u8] = &[
            85u8, 65u8, 87u8, 65u8, 86u8, 65u8, 85u8, 65u8, 84u8, 83u8, 80u8, 72u8, 137u8, 203u8,
            72u8, 137u8, 245u8, 72u8, 139u8, 68u8, 36u8, 64u8, 72u8, 139u8, 76u8, 36u8, 72u8, 73u8,
            137u8, 253u8, 73u8, 137u8, 212u8, 77u8, 137u8, 198u8, 76u8, 137u8, 206u8, 72u8, 137u8,
            199u8, 73u8, 137u8, 200u8, 232u8, 0u8, 0u8, 0u8, 0u8, 72u8, 131u8, 196u8, 8u8, 91u8,
            65u8, 92u8, 65u8, 93u8, 65u8, 94u8, 65u8, 95u8, 93u8, 195u8,
        ];
        const ENTRY: ::stucco::stencil::Jump = ::stucco::stencil::Jump {
            size: ::stucco::stencil::JumpSize::Default,
            offset: 46u32,
            addend: -4i32,
            fallthrough: ::stucco::stencil::Fallthrough::None,
        };
    }
    pub struct HaltVariant0 {}
    impl Default for HaltVariant0 {
        fn default() -> Self {
            HaltVariant0 {}
        }
    }
    pub struct HaltVariant0Jumps {}
    unsafe impl ::stucco::stencil::Variant for HaltVariant0 {
        type Module = Module;
        type Jumps = HaltVariant0Jumps;
        const BYTECODE: &[u8] = &[195u8];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            HaltVariant0Jumps {}
        }
    }
    pub struct IncrementVariant0 {
        pub by: u64,
    }
    pub struct IncrementVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for IncrementVariant0 {
        type Module = Module;
        type Jumps = IncrementVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8, 137u8, 68u8, 36u8, 248u8,
            72u8, 139u8, 68u8, 36u8, 248u8, 73u8, 1u8, 0u8, 233u8, 0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.by),
                    offset: 2u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 24u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            IncrementVariant0Jumps { next }
        }
    }
    pub struct GetsVariant0 {
        pub read: u64,
    }
    pub struct GetsVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for GetsVariant0 {
        type Module = Module;
        type Jumps = GetsVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 131u8, 236u8, 24u8, 77u8, 137u8, 199u8, 72u8, 137u8, 124u8, 36u8, 16u8, 72u8,
            137u8, 116u8, 36u8, 8u8, 72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8,
            137u8, 4u8, 36u8, 255u8, 20u8, 36u8, 73u8, 137u8, 7u8, 72u8, 139u8, 116u8, 36u8, 8u8,
            72u8, 139u8, 124u8, 36u8, 16u8, 77u8, 137u8, 248u8, 72u8, 131u8, 196u8, 24u8, 233u8,
            0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.read),
                    offset: 19u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 55u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            GetsVariant0Jumps { next }
        }
    }
    pub struct JumpBackwardVariant0 {}
    impl Default for JumpBackwardVariant0 {
        fn default() -> Self {
            JumpBackwardVariant0 {}
        }
    }
    pub struct JumpBackwardVariant0Jumps {
        pub backward: ::stucco::stencil::Cont,
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for JumpBackwardVariant0 {
        type Module = Module;
        type Jumps = JumpBackwardVariant0Jumps;
        const BYTECODE: &[u8] = &[
            73u8, 131u8, 56u8, 0u8, 15u8, 132u8, 0u8, 0u8, 0u8, 0u8, 233u8, 0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let backward = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 11u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 6u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::None,
            });
            JumpBackwardVariant0Jumps { backward, next }
        }
    }
    pub struct DecrementVariant0 {
        pub by: u64,
    }
    pub struct DecrementVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for DecrementVariant0 {
        type Module = Module;
        type Jumps = DecrementVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8, 137u8, 68u8, 36u8, 248u8,
            72u8, 139u8, 68u8, 36u8, 248u8, 73u8, 41u8, 0u8, 233u8, 0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.by),
                    offset: 2u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 24u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            DecrementVariant0Jumps { next }
        }
    }
    pub struct PreviousVariant0 {
        pub count: u64,
    }
    pub struct PreviousVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for PreviousVariant0 {
        type Module = Module;
        type Jumps = PreviousVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8, 137u8, 68u8, 36u8, 248u8,
            72u8, 139u8, 68u8, 36u8, 248u8, 72u8, 193u8, 224u8, 3u8, 73u8, 41u8, 192u8, 233u8, 0u8,
            0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.count),
                    offset: 2u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 28u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            PreviousVariant0Jumps { next }
        }
    }
    pub struct JumpForwardVariant0 {}
    impl Default for JumpForwardVariant0 {
        fn default() -> Self {
            JumpForwardVariant0 {}
        }
    }
    pub struct JumpForwardVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
        pub forward: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for JumpForwardVariant0 {
        type Module = Module;
        type Jumps = JumpForwardVariant0Jumps;
        const BYTECODE: &[u8] = &[
            73u8, 131u8, 56u8, 0u8, 15u8, 133u8, 0u8, 0u8, 0u8, 0u8, 233u8, 0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 6u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::None,
            });
            let forward = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 11u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            JumpForwardVariant0Jumps { next, forward }
        }
    }
    pub struct NextVariant0 {
        pub count: u64,
    }
    pub struct NextVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for NextVariant0 {
        type Module = Module;
        type Jumps = NextVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8, 137u8, 68u8, 36u8, 248u8,
            72u8, 139u8, 68u8, 36u8, 248u8, 77u8, 141u8, 4u8, 192u8, 233u8, 0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.count),
                    offset: 2u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 25u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            NextVariant0Jumps { next }
        }
    }
    pub struct PutVariant0 {
        pub write: u64,
    }
    pub struct PutVariant0Jumps {
        pub next: ::stucco::stencil::Cont,
    }
    unsafe impl ::stucco::stencil::Variant for PutVariant0 {
        type Module = Module;
        type Jumps = PutVariant0Jumps;
        const BYTECODE: &[u8] = &[
            72u8, 131u8, 236u8, 24u8, 77u8, 137u8, 199u8, 72u8, 137u8, 124u8, 36u8, 16u8, 72u8,
            137u8, 116u8, 36u8, 8u8, 72u8, 184u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 72u8,
            137u8, 4u8, 36u8, 73u8, 139u8, 56u8, 255u8, 20u8, 36u8, 72u8, 139u8, 116u8, 36u8, 8u8,
            72u8, 139u8, 124u8, 36u8, 16u8, 77u8, 137u8, 248u8, 72u8, 131u8, 196u8, 24u8, 233u8,
            0u8, 0u8, 0u8, 0u8,
        ];
        fn produce_immediates<F>(self, f: &mut F)
        where
            F: FnMut(::stucco::stencil::Immediate),
        {
            let __f = f;
            let __args = self;
            {
                let imm = ::stucco::stencil::Immediate {
                    data: ::stucco::stencil::ImmediateData::U64(__args.write),
                    offset: 19u32,
                };
                __f(imm);
            }
        }
        fn collect_jumps<F>(f: &mut F) -> Self::Jumps
        where
            F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont,
        {
            let __f = f;
            let next = __f(::stucco::stencil::Jump {
                size: ::stucco::stencil::JumpSize::Default,
                offset: 55u32,
                addend: -4i32,
                fallthrough: ::stucco::stencil::Fallthrough::Omit(5u16),
            });
            PutVariant0Jumps { next }
        }
    }
}
