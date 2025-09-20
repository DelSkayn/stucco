use codegen::{StencilSet, StencilVariant};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use crate::compile::CompilationResult;

pub fn expand(compile: CompilationResult) -> TokenStream {
    let ast = &compile.code_gen.ast;
    let root = compile.root;
    let stencils = &compile.stencils;

    let name = format_ident!("{}", &ast[root].sym.unwrap().index(ast).name.index(ast));

    let bytes = &stencils.entry.text;
    let offset = stencils.entry.jump.offset;

    let jump_size = if stencils.entry.jump.size == 4 {
        quote! { ::stucco::stencil::JumpSize::Default }
    } else {
        quote! { ::stucco::stencil::JumpSize::Long }
    };
    let addend_i32 = stencils.entry.jump.addend as i32;
    let fallthrough = if stencils.entry.jump.is_fallthrough {
        let omit: u16 = (stencils.entry.jump.size + 1).try_into().unwrap();
        quote! { ::stucco::stencil::Fallthrough::Omit(#omit)}
    } else {
        quote! { ::stucco::stencil::Fallthrough::None }
    };

    let stencils = expand_stencils(stencils);

    let mut args_type = (0..compile.stencils.passing_registers - 1)
        .map(|_| quote! {u64})
        .collect::<Vec<_>>();
    args_type.push(quote! {*mut u64 });

    let mut args_value = (0..compile.stencils.passing_registers - 1)
        .map(|_| quote! {0})
        .collect::<Vec<_>>();
    args_value.push(quote! { args.0});

    quote! {
        mod #name {
            #![allow(dead_code)]
            pub struct Module;
            unsafe impl ::stucco::stencil::Module for Module{
                type Args = (*mut u64,);
                type Result = ();

                unsafe fn call(ptr: *mut (), args: Self::Args) -> Self::Result{
                    unsafe{ (::core::mem::transmute::<*mut (), fn(#(#args_type,)*)>(ptr))(#(#args_value),*) }
                }

                const BYTECODE: &[u8] = &[#(#bytes),*];

                const ENTRY: ::stucco::stencil::Jump = ::stucco::stencil::Jump {
                    size: #jump_size,
                    offset: #offset,
                    addend: #addend_i32,
                    fallthrough: #fallthrough,
                };
            }

            #(#stencils)*
        }
    }
}

pub fn expand_stencils(stencils: &StencilSet) -> Vec<TokenStream> {
    stencils
        .stencils
        .iter()
        .flat_map(|stencil| {
            stencil
                .1
                .variants
                .iter()
                .enumerate()
                .map(|(idx, variant)| expand_variants2(variant, stencil.0, idx))
        })
        .collect()
}

pub fn to_pascal_case(name: &str) -> String {
    let mut should_uppercase = true;
    let mut res = String::new();
    for c in name.chars() {
        if c == '_' {
            should_uppercase = true;
            continue;
        }
        if should_uppercase {
            c.to_uppercase().for_each(|x| res.push(x));
            should_uppercase = false;
        } else {
            res.push(c)
        }
    }
    res
}

pub fn expand_variants2(variant: &StencilVariant, stencil_name: &str, idx: usize) -> TokenStream {
    let variant_name: String = to_pascal_case(stencil_name);

    let variant_name = format_ident!("{variant_name}Variant{idx}");
    let variant_args_name = format_ident!("{variant_name}Args");
    let variant_jumps_name = format_ident!("{variant_name}Jumps");

    let bytes = variant.bytes.iter();

    let args_type = {
        let fields = variant.immediates.iter().map(|x| {
            let name = format_ident!("{}", x.0);
            let ty = if x.1.size == 8 {
                quote! { u64 }
            } else if x.1.size == 4 {
                quote! { u32 }
            } else {
                quote! { compile_error!("unsupported type") }
            };

            quote! { pub #name: #ty }
        });

        let default = if variant.immediates.is_empty() {
            quote! { impl Default for #variant_args_name {
                fn default() -> Self{
                    #variant_args_name{}
                }
            }}
        } else {
            quote! {}
        };

        quote! {
            pub struct #variant_args_name{
                #(#fields),*
            }

            #default
        }
    };

    let jumps_type = {
        let fields = variant.jumps.iter().map(|x| {
            let name = format_ident!("{}", x.0);

            quote! { pub #name: ::stucco::stencil::Cont }
        });

        quote! {
            pub struct #variant_jumps_name{
                #(#fields),*
            }
        }
    };

    let produce_imm = {
        let fields = variant.immediates.iter().map(|x| {
            let name = format_ident!("{}", x.0);

            let offset = x.1.offset;
            let data = if x.1.size == 8 {
                quote! { ::stucco::stencil::ImmediateData::U64(__args.#name) }
            } else if x.1.size == 4 {
                quote! { ::stucco::stencil::ImmediateData::U32(__args.#name) }
            } else {
                quote! { compile_error!("unsupported type") }
            };

            quote! {
                {
                    let imm = ::stucco::stencil::Immediate{
                        data: #data,
                        offset: #offset,
                    };
                    __f(imm);
                }

            }
        });

        quote! {
            let __f = f;
            let __args = args;
            #(#fields)*
        }
    };

    let collect_jump = {
        let produce_fields = variant.jumps.iter().map(|x| {
            let name = format_ident!("{}", x.0);

            let size = if x.1.size == 4 {
                quote! { ::stucco::stencil::JumpSize::Default }
            } else {
                quote! { ::stucco::stencil::JumpSize::Long }
            };
            let offset = x.1.offset;
            let addend = x.1.addend as i32;

            let fallthrough = if x.1.is_fallthrough {
                let omit: u16 = (x.1.size + 1).try_into().unwrap();
                quote! { ::stucco::stencil::Fallthrough::Omit(#omit)  }
            } else {
                quote! { ::stucco::stencil::Fallthrough::None }
            };

            quote! {
                let #name = __f(::stucco::stencil::Jump{
                    size: #size,
                    offset: #offset,
                    addend: #addend,
                    fallthrough: #fallthrough,
                });
            }
        });

        let fields = variant.jumps.iter().map(|x| format_ident!("{}", x.0));

        quote! {
            let __f = f;

            #(#produce_fields)*

            #variant_jumps_name {
                #(#fields),*
            }
        }
    };

    quote! {
        pub struct #variant_name(());

        #args_type

        #jumps_type

        unsafe impl ::stucco::stencil::Variant for #variant_name{
            type Module = Module;

            type Args = #variant_args_name;
            type Jumps = #variant_jumps_name;

            const BYTECODE: &[u8] = &[#(#bytes),*];

            fn produce_immediates<F>(args: Self::Args, f: &mut F)
                where F: FnMut(::stucco::stencil::Immediate)
            {
                #produce_imm
            }

            fn collect_jumps<F: >(f: &mut F) -> Self::Jumps
                where F: FnMut(::stucco::stencil::Jump) -> ::stucco::stencil::Cont
            {
                #collect_jump
            }
        }
    }
}
