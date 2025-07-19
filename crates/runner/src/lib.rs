use ast::{Ast, NodeId};
use codegen::{StencilSet, StencilVariant, Target};
use compiler::infer::TypeError;
use quote::{format_ident, quote};
use std::path::Path;

use parser::{Parser, parse_external_module, parse_wrapped_module};
use proc_macro2::{Literal, TokenStream};
use token::{
    T,
    token::{Ident, LitStr},
};

pub fn string_to_compile_error(s: String) -> TokenStream {
    quote! { ::core::compile_error!($s) }
}

pub fn file(items: TokenStream) -> TokenStream {
    println!("{}", std::env::current_dir().unwrap().display());

    let (name, path_lit) = match parser::Parser::parse_stream_func(items, |parser: &mut Parser| {
        let name = parser.expect::<Ident>()?;
        parser.expect::<T![=>]>()?;
        let path = parser.expect::<LitStr>()?;
        Ok((name, path))
    }) {
        Ok((x, _)) => x,
        Err(e) => return e.into_token_stream(),
    };

    let env = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let path = Path::new(&env).join(path_lit.value);

    let file = match std::fs::read_to_string(path) {
        Ok(x) => x,
        Err(e) => {
            return parser::Error::new(
                path_lit.span,
                format_args!("Failed to read template file: {e}"),
            )
            .into_token_stream();
        }
    };

    let (root, ast) = match Parser::parse_str_func(&file, move |parser: &mut Parser| {
        let module = parse_external_module(&mut (*parser))?;

        let span = name.span();
        let sym = parser.push(name)?;
        let sym = parser.push(ast::Symbol {
            span: span.into(),
            name: sym,
        })?;
        parser[module].sym = Some(sym);
        Ok(module)
    }) {
        Ok(x) => x,
        Err(e) => return e.into_token_stream(),
    };

    match compile(&file, root, ast) {
        Ok(x) => x,
        Err(e) => string_to_compile_error(e),
    }
}

pub fn module(input: TokenStream) -> TokenStream {
    let source = input.to_string();
    let (root, ast) = match Parser::parse_stream_func(input, parse_wrapped_module) {
        Ok(x) => x,
        Err(e) => return e.into_token_stream(),
    };

    match compile(&source, root, ast) {
        Ok(x) => x,
        Err(e) => string_to_compile_error(e),
    }
}

pub fn compile(source: &str, root: NodeId<ast::Module>, ast: Ast) -> Result<TokenStream, String> {
    let symbols = match compiler::resolve::resolve(root, &ast) {
        Ok(s) => s,
        Err(e) => {
            return Err(format!("RESOLVE ERROR: {}", e.render(source)));
        }
    };

    let mut types = compiler::infer::Types::new();
    match types.infer(&ast, &symbols, root) {
        Ok(()) => {}
        Err(e) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    return Err(format!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(a),
                        types.type_to_string(b)
                    ));
                }
                _ => {}
            }
            return Err(format!("TYPE ERROR: {:?}", e));
        }
    }

    let code_gen = codegen::CodeGen::new(ast, symbols, types, Default::default());
    let stencils = code_gen.generate_stencil_set(root, Target::X86_64);

    Ok(generate(root, &code_gen.ast, &stencils))
}

pub fn generate(root: NodeId<ast::Module>, ast: &Ast, stencils: &StencilSet) -> TokenStream {
    let name = format_ident!("{}", &ast[root].sym.unwrap().index(ast).name.index(ast));

    let stencils = generate_stencils(stencils);

    quote! {
        mod #name {
            #(#stencils)*
        }
    }
}

pub fn generate_stencils(stencils: &StencilSet) -> Vec<TokenStream> {
    let mut res = Vec::new();
    for (name, stencil) in stencils.stencils.iter() {
        let variants = stencil
            .variants
            .iter()
            .map(generate_variants)
            .collect::<Vec<_>>();

        let name = format_ident!("{}", name);
        res.push(quote! {
            struct #name;
            unsafe impl ::stucco::Stencil for #name{
                fn data() -> &'static ::stucco::StencilData{
                    static DATA: ::stucco::StencilData = ::stucco::StencilData{
                        variants: &[#(#variants),*]
                    };

                    &DATA
                }
            }
        })
    }
    res
}

pub fn generate_variants(variant: &StencilVariant) -> TokenStream {
    let bytes = variant
        .bytes
        .iter()
        .map(|x| Literal::u8_suffixed(*x))
        .collect::<Vec<_>>();
    let jumps = variant
        .jumps
        .iter()
        .map(|(k, v)| {
            let size = v.size;
            let offset = v.offset;
            let addend = v.addend;
            let is_fallthrough = v.is_fallthrough;

            quote! {
                (#k,::stucco::Jump{
                    size: #size,
                    offset: #offset,
                    addend: #addend,
                    is_fallthrough: #is_fallthrough,
                })
            }
        })
        .collect::<Vec<_>>();
    let immediates = variant
        .immediates
        .iter()
        .map(|(k, v)| {
            let size = v.size;
            let offset = v.offset;

            quote! {
                (#k,::stucco::Immediate{
                    size: #size,
                    offset: #offset,
                })
            }
        })
        .collect::<Vec<_>>();
    quote! {
        ::stucco::Variant{
            bytes: &[#(#bytes),*],
            jumps: ::stucco::Jumps{
                jumps: &[#(#jumps),*]
            },
            immediates: ::stucco::Immediates{
                imms: &[#(#immediates),*]
            }
        }
    }
}
