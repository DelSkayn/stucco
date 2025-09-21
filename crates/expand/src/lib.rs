use ast::{Ast, NodeId};
use codegen::{StencilSet, StencilVariant, Target};
use compile::CompilationError;
use compiler::infer::TypeError;
use quote::{format_ident, quote};
use std::path::Path;

use parser::{Parser, parse_external_module, parse_wrapped_module};
use proc_macro2::{Literal, TokenStream};
use token::{
    T,
    token::{Ident, LitStr},
};

pub mod compile;
pub mod expand;

pub fn string_to_compile_error(s: String) -> TokenStream {
    quote! { ::core::compile_error!($s) }
}

pub fn file(items: TokenStream) -> TokenStream {
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

    let file = match std::fs::read_to_string(&path) {
        Ok(x) => x,
        Err(e) => {
            return parser::Error::new(
                path_lit.span,
                format_args!("Failed to read template file at '{}': {e}", path.display()),
            )
            .into_token_stream();
        }
    };
    let tokens = match file.parse::<TokenStream>() {
        Ok(x) => x,
        Err(e) => return string_to_compile_error(e.to_string()),
    };

    let compile = match compile::compile(tokens, name) {
        Ok(x) => x,
        Err(CompilationError::Parse(x)) => {
            return x.into_token_stream();
        }
        Err(CompilationError::Resolve(x)) => {
            return string_to_compile_error(format!(
                "Failed to resolve symbols: {}",
                x.render(&file)
            ));
        }
        Err(CompilationError::Types(types, e)) => {
            match e {
                TypeError::Mismatch(a, b) => {
                    return string_to_compile_error(format!(
                        "Unexpected type {}, expected {}",
                        types.type_to_string(a),
                        types.type_to_string(b)
                    ));
                }
                _ => {}
            }
            return string_to_compile_error(format!("TYPE ERROR: {:?}", e));
        }
    };

    expand::expand(compile)
}
