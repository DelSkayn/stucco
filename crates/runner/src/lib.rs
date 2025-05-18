use std::path::Path;

use parser::{parse_external_module, parse_wrapped_module, Parser};
use proc_macro2::TokenStream;
use syn::{Error, LitStr, Result};

pub fn file(items: TokenStream) -> TokenStream {
    println!("{}", std::env::current_dir().unwrap().display());

    let literal = match syn::parse2::<LitStr>(items) {
        Ok(x) => x,
        Err(e) => return e.to_compile_error(),
    };

    let env = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let path = Path::new(&env).join(literal.value());

    let file = match std::fs::read_to_string(path) {
        Ok(x) => x,
        Err(e) => {
            return Error::new_spanned(literal, format!("Failed to read template file: {e}"))
                .to_compile_error();
        }
    };

    let tokens = match syn::parse_str::<TokenStream>(&file) {
        Ok(x) => x,
        Err(e) => return e.to_compile_error(),
    };

    match compile(tokens, true) {
        Ok(x) => x,
        Err(e) => e.to_compile_error(),
    }
}

pub fn module(input: TokenStream) -> TokenStream {
    match compile(input, false) {
        Ok(x) => x,
        Err(e) => e.to_compile_error(),
    }
}

pub fn compile(definition: TokenStream, external: bool) -> Result<TokenStream> {
    let (root, ast) = if external {
        Parser::parse_stream_func(definition, parse_external_module)?
    } else {
        Parser::parse_stream_func(definition, parse_wrapped_module)?
    };

    Ok(TokenStream::new())
}
