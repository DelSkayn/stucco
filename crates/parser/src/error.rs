use std::fmt;

use ast::PushNodeError;
pub use imp::render;
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, TokenStream, TokenTree};
use token::{Span, Spanned, TokenError};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl Error {
    pub fn new<E>(span: Span, e: E) -> Self
    where
        E: fmt::Display,
    {
        Error {
            span,
            message: e.to_string(),
        }
    }
}

impl From<PushNodeError> for Error {
    fn from(_: PushNodeError) -> Self {
        Error {
            span: Span::call_site(),
            message: "Too many AST nodes, source file too long".to_string(),
        }
    }
}

impl From<TokenError> for Error {
    fn from(e: TokenError) -> Self {
        Error {
            message: "Invalid token".to_string(),
            span: e.span,
        }
    }
}

impl Error {
    pub fn into_token_stream(self) -> TokenStream {
        let span = self.span.into();
        // ::core::compile_error!($message)
        TokenStream::from_iter([
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Joint);
                punct.set_span(span);
                punct
            }),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Alone);
                punct.set_span(span);
                punct
            }),
            TokenTree::Ident(Ident::new("core", span)),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Joint);
                punct.set_span(span);
                punct
            }),
            TokenTree::Punct({
                let mut punct = Punct::new(':', Spacing::Alone);
                punct.set_span(span);
                punct
            }),
            TokenTree::Ident(Ident::new("compile_error", span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter([TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(span);
                        string
                    })])
                });
                group.set_span(span);
                group
            }),
        ])
    }
}

#[cfg(not(feature = "span-locations"))]
mod imp {
    use super::Error;
    pub fn render(source: &str, err: Error) -> String {
        let _ = source;
        format!("{}", err)
    }
}

#[cfg(feature = "span-locations")]
mod imp {
    use super::Error;
    pub fn render(source: &str, err: Error) -> String {
        common::error::render_block(source, err.span.byte_range(), &err.message)
    }
}
