use proc_macro2::TokenStream;

use crate::{Diagnostic, render::string::CharBuffer};

mod string;
mod tokens;

impl Diagnostic<'_> {
    pub fn render_tokens(&self) -> TokenStream {
        tokens::render(self)
    }

    pub fn render_string(&self) -> String {
        string::render_string(self)
    }

    pub fn render_char_buffer(&self) -> CharBuffer {
        string::render_char_buffer(self)
    }
}
