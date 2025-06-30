mod buffer;
mod span;
pub mod token;

use std::{error::Error, fmt};

pub use buffer::{TokenBuffer, TokenSlice, TokenType};
pub use span::{Span, Spanned};
pub use token::{Peek, Token};

#[derive(Debug)]
pub struct TokenError {
    pub span: Span,
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Invalid token")
    }
}
impl Error for TokenError {}
