use std::{cmp::Ordering, fmt};

use crate::{Peek, Span, Spanned, Token, TokenError, buffer::TokenSlice};
use proc_macro2::Literal;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LitStr {
    value: String,
    span: Span,
}
impl Spanned for LitStr {
    fn span(&self) -> crate::Span {
        self.span.clone()
    }
}
impl fmt::Display for LitStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}
impl Peek for LitStr {
    fn peek(slice: &TokenSlice) -> bool {
        matches!(slice.literal(), Some(Lit::Str(_)))
    }
}
impl Token for LitStr {
    const NAME: &'static str = "string";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        let Some(Lit::Str(x)) = slice.literal() else {
            return None;
        };
        slice.advance();
        Some(x.clone())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LitBool {
    value: bool,
    span: Span,
}
impl Spanned for LitBool {
    fn span(&self) -> crate::Span {
        self.span.clone()
    }
}
impl fmt::Display for LitBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.value {
            f.write_str("true")
        } else {
            f.write_str("false")
        }
    }
}
impl Peek for LitBool {
    fn peek(slice: &TokenSlice) -> bool {
        matches!(slice.literal(), Some(Lit::Bool(_)))
    }
}
impl Token for LitBool {
    const NAME: &'static str = "boolean";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        let Some(Lit::Bool(x)) = slice.literal() else {
            return None;
        };
        slice.advance();
        Some(x.clone())
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum LitIntSuffix {
    Usize,
    Isize,
    I64,
    U64,
    None,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum IntType {
    Signed(i64),
    Unsigned(u64),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LitInt {
    value: IntType,
    suffix: LitIntSuffix,
    span: Span,
}
impl LitInt {
    pub fn suffix(&self) -> LitIntSuffix {
        self.suffix
    }

    pub fn value(&self) -> IntType {
        self.value
    }
}
impl Spanned for LitInt {
    fn span(&self) -> crate::Span {
        self.span.clone()
    }
}
impl fmt::Display for LitInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value {
            IntType::Signed(x) => x.fmt(f),
            IntType::Unsigned(x) => x.fmt(f),
        }
    }
}
impl Peek for LitInt {
    fn peek(slice: &TokenSlice) -> bool {
        matches!(slice.literal(), Some(Lit::Int(_)))
    }
}
impl Token for LitInt {
    const NAME: &'static str = "integer";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        let Some(Lit::Int(x)) = slice.literal() else {
            return None;
        };
        slice.advance();
        Some(x.clone())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lit {
    Str(LitStr),
    Int(LitInt),
    Bool(LitBool),
}
impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Str(lit_str) => lit_str.fmt(f),
            Lit::Int(lit_int) => lit_int.fmt(f),
            Lit::Bool(lit_bool) => lit_bool.fmt(f),
        }
    }
}
impl Spanned for Lit {
    fn span(&self) -> Span {
        match self {
            Lit::Str(lit_str) => lit_str.span(),
            Lit::Int(lit_int) => lit_int.span(),
            Lit::Bool(lit_bool) => lit_bool.span(),
        }
    }
}
impl Peek for Lit {
    fn peek(slice: &TokenSlice) -> bool {
        slice.literal().is_none()
    }
}
impl Token for Lit {
    const NAME: &'static str = "literal";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        let res = slice.literal()?.clone();
        slice.advance();
        Some(res)
    }
}

fn b(bytes: &[u8], idx: usize) -> u8 {
    bytes.get(idx).copied().unwrap_or(b' ')
}

pub fn parse_literal(lit: Literal) -> Result<Lit, TokenError> {
    let repr = lit.to_string();
    match b(repr.as_bytes(), 0) {
        b'0'..=b'9' | b'-' => {
            if let Some(l) = parse_lit_int(&lit, repr) {
                return Ok(Lit::Int(l));
            }
        }
        b't' => {
            if repr == "true" {
                return Ok(Lit::Bool(LitBool {
                    value: true,
                    span: lit.span().into(),
                }));
            }
        }
        b'f' => {
            if repr == "false" {
                return Ok(Lit::Bool(LitBool {
                    value: false,
                    span: lit.span().into(),
                }));
            }
        }
        _ => {}
    }

    return Err(TokenError {
        span: lit.span().into(),
    });
}

fn parse_lit_int(lit: &Literal, source: String) -> Option<LitInt> {
    let mut bytes = source.as_bytes();

    let mut neg = false;
    if b(bytes, 0) == b'-' {
        bytes = &bytes[1..];
        neg = true;
    }

    let radix = match (b(bytes, 0), b(bytes, 1)) {
        (b'0', b'x') => {
            bytes = &bytes[2..];
            16u8
        }
        (b'0', b'o') => {
            bytes = &bytes[2..];
            8u8
        }
        (b'0', b'b') => {
            bytes = &bytes[2..];
            2u8
        }
        _ => 10,
    };

    let mut value = 0u64;
    let mut had_digits = false;
    loop {
        let byte = b(bytes, 0);
        let digit = match byte {
            b'0'..=b'9' => byte - b'0',
            b'a'..=b'f' if radix > 10 => byte - b'a' + 10,
            b'A'..=b'F' if radix > 10 => byte - b'A' + 10,
            b'_' => {
                bytes = &bytes[1..];
                continue;
            }
            _ => break,
        };

        if digit >= radix {
            return None;
        }

        value = value.checked_mul(radix as u64)?;
        value = value.checked_add(digit as u64)?;
        had_digits = true;
        bytes = &bytes[1..];
    }

    if !had_digits {
        return None;
    }

    let suffix = match bytes {
        b"usize" => LitIntSuffix::Usize,
        b"isize" => LitIntSuffix::Isize,
        b"u64" => LitIntSuffix::U64,
        b"i64" => LitIntSuffix::I64,
        b"" => LitIntSuffix::None,
        _ => return None,
    };

    let value = if neg {
        let v = match value.cmp(&(i64::MAX as u64 + 1)) {
            Ordering::Less => -(value as i64),
            Ordering::Equal => i64::MIN,
            Ordering::Greater => return None,
        };
        IntType::Signed(v)
    } else {
        IntType::Unsigned(value)
    };

    Some(LitInt {
        value,
        suffix,
        span: lit.span().into(),
    })
}
