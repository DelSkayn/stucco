use crate::{Span, Spanned as _, TokenError, token::Lit};
use proc_macro2::{Delimiter, Group, Ident, Punct, TokenStream};
use std::{cell::Cell, fmt, marker::PhantomData, ptr::NonNull};

#[derive(Debug)]
pub enum TokenType {
    Ident(Ident),
    Literal(Lit),
    Punct(Punct),
    Group(Group, usize),
    Back(usize),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Ident(ident) => ident.fmt(f),
            TokenType::Literal(literal) => literal.fmt(f),
            TokenType::Punct(punct) => punct.fmt(f),
            TokenType::Group(group, _) => group.fmt(f),
            TokenType::Back(_) => writeln!(f, "ending"),
        }
    }
}

pub struct TokenBuffer {
    tokens: Box<[TokenType]>,
}

impl TokenBuffer {
    pub fn from_stream(stream: TokenStream) -> Result<Self, TokenError> {
        let mut buffer = Vec::new();

        // Buffer token to handle the empty case consistantly
        buffer.push(TokenType::Group(
            Group::new(Delimiter::None, TokenStream::new()),
            0,
        ));

        Self::collect(stream, &mut buffer)?;

        let len = buffer.len();
        buffer.push(TokenType::Back(len));
        let TokenType::Group(_, ref mut group_len) = buffer[0] else {
            unreachable!()
        };
        *group_len = len;

        Ok(TokenBuffer {
            tokens: buffer.into_boxed_slice(),
        })
    }

    fn collect(stream: TokenStream, buffer: &mut Vec<TokenType>) -> Result<(), TokenError> {
        for t in stream {
            match t {
                proc_macro2::TokenTree::Group(group) => {
                    let before_len = buffer.len();
                    let stream = group.stream();

                    buffer.push(TokenType::Group(group, 0));
                    Self::collect(stream, buffer)?;

                    let after_len = buffer.len();
                    let group_len = after_len - before_len - 1;
                    let TokenType::Group(_, ref mut len_ref) = buffer[before_len] else {
                        unreachable!()
                    };
                    *len_ref = group_len;
                    buffer.push(TokenType::Back(group_len))
                }
                proc_macro2::TokenTree::Ident(ident) => buffer.push(TokenType::Ident(ident)),
                proc_macro2::TokenTree::Punct(punct) => buffer.push(TokenType::Punct(punct)),
                proc_macro2::TokenTree::Literal(literal) => {
                    let literal = crate::token::parse_literal(literal)?;
                    buffer.push(TokenType::Literal(literal))
                }
            }
        }
        Ok(())
    }

    pub fn as_slice<'a>(&'a self) -> TokenSlice<'a> {
        let len = self.tokens.len();
        // Don't include the group
        TokenSlice::from_slice(&self.tokens[1..(len - 1)])
    }
}

#[derive(Clone)]
pub struct TokenSlice<'a> {
    cur: Cell<NonNull<TokenType>>,
    end: NonNull<TokenType>,
    _marker: PhantomData<&'a TokenType>,
}

impl<'a> TokenSlice<'a> {
    fn from_slice(tokens: &'a [TokenType]) -> Self {
        let range = tokens.as_ptr_range();
        let cur = NonNull::new(range.start as *mut _).unwrap_or_else(NonNull::dangling);
        let end = NonNull::new(range.end as *mut _).unwrap_or_else(NonNull::dangling);
        TokenSlice {
            cur: Cell::new(cur),
            end,
            _marker: PhantomData,
        }
    }

    pub fn prev(&self) -> &'a TokenType {
        unsafe { self.cur.get().as_ref() }
    }

    pub fn cur(&self) -> Option<&'a TokenType> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { self.cur.get().as_ref() })
        }
    }

    pub fn format_cur<'b>(&'b self) -> FormatToken<'b> {
        if self.is_empty() {
            FormatToken(None, PhantomData)
        } else {
            FormatToken(Some(self.cur.get()), PhantomData)
        }
    }

    pub fn advance(&self) {
        if self.is_empty() {
            return;
        }
        unsafe {
            self.cur
                .set(NonNull::new_unchecked(self.cur.get().as_ptr().add(1)));
        }
    }

    pub fn advance_by(&self, count: usize) {
        // ensure we don't go past the end
        let count = unsafe { self.end.offset_from_unsigned(self.cur.get()).min(count) };
        unsafe {
            self.cur
                .set(NonNull::new_unchecked(self.cur.get().as_ptr().add(count)));
        }
    }

    pub fn restore(&self, other: Self) {
        assert_eq!(self.end, other.end);
        self.cur.set(other.cur.get())
    }

    pub fn ident(&self) -> Option<&'a Ident> {
        if let Some(TokenType::Ident(ident)) = self.cur() {
            Some(ident)
        } else {
            None
        }
    }

    pub fn literal(&self) -> Option<&'a Lit> {
        if let Some(TokenType::Literal(lit)) = self.cur() {
            Some(lit)
        } else {
            None
        }
    }

    pub fn punct(&self) -> Option<&'a Punct> {
        if let Some(TokenType::Punct(punct)) = self.cur() {
            Some(punct)
        } else {
            None
        }
    }

    pub fn group(&self) -> Option<(&'a Group, Self)> {
        if let Some(TokenType::Group(group, len)) = self.cur() {
            let new_cur = unsafe { self.cur.get().add(1) };
            let group_slice = TokenSlice {
                cur: Cell::new(new_cur),
                end: unsafe { new_cur.add(*len) },
                _marker: PhantomData,
            };
            return Some((group, group_slice));
        }
        None
    }

    pub fn advance_group(&self) {
        let Some(TokenType::Group(_, len)) = self.cur() else {
            panic!(
                "Tried to advance past a group, while the current token wasn't the start of a group"
            )
        };
        self.advance_by(*len + 2)
    }

    pub fn is_empty(&self) -> bool {
        self.cur.get() == self.end
    }

    pub fn prev_span(&self) -> Span {
        match self.prev() {
            TokenType::Ident(ident) => ident.span().into(),
            TokenType::Literal(literal) => literal.span(),
            TokenType::Punct(punct) => punct.span().into(),
            TokenType::Back(count) => {
                let TokenType::Group(g, _) = (unsafe { self.cur.get().sub(*count).as_ref() })
                else {
                    unreachable!()
                };
                g.span_close().into()
            }
            TokenType::Group(g, _) => g.span_open().into(),
        }
    }

    pub fn span(&self) -> Span {
        if let Some(c) = self.cur() {
            match c {
                TokenType::Ident(ident) => ident.span().into(),
                TokenType::Literal(literal) => literal.span(),
                TokenType::Punct(punct) => punct.span().into(),
                TokenType::Back(count) => {
                    let TokenType::Group(g, _) =
                        (unsafe { self.cur.get().sub(count + 1).as_ref() })
                    else {
                        unreachable!()
                    };
                    g.span_close().into()
                }
                TokenType::Group(g, _) => g.span_open().into(),
            }
        } else {
            let TokenType::Back(count) = (unsafe { self.end.as_ref() }) else {
                unreachable!()
            };
            let TokenType::Group(g, _) = (unsafe { self.cur.get().sub(count + 1).as_ref() }) else {
                unreachable!()
            };
            g.span_close().into()
        }
    }
}

pub struct FormatToken<'a>(Option<NonNull<TokenType>>, PhantomData<&'a TokenSlice<'a>>);
impl fmt::Display for FormatToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(x) = self.0 else {
            return write!(f, "eof");
        };

        match unsafe { x.as_ref() } {
            TokenType::Ident(x) => x.fmt(f),
            TokenType::Literal(x) => x.fmt(f),
            TokenType::Punct(x) => x.fmt(f),
            TokenType::Group(x, _) => x.fmt(f),
            TokenType::Back(offset) => unsafe {
                let TokenType::Group(g, _) = x.sub(offset + 1).as_ref() else {
                    unreachable!()
                };
                match g.delimiter() {
                    Delimiter::Parenthesis => write!(f, "("),
                    Delimiter::Brace => write!(f, "}}"),
                    Delimiter::Bracket => write!(f, "]"),
                    Delimiter::None => write!(f, "error"),
                }
            },
        }
    }
}
