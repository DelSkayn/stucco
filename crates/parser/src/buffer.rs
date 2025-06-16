use std::{cell::Cell, fmt, marker::PhantomData, ptr::NonNull};

use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream};

pub enum Token {
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
    Group(Group, usize),
    Back(usize),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(ident) => ident.fmt(f),
            Token::Literal(literal) => literal.fmt(f),
            Token::Punct(punct) => punct.fmt(f),
            Token::Group(group, _) => group.fmt(f),
            Token::Back(_) => writeln!(f, "ending"),
        }
    }
}

pub struct TokenBuffer {
    tokens: Box<[Token]>,
}

impl TokenBuffer {
    pub fn from_stream(stream: TokenStream) -> Self {
        let mut buffer = Vec::new();

        // Buffer token to handle the empty case consistantly
        buffer.push(Token::Group(
            Group::new(Delimiter::None, TokenStream::new()),
            0,
        ));

        Self::collect(stream, &mut buffer);

        let len = buffer.len();
        buffer.push(Token::Back(len));
        let Token::Group(_, ref mut group_len) = buffer[0] else {
            unreachable!()
        };
        *group_len = len;

        TokenBuffer {
            tokens: buffer.into_boxed_slice(),
        }
    }

    fn collect(stream: TokenStream, buffer: &mut Vec<Token>) {
        for t in stream {
            match t {
                proc_macro2::TokenTree::Group(group) => {
                    let before_len = buffer.len();
                    let stream = group.stream();

                    buffer.push(Token::Group(group, 0));
                    Self::collect(stream, buffer);

                    let after_len = buffer.len();
                    let group_len = after_len - before_len - 1;
                    let Token::Group(_, ref mut len_ref) = buffer[before_len] else {
                        unreachable!()
                    };
                    *len_ref = group_len;
                    buffer.push(Token::Back(group_len))
                }
                proc_macro2::TokenTree::Ident(ident) => buffer.push(Token::Ident(ident)),
                proc_macro2::TokenTree::Punct(punct) => buffer.push(Token::Punct(punct)),
                proc_macro2::TokenTree::Literal(literal) => buffer.push(Token::Literal(literal)),
            }
        }
    }

    pub fn as_slice<'a>(&'a self) -> TokenSlice<'a> {
        let len = self.tokens.len();
        // Don't include the group
        TokenSlice::from_slice(&self.tokens[1..(len - 1)])
    }
}

#[derive(Clone)]
pub struct TokenSlice<'a> {
    cur: Cell<NonNull<Token>>,
    end: NonNull<Token>,
    _marker: PhantomData<&'a Token>,
}

impl<'a> TokenSlice<'a> {
    fn from_slice(tokens: &'a [Token]) -> Self {
        let range = tokens.as_ptr_range();
        let cur = NonNull::new(range.start as *mut _).unwrap_or_else(NonNull::dangling);
        let end = NonNull::new(range.end as *mut _).unwrap_or_else(NonNull::dangling);
        TokenSlice {
            cur: Cell::new(cur),
            end,
            _marker: PhantomData,
        }
    }

    pub fn cur(&self) -> Option<&'a Token> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { self.cur.get().as_ref() })
        }
    }

    pub fn format_cur(&self) -> FormatToken<'a> {
        FormatToken(self.cur())
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
        if let Some(Token::Ident(ident)) = self.cur() {
            Some(ident);
        }
        None
    }

    pub fn literal(&self) -> Option<&'a Literal> {
        if let Some(Token::Literal(lit)) = self.cur() {
            Some(lit);
        }
        None
    }

    pub fn punct(&self) -> Option<&'a Punct> {
        if let Some(Token::Punct(punct)) = self.cur() {
            return Some(punct);
        }
        None
    }

    pub fn group(&self) -> Option<(&'a Group, Self)> {
        if let Some(Token::Group(group, len)) = self.cur() {
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
        let Some(Token::Group(_, len)) = self.cur() else {
            panic!(
                "Tried to advance past a group, while the current token wasn't the start of a group"
            )
        };
        self.advance_by(*len + 2)
    }

    pub fn is_empty(&self) -> bool {
        self.cur.get() == self.end
    }

    pub fn span(&self) -> Span {
        if let Some(c) = self.cur() {
            match c {
                Token::Ident(ident) => ident.span(),
                Token::Literal(literal) => literal.span(),
                Token::Punct(punct) => punct.span(),
                Token::Group(..) | Token::Back(_) => unreachable!(),
            }
        } else {
            let Token::Back(count) = (unsafe { self.end.as_ref() }) else {
                unreachable!()
            };
            let Token::Group(g, _) = (unsafe { self.cur.get().sub(count + 1).as_ref() }) else {
                unreachable!()
            };
            g.span_close()
        }
    }
}

pub struct FormatToken<'a>(Option<&'a Token>);
impl fmt::Display for FormatToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = self.0 {
            writeln!(f, "{x}")
        } else {
            writeln!(f, "eof")
        }
    }
}
