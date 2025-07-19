//! Token implementations.
use crate::buffer::TokenSlice;
use proc_macro2::Spacing;
pub use proc_macro2::{Delimiter, Ident, extra::DelimSpan};

mod lit;
pub use lit::{IntType, Lit, LitBool, LitInt, LitIntSuffix, LitStr, parse_literal};

pub trait Token: Peek + Sized {
    const NAME: &'static str;

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self>;
}

impl Peek for Ident {
    fn peek(slice: &TokenSlice) -> bool {
        slice.ident().is_some()
    }
}

impl Token for Ident {
    const NAME: &'static str = "identifier";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        if let Some(ident) = slice.ident() {
            slice.advance();
            return Some(ident.clone());
        } else {
            None
        }
    }
}

/*
impl Token for Lit{
    const NAME = "literal";

    fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
        if let Some(x) = slice.literal() {
            slice.advance();
            Some(x.clone())
        }else{
            None
        }
    }
}
*/

pub trait Peek: Sized {
    //const LENGTH: usize;

    fn peek(slice: &TokenSlice) -> bool;
}

macro_rules! impl_keywords {
    ($($kw:ident => $type:ident),*$(,)?) => {
        $(
            pub struct $type(pub $crate::Span);

            impl $crate::Spanned for $type{
                fn span(&self) -> $crate::Span{
                    self.0.clone()
                }
            }

            impl Token for $type{
                const NAME: &'static str = stringify!($kw);

                fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
                    let ident = slice.ident()?;
                    if ident == stringify!($kw) {
                        slice.advance();
                        Some(Self(ident.span().into()))
                    } else {
                        None
                    }
                }
            }

            impl Peek for $type{
                //const LENGTH: usize = 1;

                fn peek(slice: &TokenSlice) -> bool {
                    let Some(ident) = slice.ident() else {
                        return false
                    };
                    ident == stringify!($kw)
                }
            }
        )*

        #[doc(hidden)]
        #[macro_export]
        macro_rules! T_keyword{
            $(
                ($kw) => {
                    $crate::token::$type
                };
            )*
        }
    };
}

impl_keywords! {
    become => Become,
    break => Break,
    const => Const,
    else => Else,
    fn => Fn,
    if => If,
    loop => Loop,
    let => Let,
    mut => Mut,
    return => Return,
    while => While,
    mod => Mod,
    as => As,
    imm => Imm,
    slot => Slot,
    stencil => Stencil,
    variant => Variant,
}

macro_rules! impl_punct{
    ($([$punct:tt]=> $type:ident),*$(,)?) => {
        $(

        #[derive(Debug)]
        pub struct $type(pub $crate::Span);

        impl $crate::Spanned for $type{
            fn span(&self) -> $crate::Span{
                self.0.clone()
            }
        }

        impl Token for $type{
            const NAME: &'static str = stringify!($punct);

            fn lex<'a>(slice: &TokenSlice<'a>) -> Option<Self> {
                assert!(const{ Self::NAME.len() != 0 });

                let bytes = Self::NAME.as_bytes();

                let mut punct = slice.punct()?;
                if punct.as_char() != bytes[0] as char {
                    return None;
                }

                let check_point = slice.clone();
                slice.advance();

                let span: $crate::Span = punct.span().into();

                for i in 1..bytes.len() {
                    if let Spacing::Alone = punct.spacing() {
                        slice.restore(check_point);
                        return None;
                    }


                    let Some(new_punct) = slice.punct() else{
                        slice.restore(check_point);
                        return None
                    };

                    if new_punct.as_char() != bytes[i] as char {
                        slice.restore(check_point);
                        return None;
                    }

                    slice.advance();
                    punct = new_punct;
                }

                if let Spacing::Joint = punct.spacing() {
                    slice.restore(check_point);
                    return None;
                }


                Some(Self(span.try_join(punct.span().into())))
            }
        }

        impl Peek for $type{
            //const LENGTH: usize = stringify!($punct).len();

            fn peek(slice: &TokenSlice) -> bool {
                assert!(const{ Self::NAME.len() != 0 });

                let bytes = Self::NAME.as_bytes();

                let Some(mut punct) = slice.punct() else {
                    return false
                };
                if punct.as_char() != bytes[0] as char {
                    return false;
                }

                let slice = slice.clone();

                for i in 1..bytes.len() {
                    if punct.spacing() != Spacing::Joint {
                        return false;
                    }
                    slice.advance();
                    let Some(new_punct) = slice.punct() else {
                        return false
                    };
                    if new_punct.as_char() != bytes[i] as char {
                        return false;
                    }
                    punct = new_punct;
                }

                punct.spacing() == Spacing::Alone
            }
        })*

        /// Macro used for token shorthands.
        #[macro_export]
        macro_rules! T{
            $(
                ($punct) => {
                    $crate::token::$type
                };
            )*
            ($t:tt) => {
                $crate::T_keyword!($t)
            };
        }
    };
}

impl_punct! {
    [&] => And,
    [&&] => AndAnd,
    [&=] => AndEq,
    [:] => Colon,
    [,] => Comma,
    [.] => Dot,
    [=] => Eq,
    [==] => EqEq,
    [!] => Exlaim,
    [!=] => ExlaimEq,
    [/] => FSlash,
    [/=] => FSlashEq,
    [^] => Hat,
    [^=] => HatEq,
    [|] => HBar,
    [||] => HBarHBar,
    [|=] => HBarEq,
    [%] => Percent,
    [%=] => PercentEq,
    [+] => Plus,
    [+=] => PlusEq,
    [<] => LChevron,
    [<=] => LChevronEq,
    [<<] => LChevronLChevron,
    [<<=] => LChevronLChevronEq,
    [-] => Minus,
    [-=] => MinusEq,
    [->] => RArrow,
    [=>] => RFatArrow,
    [>] => RChevron,
    [>=] => RChevronEq,
    [>>] => RChevronRChevron,
    [>>=] => RChevronRChevronEq,
    [;] => SemiColon,
    [*] => Star,
    [*=] => StarEq,
}

pub struct Paren;
pub struct Bracket;
pub struct Brace;

impl Peek for Paren {
    fn peek(slice: &TokenSlice) -> bool {
        if let Some((group, _)) = slice.group() {
            group.delimiter() == Delimiter::Parenthesis
        } else {
            false
        }
    }
}

impl Peek for Bracket {
    fn peek(slice: &TokenSlice) -> bool {
        if let Some((group, _)) = slice.group() {
            group.delimiter() == Delimiter::Bracket
        } else {
            false
        }
    }
}

impl Peek for Brace {
    fn peek(slice: &TokenSlice) -> bool {
        if let Some((group, _)) = slice.group() {
            group.delimiter() == Delimiter::Brace
        } else {
            false
        }
    }
}
