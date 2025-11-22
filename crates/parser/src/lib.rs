use ast::{Ast, AstSpanned, Node, NodeId, NodeListId};
use proc_macro2::TokenStream;
use std::{
    fmt,
    ops::{Deref, DerefMut},
    result::Result as StdResult,
    str::FromStr,
};
use token::{
    Peek, Span, T, Token, TokenBuffer, TokenSlice, TokenType,
    token::{DelimSpan, Delimiter},
};

pub mod error;
mod expr;
mod prime;
mod stencil;
mod stmt;
mod ty;
mod util;
mod variant;

pub use error::Error;

pub type Result<T> = StdResult<T, error::Error>;

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait ParseFunc: Sized {
    type Result;

    fn parse_func(self, parser: &mut Parser) -> Result<Self::Result>;
}

impl<T, F: FnOnce(&mut Parser) -> Result<T>> ParseFunc for F {
    type Result = T;

    fn parse_func(self, parser: &mut Parser) -> Result<T> {
        self(parser)
    }
}

pub struct Parser<'a, 'b> {
    slice: TokenSlice<'a>,
    ast: &'b mut Ast,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn parse_stream<P: Parse + ast::Node>(
        token_stream: TokenStream,
    ) -> Result<(NodeId<P>, Ast)> {
        Self::parse_inner(token_stream, |p| p.parse_push::<P>())
    }

    pub fn parse_stream_func<F: ParseFunc>(
        token_stream: TokenStream,
        func: F,
    ) -> Result<(F::Result, Ast)> {
        Self::parse_inner(token_stream, |p| func.parse_func(p))
    }

    pub fn parse_str<P: Parse + ast::Node>(str: &str) -> Result<(NodeId<P>, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Self::parse_inner(token_stream, |p| p.parse_push::<P>())
    }

    pub fn parse_str_func<F: ParseFunc>(str: &str, func: F) -> Result<(F::Result, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Self::parse_inner(token_stream, |p| (func).parse_func(p))
    }

    fn parse_inner<R, F>(stream: TokenStream, f: F) -> Result<(R, Ast)>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        let tb = TokenBuffer::from_stream(stream)?;
        let slice = tb.as_slice();
        let mut ast = Ast::new();
        let mut parser = Parser {
            slice,
            ast: &mut ast,
        };
        let res = f(&mut parser)?;
        Ok((res, ast))
    }

    pub fn push<T: ast::Node>(&mut self, value: T) -> Result<NodeId<T>> {
        Ok(self.ast.push(value)?)
    }

    pub fn error<T: fmt::Display>(&self, message: T) -> Error {
        println!("{}", std::backtrace::Backtrace::force_capture());
        return Error {
            span: self.slice.span().into(),
            message: message.to_string(),
        };
    }

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    /// Returns the span for the next token.
    pub fn span(&self) -> Span {
        self.slice.span().into()
    }

    pub fn parse_push<P: Parse + ast::Node>(&mut self) -> Result<NodeId<P>> {
        let p = P::parse(self)?;
        self.push(p)
    }

    pub fn parse<P: Parse>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub fn parse_func<F: ParseFunc>(&mut self, func: F) -> Result<F::Result> {
        func.parse_func(self)
    }

    pub fn delimiter_to_expected(delim: Delimiter) -> &'static str {
        match delim {
            Delimiter::Parenthesis => "parenthesis, `()`",
            Delimiter::Brace => "Braces, `{}`",
            Delimiter::Bracket => "Brackets, `[]`",
            Delimiter::None => "Empty group",
        }
    }

    pub fn try_parse_delimited<F, R>(&mut self, f: F) -> Option<Result<R>>
    where
        F: FnOnce(Delimiter, DelimSpan, &mut Parser) -> Result<R>,
    {
        let Some((group, slice)) = self.slice.group() else {
            return None;
        };
        let mut parser = Parser {
            slice,
            ast: self.ast,
        };
        let res = f(group.delimiter(), group.delim_span(), &mut parser);
        if res.is_ok() {
            self.slice.advance_group();
        }
        Some(res)
    }

    pub fn parse_delimiter<F, R>(&mut self, delim: Delimiter, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser, Span) -> Result<R>,
    {
        let Some((group, slice)) = self.slice.group() else {
            println!("{}", std::backtrace::Backtrace::force_capture());
            let delim = match delim {
                Delimiter::Parenthesis => "(",
                Delimiter::Brace => "{",
                Delimiter::Bracket => "[",
                Delimiter::None => panic!("don't parse on none delimiter"),
            };
            return Err(self.error(format_args!(
                "Unexpected token '{}' expected '{}'",
                self.slice.format_cur(),
                delim
            )));
        };
        if group.delimiter() != delim {
            return Err(self.error(format_args!(
                "invalid delimiter, expected '{}'",
                Self::delimiter_to_expected(delim)
            )));
        }

        let mut parser = Parser {
            slice,
            ast: self.ast,
        };
        let res = f(&mut parser, group.span().into());
        if res.is_ok() {
            self.slice.advance_group();
        }
        res
    }

    pub fn parse_braced<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser, Span) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Brace, f)
    }

    pub fn parse_bracketed<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser, Span) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Bracket, f)
    }

    pub fn parse_parenthesized<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser, Span) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Parenthesis, f)
    }

    pub fn parse_terminated<P, D>(&mut self) -> Result<Option<NodeListId<P>>>
    where
        P: Parse + Node,
        D: Token,
    {
        let mut head = None;
        let mut current = None;
        while !self.is_empty() {
            let v = self.parse_push()?;
            self.push_list(&mut head, &mut current, v)?;

            if self.is_empty() {
                break;
            }

            self.expect::<D>()?;
        }
        Ok(head)
    }

    pub fn peek<T: Peek>(&self) -> bool {
        T::peek(&self.slice)
    }

    pub fn peek2<T: Peek>(&self) -> bool {
        let slice = self.slice.clone();
        slice.advance();
        T::peek(&slice)
    }

    pub fn peek_group(&self, delim: Delimiter) -> bool {
        let Some(TokenType::Group(g, _)) = self.slice.cur() else {
            return false;
        };
        g.delimiter() == delim
    }

    pub fn eat<T: Token>(&self) -> Option<T> {
        T::lex(&self.slice)
    }

    pub fn expect<T: Token>(&self) -> Result<T> {
        if let Some(t) = T::lex(&self.slice) {
            Ok(t)
        } else {
            Err(self.unexpected(T::NAME))
        }
    }

    pub fn unexpected(&self, expected: &str) -> Error {
        self.error(format_args!(
            "Unexpected token '{}' expected '{}'",
            self.slice.format_cur(),
            expected
        ))
    }

    pub fn last_span(&self) -> Span {
        self.slice.prev_span()
    }

    pub fn span_since(&self, span: Span) -> Span {
        span.try_join(self.last_span())
    }
}

impl Deref for Parser<'_, '_> {
    type Target = Ast;

    fn deref(&self) -> &Self::Target {
        self.ast
    }
}

impl DerefMut for Parser<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ast
    }
}

/// parses a module in the form of `mod bla { stencil foo() { .. } }`
pub fn parse_wrapped_module(parser: &mut Parser) -> Result<NodeId<ast::Module>> {
    let span = parser.expect::<T![mod]>()?.0;
    let sym = parser.parse_push()?;

    let functions = parser.parse_braced(|parser, _| {
        let mut head = None;
        let mut current = None;
        loop {
            if parser.is_empty() {
                break;
            }

            let func = parser.parse_push()?;
            parser.push_list(&mut head, &mut current, func)?;
        }
        Ok(head)
    })?;

    let span = parser.span_since(span);

    parser.push(ast::Module {
        sym: Some(sym),
        stmts: functions,
        span,
    })
}

/// parses a module in the form of `stencil foo() { .. }` i.e. imported from an external file.
pub fn parse_external_module(parser: &mut Parser) -> Result<NodeId<ast::Module>> {
    let start_span = parser.span();
    let mut end_span = None;

    let mut head = None;
    let mut current = None;
    loop {
        if parser.is_empty() {
            break;
        }
        let func = parser.parse_push()?;
        end_span = Some(func.ast_span(parser));
        parser.push_list(&mut head, &mut current, func)?;
    }

    let span = if let Some(end) = end_span {
        start_span.try_join(end)
    } else {
        start_span
    };

    parser.push(ast::Module {
        sym: None,
        stmts: head,
        span,
    })
}
