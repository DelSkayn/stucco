use ast::{Ast, AstSpanned, Node, NodeId, NodeListId, Span, Spanned};
use buffer::{TokenBuffer, TokenSlice};
use error::Error;
use proc_macro2::{Delimiter, Ident, TokenStream, extra::DelimSpan};
use std::{
    fmt,
    ops::{Deref, DerefMut},
    result::Result as StdResult,
    str::FromStr,
};

pub(crate) mod token;
use token::{Peek, T, Token};

mod buffer;
pub mod error;
mod expr;
mod prime;
mod stencil;
#[cfg(test)]
mod test;
mod ty;
mod variant;

pub type Result<T> = StdResult<T, error::Error>;

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait ParsePush: Sized {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>>;
}

impl<P: Parse + ast::Node> ParsePush for P {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let res = P::parse(parser)?;
        parser.push(res)
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if let Some(ident) = parser.slice.ident() {
            return Ok(ident.clone());
        }
        Err(parser.error(format_args!(
            "Unexpected token '{}' expected an identifier",
            parser.slice.format_cur()
        )))
    }
}

impl<T: Token> Parse for T {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if let Some(t) = T::lex(&parser.slice) {
            Ok(t)
        } else {
            Err(parser.error(format_args!(
                "Unexpected token '{}' expected an {}",
                parser.slice.format_cur(),
                T::NAME
            )))
        }
    }
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
    pub fn parse_stream<P: ParsePush>(token_stream: TokenStream) -> Result<(NodeId<P>, Ast)> {
        Self::parse_inner(token_stream, P::parse_push)
    }

    pub fn parse_stream_func<F: ParseFunc>(
        token_stream: TokenStream,
        func: F,
    ) -> Result<(F::Result, Ast)> {
        Self::parse_inner(token_stream, |p| func.parse_func(p))
    }

    pub fn parse_str<P: ParsePush>(str: &str) -> Result<(NodeId<P>, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Self::parse_inner(token_stream, P::parse_push)
    }

    pub fn parse_str_func<F: ParseFunc>(str: &str, func: F) -> Result<(F::Result, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Self::parse_inner(token_stream, |p| (func).parse_func(p))
    }

    fn parse_inner<R, F>(stream: TokenStream, f: F) -> Result<(R, Ast)>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        let tb = TokenBuffer::from_stream(stream);
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
        return Error {
            span: self.slice.span().into(),
            message: message.to_string(),
        };
    }

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    pub fn span(&self) -> Span {
        self.slice.span().into()
    }

    pub fn parse_push<P: ParsePush>(&mut self) -> Result<NodeId<P>> {
        P::parse_push(self)
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
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        let Some((group, slice)) = self.slice.group() else {
            let delim = match delim {
                Delimiter::Parenthesis => "(",
                Delimiter::Brace => "{",
                Delimiter::Bracket => "[",
                Delimiter::None => panic!("don't parse on none delimiter"),
            };
            return Err(self.error(format_args!(
                "Unexpected token '{}' expected {}",
                self.slice.format_cur(),
                delim
            )));
        };
        if group.delimiter() != delim {
            return Err(self.error(format_args!(
                "invalid delimiter, expected {}",
                Self::delimiter_to_expected(delim)
            )));
        }

        let mut parser = Parser {
            slice: slice,
            ast: self.ast,
        };
        let res = f(&mut parser);
        if res.is_ok() {
            self.slice.advance_group();
        }
        res
    }

    pub fn parse_braced<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Brace, f)
    }

    pub fn parse_bracketed<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Bracket, f)
    }

    pub fn parse_parenthesized<F, R>(&mut self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        self.parse_delimiter(Delimiter::Parenthesis, f)
    }

    pub fn parse_terminated<P, D>(&mut self) -> Result<Option<NodeListId<P>>>
    where
        P: ParsePush + Node,
        D: Parse,
    {
        let mut head = None;
        let mut current = None;
        while !self.is_empty() {
            let v = P::parse_push(self)?;
            self.push_list(&mut head, &mut current, v)?;

            if self.is_empty() {
                break;
            }

            self.parse::<D>()?;
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
        let Some(buffer::Token::Group(g, _)) = self.slice.cur() else {
            return false;
        };
        g.delimiter() == delim
    }

    pub fn eat<T: Token>(&self) -> Option<T> {
        T::lex(&self.slice)
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
    let span = parser.parse::<T![mod]>()?.0;
    let sym = parser.parse_push()?;

    let mut end_span = None;
    let functions = parser.parse_braced(|parser| {
        let mut head = None;
        let mut current = None;
        loop {
            if parser.is_empty() {
                break;
            }

            let func = parser.parse_push()?;
            parser.push_list(&mut head, &mut current, func)?;
        }
        end_span = Some(parser.span());
        Ok(head)
    })?;

    let end_span = end_span.unwrap();
    let span = span.try_join(end_span);

    Ok(parser.push(ast::Module {
        sym: Some(sym),
        stencils: functions,
        span,
    })?)
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

    Ok(parser.push(ast::Module {
        sym: None,
        stencils: head,
        span,
    })?)
}
