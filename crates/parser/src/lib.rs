use ast::{Ast, AstSpanned, Node, NodeId, NodeListId, Span, Spanned};
use proc_macro2::{Delimiter, TokenStream, extra::DelimSpan};
use std::{
    fmt,
    ops::{Deref, DerefMut},
};
use syn::{
    Result, Token,
    parse::{
        Parse as SynParse, ParseBuffer, ParseStream, Parser as _, Peek, discouraged::AnyDelimiter,
    },
};

pub mod error;
mod expr;
mod kw;
mod prime;
mod stencil;
#[cfg(test)]
mod test;
mod ty;
mod variant;

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>>;
}

pub trait ParseFunc: Sized {
    type Result;

    fn parse(self, parser: &mut Parser) -> Result<Self::Result>;
}

impl<T, F: FnOnce(&mut Parser) -> Result<T>> ParseFunc for F {
    type Result = T;

    fn parse(self, parser: &mut Parser) -> Result<T> {
        self(parser)
    }
}

pub struct Parser<'a, 'b> {
    buffer: &'a ParseBuffer<'a>,
    ast: &'b mut Ast,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn parse_stream<P: Parse>(token_stream: TokenStream) -> Result<(NodeId<P>, Ast)> {
        (Self::parse_inner(P::parse)).parse2(token_stream)
    }

    pub fn parse_stream_func<F: ParseFunc>(
        token_stream: TokenStream,
        func: F,
    ) -> Result<(F::Result, Ast)> {
        (Self::parse_inner(func)).parse2(token_stream)
    }

    pub fn parse_str<P: Parse>(str: &str) -> Result<(NodeId<P>, Ast)> {
        (Self::parse_inner(P::parse)).parse_str(str)
    }

    pub fn parse_str_func<F: ParseFunc>(str: &str, func: F) -> Result<(F::Result, Ast)> {
        (Self::parse_inner(func)).parse_str(str)
    }

    fn parse_inner<F: ParseFunc>(f: F) -> impl FnOnce(ParseStream) -> Result<(F::Result, Ast)> {
        move |buffer| {
            let mut ast = Ast::new();
            let mut parser = Parser {
                ast: &mut ast,
                buffer,
            };
            let p = f.parse(&mut parser)?;
            Ok((p, ast))
        }
    }

    pub fn push<T: ast::Node>(&mut self, value: T) -> Result<NodeId<T>> {
        Ok(self.ast.push(value)?)
    }

    pub fn error<T: fmt::Display>(&self, message: T) -> syn::Error {
        self.buffer.error(message)
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    pub fn span(&self) -> Span {
        self.buffer.span().into()
    }

    pub fn parse_syn<P: SynParse>(&self) -> Result<P> {
        P::parse(self.buffer)
    }

    pub fn parse_syn_push<P: SynParse + Node>(&mut self) -> Result<NodeId<P>> {
        let p = P::parse(self.buffer)?;
        Ok(self.push(p)?)
    }

    pub fn parse<P: Parse>(&mut self) -> Result<NodeId<P>> {
        P::parse(self)
    }

    pub fn parse_func<F: ParseFunc>(&mut self, func: F) -> Result<F::Result> {
        func.parse(self)
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
        let (delimiter, span, buffer) = self.buffer.parse_any_delimiter().ok()?;
        let mut parser = Parser {
            buffer: &buffer,
            ast: self.ast,
        };
        Some(f(delimiter, span, &mut parser))
    }

    pub fn parse_delimiter<F, R>(&mut self, delim: Delimiter, f: F) -> Result<R>
    where
        F: FnOnce(&mut Parser) -> Result<R>,
    {
        let (delimiter, _, buffer) = self.buffer.parse_any_delimiter()?;
        if delimiter != delim {
            self.buffer.error(format_args!(
                "invalid delimiter, expected {}",
                Self::delimiter_to_expected(delim)
            ));
        }

        let mut parser = Parser {
            buffer: &buffer,
            ast: self.ast,
        };
        f(&mut parser)
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
        P: Parse + Node,
        D: SynParse,
    {
        let mut head = None;
        let mut current = None;
        while !self.is_empty() {
            let v = P::parse(self)?;
            self.push_list(&mut head, &mut current, v)?;

            if self.is_empty() {
                break;
            }

            self.parse_syn::<D>()?;
        }
        Ok(head)
    }

    pub fn peek<T: Peek>(&self, token: T) -> bool {
        self.buffer.peek(token)
    }

    pub fn eat<T: Peek + SynParse>(&self, token: T) -> bool {
        if self.peek(token) {
            self.parse_syn::<T>().unwrap();
            true
        } else {
            false
        }
    }

    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        self.buffer.peek2(token)
    }

    pub fn peek3<T: Peek>(&self, token: T) -> bool {
        self.buffer.peek3(token)
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
    let span = parser.parse_syn::<Token![mod]>()?.span();
    let sym = parser.parse()?;

    let mut end_span = None;
    let functions = parser.parse_braced(|parser| {
        let mut head = None;
        let mut current = None;
        loop {
            if parser.is_empty() {
                break;
            }

            let func = parser.parse()?;
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
        let func = parser.parse()?;
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
