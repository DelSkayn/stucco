use ast::{Ast, AstSpanned, Node, NodeId, NodeListId};
use error::{AnnotationKind, Level, Snippet};
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

mod expr;
mod prime;
mod stencil;
mod stmt;
mod ty;
mod util;
mod variant;

pub use error::Diagnostic;

pub type ParseResult<'a, T> = StdResult<T, Diagnostic<'a>>;

pub trait Parse<'src>: Sized {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self>;
}

pub trait ParseFunc<'src>: Sized {
    type Result;

    fn parse_func(self, parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self::Result>;
}

impl<'src, T, F: FnOnce(&mut Parser<'src, '_, '_>) -> ParseResult<'src, T>> ParseFunc<'src> for F {
    type Result = T;

    fn parse_func<'a>(self, parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, T> {
        self(parser)
    }
}

pub struct Parser<'src, 'slice, 'ast> {
    source: Option<&'src str>,
    slice: TokenSlice<'slice>,
    ast: &'ast mut Ast,
}

impl<'slice, 'ast> Parser<'static, 'slice, 'ast> {
    pub fn parse_stream<P: Parse<'static> + ast::Node>(
        token_stream: TokenStream,
    ) -> ParseResult<'static, (NodeId<P>, Ast)> {
        Self::parse_inner(None, token_stream, |p| p.parse_push::<P>())
    }

    pub fn parse_stream_func<F: ParseFunc<'static>>(
        token_stream: TokenStream,
        func: F,
    ) -> ParseResult<'static, (F::Result, Ast)> {
        Parser::parse_inner(None, token_stream, |p| func.parse_func(p))
    }
}

impl<'src, 'slice, 'ast> Parser<'src, 'slice, 'ast> {
    pub fn parse_str<P: Parse<'src> + ast::Node>(
        str: &'src str,
    ) -> ParseResult<'src, (NodeId<P>, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Parser::parse_inner(Some(str), token_stream, |p| p.parse_push::<P>())
    }

    pub fn parse_str_func<F: ParseFunc<'src>>(
        str: &'src str,
        func: F,
    ) -> ParseResult<'src, (F::Result, Ast)> {
        let token_stream = TokenStream::from_str(str).unwrap();
        Self::parse_inner(Some(str), token_stream, |p| (func).parse_func(p))
    }

    fn parse_inner<R, F>(
        source: Option<&'src str>,
        stream: TokenStream,
        f: F,
    ) -> ParseResult<'src, (R, Ast)>
    where
        F: FnOnce(&mut Parser<'src, '_, '_>) -> ParseResult<'src, R>,
    {
        let tb = TokenBuffer::from_stream(stream).unwrap();
        let slice = tb.as_slice();
        let mut ast = Ast::new();
        let mut parser = Parser {
            source,
            slice,
            ast: &mut ast,
        };
        let res = f(&mut parser)?;
        Ok((res, ast))
    }

    pub fn push<T: ast::Node>(&mut self, value: T) -> ParseResult<'src, NodeId<T>> {
        match self.ast.push(value) {
            Ok(x) => Ok(x),
            Err(_) => Err(self.with_error(|this| {
                Level::Error
                    .title("Source to large, ast contains too many nodes")
                    .snippet(
                        Snippet::source(this.source)
                            .annotate(AnnotationKind::Primary.span(this.last_span())),
                    )
                    .to_diagnostic()
            })),
        }
    }

    pub fn push_set<T: ast::UniqueNode>(&mut self, value: T) -> ParseResult<'src, NodeId<T>> {
        match self.ast.push_set(value) {
            Ok(x) => Ok(x),
            Err(_) => Err(self.with_error(|this| {
                Level::Error
                    .title("Source to large, ast contains too many nodes")
                    .snippet(
                        Snippet::source(this.source)
                            .annotate(AnnotationKind::Primary.span(this.last_span())),
                    )
                    .to_diagnostic()
            })),
        }
    }

    #[cold]
    pub fn with_error<F>(&mut self, f: F) -> Diagnostic<'src>
    where
        F: FnOnce(&mut Self) -> Diagnostic<'src>,
    {
        f(self)
    }

    pub fn error<T: fmt::Display>(&self, message: T) -> Diagnostic<'src> {
        Level::Error
            .title(message.to_string())
            .snippet(
                Snippet::source(self.source)
                    .annotate(AnnotationKind::Primary.span(self.slice.span().into())),
            )
            .to_diagnostic()
    }

    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    /// Returns the span for the next token.
    pub fn span(&self) -> Span {
        self.slice.span().into()
    }

    pub fn parse_push<P: Parse<'src> + ast::Node>(&mut self) -> ParseResult<'src, NodeId<P>> {
        let p = P::parse(self)?;
        self.push(p)
    }

    pub fn parse<P: Parse<'src>>(&mut self) -> ParseResult<'src, P> {
        P::parse(self)
    }

    pub fn parse_func<F: ParseFunc<'src>>(&mut self, func: F) -> ParseResult<'src, F::Result> {
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

    pub fn try_parse_delimited<F, R>(&mut self, f: F) -> Option<ParseResult<'src, R>>
    where
        F: FnOnce(Delimiter, DelimSpan, &mut Parser<'src, '_, '_>) -> ParseResult<'src, R>,
    {
        let Some((group, slice)) = self.slice.group() else {
            return None;
        };
        let mut parser = Parser {
            source: self.source,
            slice,
            ast: &mut *self.ast,
        };
        let res = f(group.delimiter(), group.delim_span(), &mut parser);
        if res.is_ok() {
            self.slice.advance_group();
        }
        Some(res)
    }

    pub fn parse_delimiter<F, R>(&mut self, delim: Delimiter, f: F) -> ParseResult<'src, R>
    where
        F: FnOnce(&mut Parser<'src, '_, '_>, Span) -> ParseResult<'src, R>,
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
            source: self.source,
            slice,
            ast: &mut *self.ast,
        };
        let res = f(&mut parser, group.span().into());
        if res.is_ok() {
            self.slice.advance_group();
        }
        res
    }

    pub fn parse_braced<F, R>(&mut self, f: F) -> ParseResult<'src, R>
    where
        F: FnOnce(&mut Parser<'src, '_, '_>, Span) -> ParseResult<'src, R>,
    {
        self.parse_delimiter(Delimiter::Brace, f)
    }

    pub fn parse_bracketed<F, R>(&mut self, f: F) -> ParseResult<'src, R>
    where
        F: FnOnce(&mut Parser<'src, '_, '_>, Span) -> ParseResult<'src, R>,
    {
        self.parse_delimiter(Delimiter::Bracket, f)
    }

    pub fn parse_parenthesized<F, R>(&mut self, f: F) -> ParseResult<'src, R>
    where
        F: FnOnce(&mut Parser<'src, '_, '_>, Span) -> ParseResult<'src, R>,
    {
        self.parse_delimiter(Delimiter::Parenthesis, f)
    }

    pub fn parse_terminated<P, D>(&mut self) -> ParseResult<'src, Option<NodeListId<P>>>
    where
        P: Parse<'src> + Node,
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

    pub fn expect<T: Token>(&self) -> ParseResult<'src, T> {
        if let Some(t) = T::lex(&self.slice) {
            Ok(t)
        } else {
            Err(self.unexpected(T::NAME))
        }
    }

    pub fn unexpected(&self, expected: &str) -> Diagnostic<'src> {
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

    pub fn push_list<T: Node>(
        &mut self,
        head: &mut Option<NodeListId<T>>,
        current: &mut Option<NodeListId<T>>,
        value: NodeId<T>,
    ) -> ParseResult<'src, ()> {
        if self.ast.push_list(head, current, value).is_err() {
            return Err(Level::Error
                .title("Source to large")
                .snippet(
                    Snippet::source(self.source).annotate(
                        AnnotationKind::Primary
                            .span(self.last_span())
                            .label("Hit the maximum source code size here"),
                    ),
                )
                .to_diagnostic());
        }
        Ok(())
    }
}

impl Deref for Parser<'_, '_, '_> {
    type Target = Ast;

    fn deref(&self) -> &Self::Target {
        self.ast
    }
}

impl DerefMut for Parser<'_, '_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ast
    }
}

/// parses a module in the form of `mod bla { stencil foo() { .. } }`
pub fn parse_wrapped_module<'src>(
    parser: &mut Parser<'src, '_, '_>,
) -> ParseResult<'src, NodeId<ast::Module>> {
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
pub fn parse_external_module<'src>(
    parser: &mut Parser<'src, '_, '_>,
) -> ParseResult<'src, NodeId<ast::Module>> {
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
