use ast::Variant;
use error::{AnnotationKind, Level, Snippet};
use token::{Spanned, T};

use crate::{Parse, ParseResult, Parser};

impl<'src> Parse<'src> for ast::Stencil {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![stencil]>()?.0;
        let sym = parser.parse_push()?;

        if let Some(x) = parser.eat::<T![<]>() {
            return Err(parser.with_error(|p| {
                Level::Error
                    .title("Unexpected token `<`, expected `(`")
                    .snippet(
                        Snippet::source(p.source).annotate(
                            AnnotationKind::Primary
                                .span(x.span())
                                .label("Stencils cannot have templates"),
                        ),
                    )
                    .to_diagnostic()
            }));
        }

        let parameters =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let mut head = None;
        let mut current = None;
        while parser.peek::<T![variant]>() {
            let item = parser.parse_push::<Variant>()?;
            parser.push_list(&mut head, &mut current, item)?;
        }
        let variants = head;

        let body = parser.parse_push()?;
        let body = parser.push(ast::Expr::Block(body))?;
        let span = parser.span_since(span);

        Ok(ast::Stencil {
            sym,
            parameters,
            span,
            variants,
            output,
            body,
        })
    }
}

impl<'src> Parse<'src> for ast::Function {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![fn]>()?.0;
        let sym = parser.parse_push()?;

        let mut templates = None;
        if parser.eat::<T![<]>().is_some() {
            let mut current = None;
            loop {
                if parser.eat::<T![>]>().is_some() {
                    break;
                }

                let v = parser.parse_push::<ast::TypeName>()?;
                parser.push_list(&mut templates, &mut current, v)?;

                if parser.eat::<T![,]>().is_none() {
                    parser.expect::<T![>]>()?;
                    break;
                }
            }
        }

        let parameters =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let body = parser.parse_push()?;
        let body = parser.push(ast::Expr::Block(body))?;
        let span = parser.span_since(span);

        Ok(ast::Function {
            sym,
            templates,
            parameters,
            span,
            output,
            body,
        })
    }
}

impl<'src> Parse<'src> for ast::Parameter {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();
        let sym = parser.parse_push()?;
        parser.expect::<T![:]>()?;
        let ty = parser.parse_push()?;

        Ok(Self { sym, ty, span })
    }
}
