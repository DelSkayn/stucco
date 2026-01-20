use ast::AstSpanned;
use token::{Span, T};

use crate::{Parse, ParseResult, Parser};

impl<'src> Parse<'src> for ast::Stmt {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        if parser.peek::<T![stencil]>() {
            let s = parser.parse_push()?;
            return Ok(ast::Stmt::Stencil(s));
        }

        if parser.peek::<T![fn]>() {
            let s = parser.parse_push()?;
            return Ok(ast::Stmt::Function(s));
        }

        if parser.peek::<T![struct]>() {
            let s = parser.parse_push()?;
            return Ok(ast::Stmt::Struct(s));
        }

        if parser.peek::<T![impl]>() {
            let s = parser.parse_push()?;
            return Ok(ast::Stmt::Impl(s));
        }

        if parser.peek::<T![mod]>() {
            let s = parser.parse_push()?;
            parser.expect::<T![;]>()?;
            return Ok(ast::Stmt::Definition(s));
        }

        if parser.peek::<T![pub]>() {
            if parser.peek2::<T![struct]>() {
                let s = parser.parse_push()?;
                return Ok(ast::Stmt::Struct(s));
            }

            return Err(parser.unexpected("`struct`"));
        }

        Err(parser.unexpected("`stencil`, `fn`, `struct`, or `pub`"))
    }
}

impl<'src> Parse<'src> for ast::ModuleDefinition {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![mod]>()?.0;
        let sym = parser.parse_push()?;

        let (args, args_span) = parser.parse_parenthesized(|parser, span| {
            let mut head = None;
            let mut current = None;

            loop {
                if parser.is_empty() {
                    break;
                }

                let ty = parser.parse_push()?;
                parser.push_list(&mut head, &mut current, ty)?;

                if !parser.peek::<T![,]>() {
                    if !parser.is_empty() {
                        parser.unexpected(",");
                    }
                    break;
                }
            }
            Ok((head, span))
        })?;

        let output = if parser.peek::<T![->]>() {
            parser.expect::<T![->]>()?;
            let ty = parser.parse_push::<ast::Type>()?;
            Some(ty)
        } else {
            None
        };

        let fn_ty_span = if let Some(output) = output {
            args_span.try_join(output.ast_span(&*parser))
        } else {
            args_span
        };

        let ty = parser.push(ast::TypeFn {
            params: args,
            output,
            span: fn_ty_span,
        })?;

        let span = span.try_join(fn_ty_span);

        Ok(ast::ModuleDefinition {
            name: sym,
            ty,
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::Impl {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![impl]>()?.0;

        let mut generic = None;
        if parser.peek::<T![<]>() {
            parser.expect::<T![<]>()?;

            let mut tail = None;
            while !parser.peek::<T![>]>() {
                let name = parser.parse_push()?;
                parser.push_list(&mut generic, &mut tail, name)?;
            }

            parser.expect::<T![>]>()?;
        }

        let ty = parser.parse_push()?;

        let mut functions = None;
        let last_span = parser.parse_braced(|parser, span| {
            let mut tail = None;
            while !parser.is_empty() {
                let func = parser.parse_push()?;
                parser.push_list(&mut functions, &mut tail, func)?;
            }
            Ok(span)
        })?;

        Ok(ast::Impl {
            generic,
            ty,
            functions,
            span: span.try_join(last_span),
        })
    }
}
