use crate::{Parse, ParseResult, Parser};
use ::token::{T, token};

impl<'src> Parse<'src> for ast::Type {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        if parser.peek::<T![*]>() {
            let v = ast::Type::Ptr(parser.parse_push()?);
            return Ok(v);
        }

        if parser.peek::<T![fn]>() {
            let v = ast::Type::Fn(parser.parse_push()?);
            return Ok(v);
        }

        if parser.peek::<T![&]>() {
            let v = ast::Type::Reference(parser.parse_push()?);
            return Ok(v);
        }

        if parser.peek::<token::Paren>() {
            let v = ast::Type::Tuple(parser.parse_push()?);
            return Ok(v);
        }

        if parser.peek::<token::Bracket>() {
            let v = ast::Type::Array(parser.parse_push()?);
            return Ok(v);
        }

        Ok(ast::Type::Name(parser.parse_push()?))
    }
}

impl<'src> Parse<'src> for ast::TypeFn {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![fn]>()?.0;
        let params =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;

        let output = if let Some(_) = parser.eat::<T![->]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        Ok(ast::TypeFn {
            params,
            output,
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::TypePtr {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![*]>()?.0;
        let mutable = if let Some(_) = parser.eat::<T![mut]>() {
            true
        } else if let Some(_) = parser.eat::<T![const]>() {
            false
        } else {
            return Err(parser.error("expected either `mut` or `const`"));
        };
        let ty = parser.parse_push()?;

        Ok(ast::TypePtr { mutable, span, ty })
    }
}

impl<'src> Parse<'src> for ast::TypeReference {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.expect::<T![*]>()?.0;
        let mutable = parser.eat::<T![mut]>().is_some();
        let ty = parser.parse_push()?;
        Ok(ast::TypeReference { mutable, span, ty })
    }
}

impl<'src> Parse<'src> for ast::TypeArray {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();
        parser.parse_bracketed(|parser, _| {
            let ty = parser.parse_push()?;
            parser.expect::<T![;]>()?;
            let len = parser.parse_push()?;
            Ok(ast::TypeArray {
                span,
                elem: ty,
                len,
            })
        })
    }
}

impl<'src> Parse<'src> for ast::TypeTuple {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let span = parser.span();
        let fields =
            parser.parse_parenthesized(|parser, _| parser.parse_terminated::<_, T![,]>())?;
        Ok(ast::TypeTuple { fields, span })
    }
}

impl<'src> Parse<'src> for ast::TypeName {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let ident = parser.expect::<token::Ident>()?;
        let span = ident.span().into();
        let name = parser.push_set(ident)?;
        Ok(ast::TypeName { span, name })
    }
}

impl<'src> Parse<'src> for ast::Struct {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let start = parser.span();
        let public = parser.eat::<T![pub]>().is_some();

        parser.expect::<T![struct]>()?;

        let name = parser.parse_push()?;

        let fields =
            parser.parse_braced(|parser, _| parser.parse_terminated::<ast::Field, T![,]>())?;

        let span = parser.span_since(start);

        Ok(ast::Struct {
            public,
            name,
            fields,
            span,
        })
    }
}

impl<'src> Parse<'src> for ast::Field {
    fn parse(parser: &mut Parser<'src, '_, '_>) -> ParseResult<'src, Self> {
        let start = parser.span();
        let name = parser.expect()?;
        let name = parser.push_set(name)?;
        parser.expect::<T![:]>()?;

        let ty = parser.parse_push()?;
        let span = start.try_join(parser.last_span());
        Ok(ast::Field { name, ty, span })
    }
}
