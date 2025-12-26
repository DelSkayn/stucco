use token::T;

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
