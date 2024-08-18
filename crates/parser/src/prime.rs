use ast::{NodeId, NodeListId, Spanned as _};
use syn::{token::Paren, Ident, Lit, Result, Token};

use crate::{kw, Parse, Parser};

pub fn parse_block(parser: &mut Parser) -> Result<Option<NodeListId<ast::Expr>>> {
    parser.parse_braced(|parser| {
        let mut head = None;
        let mut current = None;
        while !parser.is_empty() {
            let expr = parser.parse()?;
            parser.push_list(&mut head, &mut current, expr)?;

            if parser.peek(Token![;]) {
                parser.parse_syn::<Token![;]>()?;
            }
        }
        Ok(head)
    })
}

pub fn parse_prime(parser: &mut Parser) -> Result<NodeId<ast::Expr>> {
    if parser.peek(Token![if]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::If(expr));
    }
    if parser.peek(Token![while]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::While(expr));
    }
    if parser.peek(kw::tail) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Tail(expr));
    }
    if parser.peek(Token![let]) {
        let expr = parser.parse()?;
        return parser.push(ast::Expr::Let(expr));
    }
    if parser.peek(Lit) {
        let expr = parser.parse_syn_push()?;
        return parser.push(ast::Expr::Literal(expr));
    }
    if parser.peek(Paren) {
        let expr = parser.parse_parenthesized(|parser| {
            let res = parser.parse()?;
            if !parser.is_empty() {
                return Err(parser.error("expected expression to end"));
            }
            Ok(res)
        })?;
        return parser.push(ast::Expr::Covered(expr));
    }
    let parse = parser.parse_syn_push::<Ident>()?;
    parser.push(ast::Expr::Ident(parse))
}

impl Parse for ast::If {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![if]>()?.span();
        let condition = parser.parse()?;
        let then = parse_block(parser)?;
        let otherwise = if parser.peek(Token![else]) {
            parser.parse_syn::<Token![else]>()?;
            parse_block(parser)?
        } else {
            None
        };

        parser.push(ast::If {
            span,
            condition,
            then,
            otherwise,
        })
    }
}

impl Parse for ast::While {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![while]>()?.span();
        let condition = parser.parse()?;
        let then = parse_block(parser)?;

        parser.push(ast::While {
            span,
            condition,
            then,
        })
    }
}

impl Parse for ast::Tail {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        parser.parse_syn::<kw::tail>()?;
        parser.parse_syn::<Token![!]>()?;
        parser.parse_parenthesized(|parser| {
            let callee = parser.parse_syn_push()?;
            let args =
                parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;
            parser.push(Self { callee, args, span })
        })
    }
}

impl Parse for ast::Let {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse_syn::<Token![let]>()?.span();
        let name = parser.parse_syn_push()?;
        parser.parse_syn::<Token![=]>()?;
        let expr = parser.parse()?;
        parser.push(Self { name, expr, span })
    }
}
