use crate::{
    ParsePush, Parser, Result,
    token::{self, T},
};
use ast::{NodeId, Spanned};
use proc_macro2::Ident;

pub fn parse_prime(parser: &mut Parser) -> Result<NodeId<ast::Expr>> {
    if parser.peek::<T![if]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::If(expr));
    }
    if parser.peek::<T![while]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::While(expr));
    }
    if parser.peek::<T![loop]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Loop(expr));
    }
    if parser.peek::<T![become]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Become(expr));
    }
    if parser.peek::<T![let]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Let(expr));
    }
    if parser.peek::<T![return]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Return(expr));
    }
    if parser.peek::<T![break]>() {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Break(expr));
    }
    if parser.peek(Lit) {
        let expr = parser.parse_push()?;
        return parser.push(ast::Expr::Literal(expr));
    }
    if parser.peek::<token::Paren>() {
        let expr = parser.parse_parenthesized(|parser| {
            let res = parser.parse_push()?;
            if !parser.is_empty() {
                return Err(parser.error("expected expression to end"));
            }
            Ok(res)
        })?;
        return parser.push(ast::Expr::Covered(expr));
    }
    let parse = parser.parse_push()?;
    parser.push(ast::Expr::Symbol(parse))
}

impl ParsePush for ast::If {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![if]>()?.0;
        let condition = parser.parse_push()?;
        let then = parser.parse_push()?;
        let otherwise = if let Some(_) = parser.eat::<T![else]>() {
            Some(parser.parse_push()?)
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

impl ParsePush for ast::While {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![while]>()?.0;
        let condition = parser.parse_push()?;
        let then = parser.parse_push()?;

        parser.push(ast::While {
            span,
            condition,
            then,
        })
    }
}

impl ParsePush for ast::Become {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        parser.parse::<T![become]>()?;
        let callee = parser.parse_push()?;
        let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
        parser.push(Self { callee, args, span })
    }
}

impl ParsePush for ast::Let {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![let]>()?.0;

        let mutable = parser.eat::<T![mut]>().is_some();

        let ty = if let Some(_) = parser.eat::<T![:]>() {
            Some(parser.parse_push()?)
        } else {
            None
        };

        let sym = parser.parse_push()?;
        parser.parse::<T![=]>()?;
        let expr = parser.parse_push()?;

        parser.push(Self {
            sym,
            mutable,
            ty,
            expr,
            span,
        })
    }
}

impl ParsePush for ast::Return {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![return]>()?.span();
        if parser.peek::<T![;]>() || parser.is_empty() {
            return parser.push(ast::Return { expr: None, span });
        }

        let expr = parser.parse_push()?;
        parser.push(ast::Return {
            expr: Some(expr),
            span,
        })
    }
}

impl ParsePush for ast::Break {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.parse::<T![break]>()?.0;
        if parser.peek::<T![;]>() || parser.is_empty() {
            return parser.push(ast::Break { expr: None, span });
        }

        let expr = parser.parse_push()?;
        parser.push(ast::Break {
            expr: Some(expr),
            span,
        })
    }
}

impl ParsePush for ast::Symbol {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let ident: Ident = parser.parse()?;
        let span = Spanned::span(&ident);
        let ident = parser.push(ident)?;
        parser.push(ast::Symbol { name: ident, span })
    }
}

impl ParsePush for ast::Block {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let span = parser.span();
        let mut returns_last = false;
        let body = parser.parse_braced(|parser| {
            let mut head = None;
            let mut current = None;
            loop {
                if parser.is_empty() {
                    break;
                }

                let expr = parser.parse_push()?;
                parser.push_list(&mut head, &mut current, expr)?;

                returns_last = true;

                // TODO: Handle semicolons better
                if let Some(_) = parser.eat::<T![;]>() {
                    returns_last = false;
                }
            }
            Ok(head)
        })?;

        parser.push(ast::Block {
            body,
            span,
            returns_last,
        })
    }
}

impl ParsePush for Lit {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
        let p = parser.parse_syn::<Lit>()?;
        match p {
            Lit::Str(_) | Lit::ByteStr(_) | Lit::CStr(_) | Lit::Byte(_) | Lit::Char(_) => {
                return Err(syn::Error::new(p.span(), "Literal type not yet supported"));
            }
            Lit::Int(ref x) => match x.suffix() {
                "i64" | "u64" | "" => {}
                "isize" | "usize" | "i32" | "u32" | "i16" | "u16" | "i8" | "u8" => {
                    return Err(syn::Error::new(
                        p.span(),
                        format_args!("Invalid integer suffix, {} not yet supported", x.suffix()),
                    ));
                }
                "i128" | "u128" => {
                    return Err(syn::Error::new(
                        p.span(),
                        "Invalid integer suffix, 128 bit integers are not supported",
                    ));
                }
                _ => return Err(syn::Error::new(p.span(), "Invalid integer suffix")),
            },
            Lit::Float(ref f) => match f.suffix() {
                "f64" | "" => {}
                "f32" => {
                    return Err(syn::Error::new(
                        p.span(),
                        format_args!("Invalid integer suffix, {} not yet supported", f.suffix()),
                    ));
                }
                _ => return Err(syn::Error::new(p.span(), "Invalid floating point suffix")),
            },
            Lit::Bool(_) => {}
            Lit::Verbatim(_) => return Err(syn::Error::new(p.span(), "Invalid token")),
            _ => unreachable!(),
        }
        parser.push(p)
    }
}
