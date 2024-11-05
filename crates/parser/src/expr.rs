use ast::{BinOp, NodeId, Spanned as _, UnOp};
use syn::{token, Result, Token};

use crate::{prime::parse_prime, Parse, Parser};

impl Parse for ast::Expr {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        parse_binding(parser, BindingPower::Base)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum BindingPower {
    Base,
    Assign,
    Or,
    And,
    Cmp,
    BitOr,
    BitXor,
    BitAnd,
    Shift,
    AddSub,
    MulDiv,
    Cast,
    Unary,
    CallIndex,
    Field,
    Method,
}

macro_rules! parse_bin_op {
    ($bind:ident,$parser:expr,$i:ident {
        $($bp:expr => {
            $(Token![$t:tt] => $op:expr),* $(,)?
        })*
    }) => {
        {
            $($(
                if $parser.peek(Token![$t]){
                    if $bp < $i {
                        break
                    }
                    let span = $parser.parse_syn::<Token![$t]>()?.span();
                    let right = parse_binding($parser, $bp)?;
                    let new_expr = $parser.push(ast::BinaryExpr{
                        left: $bind,
                        op: $op,
                        right,
                        span,
                    })?;
                    $bind = $parser.push(ast::Expr::Binary(new_expr))?;
                    continue
                }
            )*)*
        }
    };
}

fn parse_binding(parser: &mut Parser, bp: BindingPower) -> Result<NodeId<ast::Expr>> {
    let mut lhs = if bp <= BindingPower::Unary {
        parse_unary(parser)?
    } else {
        parse_prime(parser)?
    };

    loop {
        if parser.peek(token::Paren) {
            if BindingPower::CallIndex < bp {
                break;
            }
            lhs = parse_call(parser, lhs)?;
            continue;
        }
        if parser.peek(Token![.]) {
            if BindingPower::CallIndex < bp {
                break;
            }
            lhs = parse_dot(parser, lhs)?;
            continue;
        }

        parse_bin_op! {
            lhs,parser,bp {
                BindingPower::Cmp => {
                    Token![<=] =>  BinOp::Le,
                    Token![>=] =>  BinOp::Ge,
                    Token![==] =>  BinOp::Eq,
                    Token![!=] =>  BinOp::Ne,
                }
                BindingPower::Assign => {
                    Token![=] => BinOp::Assign,
                    Token![+=] => BinOp::AddAssign,
                    Token![-=] => BinOp::SubAssign,
                    Token![*=] => BinOp::MullAssign,
                    Token![/=] => BinOp::DivAssign,
                    Token![%=] => BinOp::RemAssign,
                    Token![<<=] => BinOp::ShlAssign,
                    Token![>>=] => BinOp::ShrAssign,
                    Token![|=] => BinOp::BitOrAssign,
                    Token![^=] => BinOp::BitXorAssign,
                    Token![&=] => BinOp::BitAndAssign,
                }
                BindingPower::Or => {
                    Token![||] => BinOp::Or,
                }
                 BindingPower::And => {
                    Token![&&] => BinOp::And,
                }
                 BindingPower::BitOr => {
                    Token![|] => BinOp::BitOr,
                 }
                 BindingPower::BitXor => {
                    Token![^] => BinOp::BitXor,
                 }
                 BindingPower::BitAnd => {
                    Token![&] => BinOp::BitAnd,
                 }
                 BindingPower::Shift => {
                    Token![<<] => BinOp::Shl,
                    Token![>>] => BinOp::Shr,
                 }
                 BindingPower::Cmp => {
                    Token![<] =>  BinOp::Lt,
                    Token![>] =>  BinOp::Gt,
                 }
                 BindingPower::AddSub => {
                    Token![+] => BinOp::Add,
                    Token![-] => BinOp::Sub,
                 }
                 BindingPower::MulDiv => {
                    Token![*] => BinOp::Mull,
                    Token![/] => BinOp::Div,
                    Token![%] => BinOp::Rem,
                 }
            }
        }

        break;
    }

    Ok(lhs)
}

fn parse_unary(parser: &mut Parser) -> Result<NodeId<ast::Expr>> {
    let (span, op) = if parser.peek(Token![!]) {
        let span = parser.parse_syn::<Token![!]>()?.span();
        (span, UnOp::Not)
    } else if parser.peek(Token![*]) {
        let span = parser.parse_syn::<Token![*]>()?.span();
        (span, UnOp::Star)
    } else if parser.peek(Token![*]) {
        let span = parser.parse_syn::<Token![-]>()?.span();
        (span, UnOp::Minus)
    } else {
        return parse_prime(parser);
    };

    let left = parse_prime(parser)?;

    let unary = parser.push(ast::UnaryExpr {
        op,
        span,
        expr: left,
    })?;
    parser.push(ast::Expr::Unary(unary))
}

fn parse_call(parser: &mut Parser, callee: NodeId<ast::Expr>) -> Result<NodeId<ast::Expr>> {
    let span = parser.span();
    let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;
    let call = parser.push(ast::Call {
        func: callee,
        args,
        span,
    })?;
    parser.push(ast::Expr::Call(call))
}

fn parse_dot(parser: &mut Parser, base: NodeId<ast::Expr>) -> Result<NodeId<ast::Expr>> {
    let span = parser.parse_syn::<Token![.]>()?.span();
    if parser.peek2(token::Paren) {
        let ident = parser.parse_syn_push()?;
        let args =
            parser.parse_parenthesized(|parser| parser.parse_terminated::<_, Token![,]>())?;
        let call = parser.push(ast::Method {
            receiver: base,
            name: ident,
            args,
            span,
        })?;
        parser.push(ast::Expr::Method(call))
    } else {
        let ident = parser.parse_syn_push()?;
        let field = parser.push(ast::Field {
            base,
            field: ident,
            span,
        })?;
        parser.push(ast::Expr::Field(field))
    }
}
