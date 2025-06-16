use ast::{BinOp, NodeId, Spanned as _, UnOp};
use proc_macro2::Delimiter;

use crate::{
    ParsePush, Parser, Result,
    prime::parse_prime,
    token::{self, T},
};

impl ParsePush for ast::Expr {
    fn parse_push(parser: &mut Parser) -> Result<NodeId<Self>> {
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
            $([$t:tt] => $op:expr),* $(,)?
        })*
    }) => {
        {
            $($(
                if let Some(t) = $parser.eat::<T![$t]>(){
                    if $bp < $i {
                        break
                    }
                    let span = t.0;
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
        if parser.peek_group(Delimiter::Parenthesis) {
            if BindingPower::CallIndex < bp {
                break;
            }
            lhs = parse_call(parser, lhs)?;
            continue;
        }
        if parser.peek::<T![.]>() {
            if BindingPower::CallIndex < bp {
                break;
            }
            lhs = parse_dot(parser, lhs)?;
            continue;
        }

        parse_bin_op! {
            lhs,parser,bp {
                BindingPower::Cmp => {
                    [<=] =>  BinOp::Le,
                    [>=] =>  BinOp::Ge,
                    [==] =>  BinOp::Eq,
                    [!=] =>  BinOp::Ne,
                }
                BindingPower::Assign => {
                    [=] => BinOp::Assign,
                    [+=] => BinOp::AddAssign,
                    [-=] => BinOp::SubAssign,
                    [*=] => BinOp::MullAssign,
                    [/=] => BinOp::DivAssign,
                    [%=] => BinOp::RemAssign,
                    [<<=] => BinOp::ShlAssign,
                    [>>=] => BinOp::ShrAssign,
                    [|=] => BinOp::BitOrAssign,
                    [^=] => BinOp::BitXorAssign,
                    [&=] => BinOp::BitAndAssign,
                }
                BindingPower::Or => {
                    [||] => BinOp::Or,
                }
                 BindingPower::And => {
                    [&&] => BinOp::And,
                }
                 BindingPower::BitOr => {
                    [|] => BinOp::BitOr,
                 }
                 BindingPower::BitXor => {
                    [^] => BinOp::BitXor,
                 }
                 BindingPower::BitAnd => {
                    [&] => BinOp::BitAnd,
                 }
                 BindingPower::Shift => {
                    [<<] => BinOp::Shl,
                    [>>] => BinOp::Shr,
                 }
                 BindingPower::Cmp => {
                    [<] =>  BinOp::Lt,
                    [>] =>  BinOp::Gt,
                 }
                 BindingPower::AddSub => {
                    [+] => BinOp::Add,
                    [-] => BinOp::Sub,
                 }
                 BindingPower::MulDiv => {
                    [*] => BinOp::Mull,
                    [/] => BinOp::Div,
                    [%] => BinOp::Rem,
                 }
            }
        }

        break;
    }

    Ok(lhs)
}

fn parse_unary(parser: &mut Parser) -> Result<NodeId<ast::Expr>> {
    let (span, op) = if parser.peek::<T![!]>() {
        let span = parser.parse::<T![!]>()?.0;
        (span, UnOp::Not)
    } else if parser.peek::<T![*]>() {
        let span = parser.parse::<T![*]>()?.0;
        (span, UnOp::Star)
    } else if parser.peek::<T![*]>() {
        let span = parser.parse::<T![-]>()?.0;
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
    let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
    let call = parser.push(ast::Call {
        func: callee,
        args,
        span,
    })?;
    parser.push(ast::Expr::Call(call))
}

fn parse_dot(parser: &mut Parser, base: NodeId<ast::Expr>) -> Result<NodeId<ast::Expr>> {
    let span = parser.parse::<T![.]>()?.0;
    if parser.peek2::<token::Paren>() {
        let ident = parser.parse_push()?;
        let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
        let call = parser.push(ast::Method {
            receiver: base,
            name: ident,
            args,
            span,
        })?;
        parser.push(ast::Expr::Method(call))
    } else {
        let ident = parser.parse_push()?;
        let field = parser.push(ast::Field {
            base,
            field: ident,
            span,
        })?;
        parser.push(ast::Expr::Field(field))
    }
}
