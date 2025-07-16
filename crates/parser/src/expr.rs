use ast::{BinOp, UnOp};
use proc_macro2::Delimiter;

use crate::{Parse, Parser, Result, prime::parse_prime};
use ::token::{T, token};

impl Parse for ast::Expr {
    fn parse(parser: &mut Parser) -> Result<Self> {
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
    //Cast,
    Unary,
    CallIndex,
    //Field,
    //Method,
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
                    let right = $parser.push(right)?;
                    let new_expr = ast::BinaryExpr{
                        left: $parser.push($bind)?,
                        op: $op,
                        right,
                        span,
                    };
                    let new_expr = $parser.push(new_expr)?;
                    $bind = ast::Expr::Binary(new_expr);
                    continue
                }
            )*)*
        }
    };
}

fn parse_binding(parser: &mut Parser, bp: BindingPower) -> Result<ast::Expr> {
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

        if parser.peek::<T![as]>() {
            if BindingPower::CallIndex < bp {
                break;
            }
            lhs = parse_cast(parser, lhs)?;
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

fn parse_unary(parser: &mut Parser) -> Result<ast::Expr> {
    let (span, op) = if let Some(t) = parser.eat::<T![!]>() {
        (t.0, UnOp::Not)
    } else if let Some(t) = parser.eat::<T![*]>() {
        (t.0, UnOp::Star)
    } else if let Some(t) = parser.eat::<T![-]>() {
        (t.0, UnOp::Minus)
    } else {
        return parse_prime(parser);
    };

    let left = parse_prime(parser)?;
    let left = parser.push(left)?;

    let unary = parser.push(ast::UnaryExpr {
        op,
        span,
        expr: left,
    })?;
    Ok(ast::Expr::Unary(unary))
}

fn parse_call(parser: &mut Parser, callee: ast::Expr) -> Result<ast::Expr> {
    let span = parser.span();
    let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
    let callee = parser.push(callee)?;
    let call = parser.push(ast::Call {
        func: callee,
        args,
        span,
    })?;
    Ok(ast::Expr::Call(call))
}

fn parse_dot(parser: &mut Parser, base: ast::Expr) -> Result<ast::Expr> {
    let span = parser.expect::<T![.]>()?.0;
    if parser.peek2::<token::Paren>() {
        let ident = parser.expect()?;
        let ident = parser.push(ident)?;
        let args = parser.parse_parenthesized(|parser| parser.parse_terminated::<_, T![,]>())?;
        let receiver = parser.push(base)?;
        let call = parser.push(ast::Method {
            receiver,
            name: ident,
            args,
            span,
        })?;
        Ok(ast::Expr::Method(call))
    } else {
        let ident = parser.expect()?;
        let ident = parser.push(ident)?;
        let base = parser.push(base)?;
        let field = parser.push(ast::Field {
            base,
            field: ident,
            span,
        })?;
        Ok(ast::Expr::Field(field))
    }
}

fn parse_cast(parser: &mut Parser, base: ast::Expr) -> Result<ast::Expr> {
    let expr = parser.push(base)?;
    let span = parser.expect::<T![as]>()?.0;
    let ty = parser.parse_push()?;
    let c = parser.push(ast::Cast { expr, ty, span })?;

    Ok(ast::Expr::Cast(c))
}
