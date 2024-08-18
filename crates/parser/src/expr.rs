use ast::{BinOp, NodeId, Spanned as _, UnOp};
use syn::{Result, Token};

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
            $(
                if $i <= $bp {
                    $(
                        if $parser.peek(Token![$t]){
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
                    )*
                }
            )*
        }
    };
}

fn parse_binding(parser: &mut Parser, bp: BindingPower) -> Result<NodeId<ast::Expr>> {
    let mut expr = if bp <= BindingPower::Unary {
        parse_unary(parser)?
    } else {
        parse_prime(parser)?
    };

    loop {
        parse_bin_op! {
            expr,parser,bp {
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
                 BindingPower::Cmp => {
                     Token![<] =>  BinOp::Lt,
                     Token![<=] =>  BinOp::Le,
                     Token![>=] =>  BinOp::Ge,
                     Token![>] =>  BinOp::Gt,
                     Token![==] =>  BinOp::Eq,
                     Token![!=] =>  BinOp::Ne,
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

    Ok(expr)
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

    let unary = parser.push(ast::UnaryExpr { op, span, left })?;
    parser.push(ast::Expr::Unary(unary))
}
