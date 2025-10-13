use ast::NodeId;

use crate::Parser;

pub fn expr_needs_semicolon(parser: &mut Parser, expr: NodeId<ast::Expr>) -> bool {
    match parser[expr] {
        ast::Expr::If(_) | ast::Expr::Block(_) | ast::Expr::Loop(_) | ast::Expr::While(_) => false,
        ast::Expr::Binary(_)
        | ast::Expr::Unary(_)
        | ast::Expr::Cast(_)
        | ast::Expr::Let(_)
        | ast::Expr::Continue(_)
        | ast::Expr::Break(_)
        | ast::Expr::Return(_)
        | ast::Expr::Become(_)
        | ast::Expr::Call(_)
        | ast::Expr::Method(_)
        | ast::Expr::Field(_)
        | ast::Expr::Index(_)
        | ast::Expr::Literal(_)
        | ast::Expr::Symbol(_)
        | ast::Expr::Covered(_) => true,
    }
}
