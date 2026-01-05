use ast::{Ast, NodeId};
use error::{AnnotationKind, Diagnostic, Level, Snippet};
use token::{Span, Spanned};

use crate::resolve::{
    ResolveInfo,
    types::{Type, TypeId},
};

pub fn check<'src>(
    src: &'src str,
    root: NodeId<ast::Module>,
    ast: &Ast,
    resolve: &mut ResolveInfo,
) -> Result<(), Diagnostic<'src>> {
    TypeChecker::check(src, root, ast, resolve)
}

pub struct TypeChecker<'src, 'ast, 'resolve> {
    src: &'src str,
    ast: &'ast Ast,
    resolve: &'resolve ResolveInfo,
}

impl<'src, 'ast, 'resolve> TypeChecker<'src, 'ast, 'resolve> {
    fn check_coerces<F: FnOnce() -> Span>(
        &self,
        from: TypeId,
        to: TypeId,
        origin: F,
    ) -> Result<(), Diagnostic<'src>> {
        if !self.resolve.types.coerces_to(from, to) {
            return Err(Level::Error
                .title(format!(
                    "Cannot coerce type `{}` to `{}`",
                    self.resolve.types.format_type(self.ast, from),
                    self.resolve.types.format_type(self.ast, to)
                ))
                .snippet(Snippet::source(self.src).annotate(AnnotationKind::Primary.span(origin())))
                .to_diagnostic());
        }
        Ok(())
    }

    fn check(
        src: &'src str,
        root: NodeId<ast::Module>,
        ast: &'ast Ast,
        resolve: &'resolve mut ResolveInfo,
    ) -> Result<(), Diagnostic<'src>> {
        let mut this = TypeChecker { src, ast, resolve };

        for s in ast.iter_list_node(ast[root].stmts) {
            match ast[s] {
                ast::Stmt::Stencil(n) => this.check_stencil(n)?,
                ast::Stmt::Function(n) => this.check_function(n)?,
                ast::Stmt::Struct(_) => {}
                ast::Stmt::Definition(_) => {}
            }
        }

        Ok(())
    }

    fn check_stencil(&mut self, n: NodeId<ast::Stencil>) -> Result<(), Diagnostic<'src>> {
        self.check_expr(self.ast[n].body, TypeId::NEVER)
    }

    fn check_function(&mut self, n: NodeId<ast::Function>) -> Result<(), Diagnostic<'src>> {
        let fn_ty = self.resolve.types.ast_fn_to_type[n];
        let Type::Fn { output, .. } = self.resolve.types.types[fn_ty] else {
            unreachable!("functions should have a function type")
        };

        self.check_expr(self.ast[n].body, output)
    }

    fn check_expr(&mut self, node: NodeId<ast::Expr>, ty: TypeId) -> Result<(), Diagnostic<'src>> {
        match self.ast[node] {
            ast::Expr::If(n) => {
                self.check_expr(self.ast[n].condition, TypeId::BOOL)?;
                if let Some(otherwise) = self.ast[n].otherwise {
                    self.check_block(self.ast[n].then, ty)?;
                    self.check_block(otherwise, ty)?;
                } else {
                    self.check_block(self.ast[n].then, TypeId::NIL)?;
                    self.check_coerces(TypeId::NIL, ty, || self.ast[n].span)?;
                }
            }
            ast::Expr::Binary(node_id) => todo!(),
            ast::Expr::Unary(node_id) => todo!(),
            ast::Expr::Block(node_id) => todo!(),
            ast::Expr::Cast(node_id) => todo!(),
            ast::Expr::Loop(node_id) => todo!(),
            ast::Expr::While(node_id) => todo!(),
            ast::Expr::Let(node_id) => todo!(),
            ast::Expr::Continue(span) => todo!(),
            ast::Expr::Break(node_id) => todo!(),
            ast::Expr::Return(node_id) => todo!(),
            ast::Expr::Become(node_id) => todo!(),
            ast::Expr::Call(node_id) => todo!(),
            ast::Expr::Method(node_id) => todo!(),
            ast::Expr::Field(node_id) => todo!(),
            ast::Expr::Index(node_id) => todo!(),
            ast::Expr::Literal(node_id) => todo!(),
            ast::Expr::Symbol(node_id) => todo!(),
            ast::Expr::Covered(n) => self.check_expr(n, ty)?,
        }
        todo!()
    }

    fn check_block(
        &mut self,
        node: NodeId<ast::Block>,
        ty: TypeId,
    ) -> Result<(), Diagnostic<'src>> {
        todo!()
    }

    fn check_literal(
        &mut self,
        node: NodeId<ast::Lit>,
        ty: TypeId,
    ) -> Result<(), Diagnostic<'src>> {
        match &self.ast[node] {
            ast::Lit::Str(_) => todo!(),
            ast::Lit::Int(lit_int) => self.check_coerces(TypeId::U64, ty, || lit_int.span())?,
            ast::Lit::Bool(lit_bool) => self.check_coerces(TypeId::BOOL, ty, || lit_bool.span)?,
            ast::Lit::Nil(span) => self.check_coerces(TypeId::NIL, ty, || span.clone())?,
        }
        Ok(())
    }
}
