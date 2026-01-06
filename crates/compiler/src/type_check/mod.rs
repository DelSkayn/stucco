use ast::{Ast, AstSpanned, NodeId};
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
    resolve: &'resolve mut ResolveInfo,
    pending_blocks: Vec<NodeId<ast::Expr>>,
    return_type: Option<TypeId>,
}

impl<'src, 'ast, 'resolve> TypeChecker<'src, 'ast, 'resolve> {
    #[track_caller]
    fn todo<N: AstSpanned>(&self, n: N) -> Result<(), Diagnostic<'src>> {
        let loc = std::panic::Location::caller();
        Err(Level::Error
            .title(format!(
                "Type inference not yet implemented, {}:{}:{}",
                loc.file(),
                loc.line(),
                loc.column()
            ))
            .snippet(
                Snippet::source(self.src)
                    .annotate(AnnotationKind::Primary.span(n.ast_span(self.ast))),
            )
            .to_diagnostic())
    }

    /// Checks if type `from` is a subtype of `to`.
    /// If it is not the function return an error
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
        let mut this = TypeChecker {
            src,
            ast,
            resolve,
            pending_blocks: Vec::new(),
            return_type: None,
        };

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
        for a in self.ast.iter_list_node(self.ast[n].parameters) {
            let sym_id = self.resolve.symbols.ast_to_symbol[self.ast[a].sym];
            let ty = self.resolve.types.ast_to_type[self.ast[a].ty];
            self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
        }

        let Type::Fn { output, .. } =
            self.resolve.types.types[self.resolve.types.definition.unwrap().1]
        else {
            unreachable!("functions should have a function type")
        };

        self.return_type = Some(output);
        self.check_expr(self.ast[n].body, TypeId::NEVER)?;
        self.return_type = None;

        Ok(())
    }

    fn check_function(&mut self, n: NodeId<ast::Function>) -> Result<(), Diagnostic<'src>> {
        for a in self.ast.iter_list_node(self.ast[n].parameters) {
            let sym_id = self.resolve.symbols.ast_to_symbol[self.ast[a].sym];
            let ty = self.resolve.types.ast_to_type[self.ast[a].ty];
            self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
        }

        let fn_ty = self.resolve.types.ast_fn_to_type[n];
        let Type::Fn { output, .. } = self.resolve.types.types[fn_ty] else {
            unreachable!("functions should have a function type")
        };

        self.return_type = Some(output);
        self.check_expr(self.ast[n].body, output)?;
        self.return_type = None;

        Ok(())
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
            ast::Expr::Binary(n) => self.check_binary(n, ty)?,
            ast::Expr::Unary(node_id) => self.todo(node_id)?,
            ast::Expr::Block(n) => self.check_block(n, ty)?,
            ast::Expr::Cast(node_id) => self.todo(node_id)?,
            ast::Expr::Loop(node_id) => self.todo(node_id)?,
            ast::Expr::While(n) => {
                self.check_expr(self.ast[n].condition, TypeId::BOOL)?;
                self.check_block(self.ast[n].then, TypeId::NIL)?;
            }
            ast::Expr::Let(n) => {
                self.check_coerces(TypeId::NIL, ty, || self.ast[n].span)?;
                let sym_id = self.resolve.symbols.ast_to_symbol[self.ast[n].sym];
                if let Some(x) = self.ast[n].ty {
                    let ty = self.resolve.types.ast_to_type[x];
                    self.check_expr(self.ast[n].expr, ty)?;
                    self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
                } else {
                    let ty = self.infer_expr(self.ast[n].expr)?;
                    self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
                };
            }
            ast::Expr::Continue(span) => {
                self.check_coerces(TypeId::NEVER, ty, || span)?;
            }
            ast::Expr::Break(n) => {
                self.check_coerces(TypeId::NEVER, ty, || self.ast[n].span)?;
            }
            ast::Expr::Return(n) => {
                self.check_coerces(TypeId::NEVER, ty, || self.ast[n].span)?;
                let return_type = self
                    .return_type
                    .expect("return type should be set when checking an expression");
                if let Some(x) = self.ast[n].expr {
                    self.check_expr(x, return_type)?;
                } else {
                    self.check_coerces(TypeId::NIL, return_type, || self.ast[n].span)?;
                }
            }
            ast::Expr::Become(node_id) => self.todo(node_id)?,
            ast::Expr::Call(node_id) => self.todo(node_id)?,
            ast::Expr::Method(node_id) => self.todo(node_id)?,
            ast::Expr::Field(node_id) => self.todo(node_id)?,
            ast::Expr::Index(node_id) => self.todo(node_id)?,
            ast::Expr::Literal(n) => self.check_literal(n, ty)?,
            ast::Expr::Symbol(n) => {
                let symbol_id = self.resolve.symbols.ast_to_symbol[n];
                // All symbols are declared so they must be in the symbol_to_ty list;
                let ty_id = self.resolve.types.symbol_to_ty[symbol_id];
                self.resolve.types.expr_to_ty.insert_fill(node, ty_id);

                self.check_coerces(ty_id, ty, || self.ast[n].span)?;
            }
            ast::Expr::Covered(n) => self.check_expr(n, ty)?,
        }
        Ok(())
    }

    fn check_binary(
        &mut self,
        node: NodeId<ast::BinaryExpr>,
        ty: TypeId,
    ) -> Result<(), Diagnostic<'src>> {
        match self.ast[node].op {
            ast::BinOp::Add
            | ast::BinOp::Sub
            | ast::BinOp::Mull
            | ast::BinOp::Div
            | ast::BinOp::Rem
            | ast::BinOp::BitXor
            | ast::BinOp::BitAnd
            | ast::BinOp::BitOr
            | ast::BinOp::Shl
            | ast::BinOp::Shr
            | ast::BinOp::Eq
            | ast::BinOp::Lt
            | ast::BinOp::Le
            | ast::BinOp::Ne
            | ast::BinOp::Ge
            | ast::BinOp::Gt => {
                self.check_expr(self.ast[node].left, TypeId::U64)?;
                self.check_expr(self.ast[node].right, TypeId::U64)?;
                self.check_coerces(TypeId::U64, ty, || self.ast[node].span)?;
            }
            ast::BinOp::And | ast::BinOp::Or => {
                self.check_expr(self.ast[node].left, TypeId::BOOL)?;
                self.check_expr(self.ast[node].right, TypeId::BOOL)?;
                self.check_coerces(TypeId::BOOL, ty, || self.ast[node].span)?;
            }
            ast::BinOp::Assign => {
                let id = self.infer_expr(self.ast[node].right)?;
                self.check_expr(self.ast[node].left, id)?;
                self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
            }
            ast::BinOp::AddAssign => todo!(),
            ast::BinOp::SubAssign => todo!(),
            ast::BinOp::MullAssign => todo!(),
            ast::BinOp::DivAssign => todo!(),
            ast::BinOp::RemAssign => todo!(),
            ast::BinOp::BitXorAssign => todo!(),
            ast::BinOp::BitAndAssign => todo!(),
            ast::BinOp::BitOrAssign => todo!(),
            ast::BinOp::ShlAssign => todo!(),
            ast::BinOp::ShrAssign => todo!(),
        }
        Ok(())
    }

    fn check_block(
        &mut self,
        node: NodeId<ast::Block>,
        ty: TypeId,
    ) -> Result<(), Diagnostic<'src>> {
        let Some(head) = self.ast[node].body else {
            self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
            return Ok(());
        };

        let mut head = Some(head);
        while let Some(x) = self.ast.next_list(&mut head) {
            if head.is_none() {
                if self.ast[node].returns_last {
                    self.check_expr(x, ty)?;
                    break;
                } else {
                    self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
                }
            }
            self.check_expr(x, TypeId::IGNORE)?;
        }
        Ok(())
    }

    fn check_literal(
        &mut self,
        node: NodeId<ast::Lit>,
        ty: TypeId,
    ) -> Result<(), Diagnostic<'src>> {
        match &self.ast[node] {
            ast::Lit::Str(n) => self.todo(n.span)?,
            // TODO: handle other types of numbers.
            ast::Lit::Int(lit_int) => self.check_coerces(TypeId::U64, ty, || lit_int.span())?,
            ast::Lit::Bool(lit_bool) => self.check_coerces(TypeId::BOOL, ty, || lit_bool.span)?,
            ast::Lit::Nil(span) => self.check_coerces(TypeId::NIL, ty, || span.clone())?,
        }
        Ok(())
    }

    fn infer_expr(&mut self, node: NodeId<ast::Expr>) -> Result<TypeId, Diagnostic<'src>> {
        match self.ast[node] {
            ast::Expr::If(node_id) => self.todo(node_id)?,
            ast::Expr::Binary(node_id) => self.todo(node_id)?,
            ast::Expr::Unary(node_id) => self.todo(node_id)?,
            ast::Expr::Block(node_id) => self.todo(node_id)?,
            ast::Expr::Cast(node_id) => self.todo(node_id)?,
            ast::Expr::Loop(node_id) => self.todo(node_id)?,
            ast::Expr::While(node_id) => self.todo(node_id)?,
            ast::Expr::Let(node_id) => self.todo(node_id)?,
            ast::Expr::Continue(span) => self.todo(span)?,
            ast::Expr::Break(node_id) => self.todo(node_id)?,
            ast::Expr::Return(node_id) => self.todo(node_id)?,
            ast::Expr::Become(node_id) => self.todo(node_id)?,
            ast::Expr::Call(node_id) => self.todo(node_id)?,
            ast::Expr::Method(node_id) => self.todo(node_id)?,
            ast::Expr::Field(node_id) => self.todo(node_id)?,
            ast::Expr::Index(node_id) => self.todo(node_id)?,
            ast::Expr::Literal(node_id) => return self.infer_literal(node_id),
            ast::Expr::Symbol(n) => {
                let symbol_id = self.resolve.symbols.ast_to_symbol[n];
                // All symbols are declared so they must be in the symbol_to_ty list;
                return Ok(self.resolve.types.symbol_to_ty[symbol_id]);
            }
            ast::Expr::Covered(n) => return self.infer_expr(n),
        }
        todo!()
    }

    fn infer_literal(&mut self, n: NodeId<ast::Lit>) -> Result<TypeId, Diagnostic<'src>> {
        match self.ast[n] {
            ast::Lit::Str(_) => todo!(),
            ast::Lit::Int(_) => Ok(TypeId::U64),
            ast::Lit::Bool(_) => Ok(TypeId::BOOL),
            ast::Lit::Nil(_) => Ok(TypeId::NIL),
        }
    }
}
