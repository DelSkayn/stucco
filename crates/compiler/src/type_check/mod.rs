use ast::{Ast, AstSpanned, NodeId, NodeListId};
use error::{AnnotationKind, Diagnostic, Level, Snippet};
use token::{Span, Spanned};

use crate::resolve::{
    ResolveInfo,
    types::{Type, TypeId, TypeTupleId},
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
    fn todo<T, N: AstSpanned>(&self, n: N) -> Result<T, Diagnostic<'src>> {
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
                ast::Stmt::Function(n) => {
                    let sym = this.resolve.symbols.ast_to_symbol[ast[n].sym];
                    let ty = this.resolve.types.ast_fn_to_type[n];
                    this.resolve.types.symbol_to_ty.insert_fill(sym, ty);
                }
                ast::Stmt::Stencil(_) => {}
                ast::Stmt::Struct(_) => {}
                ast::Stmt::Definition(_) => {}
                ast::Stmt::Impl(_) => {}
            }
        }

        for s in ast.iter_list_node(ast[root].stmts) {
            match ast[s] {
                ast::Stmt::Stencil(n) => this.check_stencil(n)?,
                ast::Stmt::Function(n) => this.check_function(n)?,
                ast::Stmt::Struct(_) => {}
                ast::Stmt::Definition(_) => {}
                ast::Stmt::Impl(_) => {}
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
        n.index(self.ast).body.index(self.ast);
        self.check_expr(self.ast[n].body, output)?;
        self.return_type = None;

        Ok(())
    }

    fn check_expr(
        &mut self,
        node: NodeId<ast::Expr>,
        ty: TypeId,
    ) -> Result<TypeId, Diagnostic<'src>> {
        match self.ast[node] {
            ast::Expr::If(n) => {
                self.check_expr(self.ast[n].condition, TypeId::BOOL)?;
                if let Some(otherwise) = self.ast[n].otherwise {
                    let ty = self.check_block(self.ast[n].then, ty)?;
                    self.check_block(otherwise, ty)?;
                    return Ok(ty);
                } else {
                    self.check_block(self.ast[n].then, TypeId::NIL)?;
                    self.check_coerces(TypeId::NIL, ty, || self.ast[n].span)?;
                    return Ok(TypeId::NIL);
                }
            }
            ast::Expr::Binary(n) => self.check_binary(n, ty),
            ast::Expr::Unary(node_id) => self.todo(node_id),
            ast::Expr::Block(n) => self.check_block(n, ty),
            ast::Expr::Cast(node_id) => self.todo(node_id),
            ast::Expr::Loop(node_id) => self.todo(node_id),
            ast::Expr::While(n) => {
                self.check_expr(self.ast[n].condition, TypeId::BOOL)?;
                self.check_block(self.ast[n].then, TypeId::NIL)?;
                Ok(TypeId::NIL)
            }
            ast::Expr::Let(n) => {
                let sym_id = self.resolve.symbols.ast_to_symbol[self.ast[n].sym];
                if let Some(x) = self.ast[n].ty {
                    let ty = self.resolve.types.ast_to_type[x];
                    self.check_expr(self.ast[n].expr, ty)?;
                    self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
                } else {
                    let Some(ty) = self.infer_expr(self.ast[n].expr)? else {
                        return Err(Level::Error
                            .title("Cannot infer type for symbol")
                            .snippet(
                                Snippet::source(self.src).annotate(
                                    AnnotationKind::Primary
                                        .span(n.index(self.ast).sym.index(self.ast).span),
                                ),
                            )
                            .to_diagnostic());
                    };
                    self.resolve.types.symbol_to_ty.insert_fill(sym_id, ty);
                };
                self.check_coerces(TypeId::NIL, ty, || self.ast[n].span)?;
                Ok(TypeId::NIL)
            }
            ast::Expr::Continue(span) => {
                self.check_coerces(TypeId::NEVER, ty, || span)?;
                Ok(ty)
            }
            ast::Expr::Break(n) => {
                self.check_coerces(TypeId::NEVER, ty, || self.ast[n].span)?;
                // TODO: set block return value.
                Ok(ty)
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
                Ok(ty)
            }
            ast::Expr::Become(n) => {
                for a in self.ast.iter_list_node(self.ast[n].args) {
                    if self.infer_expr(a)?.is_none() {
                        return Err(Level::Error
                            .title("Cannot infer type for `become` argument")
                            .snippet(
                                Snippet::source(self.src)
                                    .annotate(AnnotationKind::Primary.span(a.ast_span(self.ast))),
                            )
                            .to_diagnostic());
                    }
                }
                self.check_coerces(TypeId::NEVER, ty, || self.ast[n].span)?;
                Ok(TypeId::NEVER)
            }
            ast::Expr::Call(n) => {
                let Some(ty) = self.infer_expr(self.ast[n].func)? else {
                    return Err(Level::Error
                        .title(format!("Cannot infer type for call expression",))
                        .snippet(
                            Snippet::source(self.src).annotate(
                                AnnotationKind::Primary.span(self.ast[n].span).label(
                                    "Type of this expression must be known before this point",
                                ),
                            ),
                        )
                        .to_diagnostic());
                };
                let Type::Fn { args, output } = self.resolve.types.types[ty] else {
                    return Err(Level::Error
                        .title(format!(
                            "Tried to call non function type `{}`",
                            self.resolve.types.format_type(self.ast, ty)
                        ))
                        .snippet(Snippet::source(self.src).annotate(
                            AnnotationKind::Primary.span(self.ast[n].func.ast_span(self.ast)),
                        ))
                        .to_diagnostic());
                };

                let expr_args = self.ast[n].args;
                self.check_args(args, expr_args, &self.ast[n].span)?;
                self.check_coerces(output, ty, || self.ast[n].span)?;
                Ok(output)
            }
            ast::Expr::Method(node_id) => self.todo(node_id)?,
            ast::Expr::Field(node_id) => self.todo(node_id)?,
            ast::Expr::Index(node_id) => self.todo(node_id)?,
            ast::Expr::Literal(n) => self.check_literal(n, ty),
            ast::Expr::Symbol(n) => {
                let symbol_id = self.resolve.symbols.ast_to_symbol[n];
                // All symbols are declared so they must be in the symbol_to_ty list;
                let ty_id = self.resolve.types.symbol_to_ty[symbol_id];
                self.resolve.types.expr_to_ty.insert_fill(node, ty_id);

                self.check_coerces(ty_id, ty, || self.ast[n].span)?;
                Ok(ty_id)
            }
            ast::Expr::Covered(n) => self.check_expr(n, ty),
        }
    }

    fn check_args(
        &mut self,
        tuple: Option<TypeTupleId>,
        arg: Option<NodeListId<ast::Expr>>,
        func_span: &Span,
    ) -> Result<(), Diagnostic<'src>> {
        if let Some(tuple) = tuple {
            if let Some(x) = self.check_args_rec(tuple, arg, func_span)? {
                return Err(Level::Error
                    .title("Too many arguments for function call")
                    .snippet(Snippet::source(self.src).annotate(
                        AnnotationKind::Primary.span(x.index(self.ast).value.ast_span(self.ast)),
                    ))
                    .to_diagnostic());
            }
        } else {
            if let Some(x) = arg {
                return Err(Level::Error
                    .title("Too many arguments for function call")
                    .snippet(Snippet::source(self.src).annotate(
                        AnnotationKind::Primary.span(x.index(self.ast).value.ast_span(self.ast)),
                    ))
                    .to_diagnostic());
            }
        }
        Ok(())
    }

    // Kind of annoying shuffle we have to do because argument types are pushed in reverse order.
    fn check_args_rec(
        &mut self,
        tuple: TypeTupleId,
        arg: Option<NodeListId<ast::Expr>>,
        func_span: &Span,
    ) -> Result<Option<NodeListId<ast::Expr>>, Diagnostic<'src>> {
        let ty = self.resolve.types.type_tuples[tuple].ty;
        let next = self.resolve.types.type_tuples[tuple].next;

        let arg_check = if let Some(next) = next {
            self.check_args_rec(next, arg, func_span)?
        } else {
            arg
        };

        if let Some(x) = arg_check {
            self.check_expr(self.ast[x].value, ty)?;
            Ok(self.ast[x].next)
        } else {
            Err(Level::Error
                .title("Missing argument in function call")
                .snippet(Snippet::source(self.src).annotate(
                    AnnotationKind::Primary.span(*func_span).label(format!(
                        "Expected aditional argument of type {}",
                        self.resolve.types.format_type(self.ast, ty)
                    )),
                ))
                .to_diagnostic())
        }
    }

    fn check_binary(
        &mut self,
        node: NodeId<ast::BinaryExpr>,
        ty: TypeId,
    ) -> Result<TypeId, Diagnostic<'src>> {
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
                Ok(TypeId::U64)
            }
            ast::BinOp::And | ast::BinOp::Or => {
                self.check_expr(self.ast[node].left, TypeId::BOOL)?;
                self.check_expr(self.ast[node].right, TypeId::BOOL)?;
                self.check_coerces(TypeId::BOOL, ty, || self.ast[node].span)?;
                Ok(TypeId::BOOL)
            }
            ast::BinOp::Assign => {
                let Some(id) = self.infer_expr(self.ast[node].right)? else {
                    return self.todo(node);
                };
                self.check_expr(self.ast[node].left, id)?;
                self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
                Ok(TypeId::NIL)
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
    }

    fn check_block(
        &mut self,
        node: NodeId<ast::Block>,
        ty: TypeId,
    ) -> Result<TypeId, Diagnostic<'src>> {
        let Some(head) = self.ast[node].body else {
            self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
            return Ok(TypeId::NIL);
        };

        let mut head = Some(head);
        let mut res_ty = TypeId::NIL;
        while let Some(x) = self.ast.next_list(&mut head) {
            if head.is_none() {
                if self.ast[node].returns_last {
                    res_ty = self.check_expr(x, ty)?;
                    break;
                } else {
                    self.check_coerces(TypeId::NIL, ty, || self.ast[node].span)?;
                }
            }
            self.check_expr(x, TypeId::IGNORE)?;
        }
        Ok(res_ty)
    }

    fn check_literal(
        &mut self,
        node: NodeId<ast::Lit>,
        ty: TypeId,
    ) -> Result<TypeId, Diagnostic<'src>> {
        match &self.ast[node] {
            ast::Lit::Str(n) => self.todo(n.span)?,
            // TODO: handle other types of numbers.
            ast::Lit::Int(lit_int) => {
                self.check_coerces(TypeId::U64, ty, || lit_int.span())?;
                Ok(TypeId::U64)
            }
            ast::Lit::Bool(lit_bool) => {
                self.check_coerces(TypeId::BOOL, ty, || lit_bool.span)?;
                Ok(TypeId::BOOL)
            }
            ast::Lit::Nil(span) => {
                self.check_coerces(TypeId::NIL, ty, || span.clone())?;
                Ok(TypeId::NIL)
            }
        }
    }

    fn infer_expr_either<F: FnOnce(&mut Self) -> Span>(
        &mut self,
        a: NodeId<ast::Expr>,
        b: NodeId<ast::Expr>,
        origin: F,
    ) -> Result<TypeId, Diagnostic<'src>> {
        if let Some(x) = self.infer_expr(a)? {
            self.check_expr(b, x)?;
            return Ok(x);
        }
        if let Some(x) = self.infer_expr(b)? {
            self.check_expr(a, x)?;
            return Ok(x);
        }

        return Err(Level::Error
            .title(format!("Cannot infer type for expression",))
            .snippet(Snippet::source(self.src).annotate(AnnotationKind::Primary.span(origin(self))))
            .to_diagnostic());
    }

    fn infer_expr(&mut self, node: NodeId<ast::Expr>) -> Result<Option<TypeId>, Diagnostic<'src>> {
        match self.ast[node] {
            ast::Expr::If(n) => {
                self.check_expr(self.ast[n].condition, TypeId::BOOL)?;
                if let Some(otherwise) = self.ast[n].otherwise {
                    if let Some(x) = self.infer_block(self.ast[n].then)? {
                        self.check_block(otherwise, x)?;
                        return Ok(Some(x));
                    }
                    if let Some(x) = self.infer_block(otherwise)? {
                        self.check_block(self.ast[n].then, x)?;
                        return Ok(Some(x));
                    }

                    return Ok(None);
                } else {
                    self.check_block(self.ast[n].then, TypeId::NIL)?;
                    return Ok(Some(TypeId::NIL));
                }
            }
            ast::Expr::Binary(node_id) => self.todo(node_id)?,
            ast::Expr::Unary(node_id) => self.todo(node_id)?,
            ast::Expr::Block(n) => return self.infer_block(n),
            ast::Expr::Cast(node_id) => self.todo(node_id)?,
            ast::Expr::Loop(node_id) => self.todo(node_id)?,
            ast::Expr::While(node_id) => self.todo(node_id)?,
            ast::Expr::Let(node_id) => self.todo(node_id)?,
            ast::Expr::Continue(_) => return Ok(Some(TypeId::NEVER)),
            ast::Expr::Break(node_id) => self.todo(node_id)?,
            ast::Expr::Return(n) => {
                let return_type = self
                    .return_type
                    .expect("return type should be set when checking an expression");
                if let Some(x) = self.ast[n].expr {
                    self.check_expr(x, return_type)?;
                } else {
                    self.check_coerces(TypeId::NIL, return_type, || self.ast[n].span)?;
                }
                return Ok(Some(TypeId::NEVER));
            }
            ast::Expr::Become(n) => {
                for a in self.ast.iter_list_node(self.ast[n].args) {
                    if self.infer_expr(a)?.is_none() {
                        return Err(Level::Error
                            .title("Cannot infer type for `become` argument")
                            .snippet(
                                Snippet::source(self.src)
                                    .annotate(AnnotationKind::Primary.span(a.ast_span(self.ast))),
                            )
                            .to_diagnostic());
                    }
                }
                return Ok(Some(TypeId::NEVER));
            }
            ast::Expr::Call(n) => {
                let Some(ty) = self.infer_expr(self.ast[n].func)? else {
                    return Err(Level::Error
                        .title(format!("Cannot infer type for call expression",))
                        .snippet(
                            Snippet::source(self.src).annotate(
                                AnnotationKind::Primary.span(self.ast[n].span).label(
                                    "Type of this expression must be known before this point",
                                ),
                            ),
                        )
                        .to_diagnostic());
                };

                let Type::Fn { args, output } = self.resolve.types.types[ty] else {
                    return Err(Level::Error
                        .title(format!(
                            "Tried to call non function type `{}`",
                            self.resolve.types.format_type(self.ast, ty)
                        ))
                        .snippet(Snippet::source(self.src).annotate(
                            AnnotationKind::Primary.span(self.ast[n].func.ast_span(self.ast)),
                        ))
                        .to_diagnostic());
                };

                let expr_args = self.ast[n].args;
                self.check_args(args, expr_args, &self.ast[n].span)?;
                return Ok(Some(output));
            }
            ast::Expr::Method(node_id) => self.todo(node_id)?,
            ast::Expr::Field(node_id) => self.todo(node_id)?,
            ast::Expr::Index(node_id) => self.todo(node_id)?,
            ast::Expr::Literal(node_id) => return self.infer_literal(node_id),
            ast::Expr::Symbol(n) => {
                let symbol_id = self.resolve.symbols.ast_to_symbol[n];
                // All symbols are declared so they must be in the symbol_to_ty list;
                return Ok(Some(self.resolve.types.symbol_to_ty[symbol_id]));
            }
            ast::Expr::Covered(n) => return self.infer_expr(n),
        }
        todo!()
    }

    fn infer_block(
        &mut self,
        node: NodeId<ast::Block>,
    ) -> Result<Option<TypeId>, Diagnostic<'src>> {
        let Some(head) = self.ast[node].body else {
            return Ok(Some(TypeId::NIL));
        };

        let mut head = Some(head);
        while let Some(x) = self.ast.next_list(&mut head) {
            if head.is_none() {
                if self.ast[node].returns_last {
                    return self.infer_expr(x);
                }
            }
            self.check_expr(x, TypeId::IGNORE)?;
        }
        return Ok(Some(TypeId::NIL));
    }

    fn infer_literal(&mut self, n: NodeId<ast::Lit>) -> Result<Option<TypeId>, Diagnostic<'src>> {
        match self.ast[n] {
            ast::Lit::Str(_) => todo!(),
            ast::Lit::Int(_) => Ok(Some(TypeId::U64)),
            ast::Lit::Bool(_) => Ok(Some(TypeId::BOOL)),
            ast::Lit::Nil(_) => Ok(Some(TypeId::NIL)),
        }
    }
}
