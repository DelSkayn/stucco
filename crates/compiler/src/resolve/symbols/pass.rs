use std::collections::{HashMap, hash_map::Entry};

use ast::{
    Ast, NodeId, NodeListId, Variation,
    visit::{self, Visit},
};
use common::id::IdRange;
use error::{AnnotationKind, Diagnostic, Level, Snippet};
use token::token::Ident;

use crate::resolve::{
    Scope, ScopeId, SymbolId, SymbolTable,
    symbols::{ScopeDeclaration, ScratchFlags, Symbol, SymbolKind},
};

pub struct SymbolResolvePass<'src, 't> {
    source: &'src str,
    table: &'t mut SymbolTable,
    active_symbols: HashMap<NodeId<Ident>, SymbolId>,
    // list of pending symbols and pending scopes.
    // Because we want scopes and a list of symbolid's where symbol id's which are declared in the
    // same scope are next to each other we need to defer pushing scopes and symbol id's into the
    // table until after a scope has completed. These vecs hold the scopes/symbolids until then.
    pending_symbols: Vec<SymbolId>,
    pending_scopes: Vec<Scope>,
}

impl<'src, 't> SymbolResolvePass<'src, 't> {
    pub fn new(source: &'src str, symbols: &'t mut SymbolTable) -> Self {
        SymbolResolvePass {
            source,
            table: symbols,
            active_symbols: HashMap::new(),
            pending_symbols: Vec::new(),
            pending_scopes: Vec::new(),
        }
    }

    pub fn pass(
        mut self,
        ast: &Ast,
        root: NodeId<ast::Module>,
    ) -> Result<ScopeId, Diagnostic<'src>> {
        self.enter_scope(ast, Scope::new(ScopeDeclaration::Root), |this| {
            for stmt in ast.iter_list_node(ast[root].stmts) {
                match ast[stmt] {
                    ast::Stmt::Stencil(n) => {
                        this.declare_symbol(ast, ast[n].sym, SymbolKind::Stencil)?;
                    }
                    ast::Stmt::Struct(_) => continue,
                    ast::Stmt::Function(n) => {
                        this.declare_symbol(ast, ast[n].sym, SymbolKind::Function)?;
                    }
                    ast::Stmt::Definition(_) => continue,
                    ast::Stmt::Impl(_) => continue,
                }
            }

            this.visit_module(ast, root)
        })?;

        assert_eq!(self.pending_scopes.len(), 1);

        // clean up the last root scope.
        let last = self.pending_scopes.pop().unwrap();
        let grand_children = last.children.clone();
        let symbol_grand_children = last.symbols.clone();
        let scope_id = self.table.scopes.push_expect(last);

        // set the id for the parent on the children of the newly pushed scope.
        if let Some(grand_children) = grand_children {
            for s in self.table.scopes[grand_children].iter_mut() {
                s.parent = Some(scope_id);
            }
        }

        // set the id for the scope on the child symbols of the newly pushed scope.
        if let Some(symbol_grand_children) = symbol_grand_children {
            for s in self.table.scope_symbols[symbol_grand_children]
                .iter()
                .copied()
            {
                self.table.symbols[s].scope = Some(scope_id);
            }
        }

        // push all the symbols in the right place.
        let mut first = None;
        let mut last = None;
        for (idx, child) in self.pending_symbols.iter().copied().rev().enumerate() {
            if let Some(shadow) = self.table.symbols[child].shadows {
                let active = self
                    .active_symbols
                    .get_mut(&self.table.symbols[child].declared.index(ast).name)
                    .expect("declared symbol was not active");
                *active = shadow
            } else {
                self.active_symbols
                    .remove(&self.table.symbols[child].declared.index(ast).name)
                    .expect("declared symbol was not active");
            };

            let id = self.table.scope_symbols.push_expect(child);
            if idx == 0 {
                first = Some(id);
            }
            last = Some(id);
        }

        if let Some(first) = first {
            let last = last.expect("if first is set, then last must also be set");
            self.table.scopes[scope_id].symbols = Some(IdRange::new(first, last));
        }

        assert!(self.active_symbols.is_empty());

        Ok(scope_id)
    }

    fn enter_scope<F>(&mut self, ast: &Ast, scope: Scope, cb: F) -> Result<(), Diagnostic<'src>>
    where
        F: FnOnce(&mut Self) -> Result<(), Diagnostic<'src>>,
    {
        self.pending_scopes.push(scope);
        let scope_child_idx = self.pending_scopes.len();
        let symbol_child_idx = self.pending_symbols.len();

        let r = cb(self);

        // push all the scopes in their right place
        let mut first = None;
        let mut last = None;
        for (idx, child) in self.pending_scopes.drain(scope_child_idx..).enumerate() {
            let grand_children = child.children.clone();
            let symbol_grand_children = child.symbols.clone();
            let scope_id = self.table.scopes.push_expect(child);

            // set the id for the parent on the children of the newly pushed scope.
            if let Some(grand_children) = grand_children {
                for s in self.table.scopes[grand_children].iter_mut() {
                    s.parent = Some(scope_id);
                }
            }

            // set the id for the scope on the child symbols of the newly pushed scope.
            if let Some(symbol_grand_children) = symbol_grand_children {
                for s in self.table.scope_symbols[symbol_grand_children]
                    .iter()
                    .copied()
                {
                    self.table.symbols[s].scope = Some(scope_id);
                }
            }

            if idx == 0 {
                first = Some(scope_id);
            }
            last = Some(scope_id)
        }

        if let Some(first) = first {
            let last = last.expect("if first is set, then last must also be set");
            let current = self.pending_scopes.last_mut().unwrap();
            current.children = Some(IdRange::new(first, last));
        }

        // push all the symbols in the right place.
        let mut first = None;
        let mut last = None;
        for (idx, child) in self
            .pending_symbols
            .drain(symbol_child_idx..)
            .rev()
            .enumerate()
        {
            if let Some(shadow) = self.table.symbols[child].shadows {
                let active = self
                    .active_symbols
                    .get_mut(&self.table.symbols[child].declared.index(ast).name)
                    .expect("declared symbol was not active");
                *active = shadow
            } else {
                self.active_symbols
                    .remove(&self.table.symbols[child].declared.index(ast).name)
                    .expect("declared symbol was not active");
            };

            let id = self.table.scope_symbols.push_expect(child);
            if idx == 0 {
                first = Some(id);
            }
            last = Some(id);
        }

        if let Some(first) = first {
            let last = last.expect("if first is set, then last must also be set");
            let current = self.pending_scopes.last_mut().unwrap();
            current.symbols = Some(IdRange::new(first, last));
        }

        r
    }

    fn declare_symbol(
        &mut self,
        ast: &Ast,
        sym: NodeId<ast::Symbol>,
        kind: SymbolKind,
    ) -> Result<(), Diagnostic<'src>> {
        let mut shadows = None;
        match self.active_symbols.entry(ast[sym].name) {
            Entry::Occupied(mut ent) => {
                // TODO: Check for same/different scope
                if !kind.can_shadow_in_scope(self.table.symbols[*ent.get()].kind) {
                    let prev_span = self.table.symbols[*ent.get()].declared.index(ast).span;
                    return Err(Level::Error
                        .title(format!(
                            "Cannot redeclare symbol `{}`",
                            sym.index(ast).name.index(ast)
                        ))
                        .snippet(
                            Snippet::source(self.source)
                                .annotate(AnnotationKind::Primary.span(ast[sym].span))
                                .annotate(
                                    AnnotationKind::Context
                                        .span(prev_span)
                                        .label("Previously declared here"),
                                ),
                        )
                        .to_diagnostic());
                }

                shadows = Some(*ent.get());
                ent.insert(self.table.symbols.next_id_expect());
            }
            Entry::Vacant(ent) => {
                ent.insert(self.table.symbols.next_id_expect());
            }
        }

        let id = self.table.symbols.push_expect(Symbol {
            declared: sym,
            shadows,
            scope: None,
            kind,
            scratch: ScratchFlags::empty(),
        });

        self.table.ast_to_symbol.insert_fill(sym, id);
        self.pending_symbols.push(id);
        Ok(())
    }

    fn use_symbol(
        &mut self,
        ast: &Ast,
        ast_sym: NodeId<ast::Symbol>,
    ) -> Result<(), Diagnostic<'src>> {
        if let Some(sym) = self.active_symbols.get(&ast[ast_sym].name) {
            self.table.ast_to_symbol.insert_fill(ast_sym, *sym);
        } else {
            return Err(Level::Error
                .title(format!(
                    "Use of undeclared symbol `{}`",
                    ast_sym.index(ast).name.index(ast)
                ))
                .snippet(
                    Snippet::source(self.source)
                        .annotate(AnnotationKind::Primary.span(ast[ast_sym].span)),
                )
                .to_diagnostic());
        }
        Ok(())
    }

    fn handle_variant(
        &mut self,
        ast: &Ast,
        params: Option<NodeListId<ast::Parameter>>,
        variant: NodeId<ast::Variant>,
    ) -> Result<(), Diagnostic<'src>> {
        for v in ast.iter_list_node(ast[variant].variations) {
            let ast_sym = match ast[v] {
                Variation::Immediate(n) => n.index(ast).sym,
                Variation::Slot(n) => n.index(ast).sym,
                Variation::Const(n) => n.index(ast).sym,
            };

            let Some(sym) = self.active_symbols.get(&ast[ast_sym].name).copied() else {
                return Err(Level::Error
                    .title(format!(
                        "Use of undeclared parameter `{}` in variant",
                        ast_sym.index(ast).name.index(ast)
                    ))
                    .snippet(
                        Snippet::source(self.source)
                            .annotate(AnnotationKind::Primary.span(ast[ast_sym].span)),
                    )
                    .to_diagnostic());
            };

            assert_eq!(self.table.symbols[sym].kind, SymbolKind::Parameter);

            if self.table.symbols[sym]
                .scratch
                .contains(ScratchFlags::InVariation)
            {
                return Err(Level::Error
                    .title(format!(
                        "Parameter `{}` used twice in the same variant",
                        ast_sym.index(ast).name.index(ast)
                    ))
                    .snippet(
                        Snippet::source(self.source)
                            .annotate(AnnotationKind::Primary.span(ast[ast_sym].span)),
                    )
                    .to_diagnostic());
            }
            self.table.symbols[sym].scratch |= ScratchFlags::InVariation;
            self.table.ast_to_symbol.insert_fill(ast_sym, sym);
        }

        for p in ast.iter_list_node(params) {
            let sym = self
                .table
                .ast_to_symbol
                .get(ast[p].sym)
                .copied()
                .expect("Parameter must already have been declared");

            if !self.table.symbols[sym]
                .scratch
                .contains(ScratchFlags::InVariation)
            {
                return Err(Level::Error
                    .title(format!(
                        "Parameter `{}` missing in the variant `{}`",
                        p.index(ast).sym.index(ast).name.index(ast),
                        variant.index(ast).name.index(ast)
                    ))
                    .snippet(
                        Snippet::source(self.source)
                            .annotate(AnnotationKind::Primary.span(ast[variant].span))
                            .annotate(
                                AnnotationKind::Context
                                    .span(ast[p].sym.index(ast).span)
                                    .label("Missing this parameter"),
                            ),
                    )
                    .to_diagnostic());
            }

            self.table.symbols[sym]
                .scratch
                .remove(ScratchFlags::InVariation);
        }

        Ok(())
    }
}

impl<'src, 't> Visit for SymbolResolvePass<'src, 't> {
    type Error = Diagnostic<'src>;

    fn visit_function(&mut self, ast: &Ast, id: NodeId<ast::Function>) -> Result<(), Self::Error> {
        self.enter_scope(ast, Scope::new(ScopeDeclaration::Function(id)), |this| {
            for p in ast.iter_list_node(ast[id].parameters) {
                this.declare_symbol(ast, ast[p].sym, SymbolKind::Parameter)?;
            }

            visit::visit_expr(this, ast, ast[id].body)
        })
    }

    fn visit_stencil(&mut self, ast: &Ast, id: NodeId<ast::Stencil>) -> Result<(), Self::Error> {
        self.enter_scope(ast, Scope::new(ScopeDeclaration::Stencil(id)), |this| {
            for p in ast.iter_list_node(ast[id].parameters) {
                this.declare_symbol(ast, ast[p].sym, SymbolKind::Parameter)?;
            }

            for v in ast.iter_list_node(ast[id].variants) {
                this.handle_variant(ast, ast[id].parameters, v)?;
            }
            visit::visit_expr(this, ast, ast[id].body)
        })
    }

    fn visit_block(&mut self, ast: &Ast, id: NodeId<ast::Block>) -> Result<(), Self::Error> {
        if let Some(b) = ast[id].body {
            self.enter_scope(ast, Scope::new(ScopeDeclaration::Block(b)), |this| {
                visit::visit_inner_block(this, ast, Some(b))
            })
        } else {
            Ok(())
        }
    }
    fn visit_let(&mut self, ast: &Ast, id: NodeId<ast::Let>) -> Result<(), Self::Error> {
        self.visit_expr(ast, ast[id].expr)?;
        let kind = if ast[id].mutable {
            SymbolKind::LocalMut
        } else {
            SymbolKind::Local
        };
        self.declare_symbol(ast, ast[id].sym, kind)
    }

    fn visit_parameter(&mut self, _: &Ast, _: NodeId<ast::Parameter>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_symbol(&mut self, ast: &Ast, id: NodeId<ast::Symbol>) -> Result<(), Self::Error> {
        self.use_symbol(ast, id)
    }
}
