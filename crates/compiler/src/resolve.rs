//! Resolve identifiers to symbol.

use std::ops::Range;
use std::{collections::HashMap, u32};

use ast::{
    Ast, NodeId, NodeListId,
    visit::{self, Visit},
};
use ast::{AstSpanned, Stencil};
use common::{id, id::IdVec};
use syn::Ident;

use crate::Error;

id!(SymbolId);
id!(ScopeId);
id!(ScopeSymbolId);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum SymbolKind {
    Module,
    Stencil,
    Parameter,
    Local,
    LocalMut,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ScopeKind {
    Root,
    Function,
}

bitflags::bitflags! {
    #[derive(Debug, Eq, PartialEq, Clone,Copy)]
    struct ScratchFlags: u8{
        const InVariation = 0x1;
    }

}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Symbol {
    declared: NodeId<ast::Symbol>,
    shadows: Option<SymbolId>,
    scope: Option<ScopeId>,
    kind: SymbolKind,
    scratch: ScratchFlags,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ScopeDeclaration {
    Root,
    StencilFunction(NodeId<Stencil>),
    Block(NodeListId<ast::Expr>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scope {
    parent: Option<ScopeId>,
    declared: ScopeDeclaration,
    children: Range<ScopeId>,
    symbols: Range<ScopeSymbolId>,
}

impl Scope {
    pub fn new(decl: ScopeDeclaration) -> Self {
        Scope {
            parent: None,
            declared: decl,
            children: ScopeId::MIN..ScopeId::MIN,
            symbols: ScopeSymbolId::MIN..ScopeSymbolId::MIN,
        }
    }
}

/// Struct containing the symbol table.
/// Symbols and scopes are stored into two list each have their values pushed in depth first
/// order.
///
/// To get the children of a scope for example one looks up the scope in the scope list via it's index and
/// then get's the children range which can then be used on the same list to get the children scope.
#[derive(Clone, Debug)]
pub struct Symbols {
    pub symbols: IdVec<SymbolId, Symbol>,
    pub scopes: IdVec<ScopeId, Scope>,
    pub scope_symbols: IdVec<ScopeSymbolId, SymbolId>,
    pub ast_to_symbol: IdVec<NodeId<ast::Symbol>, Option<SymbolId>>,
    pub block_to_scope: IdVec<NodeListId<ast::Expr>, Option<ScopeId>>,
}

pub fn resolve(root: NodeId<ast::Module>, ast: &Ast) -> Result<Symbols, Error> {
    let symbols = Symbols {
        symbols: IdVec::new(),
        scopes: IdVec::new(),
        scope_symbols: IdVec::new(),
        ast_to_symbol: IdVec::new(),
        block_to_scope: IdVec::new(),
    };

    let mut resolver = Resolver {
        symbols,
        active_symbols: HashMap::new(),
        pending_symbols: Vec::new(),
        pending_scopes: vec![Scope::new(ScopeDeclaration::Root)],
    };
    resolver.visit_module(ast, root)?;
    resolver.finish_resolve(ast)?;

    Ok(resolver.symbols)
}

pub struct Resolver {
    symbols: Symbols,
    /// Hash map of active symbols mapped by identifier.
    active_symbols: HashMap<NodeId<Ident>, SymbolId>,
    /// list of pending symbols along with the index of the symbol which this symbol shadowed.
    /// Shadowing happens when a symbol with the same name is declared in a child scope.
    pending_symbols: Vec<SymbolId>,
    pending_scopes: Vec<Scope>,
}

impl Resolver {
    pub fn finish_scope(
        &mut self,
        ast: &Ast,
        next_scope: usize,
        next_symbol: usize,
    ) -> Result<(), Error> {
        // push all the child scopes
        let start = self.symbols.scopes.next_id().ok_or(Error::PushNode)?;
        for s in self.pending_scopes.drain(next_scope..) {
            let grand_child_range = s.children.clone();
            let grand_symbols_range = s.symbols.clone();
            let declared = s.declared.clone();

            let id = self.symbols.scopes.push(s).ok_or(Error::PushNode)?;
            if let ScopeDeclaration::Block(x) = declared {
                self.symbols
                    .block_to_scope
                    .insert_fill(x, Some(id), || None)
            }

            // update the parent for the current grand children since this child now has an id.
            for s in self.symbols.scopes[grand_child_range].iter_mut() {
                s.parent = Some(id);
            }
            for s in self.symbols.scope_symbols[grand_symbols_range]
                .iter()
                .copied()
            {
                self.symbols.symbols[s].scope = Some(id);
            }
        }
        let end = self.symbols.scopes.next_id().ok_or(Error::PushNode)?;
        self.pending_scopes[next_scope - 1].children = start..end;

        // push all the symbols for this scope.
        let start = self
            .symbols
            .scope_symbols
            .next_id()
            .ok_or(Error::PushNode)?;
        // first update the active_symbols list since this needs to be done in reverse order.
        for s in self.pending_symbols[next_symbol..].iter().rev().copied() {
            // update active_symbols list.
            let ident = self.symbols.symbols[s].declared;
            if let Some(shadows) = self.symbols.symbols[s].shadows {
                let Some(x) = self.active_symbols.get_mut(&ast[ident].name) else {
                    panic!("Shadowed variable not previously declared")
                };
                *x = shadows
            } else {
                let was_declared = self.active_symbols.remove(&ast[ident].name).is_some();
                assert!(was_declared);
            }
        }

        // then push all the symbols
        for s in self.pending_symbols.drain(next_symbol..) {
            self.symbols.scope_symbols.push(s).ok_or(Error::PushNode)?;
        }

        let end = self
            .symbols
            .scope_symbols
            .next_id()
            .ok_or(Error::PushNode)?;
        self.pending_scopes[next_scope - 1].symbols = start..end;

        Ok(())
    }

    pub fn finish_resolve(&mut self, ast: &Ast) -> Result<(), Error> {
        self.finish_scope(ast, 1, 0)?;

        assert_eq!(self.pending_scopes.len(), 1);
        let scope = self.pending_scopes.pop().unwrap();
        let grand_child_range = scope.children.clone();
        let grand_symbols_range = scope.symbols.clone();
        let declared = scope.declared.clone();
        let id = self.symbols.scopes.push(scope).ok_or(Error::PushNode)?;
        if let ScopeDeclaration::Block(x) = declared {
            self.symbols
                .block_to_scope
                .insert_fill(x, Some(id), || None)
        }

        // update the parent for the current grand children since this child now has an id.
        for s in self.symbols.scopes[grand_child_range].iter_mut() {
            s.parent = Some(id);
        }
        for s in self.symbols.scope_symbols[grand_symbols_range]
            .iter()
            .copied()
        {
            self.symbols.symbols[s].scope = Some(id);
        }
        Ok(())
    }

    pub fn declare_symbol(
        &mut self,
        ast: &Ast,
        sym: NodeId<ast::Symbol>,
        kind: SymbolKind,
    ) -> Result<SymbolId, Error> {
        let idx = self.symbols.symbols.next_id().ok_or(Error::PushNode)?;
        let shadows = self.active_symbols.insert(ast[sym].name, idx);

        if let Some(shadows) = shadows {
            match self.symbols.symbols[shadows].kind {
                SymbolKind::Module => {}
                SymbolKind::Stencil => match kind {
                    SymbolKind::Stencil => {
                        return Err(Error::RedeclaredFunction(sym.ast_span(&ast)));
                    }
                    SymbolKind::Module
                    | SymbolKind::Parameter
                    | SymbolKind::Local
                    | SymbolKind::LocalMut => {}
                },
                SymbolKind::Parameter => match kind {
                    SymbolKind::Parameter => {
                        let cur = sym.ast_span(&ast);
                        let original = self.symbols.symbols[idx].declared.ast_span(&ast);

                        return Err(Error::RedeclaredParameter {
                            redecl: cur,
                            original,
                        });
                    }
                    SymbolKind::Stencil
                    | SymbolKind::Module
                    | SymbolKind::Local
                    | SymbolKind::LocalMut => {}
                },
                SymbolKind::Local | SymbolKind::LocalMut => {}
            }
        }

        let id = self
            .symbols
            .symbols
            .push(Symbol {
                declared: sym,
                shadows,
                kind,
                scope: None,
                scratch: ScratchFlags::empty(),
            })
            .ok_or(Error::PushNode)?;

        self.pending_symbols.push(id);
        self.symbols
            .ast_to_symbol
            .insert_fill(sym, Some(id), || None);

        Ok(id)
    }
}

impl Visit for Resolver {
    type Error = Error;

    fn visit_let(&mut self, ast: &Ast, f: NodeId<ast::Let>) -> Result<(), Self::Error> {
        self.visit_expr(ast, f.index(ast).expr)?;

        let kind = if ast[f].mutable {
            SymbolKind::LocalMut
        } else {
            SymbolKind::Local
        };

        self.declare_symbol(ast, ast[f].sym, kind)?;
        Ok(())
    }

    fn visit_become(&mut self, ast: &Ast, t: NodeId<ast::Become>) -> Result<(), Self::Error> {
        let mut cur = ast[t].args;
        while let Some(x) = ast.next_list(&mut cur) {
            self.visit_expr(ast, x)?;
        }
        Ok(())
    }

    fn visit_field(&mut self, ast: &Ast, f: NodeId<ast::Field>) -> Result<(), Self::Error> {
        self.visit_expr(ast, ast[f].base)
    }

    fn visit_method(&mut self, ast: &Ast, f: NodeId<ast::Method>) -> Result<(), Self::Error> {
        let mut cur = ast[f].args;
        while let Some(x) = ast.next_list(&mut cur) {
            self.visit_expr(ast, x)?;
        }

        self.visit_expr(ast, ast[f].receiver)
    }

    fn visit_symbol(&mut self, ast: &Ast, sym: NodeId<ast::Symbol>) -> Result<(), Self::Error> {
        let Some(s) = self.active_symbols.get(&sym.index(ast).name) else {
            return Err(Error::UndeclaredSymbol {
                symbol: sym.ast_span(ast),
            });
        };

        self.symbols
            .ast_to_symbol
            .insert_fill(sym, Some(*s), || None);

        Ok(())
    }

    fn visit_stencil(&mut self, ast: &Ast, f: NodeId<ast::Stencil>) -> Result<(), Self::Error> {
        self.declare_symbol(ast, f.index(ast).sym, SymbolKind::Stencil)?;

        self.pending_scopes
            .push(Scope::new(ScopeDeclaration::StencilFunction(f)));

        let next_scope = self.pending_scopes.len();
        let next_symbol = self.pending_symbols.len();

        for param in ast.iter_list_node(ast[f].parameters) {
            self.declare_symbol(ast, ast[param].sym, SymbolKind::Parameter)?;
        }
        // Resolve variations
        //
        // Ensures both that variations are defining all parameters, and that they are not
        // duplicated.
        for var in ast.iter_list_node(ast[f].variants) {
            // resolve the variant
            for variation in ast.iter_list_node(ast[var].variations) {
                let sym = match ast[variation] {
                    ast::Variation::Constant(node_id) => ast[node_id].sym,
                    ast::Variation::Slot(node_id) => ast[node_id].sym,
                };
                self.visit_symbol(ast, sym)?;
                let symbol_id = self.symbols.ast_to_symbol[sym].unwrap();
                if self.symbols.symbols[symbol_id]
                    .scratch
                    .contains(ScratchFlags::InVariation)
                {
                    return Err(Error::RedeclaredVariation(ast[sym].span));
                }
                self.symbols.symbols[symbol_id].scratch |= ScratchFlags::InVariation;
            }

            // ensure a variant contains all parameters.
            for param in ast.iter_list_node(ast[f].parameters) {
                let symbol_id = self.symbols.ast_to_symbol[ast[param].sym].unwrap();
                if !self.symbols.symbols[symbol_id]
                    .scratch
                    .contains(ScratchFlags::InVariation)
                {
                    return Err(Error::VariationMissingParameter {
                        variation: ast[var].span,
                        parameter: ast[param].span,
                    });
                }
                self.symbols.symbols[symbol_id]
                    .scratch
                    .remove(ScratchFlags::InVariation);
            }
        }

        visit::visit_expr(self, ast, ast[f].body)?;

        self.finish_scope(ast, next_scope, next_symbol)
    }

    fn visit_inner_block(
        &mut self,
        ast: &Ast,
        f: Option<NodeListId<ast::Expr>>,
    ) -> Result<(), Self::Error> {
        let Some(body) = f else { return Ok(()) };

        self.pending_scopes
            .push(Scope::new(ScopeDeclaration::Block(body)));

        let next_scope = self.pending_scopes.len();
        let next_symbol = self.pending_symbols.len();

        visit::visit_inner_block(self, ast, f)?;

        self.finish_scope(ast, next_scope, next_symbol)
    }

    fn visit_type(&mut self, _ast: &Ast, _f: NodeId<ast::Type>) -> Result<(), Self::Error> {
        Ok(())
    }
}
