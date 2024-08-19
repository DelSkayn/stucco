//! Resolve identifiers to symbol.

use ast::{
    visit::{self, Visit},
    Ast, NodeId, NodeListId,
};
use common::{id, id::IdVec};
use syn::Ident;

use crate::Error;

id!(SymbolId);
id!(ScopeId);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum SymbolKind {
    Module,
    StencilFunction,
    Variant,
    Local,
    LocalMut,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Symbol {
    declared: NodeId<Ident>,
    scope: ScopeId,
    kind: SymbolKind,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scope {
    parent: Option<ScopeId>,
    declared: Option<NodeListId<ast::Expr>>,
    childeren: Vec<ScopeId>,
}

pub struct Symbols {
    symbols: IdVec<SymbolId, Symbol>,
    scopes: IdVec<ScopeId, Scope>,
    ident_to_symbol: IdVec<NodeId<Ident>, Option<SymbolId>>,
    block_to_scope: IdVec<NodeListId<ast::Expr>, Option<ScopeId>>,
}

pub fn resolve(root: NodeId<ast::Module>, ast: &Ast) -> Result<Symbols, Error> {
    let mut symbols = Symbols {
        symbols: IdVec::new(),
        scopes: IdVec::new(),
        ident_to_symbol: IdVec::new(),
        block_to_scope: IdVec::new(),
    };

    let current_scope = symbols
        .scopes
        .push(Scope {
            parent: None,
            declared: None,
            childeren: Vec::new(),
        })
        .ok_or(Error::PushNode)?;

    let mut resolver = Resolver {
        symbols,
        current_scope,
    };
    resolver.visit_module(ast, root)?;

    Ok(resolver.symbols)
}

pub struct Resolver {
    symbols: Symbols,
    current_scope: ScopeId,
}

impl Visit for Resolver {
    type Error = Error;

    fn visit_let(&mut self, ast: &Ast, f: NodeId<ast::Let>) -> Result<(), Self::Error> {
        let kind = if ast[f].mutable {
            SymbolKind::LocalMut
        } else {
            SymbolKind::Local
        };

        let id = self
            .symbols
            .symbols
            .push(Symbol {
                declared: ast[f].name,
                scope: self.current_scope,
                kind,
            })
            .ok_or(Error::PushNode)?;

        let ident = ast[f].name;

        self.symbols
            .ident_to_symbol
            .insert_fill(ident, Some(id), || None);

        Ok(())
    }

    fn visit_ident(&mut self, ast: &Ast, f: NodeId<Ident>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_inner_block(
        &mut self,
        ast: &Ast,
        f: Option<NodeListId<ast::Expr>>,
    ) -> Result<(), Self::Error> {
        let Some(f) = f else { return Ok(()) };

        let id = self
            .symbols
            .scopes
            .push(Scope {
                parent: Some(self.current_scope),
                declared: Some(f),
                childeren: Vec::new(),
            })
            .ok_or(Error::PushNode)?;

        self.symbols
            .block_to_scope
            .insert_fill(f, Some(id), || None);

        self.symbols.scopes[self.current_scope].childeren.push(id);
        self.current_scope = id;

        visit::visit_inner_block(self, ast, Some(f))?;

        self.current_scope = self.symbols.scopes[self.current_scope].parent.unwrap();

        Ok(())
    }

    fn visit_type(&mut self, _ast: &Ast, _f: NodeId<ast::Type>) -> Result<(), Self::Error> {
        Ok(())
    }
}
