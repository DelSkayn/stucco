use ast::{visit::Visit, Ast, NodeId, NodeListId};
use common::{id, id::IdVec};
use syn::Ident;

use crate::Error;

id!(SymbolId);
id!(ScopeId);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum SymbolKind {
    Module,
    Variant,
    Local,
    LocalMut,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Symbol {
    declared: NodeId<Ident>,
    scope: ScopeId,
    kind: SymbolKind,
    // TODO:
    ty: (),
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
    ident_to_symbol: IdVec<NodeId<Ident>, SymbolId>,
}

pub fn resolve(root: NodeId<ast::Module>, ast: &Ast) -> Result<Symbols, Error> {
    let mut symbols = Symbols {
        symbols: IdVec::new(),
        scopes: IdVec::new(),
        ident_to_symbol: IdVec::new(),
    };

    let current = symbols.scopes.push(Scope {
        parent: None,
        declared: None,
        childeren: Vec::new(),
    })?;

    Ok(symbols)
}

pub struct Resolver {
    symbols: Symbols,
    current_scope: ScopeId,
}

impl Visit for Resolver {
    type Error = Error;

    fn visit_type(&mut self, _ast: &Ast, _f: NodeId<ast::Type>) -> Result<(), Self::Error> {
        Ok(())
    }
}
