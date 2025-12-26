use std::hash::Hash;
use std::ops::RangeInclusive;
use std::u32;

use ast::{Function, Stencil};
use ast::{NodeId, NodeListId};
use common::id::PartialIndexMap;
use common::{id, id::IndexMap};

pub mod pass;
#[cfg(feature = "dev")]
pub mod print;

id!(SymbolId);
id!(ScopeId);
id!(ScopeSymbolId);

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum SymbolKind {
    Module,
    Stencil,
    Function,
    BuiltinFunction,
    Parameter,
    Local,
    LocalMut,
}

impl SymbolKind {
    pub fn can_shadow_in_scope(&self, other: SymbolKind) -> bool {
        match self {
            SymbolKind::Module => other != SymbolKind::Module,
            SymbolKind::Stencil | SymbolKind::Function | SymbolKind::BuiltinFunction => match other
            {
                SymbolKind::Stencil | SymbolKind::Function | SymbolKind::BuiltinFunction => false,
                SymbolKind::Module
                | SymbolKind::Parameter
                | SymbolKind::Local
                | SymbolKind::LocalMut => true,
            },
            SymbolKind::Local | SymbolKind::LocalMut => true,
            SymbolKind::Parameter => other != SymbolKind::Parameter,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ScopeKind {
    Root,
    Function,
}

bitflags::bitflags! {
    #[derive(Debug, Eq, PartialEq, Clone,Copy)]
    pub struct ScratchFlags: u8{
        const InVariation = 0x1;
    }

}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Symbol {
    pub declared: NodeId<ast::Symbol>,
    pub shadows: Option<SymbolId>,
    pub scope: Option<ScopeId>,
    pub kind: SymbolKind,
    pub scratch: ScratchFlags,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ScopeDeclaration {
    Root,
    Stencil(NodeId<Stencil>),
    Function(NodeId<Function>),
    Block(NodeListId<ast::Expr>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub declared: ScopeDeclaration,
    pub children: Option<RangeInclusive<ScopeId>>,
    pub symbols: Option<RangeInclusive<ScopeSymbolId>>,
}

impl Scope {
    pub fn new(decl: ScopeDeclaration) -> Self {
        Scope {
            parent: None,
            declared: decl,
            children: None,
            symbols: None,
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
pub struct SymbolTable {
    pub symbols: IndexMap<SymbolId, Symbol>,
    pub scopes: IndexMap<ScopeId, Scope>,
    pub scope_symbols: IndexMap<ScopeSymbolId, SymbolId>,
    pub ast_to_symbol: PartialIndexMap<NodeId<ast::Symbol>, SymbolId>,
    pub block_to_scope: PartialIndexMap<NodeListId<ast::Expr>, ScopeId>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: IndexMap::new(),
            scopes: IndexMap::new(),
            scope_symbols: IndexMap::new(),
            ast_to_symbol: PartialIndexMap::new(),
            block_to_scope: PartialIndexMap::new(),
        }
    }
}
