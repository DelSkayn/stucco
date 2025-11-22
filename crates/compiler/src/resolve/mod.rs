//! Resolve identifiers to symbol.

use ast::{Ast, NodeId, visit::Visit};

use crate::Error;

#[cfg(feature = "dev")]
pub mod print;

pub mod symbols;
use symbols::SymbolsResolver;
pub use symbols::{Scope, ScopeId, SymbolId, SymbolTable};

pub struct ResolveInfo {
    pub symbols: SymbolTable,
}

impl ResolveInfo {
    pub fn new() -> ResolveInfo {
        ResolveInfo {
            symbols: SymbolTable::new(),
        }
    }
}

pub fn resolve(root: NodeId<ast::Module>, ast: &Ast, info: &mut ResolveInfo) -> Result<(), Error> {
    let mut resolver = SymbolsResolver::new(&mut info.symbols);
    resolver.visit_module(ast, root)?;
    resolver.finish_resolve(ast)?;
    Ok(())
}
