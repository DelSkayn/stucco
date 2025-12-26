//! Resolve identifiers to symbol.

use ast::{Ast, NodeId};

pub mod symbols;
pub mod types;
use error::Diagnostic;
pub use symbols::{Scope, ScopeId, SymbolId, SymbolTable, pass::SymbolResolvePass};
pub use types::{TypeTable, pass::TypeResolvePass};

pub struct ResolveInfo {
    pub symbols: SymbolTable,
    pub types: TypeTable,
}

impl ResolveInfo {
    pub fn new() -> ResolveInfo {
        ResolveInfo {
            symbols: SymbolTable::new(),
            types: TypeTable::new(),
        }
    }
}

pub fn resolve<'a>(
    source: &'a str,
    root: NodeId<ast::Module>,
    ast: &Ast,
    info: &mut ResolveInfo,
) -> Result<ScopeId, Diagnostic<'a>> {
    let scope_id = SymbolResolvePass::new(source, &mut info.symbols).pass(ast, root)?;
    TypeResolvePass::new(source, &mut info.types).pass(ast, root)?;
    Ok(scope_id)
}
