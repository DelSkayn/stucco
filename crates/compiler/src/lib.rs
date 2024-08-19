use ast::{Ast, NodeId, Span};

pub mod resolve;

#[derive(Debug)]
pub enum Error {
    UndeclaredSymbol(Span),
    PushNode,
}

pub fn compile(ast: Ast, root: NodeId<ast::Module>) -> Result<(), Error> {
    let _symbols = resolve::resolve(root, &ast)?;
    todo!()
}
