use ast::{Ast, NodeId};
use compiler::{infer::Types, resolve::Symbols};
use inkwell::{builder::Builder, context::Context, module::Module};

mod obj;
mod target;

#[cfg(not(any(feature = "stand-alone", feature = "proc-macro")))]
compile_error!(
    "Missing feature on stucco_codegen, either feature stand-alone or feature proc-macro must be enabled"
);

pub struct CodeGen {
    context: Context,
}

impl CodeGen {
    pub fn new() -> Self {
        let context = Context::create();
        CodeGen { context }
    }

    pub fn generate_variation(
        ast: &Ast,
        symbols: &Symbols,
        types: &Types,
        stencil: NodeId<ast::Stencil>,
        variation: NodeId<ast::Variation>,
    ) {
    }
}

pub struct VariationGen<'ctx, 't> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    ast: &'t Ast,
    symbols: &'t Symbols,
    types: &'t Types,
    id_gen: usize,
}

impl<'ctx, 't> VariationGen<'ctx, 't> {
    pub fn new(
        context: &'ctx Context,
        ast: &'t Ast,
        symbols: &'t Symbols,
        types: &'t Types,
        stencil: NodeId<ast::Stencil>,
    ) -> Self {
        let module = context.create_module(
            &stencil
                .index(ast)
                .sym
                .index(ast)
                .name
                .index(ast)
                .to_string(),
        );
        let builder = context.create_builder();

        VariationGen {
            context,
            ast,
            symbols,
            types,
            module,
            builder,
            id_gen: 0,
        }
    }

    pub fn generate_variation(
        &mut self,
        stencil: NodeId<ast::Stencil>,
        variation: NodeId<ast::Variation>,
    ) {
    }
}
