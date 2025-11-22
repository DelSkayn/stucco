use ast::{Module, NodeId};
use codegen::{CodeGen, StencilSet};
use compiler::{type_check::Types, resolve::ResolveInfo};
use parser::{Parser, parse_external_module};
use proc_macro2::{Ident, TokenStream};

pub enum CompilationError {
    Parse(parser::Error),
    Resolve(compiler::Error),
    Types(Types, compiler::type_check::TypeError),
}

pub struct CompilationResult {
    pub root: NodeId<Module>,
    pub stencils: StencilSet,
    pub code_gen: CodeGen,
}

pub fn compile(
    stream: TokenStream,
    module_name: Ident,
) -> Result<CompilationResult, CompilationError> {
    let (root, ast) = Parser::parse_stream_func(stream, move |parser: &mut Parser| {
        // TODO: Overhaul this whole external/internal module stuff.
        let module = parse_external_module(&mut (*parser))?;

        let span = module_name.span();
        let sym = parser.push_set(module_name)?;
        let sym = parser.push(ast::Symbol {
            span: span.into(),
            name: sym,
        })?;
        parser[module].sym = Some(sym);
        Ok(module)
    })
    .map_err(CompilationError::Parse)?;

    let mut info = ResolveInfo::new();
    compiler::resolve::resolve(root, &ast, &mut info).map_err(CompilationError::Resolve)?;
    let mut types = compiler::type_check::Types::new();
    match types.infer(&ast, &info.symbols, root) {
        Ok(_) => {}
        Err(e) => return Err(CompilationError::Types(types, e)),
    };

    let code_gen = codegen::CodeGen::new(ast, info.symbols, types, Default::default());
    let stencils = code_gen.generate_stencil_set(root, codegen::Target::X86_64);
    Ok(CompilationResult {
        root,
        stencils,
        code_gen,
    })
}
