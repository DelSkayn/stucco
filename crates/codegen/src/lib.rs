#![allow(dead_code)]

use std::{collections::HashMap, convert::Infallible};

use ast::{Ast, NodeId, visit::Visit};
use compiler::{
    resolve::SymbolTable,
    type_check::{PrimTy, Ty, Types},
};
use inkwell::context::Context;
use ir::{VariantGen, VariantModule};
pub use target::Target;

mod ir;
mod obj;
mod target;
pub mod util;
mod value;
mod wrapper;
pub use obj::{Immediate, Jump, Stencil, StencilSet, StencilVariant};

#[cfg(not(any(feature = "stand-alone", feature = "proc-macro")))]
compile_error!(
    "Missing feature on stucco_codegen, either feature stand-alone or feature proc-macro must be enabled"
);
#[cfg(all(feature = "stand-alone", feature = "proc-macro"))]
compile_error!("Can't have both feature stand-alone and proc-macro enabled at the same time");

pub struct Config {
    pub num_passing_register: usize,
    pub clobber_immediates: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            num_passing_register: 8,
            clobber_immediates: true,
        }
    }
}

pub struct CodeGen {
    context: Context,
    pub ast: Ast,
    pub symbols: SymbolTable,
    pub types: Types,
    pub config: Config,
}

enum NumberType {
    Float,
    Signed,
    Unsigned,
}

impl NumberType {
    pub fn from_ty(ty: &Ty) -> Option<Self> {
        match ty {
            Ty::Prim(p) => match p {
                PrimTy::Usize | PrimTy::U64 | PrimTy::U32 | PrimTy::U16 | PrimTy::U8 => {
                    Some(NumberType::Unsigned)
                }
                PrimTy::Isize | PrimTy::I64 | PrimTy::I32 | PrimTy::I16 | PrimTy::I8 => {
                    Some(NumberType::Signed)
                }
                PrimTy::F32 | PrimTy::F64 => Some(NumberType::Float),
                _ => None,
            },
            _ => None,
        }
    }
}

struct StencilVisitor<F>(F);

impl<F: FnMut(NodeId<ast::Stencil>)> Visit for StencilVisitor<F> {
    type Error = Infallible;

    fn visit_function(
        &mut self,
        _: &ast::Ast,
        _: NodeId<ast::Function>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_struct(&mut self, _: &ast::Ast, _: NodeId<ast::Struct>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_stencil(&mut self, _: &ast::Ast, m: NodeId<ast::Stencil>) -> Result<(), Self::Error> {
        (self.0)(m);
        Ok(())
    }
}

impl CodeGen {
    pub fn new(ast: Ast, symbols: SymbolTable, types: Types, config: Config) -> Self {
        let context = Context::create();
        CodeGen {
            context,
            ast,
            symbols,
            types,
            config,
        }
    }

    pub fn generate_stencil_set(
        &self,
        root: NodeId<ast::Module>,
        target: Target,
    ) -> obj::StencilSet {
        let entry = self.generate_entry();
        let object = entry.into_object(target);
        let entry = obj::extract_entry_stencil(&object);

        let mut set = obj::StencilSet {
            passing_registers: self.config.num_passing_register,
            entry,
            stencils: HashMap::new(),
        };

        let _ = StencilVisitor(|s| {
            let mut stencil = obj::Stencil {
                variants: Vec::new(),
            };
            for v in self.ast.iter_list_node(self.ast[s].variants) {
                let module = self.generate_variant(s, v);
                let object = module.into_object(target);
                let variant = obj::extract_stencil_variant(
                    self.ast[v].name.index(&self.ast).to_string(),
                    &object,
                );
                stencil.variants.push(variant);
            }
            set.stencils.insert(
                self.ast[s]
                    .sym
                    .index(&self.ast)
                    .name
                    .index(&self.ast)
                    .to_string(),
                stencil,
            );
        })
        .visit_module(&self.ast, root);

        set
    }

    pub fn generate_variant<'ctx>(
        &'ctx self,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) -> VariantModule<'ctx> {
        VariantGen::build(&self, stencil, variant)
    }

    pub fn generate_entry<'ctx>(&'ctx self) -> VariantModule<'ctx> {
        VariantGen::generate_entry(self)
    }
}
