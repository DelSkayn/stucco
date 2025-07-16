#![allow(dead_code)]

use std::{collections::HashMap, fmt::Write as _};

use ast::{Ast, NodeId};
use compiler::{
    infer::{PrimTy, Ty, TyId, Types},
    resolve::{SymbolId, Symbols},
};
use inkwell::{
    AddressSpace,
    attributes::Attribute,
    builder::Builder,
    context::Context,
    llvm_sys::LLVMCallConv,
    module::{Linkage, Module},
    targets::{FileType, Target as LLVMTarget, TargetMachine, TargetTriple},
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType},
    values::{AnyValue, LLVMTailCallKind},
};
use ir::{VariantGen, VariantModule};
pub use target::Target;
use token::token::{IntType, Lit};
use util::{NonBasicTypeEnum, try_any_to_basic};
use value::Value;
use wrapper::GlobalValueExt;

mod ir;
mod obj;
mod target;
pub mod util;
mod value;
mod wrapper;

#[cfg(not(any(feature = "stand-alone", feature = "proc-macro")))]
compile_error!(
    "Missing feature on stucco_codegen, either feature stand-alone or feature proc-macro must be enabled"
);

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
    pub symbols: Symbols,
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

impl CodeGen {
    pub fn new(ast: Ast, symbols: Symbols, types: Types, config: Config) -> Self {
        let context = Context::create();
        CodeGen {
            context,
            ast,
            symbols,
            types,
            config,
        }
    }

    pub fn generate_variant<'ctx>(
        &'ctx self,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) -> VariantModule<'ctx> {
        VariantGen::build(&self, stencil, variant)
    }
}
