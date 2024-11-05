use std::collections::HashMap;

use ast::{Ast, NodeId};
use common::id::IdVec;
use syn::Ident;

use crate::resolve::{SymbolId, Symbols};

#[derive(Debug)]
pub enum PrimitiveType {
    Nil,
    Bool,
    Usize,
    Isize,
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
    F32,
    F64,
    /// Unconstraint float type.
    /// Type of `1.0` before being constrained down to a specific type.
    Float,
    /// Unconstraint integer.
    /// Type of `1` before being constrained down to a specific type.
    Integer,
}

pub struct Struct {
    fields: Vec<(Ident, Ty)>,
}

#[derive(Debug)]
pub enum Ty {
    Prim(PrimitiveType),
    Ptr(Box<Ty>),
    PtrMut(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Unknown,
}

pub struct Types {
    pub types: IdVec<NodeId<ast::Expr>, Ty>,
    pub symbols: IdVec<SymbolId, Ty>,
}

pub fn infer(
    symbols: &Symbols,
    ast: &Ast,
    root: NodeId<ast::Module>,
) -> Result<Types, crate::Error> {
    let mut types = Types {
        types: IdVec::new(),
        symbols: IdVec::with_capacity(symbols.symbols.len()),
    };

    todo!()
}
