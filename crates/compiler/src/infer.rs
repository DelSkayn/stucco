use std::{collections::HashMap, convert::Infallible};

use ast::{
    visit::{self, Visit},
    Ast, NodeId,
};
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
    Ref(Box<Ty>),
    RefMut(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Array(Box<Ty>, usize),
    Unknown,
}

impl Ty {
    pub fn from_ast(ast: &Ast, ty: NodeId<ast::Type>) -> Self {
        match ast[ty] {
            ast::Type::Array(_) => todo!(),
            ast::Type::Fn(x) => Self::from_ast_fn(ast, x),
            ast::Type::Tuple(x) => Self::from_ast_tuple(ast, x),
            ast::Type::Ptr(x) => Self::from_ast_ptr(ast, x),
            ast::Type::Reference(x) => Self::from_ast_ref(ast, x),
            ast::Type::Direct(_) => todo!(),
        }
    }

    pub fn from_ast_tuple(ast: &Ast, ty: NodeId<ast::TypeTuple>) -> Self {
        let types = ast
            .iter_list_node(ast[ty].fields)
            .map(|x| Self::from_ast(ast, x))
            .collect();

        Ty::Tuple(types)
    }

    pub fn from_ast_fn(ast: &Ast, ty: NodeId<ast::TypeFn>) -> Self {
        let params = ast
            .iter_list_node(ast[ty].params)
            .map(|x| Self::from_ast(ast, x))
            .collect();

        let res = if let Some(out) = ast[ty].output {
            Self::from_ast(ast, out)
        } else {
            Ty::Prim(PrimitiveType::Nil)
        };

        Ty::Fn(params, Box::new(res))
    }

    pub fn from_ast_ptr(ast: &Ast, ty: NodeId<ast::TypePtr>) -> Self {
        let ptr_ty = Ty::from_ast(ast, ast[ty].ty);

        if ast[ty].mutable {
            Ty::PtrMut(Box::new(ptr_ty))
        } else {
            Ty::Ptr(Box::new(ptr_ty))
        }
    }

    pub fn from_ast_ref(ast: &Ast, ty: NodeId<ast::TypeReference>) -> Self {
        let ptr_ty = Ty::from_ast(ast, ast[ty].ty);

        if ast[ty].mutable {
            Ty::RefMut(Box::new(ptr_ty))
        } else {
            Ty::Ref(Box::new(ptr_ty))
        }
    }

    pub fn from_ast_array(_ast: &Ast, _ty: NodeId<ast::TypeArray>) -> Self {
        todo!()
    }
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

pub fn collect(ast: &Ast, types: &mut Types, root: NodeId<ast::Module>) {}

struct TypeCollectVisitor<'a> {
    symbols: &'a Symbols,
    types: &'a mut Types,
}

impl<'a> Visit for TypeCollectVisitor<'a> {
    type Error = Infallible;

    fn visit_let(&mut self, ast: &Ast, f: NodeId<ast::Let>) -> Result<(), Self::Error> {
        if let Some(x) = ast[f].ty {
            let ty = Ty::from_ast(ast, x);
            let sym = self.symbols.ast_to_resolved[ast[f].sym].unwrap();
            self.types.symbols.insert_fill(sym, ty, || Ty::Unknown);
        }

        visit::visit_let(self, ast, f)
    }

    fn visit_parameter(&mut self, ast: &Ast, f: NodeId<ast::Parameter>) -> Result<(), Self::Error> {
        let ty = Ty::from_ast(ast, ast[f].ty);
        let sym = self.symbols.ast_to_resolved[ast[f].sym].unwrap();

        self.types.symbols.insert_fill(sym, ty, || Ty::Unknown);

        Ok(())
    }

    fn visit_variant_constant(
        &mut self,
        ast: &Ast,
        f: NodeId<ast::VariantConstant>,
    ) -> Result<(), Self::Error> {
        let ty = Ty::from_ast(ast, ast[f].ty);
        let sym = self.symbols.ast_to_resolved[ast[f].sym].unwrap();

        self.types.symbols.insert_fill(sym, ty, || Ty::Unknown);

        Ok(())
    }

    fn visit_expr(&mut self, ast: &Ast, f: NodeId<ast::Expr>) -> Result<(), Self::Error> {
        let ty = match ast[f] {
            ast::Expr::Let(_)
            | ast::Expr::Continue(_)
            | ast::Expr::Return(_)
            | ast::Expr::Break(_)
            | ast::Expr::Tail(_) => Ty::Prim(PrimitiveType::Nil),

            ast::Expr::Literal(lit) => match &ast[lit] {
                syn::Lit::Str(_) => todo!(),
                syn::Lit::ByteStr(_) => todo!(),
                syn::Lit::CStr(_) => todo!(),
                syn::Lit::Byte(_) => Ty::Prim(PrimitiveType::U8),
                syn::Lit::Char(_) => todo!(),
                syn::Lit::Int(i) => {
                    let prim = match i.suffix() {
                        "isize" => PrimitiveType::Isize,
                        "usize" => PrimitiveType::Usize,
                        "i64" => PrimitiveType::I64,
                        "u64" => PrimitiveType::U64,
                        "i32" => PrimitiveType::I32,
                        "u32" => PrimitiveType::U32,
                        "i16" => PrimitiveType::I16,
                        "u16" => PrimitiveType::U16,
                        "i8" => PrimitiveType::I8,
                        "u8" => PrimitiveType::U8,
                        "" => PrimitiveType::Integer,
                        // Invalid suffixes should have been filtered out by the parser.
                        _ => unreachable!(),
                    };
                    Ty::Prim(prim)
                }
                syn::Lit::Float(f) => {
                    let prim = match f.suffix() {
                        "f64" => PrimitiveType::F64,
                        "f32" => PrimitiveType::F32,
                        "" => PrimitiveType::Float,
                        // Invalid suffixes should have been filtered out by the parser.
                        _ => unreachable!(),
                    };
                    Ty::Prim(prim)
                }
                syn::Lit::Bool(_) => todo!(),
                syn::Lit::Verbatim(_) => unreachable!(),
                _ => unreachable!(),
            },

            ast::Expr::If(_)
            | ast::Expr::Binary(_)
            | ast::Expr::Unary(_)
            | ast::Expr::Block(_)
            | ast::Expr::Cast(_)
            | ast::Expr::Loop(_)
            | ast::Expr::While(_)
            | ast::Expr::Call(_)
            | ast::Expr::Method(_)
            | ast::Expr::Field(_)
            | ast::Expr::Index(_)
            | ast::Expr::Symbol(_)
            | ast::Expr::Covered(_) => return Ok(()),
        };

        self.types.types.insert_fill(f, ty, || Ty::Unknown);
        Ok(())
    }
}
