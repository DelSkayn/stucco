mod pass;
#[cfg(feature = "dev")]
pub mod print;

use crate::{
    resolve::{SymbolId, SymbolTable},
    type_check::pass::InferUpPass,
};
use ast::{Ast, Expr, Method, NodeId, visit::Visit};
use common::{
    id,
    id::{Id, IndexMap},
};
use std::{cell::Cell, collections::HashMap, fmt::Write, hash::Hash};
use token::{
    Span,
    token::{Ident, IntType, Lit},
};

id!(TyId);
id!(ConstraintId);

#[derive(Debug)]
pub enum TypeError {
    Mismatch(TyId, TyId),
    ArraySize(usize, usize),
    TupleSize(TyId, TyId),
    ArgumentSize(TyId, TyId),
    TypeAnnotationRequired(TyId),
    InfiniteType,
    TypeMustBeKnown(NodeId<Method>),
    UnknownMethod(NodeId<Ident>, TyId),
    InvalidArity(NodeId<Method>, usize),
    UnknownType(NodeId<ast::Type>, Span),
    LiteralOverflow(NodeId<Lit>, TyId),
    InvalidCast { from: TyId, to: TyId },
    CantInfer(TyId),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum PrimTy {
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
    /// Type for expressions which don't have an value but diverge. `!` in rust.
    Diverges,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Narrowing {
    Integer,
    None,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Ty {
    Prim(PrimTy),
    Ptr(TyId),
    PtrMut(TyId),
    Ref(TyId),
    RefMut(TyId),
    Tuple(Vec<TyId>),
    Fn(Vec<TyId>, TyId),
    Array(TyId, usize),
    Var(Narrowing, Cell<Option<TyId>>),
    Struct(Vec<(NodeId<Ident>, TyId)>),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TySource {
    id: TyId,
    span: Span,
}

pub struct FunctionInfo {
    ty: TyId,
}

pub struct Types {
    pub type_graph: IndexMap<TyId, Ty>,
    pub type_spans: IndexMap<TyId, Option<Span>>,
    pub block_type: IndexMap<NodeId<ast::Block>, Option<TyId>>,
    pub expr_to_type: IndexMap<NodeId<ast::Expr>, Option<TyId>>,
    pub name_to_type: IndexMap<NodeId<ast::TypeName>, Option<TyId>>,
    pub symbol_to_type: IndexMap<SymbolId, Option<TyId>>,
    pub type_methods: HashMap<TyId, HashMap<String, FunctionInfo>>,
}

impl Types {
    pub fn new() -> Self {
        let mut res = Self {
            type_graph: IndexMap::new(),
            type_spans: IndexMap::new(),
            block_type: IndexMap::new(),
            expr_to_type: IndexMap::new(),
            symbol_to_type: IndexMap::new(),
            name_to_type: IndexMap::new(),
            type_methods: HashMap::new(),
        };

        res.init();

        res
    }

    pub fn ty_ast(&self, n: NodeId<ast::Expr>) -> Option<&Ty> {
        let n = self.find_type_ast(n)?;
        Some(&self.type_graph[n])
    }

    pub fn ty_block(&self, n: NodeId<ast::Block>) -> Option<&Ty> {
        let n = self.find_type_block(n)?;
        Some(&self.type_graph[n])
    }

    pub fn find_type_ast(&self, n: NodeId<ast::Expr>) -> Option<TyId> {
        let ty = self.expr_to_type.get(n).copied()??;
        Some(self.find_type(ty))
    }

    pub fn find_type_block(&self, n: NodeId<ast::Block>) -> Option<TyId> {
        let ty = self.block_type.get(n).copied()??;
        Some(self.find_type(ty))
    }

    pub fn find_type_symbol(&self, n: SymbolId) -> Option<TyId> {
        let ty = self.symbol_to_type.get(n).copied()??;
        Some(self.find_type(ty))
    }

    pub fn find_type(&self, mut ty: TyId) -> TyId {
        while let Ty::Var(_, ref forward) = self.type_graph[ty] {
            let Some(forward) = forward.get() else {
                return ty;
            };
            ty = forward;
        }

        return ty;
    }

    fn make_equal_to(&self, a: TyId, b: TyId) -> Result<(), TypeError> {
        let end = self.find_type(a);
        let Ty::Var(ref narrow, ref forward) = self.type_graph[end] else {
            panic!("type was already resolved");
        };
        match narrow {
            Narrowing::None => {}
            Narrowing::Integer => match self.type_graph[b] {
                Ty::Prim(PrimTy::I64 | PrimTy::U64 | PrimTy::Isize | PrimTy::Usize) => {}
                Ty::Prim(_) => return Err(TypeError::Mismatch(a, b)),
                _ => {}
            },
        }
        forward.set(Some(b));
        Ok(())
    }

    fn occurs_in(&self, ty: TyId, inside: TyId) -> bool {
        match self.type_graph[inside] {
            Ty::Var(..) | Ty::Prim(_) => false,
            Ty::Ptr(x) | Ty::PtrMut(x) | Ty::Ref(x) | Ty::RefMut(x) | Ty::Array(x, _) => {
                x == ty || self.occurs_in(ty, x)
            }
            Ty::Tuple(ref x) => x.iter().copied().any(|x| x == ty || self.occurs_in(ty, x)),
            Ty::Fn(ref x, r) => {
                x.contains(&r)
                    || self.occurs_in(ty, r)
                    || x.iter().copied().any(|x| x == ty || self.occurs_in(ty, x))
            }
        }
    }

    /// Returns if type a and b are the same type,
    /// will return None if either a or b is not yet resolved.
    fn same_type(&self, a_id: TyId, b_id: TyId) -> Option<bool> {
        let a = self.find_type(a_id);
        let b = self.find_type(b_id);

        match (&self.type_graph[a], &self.type_graph[b]) {
            (Ty::Var(..), _) | (_, Ty::Var(..)) => None,
            (Ty::Ptr(a), Ty::Ptr(b))
            | (Ty::PtrMut(a), Ty::PtrMut(b))
            | (Ty::Ref(a), Ty::Ref(b))
            | (Ty::RefMut(a), Ty::RefMut(b)) => {
                return self.same_type(*a, *b);
            }
            (Ty::Array(a, s1), Ty::Array(b, s2)) => {
                if s1 != s2 {
                    return Some(false);
                }
                return self.same_type(*a, *b);
            }
            (Ty::Tuple(tup1), Ty::Tuple(tup2)) => {
                if tup1.len() != tup2.len() {
                    return Some(false);
                }
                for (a, b) in tup1.iter().copied().zip(tup2.iter().copied()) {
                    match self.same_type(a, b) {
                        None => return None,
                        Some(false) => return Some(false),
                        Some(true) => {}
                    }
                }
                Some(true)
            }
            (Ty::Fn(arg1, r1), Ty::Fn(arg2, r2)) => {
                if arg1.len() != arg2.len() {
                    return Some(false);
                }
                match self.same_type(*r1, *r2) {
                    None => return None,
                    Some(false) => return Some(false),
                    Some(true) => {}
                }
                for (a, b) in arg1.iter().copied().zip(arg2.iter().copied()) {
                    match self.same_type(a, b) {
                        None => return None,
                        Some(false) => return Some(false),
                        Some(true) => {}
                    }
                }
                Some(true)
            }
            _ => Some(false),
        }
    }

    fn unify(&self, a_id: TyId, b_id: TyId) -> Result<(), TypeError> {
        let a = self.find_type(a_id);
        let b = self.find_type(b_id);

        if let Ty::Var(..) = self.type_graph[a] {
            if self.occurs_in(a, b) {
                return Err(TypeError::InfiniteType);
            }

            self.make_equal_to(a, b)?;
            return Ok(());
        }

        if matches!(self.type_graph[b], Ty::Var(..)) {
            return self.unify(b, a);
        }

        match (&self.type_graph[a], &self.type_graph[b]) {
            (Ty::Prim(prim1), Ty::Prim(prim2)) => {
                if prim1 != prim2 && *prim1 != PrimTy::Diverges && *prim2 != PrimTy::Diverges {
                    return Err(TypeError::Mismatch(a, b));
                }
            }
            (Ty::Ptr(a), Ty::Ptr(b))
            | (Ty::PtrMut(a), Ty::PtrMut(b))
            | (Ty::Ref(a), Ty::Ref(b))
            | (Ty::RefMut(a), Ty::RefMut(b)) => {
                return self.unify(*a, *b);
            }
            (Ty::Array(a, s1), Ty::Array(b, s2)) => {
                if s1 != s2 {
                    return Err(TypeError::ArraySize(*s1, *s2));
                }
                return self.unify(*a, *b);
            }
            (Ty::Tuple(tup1), Ty::Tuple(tup2)) => {
                if tup1.len() != tup2.len() {
                    return Err(TypeError::TupleSize(a, b));
                }
                for (a, b) in tup1.iter().copied().zip(tup2.iter().copied()) {
                    self.unify(a, b)?;
                }
            }
            (Ty::Fn(arg1, r1), Ty::Fn(arg2, r2)) => {
                if arg1.len() != arg2.len() {
                    return Err(TypeError::ArgumentSize(a, b));
                }
                self.unify(*r1, *r2)?;
                for (a, b) in arg1.iter().copied().zip(arg2.iter().copied()) {
                    self.unify(a, b)?;
                }
            }
            _ => return Err(TypeError::Mismatch(a, b)),
        }
        Ok(())
    }

    pub fn new_type_var(&mut self, narrowing: Narrowing) -> TyId {
        self.type_graph
            .push(Ty::Var(narrowing, Cell::new(None)))
            .unwrap()
    }

    pub fn infer(
        &mut self,
        ast: &Ast,
        symbols: &SymbolTable,
        root: NodeId<ast::Module>,
    ) -> Result<(), TypeError> {
        let mut pass = InferUpPass::new(symbols, self);
        pass.visit_module(ast, root)?;
        self.validate(ast)?;

        Ok(())
    }
    // Validates if all the types are properly infered
    fn validate(&self, ast: &Ast) -> Result<(), TypeError> {
        for (idx, expr) in ast.library().expr.iter().enumerate() {
            let id = NodeId::<ast::Expr>::from_idx(idx).unwrap();
            let ty = self.find_type_ast(id).unwrap();
            if matches!(self.type_graph[ty], Ty::Var(..)) {
                return Err(TypeError::CantInfer(ty));
            }

            if let Expr::Literal(l) = expr {
                let Ty::Prim(p) = self.type_graph[ty] else {
                    panic!(
                        "Literal does not have a primitive type: {:?}",
                        self.type_graph[ty]
                    )
                };
                match ast[*l] {
                    Lit::Int(ref lit_int) => match p {
                        PrimTy::U64 | PrimTy::Usize => {
                            if matches!(lit_int.value(), IntType::Signed(_)) {
                                return Err(TypeError::LiteralOverflow(*l, ty));
                            }
                        }
                        PrimTy::Isize | PrimTy::I64 => {
                            if let IntType::Unsigned(x) = lit_int.value() {
                                if x > i64::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        }
                        PrimTy::U32 => match lit_int.value() {
                            IntType::Signed(_) => return Err(TypeError::LiteralOverflow(*l, ty)),
                            IntType::Unsigned(x) => {
                                if x > u32::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        PrimTy::U16 => match lit_int.value() {
                            IntType::Signed(_) => return Err(TypeError::LiteralOverflow(*l, ty)),
                            IntType::Unsigned(x) => {
                                if x > u16::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        PrimTy::U8 => match lit_int.value() {
                            IntType::Signed(_) => return Err(TypeError::LiteralOverflow(*l, ty)),
                            IntType::Unsigned(x) => {
                                if x > u8::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        PrimTy::I32 => match lit_int.value() {
                            IntType::Signed(x) => {
                                if x < i32::MIN as i64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                            IntType::Unsigned(x) => {
                                if x > i32::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        PrimTy::I16 => match lit_int.value() {
                            IntType::Signed(x) => {
                                if x < i16::MIN as i64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                            IntType::Unsigned(x) => {
                                if x > i16::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        PrimTy::I8 => match lit_int.value() {
                            IntType::Signed(x) => {
                                if x < i8::MIN as i64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                            IntType::Unsigned(x) => {
                                if x > i8::MAX as u64 {
                                    return Err(TypeError::LiteralOverflow(*l, ty));
                                }
                            }
                        },
                        x => panic!("int literal not an int: {x:?}"),
                    },
                    /*
                    Lit::Float(ref lit_float) => match p {
                        PrimTy::F64 => {
                            if lit_float.base10_digits().parse::<f64>().is_err() {
                                return Err(TypeError::LiteralOverflow(*l, ty));
                            }
                        }
                        PrimTy::F32 => {
                            if lit_float.base10_digits().parse::<f32>().is_err() {
                                return Err(TypeError::LiteralOverflow(*l, ty));
                            }
                        }
                        _ => panic!(),
                    },
                    */
                    _ => return Ok(()),
                }
            } else {
                if let Ty::Var(_, _) = self.type_graph[ty] {
                    return Err(TypeError::CantInfer(ty));
                }
            }
        }
        Ok(())
    }

    /// Formats a type id as a string.
    pub fn type_to_string(&self, ty: TyId) -> String {
        let mut buf = String::new();
        let mut type_name = 0;
        self.type_to_string_rec(ty, &mut buf, &mut type_name);
        buf
    }

    fn type_to_string_rec(&self, ty: TyId, buf: &mut String, type_name: &mut usize) {
        let ty = self.find_type(ty);
        match self.type_graph[ty] {
            Ty::Prim(ref prim_ty) => {
                let s = match prim_ty {
                    PrimTy::Nil => "()",
                    PrimTy::Bool => "bool",
                    PrimTy::Usize => "usize",
                    PrimTy::Isize => "isize",
                    PrimTy::U64 => "u64",
                    PrimTy::I64 => "i64",
                    PrimTy::U32 => "u32",
                    PrimTy::I32 => "i32",
                    PrimTy::U16 => "u16",
                    PrimTy::I16 => "i16",
                    PrimTy::U8 => "u8",
                    PrimTy::I8 => "i8",
                    PrimTy::F32 => "f32",
                    PrimTy::F64 => "f64",
                    PrimTy::Diverges => "!",
                };
                buf.push_str(s);
            }
            Ty::Ptr(t) => {
                buf.push_str("*const ");
                self.type_to_string_rec(t, buf, type_name);
            }
            Ty::PtrMut(t) => {
                buf.push_str("*mut ");
                self.type_to_string_rec(t, buf, type_name);
            }
            Ty::Ref(t) => {
                buf.push_str("&");
                self.type_to_string_rec(t, buf, type_name);
            }
            Ty::RefMut(t) => {
                buf.push_str("&mut ");
                self.type_to_string_rec(t, buf, type_name);
            }
            Ty::Tuple(ref ty_ids) => {
                buf.push_str("(");
                for (idx, t) in ty_ids.iter().copied().enumerate() {
                    if idx != 0 {
                        buf.push_str(", ");
                    }
                    self.type_to_string_rec(t, buf, type_name);
                }
                buf.push_str(")");
            }
            Ty::Fn(ref ty_ids, ty_id) => {
                buf.push_str("fn(");
                for (idx, t) in ty_ids.iter().copied().enumerate() {
                    if idx != 0 {
                        buf.push_str(", ");
                    }
                    self.type_to_string_rec(t, buf, type_name);
                }
                buf.push_str(")");
                if ty_id != Types::NIL_ID {
                    buf.push_str(" -> ");
                    self.type_to_string_rec(ty_id, buf, type_name);
                }
            }
            Ty::Array(ty_id, s) => {
                buf.push_str("[");
                self.type_to_string_rec(ty_id, buf, type_name);
                buf.push_str(";");
                writeln!(buf, "{s}").unwrap();
                buf.push_str("]");
            }
            Ty::Var(ref narrow, _) => {
                match narrow {
                    Narrowing::Integer => {
                        buf.push_str("integer");
                        return;
                    }
                    Narrowing::None => {}
                }
                let mut count = *type_name;
                *type_name += 1;
                loop {
                    let t = count % 26;
                    buf.push((b'A' + t as u8) as char);
                    count = count / 26;
                    if count == 0 {
                        break;
                    }
                }
            }
        }
    }
}

macro_rules! impl_buildin_types {
    ($($const_name:ident = $prim_name:expr,)*) => {
        impl Types{
            impl_buildin_types!(@const, 0, $($const_name = $prim_name,)*);

            fn init(&mut self){
                $(
                    assert_eq!(
                        self.type_graph.push($prim_name).unwrap(),
                        Self::$const_name
                    );
                )*
            }
        }
    };

    (@const,$expr:expr,$const_name:ident = $prim_name:expr,) => {
        const $const_name: TyId = unsafe{ TyId::from_u32_unchecked($expr) };
    };

    (@const,$expr:expr,$head_const_name:ident = $head_prim_name:expr, $($const_name:ident = $prim_name:expr,)* )=> {
        const $head_const_name: TyId = unsafe{ TyId::from_u32_unchecked($expr) };
        impl_buildin_types!(@const, $expr + 1,$($const_name = $prim_name,)*);
    };


    (@const,$const_name:ident,$prim_name:expr,$expr:expr) => {
        const $const_name: TyId = unsafe { TyId::from_u32_unchecked($expr) };
    };

}

impl_buildin_types!(
    NIL_ID = Ty::Prim(PrimTy::Nil),
    BOOL_ID = Ty::Prim(PrimTy::Bool),
    USIZE_ID = Ty::Prim(PrimTy::Usize),
    ISIZE_ID = Ty::Prim(PrimTy::Isize),
    U64_ID = Ty::Prim(PrimTy::U64),
    I64_ID = Ty::Prim(PrimTy::I64),
    U32_ID = Ty::Prim(PrimTy::U32),
    I32_ID = Ty::Prim(PrimTy::I32),
    U16_ID = Ty::Prim(PrimTy::U16),
    I16_ID = Ty::Prim(PrimTy::I16),
    U8_ID = Ty::Prim(PrimTy::U8),
    I8_ID = Ty::Prim(PrimTy::I8),
    F64_ID = Ty::Prim(PrimTy::F64),
    F32_ID = Ty::Prim(PrimTy::F32),
    DIVERGES_ID = Ty::Prim(PrimTy::Diverges),
);
