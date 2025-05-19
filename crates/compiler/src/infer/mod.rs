use crate::resolve::{SymbolId, Symbols};
use core::num;
use std::{cell::Cell, collections::HashMap, fmt::Write, hash::Hash};

use ast::{
    Ast, Block, Expr, Method, NodeId, Span,
    visit::{self, Visit},
};
use common::{id, id::IdVec, iter::IterExt};
use syn::Ident;

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
    UnknownMethod(Ident, TyId),
    InvalidArity(NodeId<Method>, usize),
    UnknownType(NodeId<ast::Type>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
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
    pub type_graph: IdVec<TyId, Ty>,
    pub type_spans: IdVec<TyId, Option<Span>>,
    pub block_type: IdVec<NodeId<ast::Block>, Option<TyId>>,
    pub expr_to_type: IdVec<NodeId<ast::Expr>, Option<TyId>>,
    pub symbol_to_type: IdVec<SymbolId, Option<TyId>>,
    pub type_methods: HashMap<TyId, HashMap<String, FunctionInfo>>,
}

impl Types {
    pub fn new() -> Self {
        let mut res = Self {
            type_graph: IdVec::new(),
            type_spans: IdVec::new(),
            block_type: IdVec::new(),
            expr_to_type: IdVec::new(),
            symbol_to_type: IdVec::new(),
            type_methods: HashMap::new(),
        };

        res.init();

        res
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
        symbols: &Symbols,
        root: NodeId<ast::Module>,
    ) -> Result<(), TypeError> {
        let mut pass = InferUpPass {
            symbols,
            ty: self,
            function_stack: Vec::new(),
            loop_block_stack: Vec::new(),
        };
        pass.visit_module(ast, root)?;
        Ok(())
    }

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

pub struct InferUpPass<'a> {
    symbols: &'a Symbols,
    ty: &'a mut Types,
    function_stack: Vec<NodeId<Expr>>,
    loop_block_stack: Vec<NodeId<Block>>,
}

impl<'a> InferUpPass<'a> {
    fn type_from_ast(&mut self, ast: &ast::Ast, ty: NodeId<ast::Type>) -> Result<TyId, TypeError> {
        match ast[ty] {
            ast::Type::Array(node_id) => todo!(),
            ast::Type::Fn(n) => {
                let args = ast
                    .iter_list_node(ast[n].params)
                    .map(|x| self.type_from_ast(ast, x))
                    .collect::<Result<Vec<TyId>, TypeError>>()?;
                let output = ast[n]
                    .output
                    .map(|x| self.type_from_ast(ast, x))
                    .transpose()?
                    .unwrap_or(Types::NIL_ID);
                return Ok(self.ty.type_graph.push(Ty::Fn(args, output)).unwrap());
            }
            ast::Type::Tuple(node_id) => todo!(),
            ast::Type::Ptr(node_id) => {
                let inner = self.type_from_ast(ast, ast[node_id].ty)?;
                if ast[node_id].mutable {
                    return Ok(self.ty.type_graph.push(Ty::PtrMut(inner)).unwrap());
                } else {
                    return Ok(self.ty.type_graph.push(Ty::Ptr(inner)).unwrap());
                }
            }
            ast::Type::Reference(node_id) => todo!(),
            ast::Type::Direct(node_id) => {
                let ident = &ast[node_id];
                if ident == "f32" {
                    return Ok(Types::F32_ID);
                }
                if ident == "f64" {
                    return Ok(Types::F64_ID);
                }
                if ident == "usize" {
                    return Ok(Types::USIZE_ID);
                }
                if ident == "isize" {
                    return Ok(Types::ISIZE_ID);
                }
            }
        }
        Err(TypeError::UnknownType(ty))
    }

    // Quick and dirty way to resolve intrinisic methods.
    fn resolve_method(
        &mut self,
        ast: &Ast,
        method: NodeId<Method>,
        receiver_type: TyId,
    ) -> Result<TyId, TypeError> {
        let name = method.index(ast).name.index(ast);
        let ty = self.ty.find_type(receiver_type);

        match self.ty.type_graph[ty] {
            Ty::Prim(_) => {}
            Ty::Ptr(ty_id) => {
                if name == "add" || name == "sub" {
                    let args = ast[method].args;
                    let [count] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[count].unwrap();
                    self.ty.unify(ty, Types::USIZE_ID)?;

                    return Ok(receiver_type);
                }
                if name == "read" {
                    let args = ast[method].args;
                    let [] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 0))?;

                    return Ok(ty_id);
                }
            }
            Ty::PtrMut(ty_id) => {
                if name == "add" || name == "sub" {
                    let args = ast[method].args;
                    let [count] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[count].unwrap();
                    self.ty.unify(ty, Types::USIZE_ID)?;

                    return Ok(receiver_type);
                }
                if name == "read" {
                    let args = ast[method].args;
                    let [] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 0))?;

                    return Ok(ty_id);
                }

                if name == "write" {
                    let args = ast[method].args;
                    let [value] = ast
                        .iter_list_node(args)
                        .gather()
                        .ok_or(TypeError::InvalidArity(method, 1))?;
                    let ty = self.ty.expr_to_type[value].unwrap();
                    self.ty.unify(ty, ty_id)?;
                    return Ok(Types::NIL_ID);
                }
            }
            Ty::Ref(x) => return self.resolve_method(ast, method, x),
            Ty::RefMut(x) => return self.resolve_method(ast, method, x),
            Ty::Tuple(_) => {}
            Ty::Fn(_, _) => {}
            Ty::Array(_, _) => todo!(),
            Ty::Var(_, _) => return Err(TypeError::TypeMustBeKnown(method)),
        }
        return Err(TypeError::UnknownMethod(name.clone(), receiver_type));
    }
}

impl<'a> Visit for InferUpPass<'a> {
    type Error = TypeError;

    fn visit_block(&mut self, ast: &ast::Ast, b: NodeId<ast::Block>) -> Result<(), Self::Error> {
        let mut last_ty = None;
        let mut next = ast[b].body;
        while let Some(f) = ast.next_list(&mut next) {
            self.visit_expr(ast, f)?;
            last_ty = Some(self.ty.expr_to_type[f].unwrap());
        }

        let block_ty = if ast[b].returns_last {
            last_ty.unwrap_or(Types::NIL_ID)
        } else {
            Types::NIL_ID
        };

        self.ty.block_type.insert_fill_default(b, Some(block_ty));

        Ok(())
    }

    fn visit_stencil(
        &mut self,
        ast: &ast::Ast,
        m: NodeId<ast::Stencil>,
    ) -> Result<(), Self::Error> {
        for p in ast.iter_list_node(ast[m].parameters) {
            let ty = self.type_from_ast(ast, ast[p].ty)?;
            let sym_id = self.symbols.ast_to_resolved[ast[p].sym].expect("unresolved symbol");
            self.ty.symbol_to_type.insert_fill_default(sym_id, Some(ty));
        }

        visit::visit_stencil(self, ast, m)
    }

    fn visit_expr(&mut self, ast: &ast::Ast, e: NodeId<ast::Expr>) -> Result<(), Self::Error> {
        match ast[e] {
            ast::Expr::If(node_id) => {
                visit::visit_if(self, ast, node_id)?;
                let _if = &ast[node_id];
                // Condition must be a boolean.
                self.ty
                    .unify(self.ty.expr_to_type[_if.condition].unwrap(), Types::BOOL_ID)?;

                // if there is an `else` then all blocks must have the same type.
                // Otherwise they must all be ().
                if let Some(_else) = _if.otherwise {
                    // the if and else need to be the same type.
                    self.ty.unify(
                        self.ty.block_type[_if.then].unwrap(),
                        self.ty.block_type[_else].unwrap(),
                    )?;
                    // this expression has the same type as the if or else block.
                    self.ty
                        .expr_to_type
                        .insert_fill_default(e, self.ty.block_type[_if.then]);
                } else {
                    // No else so this must be NIL
                    self.ty
                        .unify(self.ty.block_type[_if.then].unwrap(), Types::NIL_ID)?;
                    self.ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::NIL_ID));
                }
            }
            ast::Expr::Binary(n) => {
                visit::visit_binary(self, ast, n)?;
                let left = ast[n].left;
                let right = ast[n].right;
                self.ty.unify(
                    self.ty.expr_to_type[left].unwrap(),
                    self.ty.expr_to_type[right].unwrap(),
                )?;

                match ast[n].op {
                    ast::BinOp::Add
                    | ast::BinOp::Sub
                    | ast::BinOp::Mull
                    | ast::BinOp::Div
                    | ast::BinOp::Rem
                    | ast::BinOp::Shl
                    | ast::BinOp::Shr
                    | ast::BinOp::BitXor
                    | ast::BinOp::BitAnd
                    | ast::BinOp::BitOr => {
                        let number_ty = self.ty.new_type_var(Narrowing::Integer);
                        self.ty
                            .unify(self.ty.expr_to_type[left].unwrap(), number_ty)?;
                        self.ty.expr_to_type.insert_fill_default(e, Some(number_ty));
                    }
                    ast::BinOp::And
                    | ast::BinOp::Or
                    | ast::BinOp::Eq
                    | ast::BinOp::Lt
                    | ast::BinOp::Le
                    | ast::BinOp::Ne
                    | ast::BinOp::Ge
                    | ast::BinOp::Gt => {
                        self.ty
                            .expr_to_type
                            .insert_fill_default(e, Some(Types::BOOL_ID));
                    }
                    ast::BinOp::Assign
                    | ast::BinOp::AddAssign
                    | ast::BinOp::SubAssign
                    | ast::BinOp::MullAssign
                    | ast::BinOp::DivAssign
                    | ast::BinOp::RemAssign
                    | ast::BinOp::BitXorAssign
                    | ast::BinOp::BitAndAssign
                    | ast::BinOp::BitOrAssign
                    | ast::BinOp::ShlAssign
                    | ast::BinOp::ShrAssign => {
                        self.ty
                            .expr_to_type
                            .insert_fill_default(e, Some(Types::NIL_ID));
                    }
                }
            }
            ast::Expr::Unary(n) => {
                visit::visit_unary(self, ast, n)?;
                let n = ast[n].expr;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, self.ty.expr_to_type[n]);
            }
            ast::Expr::Block(node_id) => {
                self.visit_block(ast, node_id)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(self.ty.block_type[node_id].unwrap()));
            }
            ast::Expr::Cast(node_id) => todo!(),
            ast::Expr::Loop(n) => {
                let block = ast[n].body;
                let ty_id = self.ty.new_type_var(Narrowing::None);
                self.ty.block_type.insert_fill_default(n, Some(ty_id));
                self.loop_block_stack.push(n);
                visit::visit_inner_block(self, ast, block)?;
                self.loop_block_stack.pop();
                self.ty.expr_to_type.insert_fill_default(e, Some(ty_id));
            }
            ast::Expr::While(n) => {
                visit::visit_expr(self, ast, ast[n].condition)?;
                self.ty.unify(
                    self.ty.expr_to_type[ast[n].condition].unwrap(),
                    Types::BOOL_ID,
                )?;

                let block = ast[n].then;
                let ty_id = self.ty.new_type_var(Narrowing::None);
                self.ty.block_type.insert_fill_default(block, Some(ty_id));
                self.loop_block_stack.push(block);
                visit::visit_inner_block(self, ast, ast[block].body)?;
                self.loop_block_stack.pop();
            }
            ast::Expr::Let(n) => {
                // TODO: Type declarations
                let expr = ast[n].expr;
                let symbol = self.symbols.ast_to_resolved[ast[n].sym].unwrap();
                self.visit_let(ast, n)?;
                if let Some(x) = self.ty.symbol_to_type.get(symbol).copied().flatten() {
                    self.ty.unify(x, self.ty.expr_to_type[expr].unwrap())?;
                } else {
                    // TODO: Fix too unwrap here
                    self.ty
                        .symbol_to_type
                        .insert_fill_default(symbol, self.ty.expr_to_type[expr]);
                };
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::NIL_ID));
            }
            ast::Expr::Continue(_) => {}
            ast::Expr::Break(n) => {
                visit::visit_break(self, ast, n)?;
                let ty = if let Some(expr) = ast[n].expr {
                    self.ty.expr_to_type[expr].unwrap()
                } else {
                    Types::NIL_ID
                };
                let block_ty =
                    self.ty.block_type[self.loop_block_stack.last().copied().unwrap()].unwrap();
                self.ty.unify(ty, block_ty)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Return(n) => {
                if let Some(n) = ast[n].expr {
                    todo!()
                }
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Become(n) => {
                visit::visit_become(self, ast, n)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(Types::DIVERGES_ID));
            }
            ast::Expr::Call(n) => {
                visit::visit_call(self, ast, n)?;
                let args = ast
                    .iter_list_node(ast[n].args)
                    .map(|x| self.ty.expr_to_type[x].unwrap())
                    .collect();
                let out = self.ty.new_type_var(Narrowing::None);
                let expected_ty = self.ty.type_graph.push(Ty::Fn(args, out)).unwrap();
                let func_ty = self.ty.expr_to_type[ast[n].func].unwrap();
                self.ty.unify(expected_ty, func_ty)?;
                self.ty.expr_to_type.insert_fill_default(e, Some(out));
            }
            ast::Expr::Method(n) => {
                visit::visit_method(self, ast, n)?;
                let recv_type = self.ty.expr_to_type[ast[n].receiver].unwrap();
                let ty = self.resolve_method(ast, n, recv_type)?;
                self.ty.expr_to_type.insert_fill_default(e, Some(ty))
            }
            ast::Expr::Field(node_id) => todo!(),
            ast::Expr::Index(node_id) => todo!(),
            ast::Expr::Literal(node_id) => {
                match ast[node_id] {
                    syn::Lit::Int(ref lit_int) => {
                        match lit_int.suffix() {
                            "i64" => self
                                .ty
                                .expr_to_type
                                .insert_fill_default(e, Some(Types::I64_ID)),
                            "u64" => self
                                .ty
                                .expr_to_type
                                .insert_fill_default(e, Some(Types::U64_ID)),
                            "" => {
                                let ty_var = self
                                    .ty
                                    .type_graph
                                    .push(Ty::Var(Narrowing::Integer, Cell::new(None)))
                                    .unwrap();
                                self.ty.expr_to_type.insert_fill_default(e, Some(ty_var));
                            }
                            // Should have been filtered out by the parser.
                            _ => unreachable!(),
                        }
                    }
                    syn::Lit::Float(ref lit_float) => {
                        match lit_float.suffix() {
                            "f64" => self
                                .ty
                                .expr_to_type
                                .insert_fill_default(e, Some(Types::F64_ID)),
                            "" => self
                                .ty
                                .expr_to_type
                                .insert_fill_default(e, Some(Types::F64_ID)),
                            // Should have been filtered out by the parser.
                            _ => unreachable!(),
                        }
                    }
                    syn::Lit::Bool(_) => self
                        .ty
                        .expr_to_type
                        .insert_fill_default(e, Some(Types::BOOL_ID)),
                    _ => unreachable!(),
                }
            }
            ast::Expr::Symbol(node_id) => {
                let symbol = self.symbols.ast_to_resolved[node_id].unwrap();
                let ty_id = self
                    .ty
                    .symbol_to_type
                    .get(symbol)
                    .copied()
                    .flatten()
                    .expect("undefined symbol");
                self.ty.expr_to_type.insert_fill_default(e, Some(ty_id));
            }
            ast::Expr::Covered(node_id) => {
                self.visit_expr(ast, node_id)?;
                self.ty
                    .expr_to_type
                    .insert_fill_default(e, Some(self.ty.expr_to_type[node_id].unwrap()));
            }
        }

        Ok(())
    }
}
