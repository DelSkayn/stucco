use crate::value::Value;
use ast::{self, NodeId};
use compiler::infer::{Ty, TyId, Types};
use inkwell::values::BasicValueEnum;
use llvm_sys::LLVMCallConv;
use std::{cell::OnceCell, collections::HashMap, sync::OnceLock};

use super::VariantGen;

pub struct ClosureMethod<M, G>(M, G);

impl<M, G> Method for ClosureMethod<M, G>
where
    M: Fn(TyId, &Types) -> bool + Send + Sync,
    G: for<'ctx> Fn(&mut VariantGen<'ctx>, TyId, Vec<Value<'ctx>>) -> Value<'ctx> + Send + Sync,
{
    fn match_ty(&self, reciever_ty: TyId, types: &Types) -> bool {
        (self.0)(reciever_ty, types)
    }

    fn generate<'ctx>(
        &self,
        g: &mut VariantGen<'ctx>,
        reciever_ty: TyId,
        args: Vec<Value<'ctx>>,
    ) -> Value<'ctx> {
        (self.1)(g, reciever_ty, args)
    }
}

pub trait Method: Send + Sync {
    fn match_ty(&self, id: TyId, types: &Types) -> bool;

    fn generate<'ctx>(
        &self,
        g: &mut VariantGen<'ctx>,
        reciever_ty: TyId,
        args: Vec<Value<'ctx>>,
    ) -> Value<'ctx>;
}

pub struct Methods {
    methods: HashMap<String, Vec<Box<dyn Method>>>,
}

impl Methods {
    pub fn new() -> Self {
        Methods {
            methods: HashMap::new(),
        }
    }

    pub fn with<M>(mut self, name: &str, m: M) -> Self
    where
        M: Method + 'static,
    {
        self.methods
            .entry(name.to_owned())
            .or_insert_with(Vec::new)
            .push(Box::new(m));
        self
    }

    pub fn with_closure<M, G>(self, name: &str, m: M, g: G) -> Self
    where
        M: Fn(TyId, &Types) -> bool + Send + Sync + 'static,
        G: for<'ctx> Fn(&mut VariantGen<'ctx>, TyId, Vec<Value<'ctx>>) -> Value<'ctx>
            + Send
            + Sync
            + 'static,
    {
        self.with(name, ClosureMethod(m, g))
    }

    pub fn generate<'ctx>(
        &self,
        g: &mut VariantGen<'ctx>,
        name: &str,
        args: Vec<Value<'ctx>>,
        ty: TyId,
    ) -> Option<Value<'ctx>> {
        for m in self.methods.get(name)? {
            if m.match_ty(ty, &g.ctx.types) {
                return Some(m.generate(g, ty, args));
            }
        }
        None
    }

    fn get() -> &'static Self {
        static METHODS: OnceLock<Methods> = OnceLock::new();
        METHODS.get_or_init(init)
    }
}

fn init() -> Methods {
    Methods::new()
        .with_closure(
            "add",
            |ty, types| match types.type_graph[ty] {
                Ty::Ptr(_) | Ty::PtrMut(_) => true,
                _ => false,
            },
            |g, ty, args| {
                let mut iter = args.into_iter();
                let reciever = iter.next().unwrap();
                let amount = iter.next().unwrap();

                let ptr = reciever.into_ptr();
                let offset = amount.into_int();

                let (Ty::PtrMut(pointee) | Ty::Ptr(pointee)) = g.ctx.types.type_graph[ty] else {
                    panic!()
                };

                let llvm_ty = g.tyid_to_llvm_type(pointee).unwrap();

                let res = unsafe {
                    g.builder
                        .build_gep(llvm_ty, ptr, &[offset], g.name_gen.name("gep"))
                        .unwrap()
                };

                Value::from(res)
            },
        )
        .with_closure(
            "sub",
            |ty, types| match types.type_graph[ty] {
                Ty::Ptr(_) | Ty::PtrMut(_) => true,
                _ => false,
            },
            |g, ty, args| {
                let mut iter = args.into_iter();
                let reciever = iter.next().unwrap();
                let amount = iter.next().unwrap();

                let ptr = reciever.into_ptr();
                let offset = amount.into_int();

                let (Ty::PtrMut(pointee) | Ty::Ptr(pointee)) = g.ctx.types.type_graph[ty] else {
                    panic!()
                };

                let offset = g
                    .builder
                    .build_int_sub(
                        g.ctx.context.i64_type().const_zero(),
                        offset,
                        g.name_gen.name("neg"),
                    )
                    .unwrap();

                let llvm_ty = g.tyid_to_llvm_type(pointee).unwrap();

                let res = unsafe {
                    g.builder
                        .build_gep(llvm_ty, ptr, &[offset], g.name_gen.name("gep"))
                        .unwrap()
                };

                Value::from(res)
            },
        )
        .with_closure(
            "read",
            |ty, types| match types.type_graph[ty] {
                Ty::Ptr(_) | Ty::PtrMut(_) => true,
                _ => false,
            },
            |g, ty, args| {
                let mut iter = args.into_iter();
                let reciever = iter.next().unwrap();

                let ptr = reciever.into_ptr();

                let (Ty::PtrMut(pointee) | Ty::Ptr(pointee)) = g.ctx.types.type_graph[ty] else {
                    panic!()
                };

                let llvm_ty = g.tyid_to_llvm_type(pointee).unwrap();

                let res = g
                    .builder
                    .build_load(llvm_ty, ptr, g.name_gen.name("load"))
                    .unwrap();

                Value::from(res)
            },
        )
        .with_closure(
            "write",
            |ty, types| match types.type_graph[ty] {
                Ty::Ptr(_) | Ty::PtrMut(_) => true,
                _ => false,
            },
            |g, _, args| {
                let mut iter = args.into_iter();
                let reciever = iter.next().unwrap();
                let amount = iter.next().unwrap();

                let ptr = reciever.into_ptr();

                g.builder
                    .build_store(ptr, amount.into_basic().unwrap())
                    .unwrap();

                Value::nill()
            },
        )
}

impl<'ctx> VariantGen<'ctx> {
    pub(super) fn gen_method(&mut self, expr: NodeId<ast::Method>) -> Value<'ctx> {
        let receiver = self.gen_expr(self.ctx.ast[expr].receiver);

        let mut args = vec![receiver];

        for arg in self.ctx.ast.iter_list_node(self.ctx.ast[expr].args) {
            args.push(self.gen_expr(arg))
        }

        let ty = self
            .ctx
            .types
            .find_type_ast(self.ctx.ast[expr].receiver)
            .unwrap();
        let name = self.ctx.ast[expr].name.index(&self.ctx.ast).to_string();
        let Some(res) = Methods::get().generate(self, &name, args, ty) else {
            panic!(
                "Method {} for type {} not implemented",
                name,
                self.ctx.types.type_to_string(ty)
            );
        };
        res
    }

    pub(super) fn gen_call(&mut self, expr: NodeId<ast::Call>) -> Value<'ctx> {
        let reciever = self.gen_expr(self.ctx.ast[expr].func);
        let ty = self
            .ctx
            .types
            .find_type_ast(self.ctx.ast[expr].func)
            .unwrap();
        let llvm_ty = self.tyid_to_llvm_func_type(ty).unwrap();

        let mut args = Vec::new();
        for arg in self.ctx.ast.iter_list_node(self.ctx.ast[expr].args) {
            let a = self.gen_expr(arg);
            args.push(a.into_basic_meta().unwrap())
        }

        let function = reciever.into_ptr();

        let res = self
            .builder
            .build_indirect_call(llvm_ty, function, &args, self.name_gen.name("call"))
            .unwrap();

        res.set_call_convention(LLVMCallConv::LLVMCCallConv as u32);

        res.try_as_basic_value()
            .map_left(Value::from)
            .left_or(Value::nill())
    }
}
