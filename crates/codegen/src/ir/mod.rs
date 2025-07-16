//! Implemention the llvm IR generation.

use std::{collections::HashMap, fmt::Write as _};

use ast::{NodeId, Stencil, Variant, Variation};
use compiler::resolve::SymbolId;
use inkwell::{
    AddressSpace,
    attributes::Attribute,
    builder::Builder,
    module::{Linkage, Module},
    targets::{FileType, Target as LLVMTarget, TargetMachine, TargetTriple},
    types::{AnyType as _, BasicType as _},
    values::{AnyValue, FunctionValue},
};
use llvm_sys::LLVMCallConv;

use crate::{CodeGen, Target, value::Value, wrapper::GlobalValueExt as _};

mod expr;
mod method;
mod util;

struct NameGen {
    id: usize,
    buffer: String,
}

impl NameGen {
    pub fn new() -> NameGen {
        NameGen {
            id: 0,
            buffer: String::new(),
        }
    }

    pub fn name(&mut self, prefix: &str) -> &str {
        self.buffer.clear();
        self.buffer.push_str(prefix);
        write!(&mut self.buffer, "{}", self.id).unwrap();
        self.id += 1;
        &self.buffer
    }
}

pub struct VariantModule<'ctx> {
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> VariantModule<'ctx> {
    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }

    fn create_target_machine(&self, target: Target) -> TargetMachine {
        let (target, triple) = match target {
            Target::X86_64 => {
                LLVMTarget::initialize_x86(&Default::default());
                let triple = TargetTriple::create("x86_64-unknown-gnu");
                (LLVMTarget::from_triple(&triple).unwrap(), triple)
            }
            Target::Aarch64 => todo!(),
            Target::Riscv => todo!(),
        };

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                inkwell::OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Static,
                inkwell::targets::CodeModel::Medium,
            )
            .unwrap();

        self.module
            .set_data_layout(&machine.get_target_data().get_data_layout());
        self.module.set_triple(&triple);
        machine
    }

    pub fn into_assembly(self, target: Target) -> String {
        let machine = self.create_target_machine(target);
        let buffer = machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .unwrap();

        String::from_utf8(buffer.as_slice().to_vec()).unwrap()
    }

    pub fn into_object(self, target: Target) -> Vec<u8> {
        let machine = self.create_target_machine(target);
        let buffer = machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .unwrap();

        buffer.as_slice().to_vec()
    }
}

pub struct VariantGen<'ctx> {
    ctx: &'ctx CodeGen,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,
    symbol_value: HashMap<SymbolId, Value<'ctx>>,
    name_gen: NameGen,
}

impl<'ctx> VariantGen<'ctx> {
    pub fn build(
        ctx: &'ctx CodeGen,
        stencil: NodeId<Stencil>,
        variant: NodeId<Variant>,
    ) -> VariantModule<'ctx> {
        let variant_count = ctx
            .ast
            .iter_list_node(ctx.ast[stencil].variants)
            .position(|x| x == variant)
            .expect("got variant which was not part of given stencil");

        let name = format!(
            "{}_{}",
            stencil
                .index(&ctx.ast)
                .sym
                .index(&ctx.ast)
                .name
                .index(&ctx.ast)
                .to_string(),
            variant_count
        );

        let module = ctx.context.create_module(&name);
        let builder = ctx.context.create_builder();
        let name_gen = NameGen::new();
        let function = Self::generate_signature(ctx, &module);

        let mut this = VariantGen {
            ctx,
            module,
            builder,
            name_gen,
            function,
            symbol_value: HashMap::new(),
        };

        let bb = this.ctx.context.append_basic_block(function, "entry");
        this.builder.position_at_end(bb);

        this.gen_variant(stencil, variant);

        this.gen_expr(this.ctx.ast[stencil].body);

        VariantModule {
            module: this.module,
            builder: this.builder,
        }
    }

    pub fn generate_signature(ctx: &'ctx CodeGen, module: &Module<'ctx>) -> FunctionValue<'ctx> {
        // Generate signature type.
        // It is always the same of all stencils.
        let parameters = (0..ctx.config.num_passing_register)
            .map(|_| ctx.context.i64_type().as_basic_type_enum().into())
            .collect::<Vec<_>>();

        let signature = ctx.context.void_type().fn_type(&parameters, false);

        let func = module.add_function("__main__", signature, None);
        func.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);
        func
    }

    /// Generates the values for the variation symbols.
    pub fn gen_variant(&mut self, stencil: NodeId<Stencil>, variant: NodeId<Variant>) {
        let mut slots = Vec::new();
        let mut imms = Vec::new();

        for p in self
            .ctx
            .ast
            .iter_list_node(self.ctx.ast[variant].variations)
        {
            match self.ctx.ast[p] {
                Variation::Immediate(node_id) => imms.push(node_id),
                Variation::Slot(node_id) => slots.push(node_id),
            }
        }

        // Distribute slots over the parameters
        let mut next = 0;
        for param in self
            .ctx
            .ast
            .iter_list_node(self.ctx.ast[stencil].parameters)
        {
            let param_sym = self.ctx.symbols.ast_to_symbol[self.ctx.ast[param].sym].unwrap();
            if !slots
                .iter()
                .copied()
                .any(|x| self.ctx.symbols.ast_to_symbol[self.ctx.ast[x].sym].unwrap() == param_sym)
            {
                continue;
            }

            if let Some(which) = self.ctx.config.num_passing_register.checked_sub(next + 1) {
                let param = self.function.get_nth_param(which as u32).unwrap();
                self.symbol_value.insert(param_sym, Value::from(param));
                next += 1;
            } else {
                // turn into a stack load.
                todo!()
            };
        }

        // Generate immediate values.
        for imm in imms {
            let symbol = self.ctx.symbols.ast_to_symbol[self.ctx.ast[imm].sym].unwrap();
            // Global, the address of which will be the right value.
            let global = self.module.add_global(
                self.ctx.context.i8_type(),
                None,
                &format!(
                    "__immediate_{}",
                    &imm.index(&self.ctx.ast)
                        .sym
                        .index(&self.ctx.ast)
                        .name
                        .index(&self.ctx.ast)
                ),
            );
            global.set_linkage(Linkage::External);
            global.set_dso_local(true);

            let v = if self.ctx.config.clobber_immediates {
                // Possibly remove this union type
                let union = self
                    .ctx
                    .context
                    .struct_type(&[self.ctx.context.i64_type().into()], false);

                let v = self
                    .builder
                    .build_alloca(union, self.name_gen.name("alloca_imm"))
                    .unwrap();

                self.builder
                    .build_store(v, global.as_pointer_value())
                    .unwrap();

                let ptr_ty = self.ctx.context.ptr_type(AddressSpace::default());
                let asm_fn_ty = self
                    .ctx
                    .context
                    .void_type()
                    .fn_type(&[ptr_ty.into()], false);

                let asm = self.ctx.context.create_inline_asm(
                    asm_fn_ty,
                    String::new(),
                    "*m".to_string(),
                    true,
                    false,
                    None,
                    false,
                );

                let call = self
                    .builder
                    .build_indirect_call(
                        asm_fn_ty,
                        asm,
                        &[v.into()],
                        &self.name_gen.name("clobber"),
                    )
                    .unwrap();

                let id = Attribute::get_named_enum_kind_id("elementtype");
                let attr = self.ctx.context.create_type_attribute(
                    id,
                    self.ctx
                        .context
                        .ptr_type(Default::default())
                        .as_any_type_enum(),
                );
                call.add_attribute(inkwell::attributes::AttributeLoc::Param(0), attr);
                let res = self
                    .builder
                    .build_load(
                        self.ctx.context.i64_type(),
                        v,
                        self.name_gen.name("load_imm"),
                    )
                    .unwrap();
                res.into_int_value()
            } else {
                self.builder
                    .build_ptr_to_int(
                        global.as_pointer_value(),
                        self.ctx.context.i64_type(),
                        self.name_gen.name("cast_imm"),
                    )
                    .unwrap()
            };

            let ty = self.ctx.types.find_type_symbol(symbol).unwrap();
            let v = match self.ctx.types.type_graph[ty] {
                compiler::infer::Ty::Prim(p) => match p {
                    compiler::infer::PrimTy::Nil => todo!(),
                    compiler::infer::PrimTy::Bool => todo!(),
                    compiler::infer::PrimTy::Usize
                    | compiler::infer::PrimTy::Isize
                    | compiler::infer::PrimTy::U64
                    | compiler::infer::PrimTy::I64 => v.as_any_value_enum(),
                    compiler::infer::PrimTy::U32 => todo!(),
                    compiler::infer::PrimTy::I32 => todo!(),
                    compiler::infer::PrimTy::U16 => todo!(),
                    compiler::infer::PrimTy::I16 => todo!(),
                    compiler::infer::PrimTy::U8 => todo!(),
                    compiler::infer::PrimTy::I8 => todo!(),
                    compiler::infer::PrimTy::F32 => todo!(),
                    compiler::infer::PrimTy::F64 => todo!(),
                    compiler::infer::PrimTy::Diverges => todo!(),
                },
                compiler::infer::Ty::Ref(_) => todo!(),
                compiler::infer::Ty::RefMut(_) => todo!(),
                compiler::infer::Ty::Tuple(_) => todo!(),
                compiler::infer::Ty::Ptr(_)
                | compiler::infer::Ty::PtrMut(_)
                | compiler::infer::Ty::Fn(..) => self
                    .builder
                    .build_int_to_ptr(
                        v,
                        self.ctx.context.ptr_type(AddressSpace::default()),
                        self.name_gen.name("cast"),
                    )
                    .unwrap()
                    .as_any_value_enum(),
                compiler::infer::Ty::Array(ty_id, _) => todo!(),
                compiler::infer::Ty::Var(..) => unreachable!(),
            };

            self.symbol_value.insert(symbol, Value::from(v));
        }
    }
}
