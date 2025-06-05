use std::{collections::HashMap, fmt::Write as _};

use ast::{Ast, NodeId};
use compiler::{
    infer::{PrimTy, Ty, TyId, Types},
    resolve::{SymbolId, Symbols},
};
use inkwell::{
    AddressSpace,
    attributes::Attribute,
    builder::{self, Builder},
    context::Context,
    llvm_sys::LLVMCallConv,
    module::{Linkage, Module},
    targets::{FileType, Target as LLVMTarget, TargetTriple},
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType},
    values::{AnyValue, LLVMTailCallKind},
};
pub use target::Target;
use util::{NonBasicTypeEnum, try_any_to_basic};
use value::Value;
use wrapper::GlobalValueExt;

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

pub struct CodeGen<'t> {
    context: Context,
    ast: &'t Ast,
    symbols: &'t Symbols,
    types: &'t Types,
    config: Config,
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

impl<'t> CodeGen<'t> {
    pub fn new(ast: &'t Ast, symbols: &'t Symbols, types: &'t Types, config: Config) -> Self {
        let context = Context::create();
        CodeGen {
            context,
            ast,
            symbols,
            types,
            config,
        }
    }

    pub fn generate_variation<'ctx>(
        &'ctx self,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) -> VariationGen<'ctx, 't> {
        VariationGen::build(&self, stencil, variant)
    }
}

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

pub struct VariationGen<'ctx, 't> {
    config: &'ctx Config,
    context: &'ctx Context,
    ast: &'t Ast,
    symbols: &'t Symbols,
    types: &'t Types,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    pass_through: Vec<Value<'ctx>>,
    symbol_value: HashMap<SymbolId, Value<'ctx>>,
    //variation_data: HashMap<NodeId<ast::Variation>, Value<'ctx>>,
    name_gen: NameGen,
}

impl<'ctx, 't> VariationGen<'ctx, 't> {
    pub fn build(
        cgen: &'ctx CodeGen<'t>,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) -> Self {
        let ast = &cgen.ast;
        let module = cgen.context.create_module(
            &stencil
                .index(ast)
                .sym
                .index(ast)
                .name
                .index(ast)
                .to_string(),
        );
        let builder = cgen.context.create_builder();

        let mut res = VariationGen {
            config: &cgen.config,
            context: &cgen.context,
            ast: cgen.ast,
            symbols: cgen.symbols,
            types: cgen.types,
            module,
            builder,
            pass_through: Vec::new(),
            symbol_value: HashMap::new(),
            name_gen: NameGen::new(),
        };

        res.generate_variation(stencil, variant);

        res
    }

    pub fn into_module(self) -> Module<'ctx> {
        self.module
    }

    pub fn into_assembly(self, target: Target) -> String {
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
        let buffer = machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .unwrap();

        String::from_utf8(buffer.as_slice().to_vec()).unwrap()
    }

    fn tyid_to_llvm_type(&self, id: TyId) -> AnyTypeEnum<'ctx> {
        let ty = self.types.find_type(id);
        match &self.types.type_graph[ty] {
            Ty::Prim(p) => match p {
                PrimTy::Nil => self.context.void_type().as_any_type_enum(),
                PrimTy::Bool => self.context.bool_type().as_any_type_enum(),
                // TODO: Change when supporting 32 bit.
                PrimTy::Usize => self.context.i64_type().as_any_type_enum(),
                PrimTy::Isize => self.context.i64_type().as_any_type_enum(),
                PrimTy::U64 => self.context.i64_type().as_any_type_enum(),
                PrimTy::I64 => self.context.i64_type().as_any_type_enum(),
                PrimTy::U32 => self.context.i32_type().as_any_type_enum(),
                PrimTy::I32 => self.context.i32_type().as_any_type_enum(),
                PrimTy::U16 => self.context.i16_type().as_any_type_enum(),
                PrimTy::I16 => self.context.i16_type().as_any_type_enum(),
                PrimTy::U8 => self.context.i8_type().as_any_type_enum(),
                PrimTy::I8 => self.context.i8_type().as_any_type_enum(),
                PrimTy::F32 => self.context.f32_type().as_any_type_enum(),
                PrimTy::F64 => self.context.f64_type().as_any_type_enum(),
                PrimTy::Diverges => self.context.void_type().as_any_type_enum(),
            },
            Ty::Ptr(_) | Ty::PtrMut(_) => self
                .context
                .ptr_type(AddressSpace::default())
                .as_any_type_enum(),
            Ty::Ref(ty_id) => todo!(),
            Ty::RefMut(ty_id) => todo!(),
            Ty::Tuple(ty_ids) => todo!(),
            Ty::Fn(args, res) => {
                let args: Vec<BasicMetadataTypeEnum> = args
                    .iter()
                    .copied()
                    .filter_map(|arg| {
                        let ty = self.tyid_to_llvm_type(arg);
                        match try_any_to_basic(ty) {
                            Ok(x) => Some(x.into()),
                            Err(NonBasicTypeEnum::VoidType(x)) => None,
                            Err(NonBasicTypeEnum::FunctionType(_)) => Some(
                                self.context
                                    .ptr_type(AddressSpace::default())
                                    .as_basic_type_enum()
                                    .into(),
                            ),
                        }
                    })
                    .collect();

                let ty = match try_any_to_basic(self.tyid_to_llvm_type(*res)) {
                    Ok(x) => x.fn_type(&args, false),
                    Err(NonBasicTypeEnum::VoidType(x)) => x.fn_type(&args, false),
                    Err(NonBasicTypeEnum::FunctionType(_)) => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .fn_type(&args, false),
                };
                ty.as_any_type_enum()
            }
            Ty::Array(ty_id, _) => todo!(),
            Ty::Var(..) => todo!(),
        }
    }

    pub fn generate_variation(
        &mut self,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) {
        let mut slots = Vec::new();
        let mut immediate = Vec::new();

        for p in self.ast.iter_list_node(self.ast[stencil].parameters) {
            let sym = self.symbols.ast_to_symbol[self.ast[p].sym].expect("symbol not resolved");
            let variation = self
                .ast
                .iter_list_node(self.ast[variant].variations)
                .find(|x| match self.ast[*x] {
                    ast::Variation::Immediate(n) => {
                        self.symbols.ast_to_symbol[self.ast[n].sym].unwrap() == sym
                    }
                    ast::Variation::Slot(n) => {
                        self.symbols.ast_to_symbol[self.ast[n].sym].unwrap() == sym
                    }
                })
                .expect("variation not resolved");

            match self.ast[variation] {
                ast::Variation::Immediate(v) => {
                    immediate.push(v);
                }
                ast::Variation::Slot(v) => slots.push((v, p)),
            }
        }

        let args_ty: Vec<_> = (0..self.config.num_passing_register)
            .map(|_| self.context.i64_type().into())
            .collect();
        let ty = self.context.void_type().fn_type(&args_ty, false);

        let function = self.module.add_function("__stencil__", ty, None);
        function.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);
        for i in 0..self.config.num_passing_register as u32 {
            self.pass_through.push(Value::from(
                function.get_nth_param(i).unwrap().as_any_value_enum(),
            ))
        }

        let bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(bb);

        for imm in immediate {
            self.compile_immediate(imm);
        }

        self.gen_expr(self.ast[stencil].body);
    }

    /// Add an immediate for a variation.
    fn compile_immediate(&mut self, immediate: NodeId<ast::VariationImmediate>) {
        let global = self.module.add_global(
            self.context.i8_type(),
            None,
            &format!(
                "__immediate_{}",
                &immediate
                    .index(self.ast)
                    .sym
                    .index(self.ast)
                    .name
                    .index(self.ast)
            ),
        );
        global.set_linkage(Linkage::External);
        global.set_dso_local(true);

        let v = if self.config.clobber_immediates {
            let union = self
                .context
                .struct_type(&[self.context.i64_type().into()], false);

            let v = self
                .builder
                .build_alloca(union, self.name_gen.name("alloca_imm"))
                .unwrap();

            self.builder
                .build_store(v, global.as_pointer_value())
                .unwrap();

            let ptr_ty = self.context.ptr_type(AddressSpace::default());
            let asm_fn_ty = self.context.void_type().fn_type(&[ptr_ty.into()], false);

            let asm = self.context.create_inline_asm(
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
                .build_indirect_call(asm_fn_ty, asm, &[v.into()], &self.name_gen.name("clobber"))
                .unwrap();

            let id = Attribute::get_named_enum_kind_id("elementtype");
            let attr = self.context.create_type_attribute(
                id,
                self.context.ptr_type(Default::default()).as_any_type_enum(),
            );
            call.add_attribute(inkwell::attributes::AttributeLoc::Param(0), attr);
            self.builder
                .build_load(self.context.i64_type(), v, self.name_gen.name("load_imm"))
                .unwrap()
        } else {
            self.builder
                .build_ptr_to_int(
                    global.as_pointer_value(),
                    self.context.i64_type(),
                    self.name_gen.name("cast_imm"),
                )
                .unwrap()
                .into()
        };

        /*

        */
        let symbol = self.symbols.ast_to_symbol[self.ast[immediate].sym].unwrap();
        /*
         */
        self.symbol_value.insert(symbol, Value::from(v));
    }

    fn gen_expr(&mut self, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        match self.ast[expr] {
            ast::Expr::If(node_id) => todo!(),
            ast::Expr::Binary(b) => self.gen_binary(b, expr),
            ast::Expr::Unary(node_id) => todo!(),
            ast::Expr::Block(b) => {
                let returns_last = self.ast[b].returns_last;
                let mut value = None;
                for expr in self.ast.iter_list_node(self.ast[b].body) {
                    value = Some(self.gen_expr(expr))
                }
                if returns_last {
                    value.unwrap_or_else(Value::void)
                } else {
                    Value::void()
                }
            }
            ast::Expr::Cast(node_id) => todo!(),
            ast::Expr::Loop(node_id) => todo!(),
            ast::Expr::While(node_id) => todo!(),
            ast::Expr::Let(l) => {
                let expr = self.gen_expr(self.ast[l].expr);
                let id = self.symbols.ast_to_symbol[self.ast[l].sym].unwrap();
                self.symbol_value.insert(id, expr);
                Value::void()
            }
            ast::Expr::Continue(span) => todo!(),
            ast::Expr::Break(node_id) => todo!(),
            ast::Expr::Return(node_id) => todo!(),
            ast::Expr::Become(b) => {
                self.gen_become(b);
                Value::void()
            }
            ast::Expr::Call(node_id) => todo!(),
            ast::Expr::Method(node_id) => todo!(),
            ast::Expr::Field(node_id) => todo!(),
            ast::Expr::Index(node_id) => todo!(),
            ast::Expr::Literal(l) => self.gen_lit(l, expr),
            ast::Expr::Symbol(s) => {
                let id = self.symbols.ast_to_symbol[s].unwrap();
                self.symbol_value
                    .get(&id)
                    .expect("undefined symbol")
                    .clone()
            }
            ast::Expr::Covered(c) => self.gen_expr(c),
        }
    }

    fn gen_become(&mut self, b: NodeId<ast::Become>) {
        let args: Vec<_> = (0..self.config.num_passing_register)
            .map(|_| self.context.i64_type().into())
            .collect();
        let ty = self.context.void_type().fn_type(&args, false);

        let arg_count = self.ast.iter_list_node(self.ast[b].args).count();
        let mut args = Vec::new();
        for i in 0..(self.config.num_passing_register - arg_count) {
            args.push(self.pass_through[i].clone().into_int().into())
        }

        for arg in self.ast.iter_list_node(self.ast[b].args) {
            args.push(self.gen_expr(arg).into_int().into());
        }

        let func = self.module.add_function(
            &format!("__become_{}__", b.index(self.ast).callee.index(self.ast)),
            ty,
            None,
        );
        func.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

        // TODO: Fix arguments
        let become_func = self
            .builder
            .build_call(func, &args, self.name_gen.name("become"))
            .unwrap();
        become_func.set_call_convention(LLVMCallConv::LLVMGHCCallConv as u32);
        become_func.set_tail_call_kind(LLVMTailCallKind::LLVMTailCallKindMustTail);
        self.builder.build_return(None).unwrap();
    }

    fn gen_binary(&mut self, bin: NodeId<ast::BinaryExpr>, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        match self.ast[bin].op {
            ast::BinOp::Add
            | ast::BinOp::Sub
            | ast::BinOp::Mull
            | ast::BinOp::Div
            | ast::BinOp::Rem
            | ast::BinOp::BitXor
            | ast::BinOp::BitAnd
            | ast::BinOp::BitOr
            | ast::BinOp::Shl
            | ast::BinOp::Shr => {
                let left = self.gen_expr(self.ast[bin].left);
                let right = self.gen_expr(self.ast[bin].right);
                let ty_id = self.types.find_type_for_expr(expr).unwrap();
                let number_ty = NumberType::from_ty(&self.types.type_graph[ty_id]).unwrap();

                match number_ty {
                    NumberType::Float => {
                        let left = left.into_float();
                        let right = right.into_float();

                        match self.ast[bin].op {
                            ast::BinOp::Add => self
                                .builder
                                .build_float_sub(left, right, self.name_gen.name("fadd"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Sub => self
                                .builder
                                .build_float_sub(left, right, self.name_gen.name("fsub"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Mull => self
                                .builder
                                .build_float_mul(left, right, self.name_gen.name("fmul"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Div => self
                                .builder
                                .build_float_div(left, right, self.name_gen.name("fdiv"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Rem => self
                                .builder
                                .build_float_rem(left, right, self.name_gen.name("frem"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitXor | ast::BinOp::BitAnd | ast::BinOp::BitOr => todo!(),
                            ast::BinOp::Shl => todo!(),
                            ast::BinOp::Shr => todo!(),
                            _ => unreachable!(),
                        }
                    }
                    NumberType::Unsigned => {
                        let left = left.into_int();
                        let right = right.into_int();

                        match self.ast[bin].op {
                            ast::BinOp::Add => self
                                .builder
                                .build_int_nuw_add(left, right, self.name_gen.name("uadd"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Sub => self
                                .builder
                                .build_int_nuw_sub(left, right, self.name_gen.name("usub"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Mull => self
                                .builder
                                .build_int_nuw_mul(left, right, self.name_gen.name("umul"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Div => self
                                .builder
                                .build_int_unsigned_div(left, right, self.name_gen.name("udiv"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Rem => self
                                .builder
                                .build_int_unsigned_rem(left, right, self.name_gen.name("urem"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitXor => self
                                .builder
                                .build_xor(left, right, self.name_gen.name("xor"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitAnd => self
                                .builder
                                .build_and(left, right, self.name_gen.name("and"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitOr => self
                                .builder
                                .build_and(left, right, self.name_gen.name("or"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Shl => self
                                .builder
                                .build_left_shift(left, right, self.name_gen.name("shl"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Shr => self
                                .builder
                                .build_right_shift(left, right, false, self.name_gen.name("ushr"))
                                .unwrap()
                                .into(),
                            _ => unreachable!(),
                        }
                    }
                    NumberType::Signed => {
                        let left = left.into_int();
                        let right = right.into_int();

                        match self.ast[bin].op {
                            ast::BinOp::Add => self
                                .builder
                                .build_int_nsw_add(left, right, self.name_gen.name("sadd"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Sub => self
                                .builder
                                .build_int_nsw_sub(left, right, self.name_gen.name("ssub"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Mull => self
                                .builder
                                .build_int_nsw_mul(left, right, self.name_gen.name("smul"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Div => self
                                .builder
                                .build_int_signed_div(left, right, self.name_gen.name("sdiv"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Rem => self
                                .builder
                                .build_int_signed_rem(left, right, self.name_gen.name("srem"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitXor => self
                                .builder
                                .build_xor(left, right, self.name_gen.name("xor"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitAnd => self
                                .builder
                                .build_and(left, right, self.name_gen.name("and"))
                                .unwrap()
                                .into(),
                            ast::BinOp::BitOr => self
                                .builder
                                .build_and(left, right, self.name_gen.name("or"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Shl => self
                                .builder
                                .build_left_shift(left, right, self.name_gen.name("shl"))
                                .unwrap()
                                .into(),
                            ast::BinOp::Shr => self
                                .builder
                                .build_right_shift(left, right, false, self.name_gen.name("sshr"))
                                .unwrap()
                                .into(),
                            _ => unreachable!(),
                        }
                    }
                }
            }
            ast::BinOp::Lt => todo!(),
            ast::BinOp::Le => todo!(),
            ast::BinOp::Ne => todo!(),
            ast::BinOp::Ge => todo!(),
            ast::BinOp::Gt => todo!(),
            ast::BinOp::Eq => todo!(),
            ast::BinOp::And => todo!(),
            ast::BinOp::Or => todo!(),
            ast::BinOp::Assign => todo!(),
            ast::BinOp::AddAssign => todo!(),
            ast::BinOp::SubAssign => todo!(),
            ast::BinOp::MullAssign => todo!(),
            ast::BinOp::DivAssign => todo!(),
            ast::BinOp::RemAssign => todo!(),
            ast::BinOp::BitXorAssign => todo!(),
            ast::BinOp::BitAndAssign => todo!(),
            ast::BinOp::BitOrAssign => todo!(),
            ast::BinOp::ShlAssign => todo!(),
            ast::BinOp::ShrAssign => todo!(),
        }
    }

    fn gen_lit(&self, lit: NodeId<ast::Lit>, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        match &self.ast[lit] {
            ast::Lit::Str(lit_str) => todo!(),
            ast::Lit::ByteStr(lit_byte_str) => todo!(),
            ast::Lit::CStr(lit_cstr) => todo!(),
            ast::Lit::Byte(lit_byte) => todo!(),
            ast::Lit::Char(lit_char) => todo!(),
            ast::Lit::Int(lit_int) => {
                let ty = self.types.find_type_for_expr(expr).unwrap();
                let v = match self.types.type_graph[ty] {
                    Ty::Prim(p) => match p {
                        PrimTy::Usize | PrimTy::U64 => self
                            .context
                            .i64_type()
                            .const_int(lit_int.base10_parse().unwrap(), false),
                        PrimTy::U32 => self
                            .context
                            .i32_type()
                            .const_int(lit_int.base10_parse().unwrap(), false),
                        PrimTy::U16 => self
                            .context
                            .i16_type()
                            .const_int(lit_int.base10_parse().unwrap(), false),
                        PrimTy::U8 => self
                            .context
                            .i8_type()
                            .const_int(lit_int.base10_parse().unwrap(), false),
                        PrimTy::Isize | PrimTy::I64 | PrimTy::I32 | PrimTy::I16 | PrimTy::I8 => {
                            todo!()
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                };
                v.into()
            }
            ast::Lit::Float(lit_float) => {
                let ty = self.types.find_type_for_expr(expr).unwrap();
                let v = match self.types.type_graph[ty] {
                    Ty::Prim(PrimTy::F32) => {
                        let number = lit_float.base10_parse::<f64>().unwrap();
                        self.context.f32_type().const_float(number)
                    }
                    Ty::Prim(PrimTy::F64) => {
                        let number = lit_float.base10_parse::<f64>().unwrap();
                        self.context.f32_type().const_float(number)
                    }
                    _ => unreachable!(),
                };
                v.into()
            }
            ast::Lit::Bool(lit_bool) => {
                if lit_bool.value() {
                    self.context.bool_type().const_int(1, false).into()
                } else {
                    self.context.bool_type().const_int(0, false).into()
                }
            }
            ast::Lit::Verbatim(literal) => todo!(),
            _ => todo!(),
        }
    }
}
