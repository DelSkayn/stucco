use std::{collections::HashMap, fmt::Write as _};

use ast::{Ast, NodeId};
use compiler::{
    infer::{PrimTy, Ty, TyId, Types},
    resolve::{SymbolId, Symbols},
};
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    llvm_sys::LLVMCallConv,
    module::Module,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType},
    values::LLVMTailCallKind,
};
use util::{NonBasicTypeEnum, try_any_to_basic};
use value::Value;

mod obj;
mod target;
pub mod util;
mod value;

#[cfg(not(any(feature = "stand-alone", feature = "proc-macro")))]
compile_error!(
    "Missing feature on stucco_codegen, either feature stand-alone or feature proc-macro must be enabled"
);

pub struct CodeGen {
    context: Context,
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
    pub fn new() -> Self {
        let context = Context::create();
        CodeGen { context }
    }

    pub fn generate_variation<'ctx, 't>(
        &'ctx self,
        ast: &'t Ast,
        symbols: &'t Symbols,
        types: &'t Types,
        stencil: NodeId<ast::Stencil>,
    ) -> VariationGen<'ctx, 't> {
        VariationGen::new(&self.context, ast, symbols, types, stencil)
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
        writeln!(&mut self.buffer, "{}", self.id);
        self.id += 1;
        &self.buffer
    }
}

pub struct VariationGen<'ctx, 't> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    ast: &'t Ast,
    symbols: &'t Symbols,
    types: &'t Types,
    symbol_value: HashMap<SymbolId, Value<'ctx>>,
    name_gen: NameGen,
}

impl<'ctx, 't> VariationGen<'ctx, 't> {
    pub fn new(
        context: &'ctx Context,
        ast: &'t Ast,
        symbols: &'t Symbols,
        types: &'t Types,
        stencil: NodeId<ast::Stencil>,
    ) -> Self {
        let module = context.create_module(
            &stencil
                .index(ast)
                .sym
                .index(ast)
                .name
                .index(ast)
                .to_string(),
        );
        let builder = context.create_builder();

        VariationGen {
            context,
            ast,
            symbols,
            types,
            module,
            builder,
            symbol_value: HashMap::new(),
            name_gen: NameGen::new(),
        }
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
        mut self,
        stencil: NodeId<ast::Stencil>,
        variant: NodeId<ast::Variant>,
    ) -> Module<'ctx> {
        for p in self.ast.iter_list_node(self.ast[stencil].parameters) {
            let sym = self.symbols.ast_to_symbol[self.ast[p].sym].expect("symbol not resolved");
            let variation = self
                .ast
                .iter_list_node(self.ast[variant].variations)
                .find(|x| match self.ast[*x] {
                    ast::Variation::Constant(n) => {
                        self.symbols.ast_to_symbol[self.ast[n].sym].unwrap() == sym
                    }
                    ast::Variation::Slot(n) => {
                        self.symbols.ast_to_symbol[self.ast[n].sym].unwrap() == sym
                    }
                })
                .expect("variation not resolved");

            match self.ast[variation] {
                ast::Variation::Constant(_) => {}
                ast::Variation::Slot(_) => {}
            }
        }

        self.gen_expr(self.ast[stencil].body);

        self.module
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
        let args: Vec<_> = (0..10).map(|_| self.context.i64_type().into()).collect();
        let ty = self.context.void_type().fn_type(&args, false);

        let func = self.module.add_function(
            &format!("__become_{}__", b.index(self.ast).callee.index(self.ast)),
            ty,
            None,
        );
        func.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

        // TODO: Fix arguments
        let become_func = self
            .builder
            .build_call(func, &[], self.name_gen.name("become"))
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
