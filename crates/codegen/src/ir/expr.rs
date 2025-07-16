use super::VariantGen;
use crate::{NumberType, value::Value};
use ast::NodeId;
use compiler::infer::{PrimTy, Ty};
use inkwell::{IntPredicate, values::InstructionOpcode};
use llvm_sys::{LLVMCallConv, LLVMTailCallKind};
use token::token::{IntType, Lit};

impl<'ctx> VariantGen<'ctx> {
    pub(super) fn gen_expr(&mut self, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        match self.ctx.ast[expr] {
            ast::Expr::If(x) => self.gen_if(expr, x),
            ast::Expr::Binary(b) => self.gen_binary(b, expr),
            ast::Expr::Unary(_) => todo!(),
            ast::Expr::Block(b) => self.gen_block(b),
            ast::Expr::Cast(x) => {
                let result_ty = self.ctx.types.expr_to_type[expr].unwrap();
                let result_ty = self.tyid_to_llvm_type(result_ty).unwrap();
                let receiver = self.gen_expr(self.ctx.ast[x].expr);

                let v = self
                    .builder
                    .build_cast(
                        InstructionOpcode::BitCast,
                        receiver.into_basic().unwrap(),
                        result_ty,
                        self.name_gen.name("cast"),
                    )
                    .unwrap();

                Value::from(v)
            }
            ast::Expr::Loop(_) => todo!(),
            ast::Expr::While(_) => todo!(),
            ast::Expr::Let(l) => {
                let expr = self.gen_expr(self.ctx.ast[l].expr);
                let id = self.ctx.symbols.ast_to_symbol[self.ctx.ast[l].sym].unwrap();
                self.symbol_value.insert(id, expr);
                Value::nill()
            }
            ast::Expr::Continue(_) => todo!(),
            ast::Expr::Break(_) => todo!(),
            ast::Expr::Return(x) => {
                if let Some(res) = self.ctx.ast[x].expr {
                    let v = self.gen_expr(res);
                    if let Some(v) = v.into_basic() {
                        self.builder.build_return(Some(&v)).unwrap();
                        return Value::nill();
                    }
                }

                self.builder.build_return(None).unwrap();
                return Value::nill();
            }
            ast::Expr::Become(b) => {
                self.gen_become(b);
                Value::nill()
            }
            ast::Expr::Call(x) => self.gen_call(x),
            ast::Expr::Method(x) => self.gen_method(x),
            ast::Expr::Field(_) => todo!(),
            ast::Expr::Index(_) => todo!(),
            ast::Expr::Literal(l) => self.gen_lit(l, expr),
            ast::Expr::Symbol(s) => {
                let id = self.ctx.symbols.ast_to_symbol[s].unwrap();
                let Some(x) = self.symbol_value.get(&id).cloned() else {
                    panic!("Undefined synmbol {id:?}")
                };
                x
            }
            ast::Expr::Covered(c) => self.gen_expr(c),
        }
    }

    fn gen_block(&mut self, b: NodeId<ast::Block>) -> Value<'ctx> {
        let returns_last = self.ctx.ast[b].returns_last;
        let mut value = None;
        for expr in self.ctx.ast.iter_list_node(self.ctx.ast[b].body) {
            value = Some(self.gen_expr(expr))
        }
        if returns_last {
            value.unwrap_or_else(Value::nill)
        } else {
            Value::nill()
        }
    }

    fn gen_if(&mut self, parent: NodeId<ast::Expr>, s: NodeId<ast::If>) -> Value<'ctx> {
        let cond = self.gen_expr(self.ctx.ast[s].condition);
        if let Some(otherwise) = self.ctx.ast[s].otherwise {
            let bb_then = self
                .ctx
                .context
                .append_basic_block(self.function.clone(), self.name_gen.name("then"));
            let bb_else = self
                .ctx
                .context
                .append_basic_block(self.function.clone(), self.name_gen.name("else"));

            let bb_next = self
                .ctx
                .context
                .append_basic_block(self.function.clone(), self.name_gen.name("next"));

            self.builder
                .build_conditional_branch(cond.into_int(), bb_then.clone(), bb_else.clone())
                .unwrap();

            self.builder.position_at_end(bb_then.clone());
            let then_val = self.gen_block(self.ctx.ast[s].then);
            self.builder
                .build_unconditional_branch(bb_next.clone())
                .unwrap();

            self.builder.position_at_end(bb_else.clone());
            let else_val = self.gen_block(otherwise);
            self.builder
                .build_unconditional_branch(bb_next.clone())
                .unwrap();
            self.builder.position_at_end(bb_next);

            if let Some(ty) = self.tyid_to_llvm_type(self.ctx.types.find_type_ast(parent).unwrap())
            {
                if let Some(l) = then_val.into_basic() {
                    if let Some(r) = else_val.into_basic() {
                        let phi = self
                            .builder
                            .build_phi(ty, self.name_gen.name("phi"))
                            .unwrap();

                        phi.add_incoming(&[(&l, bb_then), (&r, bb_else)]);

                        Value::from(phi)
                    } else {
                        Value::from(l)
                    }
                } else {
                    else_val
                }
            } else {
                Value::nill()
            }
        } else {
            let bb_then = self
                .ctx
                .context
                .append_basic_block(self.function.clone(), self.name_gen.name("then"));

            let bb_next = self
                .ctx
                .context
                .append_basic_block(self.function.clone(), self.name_gen.name("next"));

            self.builder
                .build_conditional_branch(cond.into_int(), bb_then.clone(), bb_next.clone())
                .unwrap();

            self.builder.position_at_end(bb_then.clone());
            self.gen_block(self.ctx.ast[s].then);

            if !matches!(
                self.ctx.types.ty_block(self.ctx.ast[s].then).unwrap(),
                Ty::Prim(PrimTy::Diverges)
            ) {
                self.builder
                    .build_unconditional_branch(bb_next.clone())
                    .unwrap();
            }

            self.builder.position_at_end(bb_next);

            Value::nill()
        }
    }

    fn gen_become(&mut self, b: NodeId<ast::Become>) {
        let ty = self.function.get_type();
        let func = self.module.add_function(
            &format!(
                "__become_{}__",
                b.index(&self.ctx.ast).callee.index(&self.ctx.ast)
            ),
            ty,
            None,
        );
        func.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

        let mut arg_values = Vec::new();
        for arg in self.ctx.ast.iter_list_node(self.ctx.ast[b].args) {
            arg_values.push(self.gen_expr(arg).into_int());
        }

        let param_count = self.ctx.config.num_passing_register;

        if param_count < arg_values.len() {
            todo!()
        }
        let pass_through_count = param_count - arg_values.len();

        let mut params = Vec::new();
        for i in 0..param_count {
            if i >= pass_through_count {
                let v = arg_values.pop().unwrap();
                params.push(v.into());
            } else {
                let v = self.function.get_nth_param(i as u32).unwrap();
                params.push(v.into());
            }
        }

        // TODO: Fix arguments
        let become_func = self
            .builder
            .build_call(func, &params, self.name_gen.name("become"))
            .unwrap();
        become_func.set_call_convention(LLVMCallConv::LLVMGHCCallConv as u32);
        become_func.set_tail_call_kind(LLVMTailCallKind::LLVMTailCallKindMustTail);
        self.builder.build_return(None).unwrap();
    }

    fn gen_binary(&mut self, bin: NodeId<ast::BinaryExpr>, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        let op = self.ctx.ast[bin].op;
        match op {
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
                let left = self.gen_expr(self.ctx.ast[bin].left);
                let right = self.gen_expr(self.ctx.ast[bin].right);
                let ty_id = self.ctx.types.find_type_ast(expr).unwrap();
                let number_ty = NumberType::from_ty(&self.ctx.types.type_graph[ty_id]).unwrap();

                match number_ty {
                    NumberType::Float => {
                        let left = left.into_float();
                        let right = right.into_float();

                        match self.ctx.ast[bin].op {
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

                        match self.ctx.ast[bin].op {
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

                        match self.ctx.ast[bin].op {
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
            ast::BinOp::Lt
            | ast::BinOp::Le
            | ast::BinOp::Ne
            | ast::BinOp::Ge
            | ast::BinOp::Gt
            | ast::BinOp::Eq => {
                let left = self.gen_expr(self.ctx.ast[bin].left);
                let right = self.gen_expr(self.ctx.ast[bin].right);
                let ty_id = self
                    .ctx
                    .types
                    .find_type_ast(self.ctx.ast[bin].left)
                    .unwrap();
                match self.ctx.types.type_graph[ty_id] {
                    Ty::Prim(prim_ty) => match prim_ty {
                        PrimTy::Nil => todo!(),
                        PrimTy::Bool => todo!(),
                        PrimTy::Usize | PrimTy::U64 | PrimTy::U32 | PrimTy::U16 | PrimTy::U8 => {
                            let predicate = match op {
                                ast::BinOp::Lt => IntPredicate::ULT,
                                ast::BinOp::Gt => IntPredicate::UGT,
                                ast::BinOp::Le => IntPredicate::ULE,
                                ast::BinOp::Ge => IntPredicate::UGE,
                                ast::BinOp::Ne => IntPredicate::NE,
                                ast::BinOp::Eq => IntPredicate::EQ,
                                _ => unreachable!(),
                            };

                            self.builder
                                .build_int_compare(
                                    predicate,
                                    left.into_int(),
                                    right.into_int(),
                                    self.name_gen.name("eq"),
                                )
                                .unwrap()
                                .into()
                        }

                        PrimTy::Isize | PrimTy::I64 | PrimTy::I32 | PrimTy::I16 | PrimTy::I8 => {
                            let predicate = match op {
                                ast::BinOp::Lt => IntPredicate::SLT,
                                ast::BinOp::Gt => IntPredicate::SGT,
                                ast::BinOp::Le => IntPredicate::SLE,
                                ast::BinOp::Ge => IntPredicate::SGE,
                                ast::BinOp::Ne => IntPredicate::NE,
                                ast::BinOp::Eq => IntPredicate::EQ,
                                _ => unreachable!(),
                            };

                            self.builder
                                .build_int_compare(
                                    predicate,
                                    left.into_int(),
                                    right.into_int(),
                                    self.name_gen.name("eq"),
                                )
                                .unwrap()
                                .into()
                        }
                        PrimTy::F32 => todo!(),
                        PrimTy::F64 => todo!(),
                        PrimTy::Diverges => todo!(),
                    },
                    Ty::Ptr(_) => todo!(),
                    Ty::PtrMut(_) => todo!(),
                    Ty::Ref(_) => todo!(),
                    Ty::RefMut(_) => todo!(),
                    Ty::Tuple(_) => todo!(),
                    Ty::Fn(..) => todo!(),
                    Ty::Array(..) => todo!(),
                    Ty::Var(..) => todo!(),
                }
            }
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

    fn gen_lit(&self, lit: NodeId<Lit>, expr: NodeId<ast::Expr>) -> Value<'ctx> {
        match &self.ctx.ast[lit] {
            Lit::Str(_) => todo!(),
            Lit::Int(lit_int) => {
                let int_value = match lit_int.value() {
                    IntType::Signed(x) => x as u64,
                    IntType::Unsigned(x) => x,
                };

                let ty = self.ctx.types.find_type_ast(expr).unwrap();
                let v = match self.ctx.types.type_graph[ty] {
                    Ty::Prim(p) => match p {
                        PrimTy::Usize | PrimTy::U64 => {
                            self.ctx.context.i64_type().const_int(int_value, false)
                        }
                        PrimTy::U32 => self.ctx.context.i32_type().const_int(int_value, false),
                        PrimTy::U16 => self.ctx.context.i16_type().const_int(int_value, false),
                        PrimTy::U8 => self.ctx.context.i8_type().const_int(int_value, false),
                        PrimTy::Isize | PrimTy::I64 | PrimTy::I32 | PrimTy::I16 | PrimTy::I8 => {
                            todo!()
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                v.into()
            }

            Lit::Bool(lit_bool) => {
                if lit_bool.value {
                    self.ctx.context.bool_type().const_int(1, false).into()
                } else {
                    self.ctx.context.bool_type().const_int(0, false).into()
                }
            }
        }
    }
}
