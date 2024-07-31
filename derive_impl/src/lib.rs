use inkwell::{
    attributes::Attribute,
    builder::Builder,
    context::Context,
    llvm_sys::{object, LLVMCallConv},
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    targets::{FileType, Target, TargetTriple},
    types::{AnyType as _, BasicType},
    values::{AnyValue, BasicValue, FunctionValue},
    AddressSpace, IntPredicate,
};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::{cell::Cell, collections::HashMap, path::Path};
use syn::{spanned::Spanned, Error, GenericParam, ItemFn, Result, ReturnType};

#[cfg(not(target_arch = "x86_64"))]
compile_error!("TARGET not supported");

mod obj;
mod value;
use value::{Mutability, Signing, Symbol, Ty, Value};

struct StencilModule<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbols: HashMap<String, Symbol<'ctx>>,
    function: Option<FunctionValue<'ctx>>,
    count: Cell<usize>,
}

impl StencilModule<'_> {
    pub fn next_name(&self, prefix: &str) -> String {
        let res = format!("{}{}", prefix, self.count.get());
        self.count.set(self.count.get() + 1);
        res
    }
}

fn path_to_ident(path: &syn::Path) -> Result<syn::Ident> {
    if path.segments.is_empty() || path.segments.len() > 1 {
        return Err(Error::new_spanned(path, "unsupported path"));
    }

    Ok(path.segments.first().unwrap().ident.clone())
}

pub fn template(attrs: TokenStream, input: TokenStream) -> TokenStream {
    match unsafe { Context::get_global(|ctx| template_inner(ctx, attrs, input)) } {
        Ok(x) => x,
        Err(e) => e.to_compile_error(),
    }
}

fn syn_type_to_ty<'a>(ty: &syn::Type) -> Result<Ty> {
    match ty {
        syn::Type::Path(x) => {
            if x.qself.is_some() {
                return Err(Error::new_spanned(x, "unsupported type"));
            }
            if x.path.segments.len() > 1 {
                return Err(Error::new_spanned(x, "unsupported type"));
            }

            let name = path_to_ident(&x.path)?;

            if name == "u64" {
                return Ok(Ty::B64(Signing::Unsigned));
            }

            if name == "i64" {
                return Ok(Ty::B64(Signing::Signed));
            }

            return Err(Error::new_spanned(x, "unsupported type"));
        }
        syn::Type::Ptr(x) => {
            let inner = syn_type_to_ty(&x.elem)?;
            return Ok(Ty::Ptr(Mutability::Mutable, Box::new(inner)));
        }
        syn::Type::BareFn(x) => {
            let args = x.inputs.iter().map(|x| syn_type_to_ty(&x.ty)).try_fold(
                Vec::new(),
                |mut acc, ty| match ty {
                    Ok(x) => {
                        acc.push(x);
                        Ok(acc)
                    }
                    Err(e) => Err(e),
                },
            )?;
            let out = if let ReturnType::Type(_, ref ty) = x.output {
                syn_type_to_ty(ty)?
            } else {
                Ty::Void
            };

            Ok(Ty::Fn(args, Box::new(out)))
        }
        _ => Err(Error::new_spanned(ty, "unsupported type")),
    }
}

fn compile_store_unary<'ctx>(
    expr: &syn::ExprUnary,
    value: Value<'ctx>,
    span: Span,
    module: &mut StencilModule<'ctx>,
) -> Result<()> {
    match expr.op {
        syn::UnOp::Deref(_) => {
            let v = compile_expr(&*expr.expr, module)?;
            v.store(span, value, module)
        }
        _ => return Err(Error::new(expr.span(), "cannot assign to this expression")),
    }
}

fn compile_store<'ctx>(
    expr: &syn::Expr,
    value: Value<'ctx>,
    span: Span,
    module: &mut StencilModule<'ctx>,
) -> Result<()> {
    match expr {
        syn::Expr::Paren(x) => compile_store(&x.expr, value, span, module),
        syn::Expr::Unary(x) => compile_store_unary(x, value, span, module),
        _ => todo!(),
    }
}

fn compile_macro<'ctx>(
    expr: &syn::ExprMacro,
    module: &mut StencilModule<'ctx>,
) -> Result<Value<'ctx>> {
    let name = path_to_ident(&expr.mac.path)?;
    if name == "next" {
        let call = expr.mac.parse_body::<syn::ExprCall>().unwrap();
        let name = match *call.func {
            syn::Expr::Path(x) => x.path.segments.first().unwrap().ident.to_string(),
            _ => panic!(),
        };

        let args = call.args.iter().map(|x| compile_expr(x, module)).try_fold(
            Vec::new(),
            |mut acc, item| match item {
                Ok(x) => {
                    acc.push(x);
                    Ok(acc)
                }
                Err(e) => Err(e),
            },
        )?;

        let types =
            args.iter()
                .map(|x| x.to_basic_meta_type())
                .try_fold(Vec::new(), |mut acc, item| match item {
                    Ok(x) => {
                        acc.push(x);
                        Ok(acc)
                    }
                    Err(e) => Err(e),
                })?;

        let fn_type = module.context.void_type().fn_type(&types, false);

        let args = args.iter().map(|x| x.to_basic_meta_value()).try_fold(
            Vec::new(),
            |mut acc, item| match item {
                Ok(x) => {
                    acc.push(x);
                    Ok(acc)
                }
                Err(e) => Err(e),
            },
        )?;

        let func = module
            .module
            .add_function(&format!("__become_{name}"), fn_type, None);
        func.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

        let become_func = module
            .builder
            .build_call(func, &args, &format!("_call_{name}"))
            .unwrap();
        become_func.set_call_convention(LLVMCallConv::LLVMGHCCallConv as u32);
        become_func.set_tail_call(true);

        module.builder.build_return(None).unwrap();

        return Ok(Value::void(expr.span()));
    }
    return Err(Error::new_spanned(expr, "unknown macro"));
}

fn compile_expr<'ctx>(expr: &syn::Expr, module: &mut StencilModule<'ctx>) -> Result<Value<'ctx>> {
    match expr {
        syn::Expr::Binary(ref x) => compile_binary_expr(x, module),
        syn::Expr::Lit(ref x) => compile_literal_expr(x, module),
        syn::Expr::Path(path) => {
            let ident = path_to_ident(&path.path)?;
            match module.symbols.get(&ident.to_string()) {
                Some(Symbol::Local(x)) => {
                    return Ok(x.clone());
                }
                Some(Symbol::Constant { ty, addr, .. }) => {
                    let v = if ty.is_ptr() {
                        let v = module
                            .builder
                            .build_load(
                                module.context.i64_type(),
                                *addr,
                                &module.next_name("load_global_ptr"),
                            )
                            .unwrap()
                            .into_int_value();

                        /*
                        v.as_instruction_value()
                            .unwrap()
                            .set_volatile(true)
                            .unwrap();
                        */

                        module
                            .builder
                            .build_int_to_ptr(
                                v,
                                module.context.ptr_type(AddressSpace::default()),
                                "int_to_ptr",
                            )
                            .unwrap()
                            .into()
                    } else {
                        let Some(t) = ty.to_basic_type(module) else {
                            return Err(Error::new_spanned(expr, "use of void constant"));
                        };
                        let t = t.into_int_type();
                        module
                            .builder
                            .build_load(t, *addr, &module.next_name("load_global_ptr"))
                            .unwrap()
                            .into()
                    };
                    return Ok(Value::new(ty.clone(), v, expr.span()));
                }
                None => return Err(Error::new_spanned(ident, "undefined symbol")),
            }
        }
        syn::Expr::Macro(x) => compile_macro(x, module),
        syn::Expr::Unary(x) => compile_unary_expr(x, module),
        syn::Expr::Paren(x) => compile_expr(&x.expr, module),
        syn::Expr::Assign(assign) => {
            let right = compile_expr(&assign.right, module)?;
            compile_store(&assign.left, right, assign.span(), module)?;
            Ok(Value::void(assign.span()))
        }
        syn::Expr::Call(x) => {
            let func = compile_expr(&x.func, module)?;

            let values =
                x.args.iter().try_fold(Vec::new(), |mut acc, item| {
                    match compile_expr(item, module) {
                        Ok(x) => {
                            acc.push(x);
                            Ok(acc)
                        }
                        Err(e) => Err(e),
                    }
                })?;

            func.call(x.span(), values, module)
        }
        syn::Expr::If(x) => {
            let branch = module.context.append_basic_block(
                module.function.clone().unwrap(),
                &module.next_name("branch"),
            );
            let merge = module
                .context
                .append_basic_block(module.function.clone().unwrap(), &module.next_name("merge"));

            let expr = compile_expr(&x.cond, module)?;

            if *expr.ty() != Ty::Bool {
                return Err(Error::new(*expr.span(), "expected boolean expression"));
            }

            module
                .builder
                .build_conditional_branch(expr.value().unwrap().into_int_value(), branch, merge)
                .unwrap();

            module.builder.position_at_end(branch);

            for s in x.then_branch.stmts.iter() {
                compile_stmt(s, module)?;
            }

            module.builder.build_unconditional_branch(merge).unwrap();

            module.builder.position_at_end(merge);

            Ok(Value::void(x.span()))
        }
        syn::Expr::Return(x) => {
            let v = x
                .expr
                .as_ref()
                .map(|x| compile_expr(&x, module).and_then(|x| x.to_basic_value()))
                .transpose()?;
            module
                .builder
                .build_return(v.as_ref().map(|x| x as &dyn BasicValue))
                .unwrap();
            Ok(Value::void(x.span()))
        }
        x => Err(Error::new_spanned(x, "unsupported expression")),
    }
}

fn compile_literal_expr<'ctx>(
    expr: &syn::ExprLit,
    module: &mut StencilModule<'ctx>,
) -> Result<Value<'ctx>> {
    match expr.lit {
        syn::Lit::Int(ref x) => {
            let v: u64 = x.base10_parse()?;
            let v = Value::from_u64(module, v, x.span());
            Ok(v)
        }
        ref x => Err(Error::new_spanned(x, "unsupported literal")),
    }
}

fn compile_unary_expr<'ctx>(
    expr: &syn::ExprUnary,
    module: &mut StencilModule<'ctx>,
) -> Result<Value<'ctx>> {
    match expr.op {
        syn::UnOp::Deref(_) => {
            let res = compile_expr(&*expr.expr, module)?;
            res.deref(expr.span(), module)
        }
        syn::UnOp::Not(_) => todo!(),
        syn::UnOp::Neg(_) => todo!(),
        _ => todo!(),
    }
}

fn compile_binary_expr<'ctx>(
    expr: &syn::ExprBinary,
    module: &mut StencilModule<'ctx>,
) -> Result<Value<'ctx>> {
    match expr.op {
        syn::BinOp::Add(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            left.add(right, expr.span(), module)
        }
        syn::BinOp::Sub(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            left.sub(right, expr.span(), module)
        }
        syn::BinOp::AddAssign(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            let v = left.add(right, expr.span(), module)?;
            compile_store(&expr.left, v, expr.span(), module)?;
            Ok(Value::void(expr.span()))
        }
        syn::BinOp::SubAssign(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            let v = left.sub(right, expr.span(), module)?;
            compile_store(&expr.left, v, expr.span(), module)?;
            Ok(Value::void(expr.span()))
        }
        syn::BinOp::Eq(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            left.cmp(IntPredicate::EQ, right, expr.span(), module)
        }
        syn::BinOp::Ne(_) => {
            let left = compile_expr(&expr.left, module)?;
            let right = compile_expr(&expr.right, module)?;
            left.cmp(IntPredicate::NE, right, expr.span(), module)
        }
        x => {
            return Err(Error::new_spanned(x, "unsupported binary operator"));
        }
    }
}

fn compile_local_stmt<'ctx>(stmt: &syn::Local, module: &mut StencilModule<'ctx>) -> Result<()> {
    let pat = match stmt.pat {
        syn::Pat::Ident(ref x) => x.ident.clone(),
        _ => panic!(),
    };

    let Some(ref init) = stmt.init else {
        return Err(Error::new_spanned(
            stmt,
            "binding without initializer is not supported",
        ));
    };
    let v = compile_expr(&init.expr, module)?;

    module.symbols.insert(pat.to_string(), Symbol::Local(v));

    Ok(())
}

fn compile_stmt<'ctx>(stmt: &syn::Stmt, module: &mut StencilModule<'ctx>) -> Result<()> {
    match stmt {
        syn::Stmt::Local(x) => {
            compile_local_stmt(x, module)?;
        }
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(x, _) => {
            compile_expr(x, module)?;
        }
        syn::Stmt::Macro(_) => {}
    }
    Ok(())
}

fn template_inner(
    context: &Context,
    attrs: TokenStream,
    input: TokenStream,
) -> Result<TokenStream> {
    let parsed = syn::parse2::<ItemFn>(input)?;

    let name = parsed.sig.ident;

    let module = context.create_module(&name.to_string());
    let builder = context.create_builder();

    let mut entry = false;

    if !attrs.is_empty() {
        let meta = syn::parse2::<syn::Meta>(attrs)?;

        match meta {
            syn::Meta::Path(x) => {
                if x.segments
                    .first()
                    .map(|x| x.ident == "entry")
                    .unwrap_or(false)
                {
                    entry = true;
                }
            }
            _ => {}
        }
    }

    let mut module = StencilModule {
        module,
        builder,
        context,
        symbols: HashMap::new(),
        count: Cell::new(0),
        function: None,
    };

    /*
    module.module.add_basic_value_flag(
        "direct-access-external-data",
        FlagBehavior::Override,
        module.context.i32_type().const_int(1, false),
    );

    module.module.add_basic_value_flag(
        "PIC Level",
        FlagBehavior::Override,
        module.context.bool_type().const_zero(),
    );
    */

    let args = parsed.sig.inputs.iter().map(|x| {
        let pat = match x {
            syn::FnArg::Receiver(_) => panic!("'self' not allowed on a template function"),
            syn::FnArg::Typed(pat) => pat,
        };

        let name = match *pat.pat {
            syn::Pat::Ident(ref x) => x.ident.clone(),
            _ => panic!("argument binding patterns other then a plain name are not allowed on a template function")
        };

        match syn_type_to_ty(&pat.ty) {
            Ok(ty) => {
                Ok((name,ty))
            }
            Err(e) => Err(e)
        }

    }).try_fold(Vec::new(), |mut acc, item| match item {
        Ok(x) => {
            acc.push(x);
            Ok(acc)
        },
        Err(e) => Err(e)
    })?;

    let args_ty: Vec<_> = args
        .iter()
        .map(|x| x.1.to_basic_meta_type(&mut module).unwrap())
        .collect();

    let func_type = match parsed.sig.output {
        syn::ReturnType::Default => context.void_type().fn_type(&args_ty, false),
        syn::ReturnType::Type(_, ref ty) => {
            let ty = syn_type_to_ty(ty)?.to_basic_type(&mut module).unwrap();
            ty.fn_type(&args_ty, false)
        }
    };

    let function = module.module.add_function("__stencil__", func_type, None);
    if entry {
        function.set_call_conventions(LLVMCallConv::LLVMFastCallConv as u32);
    } else {
        function.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);
    }

    module.function = Some(function.clone());

    let bb = context.append_basic_block(function, "entry");
    module.builder.position_at_end(bb);

    for (idx, arg) in args.iter().enumerate() {
        let existing = module
            .symbols
            .insert(
                arg.0.to_string(),
                Symbol::Local(Value::new(
                    arg.1.clone(),
                    function
                        .get_nth_param(idx as u32)
                        .unwrap()
                        .as_any_value_enum(),
                    arg.0.span(),
                )),
            )
            .is_some();

        if existing {
            return Err(Error::new_spanned(arg.0.clone(), "duplicate argument"));
        }
    }

    let union = module
        .context
        .struct_type(&[module.context.ptr_type(Default::default()).into()], false);

    for p in parsed.sig.generics.params {
        let GenericParam::Const(c) = p else {
            return Err(Error::new_spanned(p, "only constant generics are allowed"));
        };

        let ty = syn_type_to_ty(&c.ty)?;

        let global_v = module.module.add_global(
            module.context.i8_type(),
            None,
            &format!("__patch_global__{}", c.ident),
        );

        global_v.set_linkage(Linkage::External);

        let v = module
            .builder
            .build_alloca(union, &c.ident.to_string())
            .unwrap();

        module
            .builder
            .build_store(v, global_v.as_pointer_value())
            .unwrap();

        let ptr_ty = module.context.ptr_type(AddressSpace::default());
        let asm_fn_ty = module.context.void_type().fn_type(&[ptr_ty.into()], false);

        let asm = module.context.create_inline_asm(
            asm_fn_ty,
            String::new(),
            "*m".to_string(),
            true,
            false,
            None,
            false,
        );

        let call = module
            .builder
            .build_indirect_call(asm_fn_ty, asm, &[v.into()], &module.next_name("clobber"))
            .unwrap();

        let id = Attribute::get_named_enum_kind_id("elementtype");
        assert_ne!(id, 0);
        let attr = module.context.create_type_attribute(
            id,
            module
                .context
                .ptr_type(Default::default())
                .as_any_type_enum(),
        );
        call.add_attribute(inkwell::attributes::AttributeLoc::Param(0), attr);

        module.symbols.insert(
            c.ident.to_string(),
            Symbol::Constant {
                ty,
                addr: v,
                span: c.span(),
            },
        );
    }

    for stmt in parsed.block.stmts.iter() {
        compile_stmt(stmt, &mut module)?;
    }

    let str = module.module.to_string();
    let mut patched = String::new();

    let out = env!("STUCCO_OUT_DIR");

    // A hack to set options which are not supported by llvm-c
    for l in str.lines() {
        if !l.starts_with("@__patch_global__") {
            patched.push_str(l);
            patched.push('\n');
            continue;
        }

        let Some(idx) = l.find("external global") else {
            patched.push_str(l);
            patched.push('\n');
            continue;
        };

        let mut str = l.to_string();
        str.insert_str(idx + "external".len(), " dso_local");
        patched.push_str(&str);
        patched.push('\n');
    }

    std::fs::write(
        Path::new(&out).join(format!("{name}.ll")),
        patched.trim().as_bytes(),
    )
    .unwrap();

    let buffer =
        MemoryBuffer::create_from_memory_range_copy(patched.trim().as_bytes(), "__stucco__source");
    module.module = module
        .context
        .create_module_from_ir(buffer)
        .inspect_err(|_| {
            for (idx, l) in patched.lines().enumerate() {
                println!("{idx:>2}:{}", l);
            }
        })
        .expect("failed to parse IR");

    module.module.verify().unwrap();

    if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
        Target::initialize_x86(&Default::default());
    } else {
        panic!("target not supported");
    };

    let target_triple = TargetTriple::create(env!("STUCCO_TARGET"));
    //let target_triple = TargetTriple::create("aarch64-linux-gnu-gcc");
    let target = Target::from_triple(&target_triple).unwrap();
    let machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Static,
            inkwell::targets::CodeModel::Medium,
        )
        .unwrap();

    module
        .module
        .set_data_layout(&machine.get_target_data().get_data_layout());
    module.module.set_triple(&target_triple);

    let buffer = machine
        .write_to_memory_buffer(&module.module, FileType::Object)
        .unwrap();

    std::fs::write(Path::new(&out).join(format!("{name}.o")), buffer.as_slice()).unwrap();

    let patches = obj::extract_patches(buffer.as_slice());

    let bytes = patches.text.iter().map(|x| {
        quote! { #x }
    });

    let jumps = patches.jumps.iter().map(|(name, v)| {
        let offset = v.offset;
        let size = v.target_size;
        let addend = v.addend;
        quote! {
            ::stucco::TemplateJump::__create(#name,&[::stucco::Ty::U64],#offset,#addend,#size)
        }
    });

    let globals = patches.immediates.iter().map(|(name, v)| {
        let offset = v.offset;
        let size = v.target_size;
        quote! {
            ::stucco::TemplateConstant::__create(#name,::stucco::Ty::U64,#offset,#size)
        }
    });

    let res = if entry {
        quote! {
            #[allow(non_camel_case_types)]
            struct #name;

            impl ::stucco::EntryTemplate<()> for #name{
                const BYTES: &[u8] = &[
                    #(#bytes,)*
                ];
                const CONSTANTS: &[::stucco::TemplateConstant] = &[
                    #(#globals,)*
                ];
                const JUMPS: &[::stucco::TemplateJump] = &[
                    #(#jumps,)*
                ];
            }
        }
    } else {
        quote! {
            #[allow(non_camel_case_types)]
            struct #name;

            impl ::stucco::Template<()> for #name{
                const BYTES: &[u8] = &[
                    #(#bytes,)*
                ];
                const CONSTANTS: &[::stucco::TemplateConstant] = &[
                    #(#globals,)*
                ];
                const JUMPS: &[::stucco::TemplateJump] = &[
                    #(#jumps,)*
                ];
            }
        }
    };

    Ok(res)
}
