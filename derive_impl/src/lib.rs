use std::{
    collections::HashMap,
    path::Path,
    sync::{mpsc, Arc},
};

use inkwell::{
    builder::Builder,
    context::{self, Context},
    llvm_sys::LLVMCallConv,
    module::Module,
    targets::{FileType, Target, TargetTriple},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, IntValue,
    },
    AddressSpace,
};
use proc_macro2::TokenStream;
use syn::ItemFn;

#[cfg(not(target_arch = "x86_64"))]
compile_error!("TARGET not supported");

struct StencilModule<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbols: HashMap<String, AnyValueEnum<'ctx>>,
    count: usize,
}

pub fn template(_attrs: TokenStream, input: TokenStream) -> TokenStream {
    match unsafe { Context::get_global(|ctx| template_inner(ctx, input)) } {
        Ok(x) => x,
        Err(e) => e.to_compile_error(),
    }
}

fn syn_type_to_llvm<'a>(ty: &syn::Type, ctx: &'a Context) -> BasicTypeEnum<'a> {
    match ty {
        syn::Type::Path(x) => {
            if x.qself.is_some() {
                panic!("unsupported type");
            }
            if x.path.segments.len() > 1 {
                panic!("unsupported type");
            }
            let name = x.path.segments.first().unwrap();

            if name.ident == "u64" {
                return ctx.i64_type().into();
            }
            if name.ident == "i64" {
                return ctx.i64_type().into();
            }
            panic!("unsupported type")
        }
        syn::Type::Ptr(_) => ctx.ptr_type(AddressSpace::default()).into(),
        _ => panic!("unsupported type"),
    }
}

fn value_to_int<'ctx>(i: AnyValueEnum<'ctx>, module: &mut StencilModule<'ctx>) -> IntValue<'ctx> {
    if i.is_int_value() {
        return i.into_int_value();
    }

    if i.is_pointer_value() {
        let load = module
            .builder
            .build_load(
                module.context.i64_type(),
                i.into_pointer_value(),
                &format!("load_ptr{}", module.count),
            )
            .unwrap();
        module.count += 1;
        return load.into_int_value();
    }

    todo!()
}

fn compile_macro<'ctx>(
    expr: &syn::ExprMacro,
    module: &mut StencilModule<'ctx>,
) -> AnyValueEnum<'ctx> {
    if expr.mac.path.segments.first().unwrap().ident == "next" {
        let call = expr.mac.parse_body::<syn::ExprCall>().unwrap();
        let name = match *call.func {
            syn::Expr::Path(x) => x.path.segments.first().unwrap().ident.to_string(),
            _ => panic!(),
        };

        let args = call
            .args
            .iter()
            .map(|x| compile_expr(x, module))
            .collect::<Vec<_>>();

        let types = args
            .iter()
            .map(|x| BasicMetadataTypeEnum::try_from(x.get_type()).unwrap())
            .collect::<Vec<_>>();

        let fn_type = module.context.void_type().fn_type(&types, false);

        let args = args
            .iter()
            .map(|x| BasicMetadataValueEnum::try_from(x.clone()).unwrap())
            .collect::<Vec<_>>();

        let func = module
            .module
            .add_function(&format!("__become_{name}"), fn_type, None);
        func.set_call_conventions(LLVMCallConv::LLVMCCallConv as u32);

        let become_func = module
            .builder
            .build_call(func, &args, &format!("_call_{name}"))
            .unwrap();
        become_func.set_tail_call(true);

        module.builder.build_return(None);

        return module.context.i8_type().const_zero().into();
    }
    todo!()
}

fn compile_expr<'ctx>(expr: &syn::Expr, module: &mut StencilModule<'ctx>) -> AnyValueEnum<'ctx> {
    match expr {
        syn::Expr::Array(_) => todo!(),
        syn::Expr::Assign(_) => todo!(),
        syn::Expr::Async(_) => todo!(),
        syn::Expr::Await(_) => todo!(),
        syn::Expr::Binary(ref x) => compile_binary_expr(x, module),
        syn::Expr::Block(_) => todo!(),
        syn::Expr::Break(_) => todo!(),
        syn::Expr::Call(_) => todo!(),
        syn::Expr::Cast(_) => todo!(),
        syn::Expr::Closure(_) => todo!(),
        syn::Expr::Const(_) => todo!(),
        syn::Expr::Continue(_) => todo!(),
        syn::Expr::Field(_) => todo!(),
        syn::Expr::ForLoop(_) => todo!(),
        syn::Expr::Group(_) => todo!(),
        syn::Expr::If(_) => todo!(),
        syn::Expr::Index(_) => todo!(),
        syn::Expr::Infer(_) => todo!(),
        syn::Expr::Let(_) => todo!(),
        syn::Expr::Lit(ref x) => compile_literal_expr(x, module),
        syn::Expr::Loop(_) => todo!(),
        syn::Expr::Macro(x) => compile_macro(x, module),
        syn::Expr::Match(_) => todo!(),
        syn::Expr::MethodCall(_) => todo!(),
        syn::Expr::Paren(_) => todo!(),
        syn::Expr::Path(pat) => {
            let pat = dbg!(pat.path.segments.first().unwrap().ident.to_string());
            dbg!(module.symbols.get(&pat)).unwrap().clone()
        }
        syn::Expr::Range(_) => todo!(),
        syn::Expr::Reference(_) => todo!(),
        syn::Expr::Repeat(_) => todo!(),
        syn::Expr::Return(_) => todo!(),
        syn::Expr::Struct(_) => todo!(),
        syn::Expr::Try(_) => todo!(),
        syn::Expr::TryBlock(_) => todo!(),
        syn::Expr::Tuple(_) => todo!(),
        syn::Expr::Unary(_) => todo!(),
        syn::Expr::Unsafe(_) => todo!(),
        syn::Expr::Verbatim(_) => todo!(),
        syn::Expr::While(_) => todo!(),
        syn::Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
}

fn compile_literal_expr<'ctx>(
    expr: &syn::ExprLit,
    module: &mut StencilModule<'ctx>,
) -> AnyValueEnum<'ctx> {
    match expr.lit {
        syn::Lit::Str(_) => todo!(),
        syn::Lit::ByteStr(_) => todo!(),
        syn::Lit::CStr(_) => todo!(),
        syn::Lit::Byte(_) => todo!(),
        syn::Lit::Char(_) => todo!(),
        syn::Lit::Int(ref x) => module
            .context
            .i64_type()
            .const_int(x.base10_parse().unwrap(), false)
            .into(),
        syn::Lit::Float(_) => todo!(),
        syn::Lit::Bool(_) => todo!(),
        syn::Lit::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

fn compile_binary_expr<'ctx>(
    expr: &syn::ExprBinary,
    module: &mut StencilModule<'ctx>,
) -> AnyValueEnum<'ctx> {
    let left = compile_expr(&expr.left, module);
    let right = compile_expr(&expr.right, module);

    match expr.op {
        syn::BinOp::Add(_) => {
            let left = value_to_int(left, module);
            let right = value_to_int(right, module);
            let v = module
                .builder
                .build_int_add(left, right, &format!("add{}", module.count))
                .unwrap();
            module.count += 1;
            return v.into();
        }
        syn::BinOp::Sub(_) => {
            let left = value_to_int(left, module);
            let right = value_to_int(right, module);
            let v = module
                .builder
                .build_int_sub(left, right, &format!("sub{}", module.count))
                .unwrap();
            module.count += 1;
            return v.into();
        }
        syn::BinOp::Mul(_) => todo!(),
        syn::BinOp::Div(_) => todo!(),
        syn::BinOp::Rem(_) => todo!(),
        syn::BinOp::And(_) => todo!(),
        syn::BinOp::Or(_) => todo!(),
        syn::BinOp::BitXor(_) => todo!(),
        syn::BinOp::BitAnd(_) => todo!(),
        syn::BinOp::BitOr(_) => todo!(),
        syn::BinOp::Shl(_) => todo!(),
        syn::BinOp::Shr(_) => todo!(),
        syn::BinOp::Eq(_) => todo!(),
        syn::BinOp::Lt(_) => todo!(),
        syn::BinOp::Le(_) => todo!(),
        syn::BinOp::Ne(_) => todo!(),
        syn::BinOp::Ge(_) => todo!(),
        syn::BinOp::Gt(_) => todo!(),
        syn::BinOp::AddAssign(_) => todo!(),
        syn::BinOp::SubAssign(_) => todo!(),
        syn::BinOp::MulAssign(_) => todo!(),
        syn::BinOp::DivAssign(_) => todo!(),
        syn::BinOp::RemAssign(_) => todo!(),
        syn::BinOp::BitXorAssign(_) => todo!(),
        syn::BinOp::BitAndAssign(_) => todo!(),
        syn::BinOp::BitOrAssign(_) => todo!(),
        syn::BinOp::ShlAssign(_) => todo!(),
        syn::BinOp::ShrAssign(_) => todo!(),
        _ => todo!(),
    }
}

fn compile_local_stmt<'ctx>(stmt: &syn::Local, module: &mut StencilModule<'ctx>) {
    let pat = match stmt.pat {
        syn::Pat::Ident(ref x) => x.ident.clone(),
        _ => panic!(),
    };

    let Some(ref init) = stmt.init else { panic!() };
    let v = compile_expr(&init.expr, module);

    module.symbols.insert(pat.to_string(), v);
}

fn compile_stmt<'ctx>(stmt: &syn::Stmt, module: &mut StencilModule<'ctx>) {
    match stmt {
        syn::Stmt::Local(x) => {
            compile_local_stmt(x, module);
        }
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(x, _) => {
            compile_expr(x, module);
        }
        syn::Stmt::Macro(_) => {}
    }
}

fn template_inner(context: &Context, input: TokenStream) -> Result<TokenStream, syn::Error> {
    let parsed = syn::parse2::<ItemFn>(input)?;

    let name = parsed.sig.ident.to_string();

    let module = context.create_module(&name);
    let builder = context.create_builder();

    let mut module = StencilModule {
        module,
        builder,
        context,
        symbols: HashMap::new(),
        count: 0,
    };

    let i64_type = context.i64_type();

    let args = parsed.sig.inputs.iter().map(|x| {
        let pat = match x {
            syn::FnArg::Receiver(_) => panic!("'self' not allowed on a template function"),
            syn::FnArg::Typed(pat) => pat,
        };

        let name = match *pat.pat {
            syn::Pat::Ident(ref x) => x.ident.to_string(),
            _ => panic!("argument binding patterns other then a plain name are not allowed on a template function")
        };

        let ty = syn_type_to_llvm(&pat.ty, context);

        (name,ty)
    }).collect::<Vec<_>>();

    let args_ty: Vec<_> = args
        .iter()
        .map(|x| BasicMetadataTypeEnum::from(x.1.clone()))
        .collect();

    let func_type = match parsed.sig.output {
        syn::ReturnType::Default => context.void_type().fn_type(&args_ty, false),
        syn::ReturnType::Type(_, ref ty) => syn_type_to_llvm(ty, context).fn_type(&args_ty, false),
    };

    let function = module.module.add_function("__stencil__", func_type, None);
    function.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

    let bb = context.append_basic_block(function, "entry");
    module.builder.position_at_end(bb);

    for (idx, arg) in args.iter().enumerate() {
        module.symbols.insert(
            dbg!(arg.0.clone()),
            function
                .get_nth_param(idx as u32)
                .unwrap()
                .as_any_value_enum(),
        );
    }

    for stmt in parsed.block.stmts.iter() {
        compile_stmt(stmt, &mut module);
    }

    module.module.print_to_stderr();

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
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    module
        .module
        .set_data_layout(&machine.get_target_data().get_data_layout());
    module.module.set_triple(&target_triple);

    let buffer = machine
        .write_to_memory_buffer(&module.module, FileType::Object)
        .unwrap();
    let data = buffer.as_slice().to_vec();

    for (k, v) in std::env::vars() {
        println!("{} = {}", k, v);
    }

    let path = Path::new(env!("STUCCO_OUT_DIR"));

    let out_dir = path.join("stucco");

    let _ = std::fs::create_dir(&out_dir);

    let file_path = out_dir.join(format!("{name}.o"));
    dbg!(&file_path);

    std::fs::write(file_path, &data).unwrap();

    let object_file = buffer.create_object_file().unwrap();

    for sec in object_file.get_sections() {
        if sec.get_name() != Some(c".text") {
            continue;
        }

        for sym in object_file.get_symbols() {
            if sym.get_name() != Some(c"__stencil__") {
                continue;
            }
            let addr = sym.get_address() as usize;
            println!("OFFSET: {:0<2x}", addr);
            let end = addr + sym.size() as usize;
            let bytes = &sec.get_contents()[addr..end];
            print!("BYTES: ");
            for b in bytes {
                print!("{:0<2x} ", b)
            }
            println!()
        }
    }

    Ok(TokenStream::new())
}
