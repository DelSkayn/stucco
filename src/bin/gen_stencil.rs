/*
use std::error::Error;

use inkwell::{
    context::Context,
    llvm_sys::LLVMCallConv,
    targets::{FileType, Target, TargetTriple},
};

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("add");
    let builder = context.create_builder();

    let i64_type = context.i64_type();
    let void_type = context.void_type();
    let fn_type = void_type.fn_type(
        &[
            i64_type.into(),
            i64_type.into(),
            i64_type.into(),
            i64_type.into(),
        ],
        false,
    );

    let function = module.add_function("__stencil__", fn_type, None);
    function.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

    let become_fn_type =
        void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
    function.set_call_conventions(LLVMCallConv::LLVMGHCCallConv as u32);

    let function_become = module.add_function("become", become_fn_type, None);

    let bb = context.append_basic_block(function, "entry");
    builder.position_at_end(bb);

    let stk = function.get_nth_param(0).unwrap().into_int_value();
    let a = function.get_nth_param(1).unwrap().into_int_value();
    let b = function.get_nth_param(2).unwrap().into_int_value();
    let fallthrough = function.get_nth_param(3).unwrap().into_int_value();

    let sum = builder.build_int_mul(a, b, "mul").unwrap();
    let call = builder.build_call(
        function_become,
        &[stk.into(), sum.into(), fallthrough.into()],
        "call_become",
    )?;
    call.set_call_convention(LLVMCallConv::LLVMGHCCallConv as u32);
    call.set_tail_call(true);

    builder.build_return(None)?;

    module.verify()?;
    module.print_to_stderr();

    let target = if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
        //Target::initialize_aarch64(&Default::default());
        Target::initialize_x86(&Default::default());
        //} else if cfg!(any(target_arch = "aarch64")) {
    } else {
        panic!("target not supported");
    };

    let mut target = Target::get_first().unwrap();
    loop {
        dbg!(target.get_name());
        let Some(n) = target.get_next() else {
            break;
        };
        target = n;
    }

    let target_triple = TargetTriple::create(env!("TARGET"));
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

    module.set_data_layout(&machine.get_target_data().get_data_layout());
    module.set_triple(&target_triple);

    let buffer = machine.write_to_memory_buffer(&module, FileType::Object)?;
    let data = buffer.as_slice().to_vec();

    std::fs::write("out.o", &data)?;

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

    Ok(())
}
*/

fn main() {
    println!("tmp");
}
