pub enum Target {
    X86_64,
    Aarch64,
    Riscv,
}

struct TargetInfo {
    /// The amount of arguments that can be passed by register in ghccc for the target.
    register_passed_argument_count: u32,
}

const X86_64_TARGET_INFO: TargetInfo = TargetInfo {
    register_passed_argument_count: 10,
};

#[cfg(test)]
mod test {
    use std::fmt::Write;

    fn arg_produce_llvm_ir(amount: usize) -> String {
        let mut res = String::new();
        writeln!(res, r#"source_filename = "arg_llvm_test""#).unwrap();
        writeln!(res).unwrap();

        write!(res, r#"declare ghccc i64 @next(i64"#).unwrap();
        for _ in 1..amount {
            write!(res, ", i64").unwrap();
        }
        writeln!(res, ")").unwrap();

        write!(res, r#"define ghccc i64 @test_func(i64 %test"#).unwrap();
        for i in 1..amount {
            write!(res, ", i64 %arg_{i}").unwrap();
        }
        writeln!(res, ") {{").unwrap();
        write!(res, r#"entry:"#).unwrap();

        write!(res, "\t%call = musttail call ghccc i64 @next(i64 0").unwrap();
        for i in 1..amount {
            write!(res, ", i64 {i}").unwrap();
        }
        writeln!(res, ")").unwrap();
        writeln!(res, "\tret i64 %call").unwrap();
        writeln!(res, "}}").unwrap();
        res
    }

    #[test]
    fn validate_register_passed_argument() {}
}
