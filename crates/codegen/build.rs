fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    //for (k, v) in std::env::vars() {
    //println!("cargo::warning={k}::{v}");
    //}

    println!("cargo::rerun-if-changed=wrapper/rust_wrapper.cpp");

    cc::Build::new()
        .file("wrapper/rust_wrapper.cpp")
        .compile("wrapper");

    println!("cargo::rustc-link-search=native={}", out_dir);
    println!("cargo::rustc-link-lib=static=wrapper");
}
