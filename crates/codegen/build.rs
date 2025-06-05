fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    println!("cargo::rerun-if-changed=wrapper/rust_wrapper.cpp");

    cc::Build::new()
        .file("wrapper/rust_wrapper.cpp")
        .compile("wrapper");

    println!("cargo::rustc-link-search=native={}", out_dir);
    println!("cargo::rustc-link-lib=static=wrapper");
}
