fn main() {
    println!(
        "cargo:rustc-env=STUCCO_TARGET={}",
        std::env::var("TARGET").unwrap()
    );
    println!(
        "cargo:rustc-env=STUCCO_OUT_DIR={}",
        std::env::var("OUT_DIR").unwrap()
    );
}
