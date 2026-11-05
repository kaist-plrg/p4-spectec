fn main() {
    lalrpop::process_root().expect("generate SpecTec parser");
    // A K library that metacirc loads finds the host functions of
    // `metacirc::engine::k::ffi` by name, so metacirc exports its symbols
    println!("cargo:rustc-link-arg-bin=metacirc=-rdynamic");
}
