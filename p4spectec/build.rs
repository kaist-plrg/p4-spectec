fn main() {
    // Track source additions as well as edits to existing grammars.
    println!("cargo:rerun-if-changed=src");
    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .process_current_dir()
        .expect("generate SpecTec parser");
}
