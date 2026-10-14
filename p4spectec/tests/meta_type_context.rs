//! Type and function shadowing in meta value membership

use std::path::PathBuf;

use p4spectec::{
    interface,
    lang::data::{arena::Arena, value::flat::get},
    pass::structure,
    runner::{Config, NullExtern, Runner, Spec},
};

#[test]
fn membership_preserves_type_and_function_shadowing() {
    let path_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let spec_al = p4spectec::algo(&[
        path_root.join("../spec-meta"),
        path_root.join("tests/fixtures/subtyp-context.watsup"),
    ])
    .unwrap();
    let spec_sl = structure::convert(spec_al, true).unwrap();
    for cache in [false, true] {
        let mut arena = Arena::new();
        let mut runner = Runner::new(
            &mut arena,
            Spec::Sl(spec_sl.clone()),
            Config::new(cache, false, false),
            interface::al(),
            NullExtern,
        )
        .unwrap();
        let value = runner
            .eval_func(&mut arena, "test_subtyp_context", &[], &[])
            .result
            .unwrap();
        assert!(get::bool(&arena, &value).unwrap());
    }
}
