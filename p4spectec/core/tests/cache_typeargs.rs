//! Test whether a generic builtin can observe its type argument with caching.
use p4spectec::{
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            typ,
            value::flat::{self as value, Value},
        },
    },
    runner::{Config, NullExtern, build_al, build_pl, build_sl},
};
use std::path::PathBuf;

#[test]
fn print_hint_depends_on_type_argument() {
    for (stage, cache) in (0..3).flat_map(|stage| [false, true].map(|cache| (stage, cache))) {
        let path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/cache-typeargs.watsup");
        let mut arena = Arena::new();
        let config = Config::new(cache, false, true, false);
        let mut runner = match stage {
            0 => build_al(&mut arena, p4spectec::algo(&[path]).unwrap(), config, NullExtern),
            1 => build_sl(
                &mut arena,
                p4spectec::structure(&[path], true).unwrap(),
                config,
                NullExtern,
            ),
            2 => build_pl(&mut arena, p4spectec::prosify(&[path]).unwrap(), config, NullExtern),
            _ => unreachable!(),
        }
        .unwrap();
        let first_type = typ::make::var(
            p4spectec::phrase!(node: "firstType".into(), span: Span::default()),
            vec![],
        );
        let second_type = typ::make::var(
            p4spectec::phrase!(node: "secondType".into(), span: Span::default()),
            vec![],
        );
        let arg = p4spectec::case_string!(arena: &mut arena, shape: "SHARED", args: Vec::<Value>::new(), typ: "firstType", span: Span::default()).unwrap();
        let first = runner
            .eval_func(&mut arena, "print_", &[first_type], &[arg])
            .result
            .unwrap();
        let second = runner
            .eval_func(&mut arena, "print_", &[second_type], &[arg])
            .result
            .unwrap();
        assert_eq!(value::get::text(&arena, &first).unwrap(), "first");
        assert_eq!(value::get::text(&arena, &second).unwrap(), "second");
    }
}
