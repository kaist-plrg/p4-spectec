use p4spec_rust::lang::data::value::ValueArena;

#[path = "builtin/maps.rs"]
mod maps;
#[path = "builtin/sets.rs"]
mod sets;

use num_bigint::BigInt;
use p4spec_rust::{
    interface::builtin::{BuiltinError, call::Builtins},
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, get, make},
        },
        traits::print::Print,
    },
    phrase,
};

fn id(name: &str) -> p4spec_rust::lang::il::ast::Id {
    phrase!(node: name.to_owned(), span: Span::default())
}

fn invoke(
    arena: &mut ValueArena,
    builtins: &mut Builtins,
    name: &str,
    values: &[Value],
) -> Result<(Value, bool), p4spec_rust::interface::builtin::BuiltinError> {
    invoke_with_types(arena, builtins, name, &[], values)
}

fn invoke_with_types(
    arena: &mut ValueArena,
    builtins: &mut Builtins,
    name: &str,
    targs: &[p4spec_rust::lang::il::ast::Typ],
    values: &[Value],
) -> Result<(Value, bool), p4spec_rust::interface::builtin::BuiltinError> {
    builtins.invoke(arena, &id(name), targs, values)
}

#[test]
fn test_numeric_builtin_returns_value_without_side_effect() {
    let mut arena = ValueArena::new();
    let typ_list = typ::make::list(typ::make::int());
    let input = {
        let values = vec![
            make::int(&mut arena, BigInt::from(2), Span::default()).unwrap(),
            make::int(&mut arena, BigInt::from(5), Span::default()).unwrap(),
        ];
        make::list(&mut arena, typ_list.node.clone().into(), values, Span::default()).unwrap()
    };

    let (result, side_effected) =
        invoke(&mut arena, &mut Builtins::new(), "sum_int", &[input]).unwrap();

    assert_eq!(get::num(&arena, &result).unwrap().to_string(), "+7");
    assert!(!side_effected);
}

#[test]
fn test_fresh_type_ids_share_state_and_report_side_effects() {
    let mut arena = ValueArena::new();
    let mut builtins_a = Builtins::new();
    let mut builtins_b = Builtins::new();
    builtins_a.init();

    let (value_a, side_effected_a) =
        invoke(&mut arena, &mut builtins_a, "fresh_typeId", &[]).unwrap();
    let (value_b, side_effected_b) =
        invoke(&mut arena, &mut builtins_b, "fresh_typeId", &[]).unwrap();

    assert_eq!(get::text(&arena, &value_a), Ok("FRESH__0"));
    assert_eq!(get::text(&arena, &value_b), Ok("FRESH__1"));
    assert!(side_effected_a);
    assert!(side_effected_b);

    builtins_a.init();
    let (value_a, side_effected_a) =
        invoke(&mut arena, &mut builtins_a, "fresh_typeId", &[]).unwrap();
    assert_eq!(get::text(&arena, &value_a), Ok("FRESH__0"));
    assert!(side_effected_a);
}

#[test]
fn test_missing_builtin_and_wrong_arity_are_typed_failures() {
    let mut arena = ValueArena::new();
    let missing = invoke(&mut arena, &mut Builtins::new(), "missing", &[]).unwrap_err();
    assert!(matches!(
        missing,
        BuiltinError::ImplementationMissing(ref name) if name == "missing"
    ));

    let arity = invoke(&mut arena, &mut Builtins::new(), "sum_int", &[]).unwrap_err();
    assert!(matches!(arity, BuiltinError::ArgumentCountMismatch { expected: 1, actual: 0 }));
}

#[test]
fn test_list_and_text_builtins_preserve_ocaml_results() {
    let mut arena = ValueArena::new();
    let typ_list = typ::make::list(typ::make::text());
    let repeated = {
        let values = vec![
            make::text(&mut arena, "a".to_owned(), Span::default()).unwrap(),
            make::text(&mut arena, "a".to_owned(), Span::default()).unwrap(),
        ];
        make::list(&mut arena, typ_list.node.clone().into(), values, Span::default()).unwrap()
    };
    let (distinct, _) = invoke_with_types(
        &mut arena,
        &mut Builtins::new(),
        "distinct_",
        &[typ::make::text()],
        &[repeated],
    )
    .unwrap();
    assert_eq!(get::bool(&arena, &distinct), Ok(false));

    let text = make::text(&mut arena, "  a\n b\t".to_owned(), Span::default()).unwrap();
    let (stripped, _) =
        invoke(&mut arena, &mut Builtins::new(), "strip_all_whitespace", &[text]).unwrap();
    assert_eq!(get::text(&arena, &stripped), Ok("a\nb\t"));
}

#[test]
fn test_int_to_text_preserves_the_explicit_integer_sign() {
    let mut arena = ValueArena::new();
    let int = make::int(&mut arena, 7.into(), Span::default()).unwrap();
    let nat = make::nat(&mut arena, 7_u64.into(), Span::default()).unwrap();

    let (integer_text, _) =
        invoke(&mut arena, &mut Builtins::new(), "int_to_text", &[int]).unwrap();
    let (natural_text, _) =
        invoke(&mut arena, &mut Builtins::new(), "int_to_text", &[nat]).unwrap();

    assert_eq!(get::text(&arena, &integer_text), Ok("+7"));
    assert_eq!(get::text(&arena, &natural_text), Ok("7"));
}

#[test]
fn test_zero_and_negative_bit_widths_match_ocaml() {
    let mut arena = ValueArena::new();
    for width in [0, -1] {
        for name in ["bitstr_to_int", "int_to_bitstr"] {
            let values = [
                make::int(&mut arena, width.into(), Span::default()).unwrap(),
                make::int(&mut arena, 17.into(), Span::default()).unwrap(),
            ];
            let (result, _) = invoke(&mut arena, &mut Builtins::new(), name, &values).unwrap();
            assert_eq!(get::num(&arena, &result).unwrap().to_string(), "+0", "{name}");
        }
    }
}

#[test]
fn test_negative_array_width_is_rejected_like_ocaml_array_init() {
    let mut arena = ValueArena::new();
    for name in ["int_to_bits_unsigned", "int_to_bits_signed"] {
        let values = [
            make::int(&mut arena, (-1).into(), Span::default()).unwrap(),
            make::int(&mut arena, 17.into(), Span::default()).unwrap(),
        ];
        let result = invoke(&mut arena, &mut Builtins::new(), name, &values);

        assert!(result.is_err(), "{name}");
    }
}
