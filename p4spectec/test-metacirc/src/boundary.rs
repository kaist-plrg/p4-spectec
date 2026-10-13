//! Host result wrappers and malformed meta payload boundaries
//!
//! Successful values pass native meta guards before decoding.
//! Malformed constructors, arities, and payloads must fail without panicking.

use std::rc::Rc;

use p4spectec::{
    frontend::parse::parse_mixop,
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            notation::tree::Mixfix,
            typ::{TypKind, make as make_typ},
            value::flat::{Value, get, make},
        },
        traits::eq::SyntaxEq,
    },
    metacirc::al::{decode, encode},
    phrase,
};

use crate::{MetaRunner, Result, check_guard, meta_runner};

/// Checks wrappers and malformed data using the real meta result types.
pub fn run() -> Result<()> {
    let mut arena = Arena::new();
    let mut runner = meta_runner(&mut arena)?;
    let value_bool = make::bool(&mut arena, true, Span::default())?;
    let value_nat = make::nat(&mut arena, 17u64.into(), Span::default())?;

    // Preserve all signature fields before exercising result wrappers
    check_function_types(&mut arena, &mut runner)?;

    // Distinguish function results from zero, one, or many relation outputs
    check_value(&mut arena, &mut runner, value_bool)?;
    for values in [vec![], vec![value_bool], vec![value_bool, value_nat]] {
        check_values(&mut arena, &mut runner, &values)?;
    }

    // Reject malformed data through the decoder and actual runtime guards
    check_malformed(&mut arena, &mut runner)?;
    eprintln!("metacirc boundaries: result wrappers, guard separation, malformed payloads");
    Ok(())
}

/// Preserves polymorphic and nested function signatures through the type codec.
fn check_function_types(arena: &mut Arena, runner: &mut MetaRunner) -> Result<()> {
    // Distinct parameter and return types expose signature truncation
    let id = phrase!(node: "X".into(), span: Span::default());
    let typ_param_al = make_typ::var(id.clone(), vec![]);
    let typ_func_al =
        make_typ::func(vec![id], vec![typ_param_al.clone(), make_typ::int()], typ_param_al);
    let typ_nested_al = make_typ::func(
        vec![],
        vec![typ_func_al.clone()],
        make_typ::func(vec![], vec![], make_typ::nat()),
    );

    // Match the native type and the meta guard for each complete signature
    for typ_al in [typ_func_al, typ_nested_al] {
        let value_meta = encode::typ(arena, &typ_al)?;
        let typ_decode_al = decode::typ(arena, value_meta)?;
        assert!(typ_al.syntax_eq(&typ_decode_al));
        check_guard(runner, arena, "check_typ", &[value_meta])?;
    }
    Ok(())
}

/// Checks one function result's annotation, guard, and decoded payload.
fn check_value(arena: &mut Arena, runner: &mut MetaRunner, value: Value) -> Result<()> {
    let value_result = encode::ok_value(arena, value)?;
    check_annotation(arena, value_result, "valres");

    // Both result types use OK but require distinct payload cardinality
    check_guard(runner, arena, "check_valres", &[value_result])?;
    assert!(
        runner
            .eval_func(arena, "check_valsres", &[], &[value_result])
            .result
            .is_err()
    );

    // Exactly one meta val occupies the wrapper's only argument
    let value_payload = *get::one(get::case(arena, &value_result)?.args())?;
    let value_decode = decode::value(arena, value_payload)?;
    assert!(value.view(arena).syntax_eq(&value_decode.view(arena)));
    check_guard(runner, arena, "check_val", &[value_payload])
}

/// Checks a relation result's annotation, guard, and every decoded output.
fn check_values(arena: &mut Arena, runner: &mut MetaRunner, values: &[Value]) -> Result<()> {
    let value_result = encode::ok_values(arena, values)?;
    check_annotation(arena, value_result, "valsres");

    // The list remains one wrapper argument even when it has no elements
    check_guard(runner, arena, "check_valsres", &[value_result])?;
    assert!(
        runner
            .eval_func(arena, "check_valres", &[], &[value_result])
            .result
            .is_err()
    );
    let value_payload = *get::one(get::case(arena, &value_result)?.args())?;
    let values_decode = decode::values(arena, value_payload)?;
    assert_eq!(values.len(), values_decode.len());

    // Compare payload contents as well as cardinality
    for (value, value_decode) in values.iter().zip(&values_decode) {
        assert!(value.view(arena).syntax_eq(&value_decode.view(arena)));
    }
    Ok(())
}

fn check_annotation(arena: &Arena, value: Value, name: &str) {
    assert!(
        matches!(arena.value.typ(value.note).as_ref(), TypKind::Var(id, targs) if id.node.as_ref() == name && targs.is_empty())
    );
}

/// Builds intentionally invalid constructors with their declared hole counts.
fn malformed(arena: &mut Arena, text: &str, name: &str, value_payload: Value) -> Result<Value> {
    let mixop = Rc::new(parse_mixop(text)?);
    let values = vec![value_payload; mixop.arity()];
    let id = phrase!(node: name.into(), span: Span::default());
    let typ = Rc::new(make_typ::var(id, vec![]).node);
    let mixfix = Mixfix::new(mixop, values)?;
    Ok(make::case(arena, typ, mixfix, Span::default())?)
}

/// Rejects unknown tags, wrong arities, wrong payloads, and a negative natural.
fn check_malformed(arena: &mut Arena, runner: &mut MetaRunner) -> Result<()> {
    let value_bad = make::int(arena, (-1).into(), Span::default())?;

    // A valid meta constructor spelling does not validate its payload
    for text in
        ["BOGUS", "BOOL", "BOOL nat nat", "BOOL nat", "NAT nat", "LIST nat", "OPT nat", "STR nat"]
    {
        let value_meta = malformed(arena, text, "val", value_bad)?;
        assert!(decode::value(arena, value_meta).is_err(), "value decoder accepted {text}");
        assert!(
            runner
                .eval_func(arena, "check_val", &[], &[value_meta])
                .result
                .is_err(),
            "value guard accepted {text}"
        );
    }

    // Types and expressions also reject non-case payloads and invalid arities
    assert!(decode::typ(arena, value_bad).is_err());
    assert!(decode::spec(arena, value_bad).is_err());
    for text in
        ["BOGUS", "BOOL", "BOOL nat nat", "CALL nat", "SUB nat nat nat", "OPT nat", "ITER nat nat"]
    {
        let value_meta = malformed(arena, text, "exp", value_bad)?;
        assert!(decode::exp(arena, value_meta).is_err(), "expression decoder accepted {text}");
    }
    Ok(())
}
