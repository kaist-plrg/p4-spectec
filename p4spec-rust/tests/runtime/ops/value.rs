//! Runtime value-membership tests

use num_bigint::BigInt;
use p4spec_rust::lang::data::value::ValueArena;
use p4spec_rust::{
    lang::{
        common::prim::num::Natural,
        common::source::Span,
        data::{typ, value::make},
        il::ast::{FuncTyp, Id, Iter, Subcheck},
    },
    phrase,
    runtime::{
        envs::elab::TDEnv,
        ops::value::{MatchError, check, sub, subs},
        typdef::TypeDef,
    },
    util::json::json,
};

fn id(name: &str) -> p4spec_rust::lang::il::ast::Id {
    phrase!(node: name.to_owned(), span: Span::default())
}

#[test]
fn test_numeric_membership_preserves_nat_subtyping() {
    let mut arena = ValueArena::new();
    let tdenv = TDEnv::new();
    let find_typdef_opt = |id: &Id| tdenv.get(id);
    let find_func = |_: &str| None::<FuncTyp>;
    let nat = make::nat(&mut arena, Natural::from(3_u64), Span::default()).unwrap();
    let nonnegative_int = make::int(&mut arena, BigInt::from(3), Span::default()).unwrap();
    let negative_int = make::int(&mut arena, BigInt::from(-1), Span::default()).unwrap();

    assert_eq!(sub(&arena, &find_typdef_opt, &find_func, &typ::make::nat(), &nat), Ok(true));
    assert_eq!(
        sub(&arena, &find_typdef_opt, &find_func, &typ::make::nat(), &nonnegative_int),
        Ok(true)
    );
    assert_eq!(
        sub(&arena, &find_typdef_opt, &find_func, &typ::make::nat(), &negative_int),
        Ok(false)
    );
    assert_eq!(sub(&arena, &find_typdef_opt, &find_func, &typ::make::int(), &nat), Ok(true));
}

#[test]
fn optional_membership_requires_an_option_and_checks_present_values() {
    let mut arena = ValueArena::new();
    let typ_opt = typ::make::opt(typ::make::nat());
    let value_nat = make::nat(&mut arena, 3u64.into(), Span::default()).unwrap();
    let value_bool = make::bool(&mut arena, false, Span::default()).unwrap();
    let value_none =
        make::opt(&mut arena, typ_opt.node.clone().into(), None, Span::default()).unwrap();
    let value_some =
        make::opt(&mut arena, typ_opt.node.clone().into(), Some(value_nat), Span::default())
            .unwrap();
    let value_wrong =
        make::opt(&mut arena, typ_opt.node.clone().into(), Some(value_bool), Span::default())
            .unwrap();
    let value_list = make::list(
        &mut arena,
        typ::make::list(typ::make::nat()).node.into(),
        vec![],
        Span::default(),
    )
    .unwrap();
    let tdenv = TDEnv::new();
    let find_typdef_opt = |id: &Id| tdenv.get(id);
    let find_func = |_: &str| None::<FuncTyp>;
    for (value, expected) in [
        (value_none, true),
        (value_some, true),
        (value_wrong, false),
        (value_nat, false),
        (value_bool, false),
        (value_list, false),
    ] {
        assert_eq!(sub(&arena, &find_typdef_opt, &find_func, &typ_opt, &value), Ok(expected));
        let subcheck = Subcheck::Iter(Iter::Opt, Box::new(Subcheck::Recurse(typ::make::nat())));
        assert_eq!(check(&arena, &find_typdef_opt, &find_func, &subcheck, &value), Ok(expected));
    }
}

#[test]
fn test_extern_type_membership_uses_shared_type_environment() {
    let mut arena = ValueArena::new();
    let mut tdenv = TDEnv::new();
    let extern_id = id("object");
    tdenv.insert(extern_id.clone(), TypeDef::Extern);
    let extern_typ = typ::make::var(extern_id, vec![]);
    let value = make::external(
        &mut arena,
        (extern_typ).node.clone().into(),
        json::Null.into(),
        Span::default(),
    )
    .unwrap();
    let find_typdef_opt = |id: &Id| tdenv.get(id);
    let find_func = |_: &str| None::<FuncTyp>;

    assert_eq!(sub(&arena, &find_typdef_opt, &find_func, &extern_typ, &value), Ok(true));
}

#[test]
fn test_undefined_names_return_located_typed_errors() {
    let mut arena = ValueArena::new();
    let missing_typ = typ::make::var(id("missing"), vec![]);
    let value = make::bool(&mut arena, true, Span::default()).unwrap();
    let tdenv = TDEnv::new();
    let find_typdef_opt = |id: &Id| tdenv.get(id);

    let error = sub(&arena, &find_typdef_opt, &|_: &str| None::<FuncTyp>, &missing_typ, &value)
        .unwrap_err();

    assert!(matches!(error, MatchError::UndefinedType { ref name, .. } if name == "missing"));

    let mut span = Span::default();
    span.left.line = 17;
    let value = make::func(
        &mut arena,
        p4spec_rust::phrase!(node: "missing".to_owned(), span: span.clone()),
        vec![],
        vec![],
        typ::make::bool(),
        Span::default(),
    )
    .unwrap();
    let typ = typ::make::func(vec![], vec![], typ::make::bool());
    let error = sub(&arena, &find_typdef_opt, &|_| None, &typ, &value).unwrap_err();
    assert!(
        matches!(error, MatchError::UndefinedFunction { name, span: span_error } if name == "missing" && span_error == span)
    );
}

#[test]
fn test_recursive_subchecks_walk_tuple_and_list_values() {
    let mut arena = ValueArena::new();
    let bool_typ = typ::make::bool();
    let tuple_typ = typ::make::tuple(vec![bool_typ.clone(), typ::make::list(bool_typ.clone())]);
    let bool_value = make::bool(&mut arena, true, Span::default()).unwrap();
    let list_value = {
        let values = vec![make::bool(&mut arena, false, Span::default()).unwrap()];
        make::list(
            &mut arena,
            (typ::make::list(bool_typ.clone())).node.clone().into(),
            values,
            Span::default(),
        )
        .unwrap()
    };
    let tuple_value = make::tuple(
        &mut arena,
        (tuple_typ).node.clone().into(),
        vec![bool_value, list_value],
        Span::default(),
    )
    .unwrap();
    let subcheck = Subcheck::Tuple(vec![
        Subcheck::Recurse(bool_typ.clone()),
        Subcheck::Iter(Iter::List, Box::new(Subcheck::Recurse(bool_typ))),
    ]);
    let tdenv = TDEnv::new();
    let find_typdef_opt = |id: &Id| tdenv.get(id);

    assert_eq!(
        check(&arena, &find_typdef_opt, &|_: &str| None::<FuncTyp>, &subcheck, &tuple_value),
        Ok(true)
    );
}

#[test]
fn test_list_membership_rejects_arity_mismatch() {
    let mut arena = ValueArena::new();
    let values = vec![make::bool(&mut arena, true, Span::default()).unwrap()];
    let func_typ =
        FuncTyp { tparams: vec![], typs_params: vec![], typ_ret: Box::new(typ::make::bool()) };
    let tdenv = TDEnv::new();
    let find_typdef_opt = |id: &Id| tdenv.get(id);

    assert_eq!(
        subs(
            &arena,
            &find_typdef_opt,
            &|_: &str| Some(func_typ.clone()),
            &[typ::make::bool(), typ::make::bool()],
            &values
        ),
        Ok(false)
    );
}
