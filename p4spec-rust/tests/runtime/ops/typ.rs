//! Runtime type-operation tests

use p4spec_rust::{
    lang::{
        common::{ds::map::ArityMismatch, notation::mixfix::Mixfix, source::Span},
        il::ast::{self, DefTypKind, FuncTyp, Iter, Subcheck, TypKind},
    },
    runtime::{
        envs::elab::TDEnv,
        ops::typ::{
            Theta, TypeArityMismatch, TypeErrorKind, equiv_func_typ, equiv_not_typ, equiv_typ,
            expand_typ, optimize_sub_typ, sub_typ, subst_not_typ, subst_typ,
        },
        typdef::TypeDef,
    },
};

#[path = "typ/expand.rs"]
mod expand;

fn id(name: &str) -> ast::Id {
    p4spec_rust::phrase! {
        node: name.to_owned(),
        span: Span::default(),
    }
}

fn typ(kind: TypKind) -> ast::Typ {
    p4spec_rust::phrase! {
        node: kind,
        span: Span::default(),
    }
}

fn var(name: &str, args: Vec<ast::Targ>) -> ast::Typ {
    typ(TypKind::Var(id(name), args))
}

fn iter(inner: ast::Typ, iter: Iter) -> ast::Typ {
    typ(TypKind::Iter(Box::new(inner), iter))
}

fn plain(inner: ast::Typ) -> ast::DefTyp {
    p4spec_rust::phrase! {
        node: DefTypKind::Plain(inner),
        span: Span::default(),
    }
}

fn variant(cases: Vec<ast::NotTyp>) -> ast::DefTyp {
    let cases = cases
        .into_iter()
        .map(|not_typ| ast::TypCase {
            not_typ,
            typ_origin: p4spec_rust::phrase! {
                node: ast::TypOriginKind { id: id("Origin"), targs: vec![] },
                span: Span::default(),
            },
            hints: vec![],
        })
        .collect();
    p4spec_rust::phrase! {
        node: DefTypKind::Variant(cases),
        span: Span::default(),
    }
}

fn not_typ(name: &str) -> ast::NotTyp {
    p4spec_rust::phrase! {
        node: Mixfix::Arg(var(name, vec![])),
        span: Span::default(),
    }
}

fn func_typ(tparams: Vec<ast::TParam>, typs_params: Vec<ast::Typ>, typ_ret: ast::Typ) -> FuncTyp {
    FuncTyp { tparams, typs_params, typ_ret: Box::new(typ_ret) }
}

#[test]
fn test_substitution_freshens_function_binders_and_rejects_higher_order_targets() {
    let mut theta = Theta::new();
    theta.insert(id("T"), typ(TypKind::Text));
    theta.insert(id("U"), typ(TypKind::Bool));

    let func_typ =
        func_typ(vec![id("T")], vec![var("T", vec![]), var("U", vec![])], var("T", vec![]));
    let typ_func = typ(TypKind::Func(func_typ));
    let substituted = subst_typ(&|id| theta.get(id), &typ_func).expect("substitute function");
    let TypKind::Func(func_typ) = substituted.node else { panic!("function type") };
    assert_eq!(func_typ.tparams[0].node, "__FRESH0");
    assert!(
        matches!(&func_typ.typs_params[0].node, TypKind::Var(id, args) if id.node == "__FRESH0" && args.is_empty())
    );
    assert_eq!(func_typ.typs_params[1].node, TypKind::Bool);
    assert!(
        matches!(&func_typ.typ_ret.node, TypKind::Var(id, args) if id.node == "__FRESH0" && args.is_empty())
    );

    let higher_order_span = Span::new(
        p4spec_rust::lang::common::source::Position::new("higher-order", 2, 3),
        p4spec_rust::lang::common::source::Position::new("higher-order", 2, 7),
    );
    let higher_order = p4spec_rust::phrase! {
        node: TypKind::Var(id("U"), vec![typ(TypKind::Bool)]),
        span: higher_order_span.clone(),
    };
    let error = subst_typ(&|id| theta.get(id), &higher_order).unwrap_err();
    assert_eq!(error.kind, TypeErrorKind::HigherOrderSubstitutionUnsupported);
    assert_eq!(error.span, higher_order_span);
}

#[test]
fn test_substitution_maps_nested_notation_type_arguments() {
    let mut theta = Theta::new();
    theta.insert(id("T"), typ(TypKind::Bool));
    theta.insert(id("U"), typ(TypKind::Text));
    let not_typ = p4spec_rust::phrase!(
        node: Mixfix::Seq(vec![
            Mixfix::Arg(var("T", vec![])),
            Mixfix::Arg(var("U", vec![])),
        ]),
        span: Span::new(Default::default(), Default::default()),
    );

    let substituted =
        subst_not_typ(&|id| theta.get(id), &not_typ).expect("substitute notation type");

    assert_eq!(
        substituted.node,
        Mixfix::Seq(vec![Mixfix::Arg(typ(TypKind::Bool)), Mixfix::Arg(typ(TypKind::Text)),])
    );
    assert_eq!(substituted.span, not_typ.span);
}

#[test]
fn test_expansion_resolves_plain_aliases_and_reports_invalid_references() {
    let mut env = TDEnv::new();
    env.insert(
        id("Pair"),
        TypeDef::Defined(
            vec![id("T")],
            Box::new(plain(typ(TypKind::Tuple(vec![var("T", vec![]), var("T", vec![])])))),
        ),
    );

    let typ_alias = var("Pair", vec![typ(TypKind::Bool)]);
    let expanded = expand_typ(&env, &typ_alias).expect("expand parameterized alias");
    assert_eq!(expanded.node, TypKind::Tuple(vec![typ(TypKind::Bool), typ(TypKind::Bool)]));

    let arity = expand_typ(&env, &var("Pair", vec![])).unwrap_err();
    assert_eq!(
        arity.kind,
        TypeErrorKind::ArityMismatch(TypeArityMismatch::TypeArgument(ArityMismatch::new(1, 0)))
    );
    let missing = expand_typ(&env, &var("Missing", vec![])).unwrap_err();
    assert_eq!(missing.kind, TypeErrorKind::TypeUndefined("Missing".to_owned()));
}

#[test]
fn test_equivalence_expands_aliases_and_alpha_renames_function_parameters() {
    let mut env = TDEnv::new();
    env.insert(id("Truth"), TypeDef::Defined(vec![], Box::new(plain(typ(TypKind::Bool)))));
    assert!(equiv_typ(&env, &var("Truth", vec![]), &typ(TypKind::Bool)).unwrap());

    let func_typ_l = func_typ(vec![id("T")], vec![var("T", vec![])], var("T", vec![]));
    let func_typ_r = func_typ(vec![id("U")], vec![var("U", vec![])], var("U", vec![]));
    let equivalent = equiv_func_typ(&|id| env.get(id), &Span::default(), &func_typ_l, &func_typ_r)
        .expect("compare function signatures");
    assert!(equivalent);

    let func_typ_l = func_typ(vec![id("T")], vec![], typ(TypKind::Bool));
    let func_typ_r = func_typ(vec![], vec![], typ(TypKind::Bool));
    let error =
        equiv_func_typ(&|id| env.get(id), &Span::default(), &func_typ_l, &func_typ_r).unwrap_err();
    assert_eq!(
        error.kind,
        TypeErrorKind::ArityMismatch(TypeArityMismatch::TypeParameter(ArityMismatch::new(1, 0)))
    );
}

#[test]
fn test_notation_equivalence_compares_shape_and_type_arguments() {
    let mut env = TDEnv::new();
    env.insert(id("Truth"), TypeDef::Defined(vec![], Box::new(plain(typ(TypKind::Bool)))));
    let not_typ_l = not_typ("Truth");
    let not_typ_r = p4spec_rust::phrase! {
        node: Mixfix::Arg(typ(TypKind::Bool)),
        span: Span::default(),
    };
    let not_typ_shape = p4spec_rust::phrase! {
        node: Mixfix::Seq(vec![Mixfix::Arg(typ(TypKind::Bool))]),
        span: Span::default(),
    };

    assert!(equiv_not_typ(&env, &not_typ_l, &not_typ_r).unwrap());
    assert!(!equiv_not_typ(&env, &not_typ_l, &not_typ_shape).unwrap());
}

#[test]
fn test_subtyping_covers_numeric_iteration_tuple_and_variant_rules() {
    let mut env = TDEnv::new();
    env.insert(id("A"), TypeDef::Extern);
    env.insert(id("B"), TypeDef::Extern);
    env.insert(id("Small"), TypeDef::Defined(vec![], Box::new(variant(vec![not_typ("A")]))));
    env.insert(
        id("Large"),
        TypeDef::Defined(vec![], Box::new(variant(vec![not_typ("A"), not_typ("B")]))),
    );

    assert!(
        sub_typ(
            &env,
            &typ(TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Nat)),
            &typ(TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Int)),
        )
        .unwrap()
    );
    assert!(
        sub_typ(&env, &iter(typ(TypKind::Bool), Iter::Opt), &iter(typ(TypKind::Bool), Iter::List),)
            .unwrap()
    );
    assert!(
        sub_typ(
            &env,
            &typ(TypKind::Tuple(vec![var("Small", vec![])])),
            &typ(TypKind::Tuple(vec![var("Large", vec![])])),
        )
        .unwrap()
    );
    assert!(!sub_typ(&env, &var("Large", vec![]), &var("Small", vec![])).unwrap());
}

#[test]
fn test_subtype_optimization_emits_structural_checks_only_when_needed() {
    let env = TDEnv::new();
    let typ_source = typ(TypKind::Tuple(vec![
        typ(TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Int)),
        iter(typ(TypKind::Text), Iter::List),
    ]));
    let typ_target = typ(TypKind::Tuple(vec![
        typ(TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Nat)),
        iter(typ(TypKind::Bool), Iter::List),
    ]));

    assert_eq!(
        optimize_sub_typ(&env, &typ_source, &typ_target).unwrap(),
        Subcheck::Tuple(vec![
            Subcheck::Recurse(typ(TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Nat))),
            Subcheck::Iter(Iter::List, Box::new(Subcheck::Recurse(typ(TypKind::Bool)))),
        ])
    );
    assert_eq!(
        optimize_sub_typ(&env, &typ(TypKind::Bool), &typ(TypKind::Bool)).unwrap(),
        Subcheck::Skip
    );
}

#[test]
fn lookup_substitution_is_simultaneous_and_preserves_free_variables() {
    let typ_t = var("U", vec![]);
    let typ_u = typ(TypKind::Bool);
    let find_subst = |id: &ast::Id| match id.node.as_str() {
        "T" => Some(&typ_t),
        "U" => Some(&typ_u),
        _ => None,
    };
    let typ_input = typ(TypKind::Tuple(vec![
        var("T", vec![]),
        var("U", vec![]),
        var("Alias", vec![var("T", vec![])]),
    ]));
    let typ_output = subst_typ(&find_subst, &typ_input).unwrap();
    assert_eq!(
        typ_output,
        typ(TypKind::Tuple(vec![
            var("U", vec![]),
            typ(TypKind::Bool),
            var("Alias", vec![var("U", vec![])]),
        ]))
    );
}

#[test]
fn lookup_substitution_freshens_binders_and_reports_higher_order_targets() {
    let typ_t = typ(TypKind::Text);
    let typ_u = var("T", vec![]);
    let find_subst = |id: &ast::Id| match id.node.as_str() {
        "T" => Some(&typ_t),
        "U" => Some(&typ_u),
        _ => None,
    };
    let typ_input = typ(TypKind::Func(func_typ(
        vec![id("T")],
        vec![var("T", vec![]), var("U", vec![])],
        var("T", vec![]),
    )));
    let typ_output = subst_typ(&find_subst, &typ_input).unwrap();
    let TypKind::Func(func_typ) = typ_output.node else { panic!("function type") };
    assert_eq!(func_typ.tparams, vec![id("__FRESH0")]);
    assert_eq!(func_typ.typs_params, vec![var("__FRESH0", vec![]), var("T", vec![])]);
    assert_eq!(*func_typ.typ_ret, var("__FRESH0", vec![]));

    let mut typ_input = var("U", vec![typ(TypKind::Bool)]);
    typ_input.span = Span::new(
        p4spec_rust::lang::common::source::Position::new("lookup", 3, 2),
        p4spec_rust::lang::common::source::Position::new("lookup", 3, 8),
    );
    let error = subst_typ(&find_subst, &typ_input).unwrap_err();
    assert_eq!(error.kind, TypeErrorKind::HigherOrderSubstitutionUnsupported);
    assert_eq!(error.span, typ_input.span);
}

#[test]
fn function_equivalence_looks_up_fresh_parameters_before_outer_definitions() {
    let typdef = TypeDef::Defined(vec![], Box::new(plain(typ(TypKind::Bool))));
    let find_typdef_opt = |id: &ast::Id| (id.node == "__FRESH0").then_some(&typdef);
    let func_typ_l = func_typ(vec![id("T")], vec![var("T", vec![])], var("T", vec![]));
    let func_typ_r = func_typ(vec![id("U")], vec![typ(TypKind::Bool)], var("U", vec![]));
    assert!(
        !equiv_func_typ(&find_typdef_opt, &Span::default(), &func_typ_l, &func_typ_r,).unwrap()
    );
}
