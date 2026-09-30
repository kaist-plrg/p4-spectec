use super::super::*;

#[test]
fn test_antiunification_populates_each_path_in_left_to_right_expression_order() {
    let tuple = |bool_l: bool, bool_r: bool, line: usize| {
        exp(
            ast::ExpKind::Tuple(vec![
                exp(ast::ExpKind::Bool(bool_l), ast::TypKind::Bool, line),
                exp(ast::ExpKind::Bool(bool_r), ast::TypKind::Bool, line + 1),
            ]),
            ast::TypKind::Tuple(vec![typ::make::bool(), typ::make::bool()]),
            line,
        )
    };
    let exps_by_rule = vec![
        vec![tuple(true, false, 1), id_exp("shared", 3)],
        vec![tuple(false, true, 5), id_exp("shared", 7)],
    ];

    let mut ctx = Context::new();
    let (template, prems_by_rule) =
        antiunify::antiunify(&mut ctx, exps_by_rule).expect("equivalent tuple inputs");

    assert_eq!(template.len(), 2);
    let ast::ExpKind::Tuple(items) = &template[0].node else {
        panic!("expected tuple template");
    };
    let template_ids = items
        .iter()
        .map(|item| match &item.node {
            ast::ExpKind::Id(id) => id,
            _ => panic!("expected fresh unifier"),
        })
        .collect::<Vec<_>>();
    assert_ne!(template_ids[0].node, template_ids[1].node);
    assert!(ctx.frees.contains(template_ids[0]));
    assert!(ctx.frees.contains(template_ids[1]));
    assert!(matches!(&template[1].node, ast::ExpKind::Id(id) if id.node == "shared"));

    let compared_values = |prems_by_rule: &[ast::Prem]| {
        prems_by_rule
            .iter()
            .map(|prem| {
                let ast::PremKind::If(if_prem) = &prem.node else {
                    panic!("expected equality premise");
                };
                let ast::ExpKind::Cmp(_, ast::OpTyp::Bool, _, exp_r) = &if_prem.exp.node else {
                    panic!("expected equality comparison");
                };
                let ast::ExpKind::Bool(value) = exp_r.node else {
                    panic!("expected original boolean expression");
                };
                value
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(prems_by_rule.len(), 2);
    assert_eq!(compared_values(&prems_by_rule[0]), vec![true, false]);
    assert_eq!(compared_values(&prems_by_rule[1]), vec![false, true]);
}

#[test]
fn test_antiunification_freshness_avoids_collisions_within_each_operation() {
    let fresh_unifier = |mut ctx: Context| {
        let (template, _) = antiunify::antiunify(
            &mut ctx,
            vec![
                vec![exp(ast::ExpKind::Bool(true), ast::TypKind::Bool, 1)],
                vec![exp(ast::ExpKind::Bool(false), ast::TypKind::Bool, 2)],
            ],
        )
        .expect("equivalent boolean inputs");
        let ast::ExpKind::Id(id) = &template[0].node else {
            panic!("expected fresh unifier");
        };
        id.clone()
    };

    let id_first = fresh_unifier(Context::new());
    let mut ctx_collision = Context::new();
    ctx_collision.add_free(id_first.clone());
    let id_after_collision = fresh_unifier(ctx_collision);
    let id_independent = fresh_unifier(Context::new());

    assert_ne!(id_after_collision.node, id_first.node);
    assert_eq!(id_independent.node, id_first.node);
}

#[test]
fn test_antiunification_uses_runtime_equivalence_for_plain_type_aliases() {
    let alias_id = id("Flag", 1);
    let alias_typ =
        crate::phrase! { node: ast::TypKind::Var(alias_id.clone(), vec![]), span:  span(1) };
    let mut ctx = Context::new();
    ctx.tdenv.insert(
        alias_id,
        TypeDef::Defined(
            vec![],
            Box::new(
                crate::phrase! { node: ast::DefTypKind::Plain(typ::make::bool()), span:  span(1) },
            ),
        ),
    );
    let alias_value = exp(ast::ExpKind::Bool(true), alias_typ.node, 2);
    let bool_value = exp(ast::ExpKind::Bool(false), ast::TypKind::Bool, 3);

    let (template, prems) =
        antiunify::antiunify(&mut ctx, vec![vec![alias_value], vec![bool_value]])
            .expect("plain alias is equivalent to its underlying type");

    assert!(matches!(template[0].node, ast::ExpKind::Id(_)));
    assert_eq!(prems.iter().map(Vec::len).collect::<Vec<_>>(), vec![1, 1]);
}

#[test]
fn test_failed_antiunification_preserves_free_identifiers() {
    let exp_bool_a = exp(ast::ExpKind::Bool(true), ast::TypKind::Bool, 2);
    let exp_bool_b = exp(ast::ExpKind::Bool(false), ast::TypKind::Bool, 6);
    let exp_nat = exp(
        ast::ExpKind::Num(ast::Num::Nat(0_u64.into())),
        ast::TypKind::Num(prim::num::Typ::Nat),
        7,
    );
    let typ_nat = crate::phrase! {
        node: ast::TypKind::Num(prim::num::Typ::Nat),
        span: span(7),
    };
    let typ_kind_a = ast::TypKind::Tuple(vec![typ::make::bool(), typ::make::bool()]);
    let typ_kind_b = ast::TypKind::Tuple(vec![typ::make::bool(), typ_nat]);
    let exp_tuple_a = exp(ast::ExpKind::Tuple(vec![exp_bool_a.clone(), exp_bool_a]), typ_kind_a, 1);
    let exp_tuple_b = exp(ast::ExpKind::Tuple(vec![exp_bool_b, exp_nat]), typ_kind_b, 5);
    let mut ctx = Context::new();
    ctx.add_free(id("reserved", 1));
    let ids_free = ctx.frees.clone();

    // The first pair needs a fresh name; the second pair has incompatible types
    let error =
        antiunify::antiunify(&mut ctx, vec![vec![exp_tuple_a], vec![exp_tuple_b]]).unwrap_err();

    assert_eq!(error.kind, AlgoErrorKind::AntiUnification);
    assert_eq!(error.span, span(5));
    assert_eq!(ctx.frees, ids_free);
}

#[test]
fn test_nested_type_error_keeps_its_category_and_span() {
    use crate::runtime::ops::typ::TypeErrorKind;

    let typ_missing = crate::phrase! {
        node: ast::TypKind::Var(id("Missing", 3), vec![]),
        span: span(3),
    };
    let exp_bool_a = exp(ast::ExpKind::Bool(true), ast::TypKind::Bool, 2);
    let exp_bool_b = exp(ast::ExpKind::Bool(false), ast::TypKind::Bool, 6);
    let exp_missing = typed_id_exp("x", &typ_missing, 3);
    let exp_bool = exp(ast::ExpKind::Bool(true), ast::TypKind::Bool, 7);
    let typ_kind_a = ast::TypKind::Tuple(vec![typ::make::bool(), typ_missing]);
    let typ_kind_b = ast::TypKind::Tuple(vec![typ::make::bool(), typ::make::bool()]);
    let exp_tuple_a = exp(ast::ExpKind::Tuple(vec![exp_bool_a, exp_missing]), typ_kind_a, 1);
    let exp_tuple_b = exp(ast::ExpKind::Tuple(vec![exp_bool_b, exp_bool]), typ_kind_b, 5);
    let mut ctx = Context::new();
    let ids_free = ctx.frees.clone();

    let error =
        antiunify::antiunify(&mut ctx, vec![vec![exp_tuple_a], vec![exp_tuple_b]]).unwrap_err();

    let error_kind = TypeErrorKind::TypeUndefined("Missing".to_owned());
    assert_eq!(error.kind, AlgoErrorKind::Type(error_kind));
    assert_eq!(error.span, span(3));
    assert_eq!(ctx.frees, ids_free);
}
