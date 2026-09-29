use p4spec_rust::interp::shared::context::{ReadContext, WriteContext};
use p4spec_rust::interp::shared::prepare::Prepare;
use p4spec_rust::interp::shared::{
    backtrack::{Backtrack, Failure},
    util::find_var_of_exp,
};
use p4spec_rust::runtime::envs::interp::al::ast_prepared as prepared;
use p4spec_rust::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};
use std::rc::Rc;

use p4spec_rust::lang::data::value::ValueArena;

use p4spec_rust::{
    interp::al::{
        context::{Context, Global, Scope},
        eval::assign::{assign_args, assign_exp, assign_exps},
    },
    lang::{
        al::ast,
        common::source::{Position, Span},
        data::{
            typ,
            value::{Value, get, make},
        },
    },
    note_phrase, phrase,
};

fn span(line: usize) -> Span {
    Span::new(
        Position::new("assignment.watsup", line, 0),
        Position::new("assignment.watsup", line, 1),
    )
}
fn id(name: &str) -> ast::Id {
    phrase!(node: name.to_owned(), span: span(3))
}
fn exp(node: ast::ExpKind) -> ast::Exp {
    note_phrase!(node: node, note: typ::make::bool().node, span: span(4))
}
fn id_exp(name: &str) -> ast::Exp {
    p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id(name)), note: p4spec_rust::lang::il::ast::TypKind::Bool, span: span(4))
}
fn var(name: &str, iters: Vec<ast::Iter>) -> ast::Var {
    ast::Var { id: id(name), typ: phrase!(node: typ::make::bool().node, span: span(5)), iters }
}
fn value(arena: &mut ValueArena, value: bool) -> Value {
    make::bool(arena, value, span(8)).unwrap()
}
fn ok<T: std::fmt::Debug>(result: Backtrack<T>) -> T {
    match result {
        Ok(value) => value,
        other => panic!("{other:?}"),
    }
}
fn binding(
    ctx: &Context<'_>,
    layout: &mut FrameLayout,
    name: &str,
    iters: Vec<ast::Iter>,
) -> Value {
    *ctx.find_value_at_slot(
        layout
            .resolve_var(p4spec_rust::lang::il::ast::Var {
                id: id(name),
                typ: p4spec_rust::lang::data::typ::make::bool(),
                iters,
            })
            .slot,
    )
    .unwrap()
}
fn iter(exp_inner: ast::Exp, iter: ast::Iter, vars: Vec<ast::Var>) -> ast::Exp {
    exp(ast::ExpKind::Iter(Box::new(exp_inner), ast::ExpIter { iter, vars }))
}
fn tuple(arena: &mut ValueArena, values: Vec<Value>) -> Value {
    make::tuple(arena, (typ::make::bool()).node.clone().into(), values, span(8)).unwrap()
}
fn list(arena: &mut ValueArena, values: Vec<Value>) -> Value {
    make::list(arena, (typ::make::list(typ::make::bool())).node.clone().into(), values, span(8))
        .unwrap()
}

fn prepare_exp<'global>(
    global: &'global Global,
    exp_source: ast::Exp,
) -> (prepared::Exp, Context<'global>, FrameLayout) {
    let mut layout = FrameLayout::default();
    let exp_prepared = exp_source.prepare(&mut layout);
    let ctx = Context::new(global).localize_with_layout(&Rc::new(layout.clone()));
    (exp_prepared, ctx, layout)
}

#[test]
fn test_iterated_variable_fast_path_preserves_identity_and_path() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp_inner = iter(id_exp("x"), ast::Iter::Opt, vec![var("x", vec![])]);
    let exp = iter(exp_inner.clone(), ast::Iter::List, vec![var("x", vec![ast::Iter::Opt])]);
    let (exp, ctx, mut layout) = prepare_exp(&global, exp);

    assert_eq!(
        find_var_of_exp(&ctx, &exp).unwrap().var.iters,
        vec![ast::Iter::Opt, ast::Iter::List]
    );
    let value = list(&mut arena, vec![]);
    let ctx = ok(assign_exp(&mut arena, ctx, &exp, value));
    assert!((value == binding(&ctx, &mut layout, "x", vec![ast::Iter::Opt, ast::Iter::List])));
    assert!(
        ctx.find_value_at_slot(
            layout
                .resolve_var(p4spec_rust::lang::il::ast::Var {
                    id: id("x"),
                    typ: p4spec_rust::lang::data::typ::make::bool(),
                    iters: vec![]
                })
                .slot
        )
        .is_none()
    );
    let (exp_mismatch, ctx_mismatch, _) =
        prepare_exp(&global, iter(exp_inner, ast::Iter::List, vec![var("x", vec![])]));
    assert!(find_var_of_exp(&ctx_mismatch, &exp_mismatch).is_none());
}

#[test]
fn test_list_assignment_collects_rows_without_leaking_scalar_bindings() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = iter(
        exp(ast::ExpKind::Tuple(vec![id_exp("x"), id_exp("y")])),
        ast::Iter::List,
        vec![var("x", vec![]), var("y", vec![])],
    );
    let (exp, mut ctx, mut layout) = prepare_exp(&global, exp);
    ctx.add_value_at_slot(layout.resolve_var(var("x", vec![])).slot, value(&mut arena, false));

    let ctx_result = ok({
        let value = {
            let values = vec![
                {
                    let values = vec![value(&mut arena, true), value(&mut arena, false)];
                    tuple(&mut arena, values)
                },
                {
                    let values = vec![value(&mut arena, false), value(&mut arena, true)];
                    tuple(&mut arena, values)
                },
            ];
            list(&mut arena, values)
        };
        assign_exp(&mut arena, ctx.clone(), &exp, value)
    });
    let xs = binding(&ctx_result, &mut layout, "x", vec![ast::Iter::List]);
    let ys = binding(&ctx_result, &mut layout, "y", vec![ast::Iter::List]);
    assert_eq!(arena.span(&xs).clone(), Span::default());
    assert_eq!(arena.span(&ys).clone(), Span::default());
    assert_eq!(
        get::list(&arena, &xs)
            .unwrap()
            .iter()
            .map(|v| get::bool(&arena, v).unwrap())
            .collect::<Vec<_>>(),
        vec![true, false]
    );
    assert_eq!(
        get::list(&arena, &ys)
            .unwrap()
            .iter()
            .map(|v| get::bool(&arena, v).unwrap())
            .collect::<Vec<_>>(),
        vec![false, true]
    );
    assert!(!get::bool(&arena, &binding(&ctx_result, &mut layout, "x", vec![])).unwrap());
    assert!(
        ctx_result
            .find_value_at_slot(
                layout
                    .resolve_var(p4spec_rust::lang::il::ast::Var {
                        id: id("y"),
                        typ: p4spec_rust::lang::data::typ::make::bool(),
                        iters: vec![]
                    })
                    .slot
            )
            .is_none()
    );
    assert!(
        ctx.find_value_at_slot(
            layout
                .resolve_var(p4spec_rust::lang::il::ast::Var {
                    id: id("x"),
                    typ: p4spec_rust::lang::data::typ::make::bool(),
                    iters: vec![ast::Iter::List]
                })
                .slot
        )
        .is_none()
    );
}

#[test]
#[should_panic(expected = "value must be bound")]
fn test_list_rows_cannot_collect_unassigned_outer_values() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = iter(
        exp(ast::ExpKind::Tuple(vec![id_exp("x")])),
        ast::Iter::List,
        vec![var("missing", vec![])],
    );
    let (exp, mut ctx, mut layout) = prepare_exp(&global, exp);
    ctx.add_value_at_slot(layout.resolve_var(var("missing", vec![])).slot, value(&mut arena, true));

    let _ = {
        let value = {
            let values = vec![{
                let values = vec![value(&mut arena, false)];
                tuple(&mut arena, values)
            }];
            list(&mut arena, values)
        };
        assign_exp(&mut arena, ctx, &exp, value)
    };
}

#[test]
fn test_optional_assignment_exports_only_iterated_bindings() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp =
        iter(exp(ast::ExpKind::Tuple(vec![id_exp("x")])), ast::Iter::Opt, vec![var("x", vec![])]);
    let (exp, ctx, mut layout) = prepare_exp(&global, exp);

    let typ = typ::make::opt(typ::make::bool());
    let ctx = ok({
        let value = {
            let value = Some({
                let values = vec![value(&mut arena, true)];
                tuple(&mut arena, values)
            });
            make::opt(&mut arena, typ.node.clone().into(), value, span(8))
        }
        .unwrap();
        assign_exp(&mut arena, ctx, &exp, value)
    });
    assert!(
        ctx.find_value_at_slot(
            layout
                .resolve_var(p4spec_rust::lang::il::ast::Var {
                    id: id("x"),
                    typ: p4spec_rust::lang::data::typ::make::bool(),
                    iters: vec![]
                })
                .slot
        )
        .is_none()
    );
    assert!(
        get::bool(
            &arena,
            &get::opt(&arena, &binding(&ctx, &mut layout, "x", vec![ast::Iter::Opt]))
                .unwrap()
                .unwrap()
        )
        .unwrap()
    );
    assert_eq!(
        arena
            .span(&binding(&ctx, &mut layout, "x", vec![ast::Iter::Opt]))
            .clone(),
        Span::default()
    );
    let ctx = ok({
        let value = make::opt(&mut arena, typ.node.clone().into(), None, span(8)).unwrap();
        assign_exp(&mut arena, ctx, &exp, value)
    });
    assert_eq!(
        arena
            .span(&binding(&ctx, &mut layout, "x", vec![ast::Iter::Opt]))
            .clone(),
        Span::default()
    );
    assert!(
        get::opt(&arena, &binding(&ctx, &mut layout, "x", vec![ast::Iter::Opt]))
            .unwrap()
            .is_none()
    );
    assert!(
        ctx.find_value_at_slot(
            layout
                .resolve_var(p4spec_rust::lang::il::ast::Var {
                    id: id("x"),
                    typ: p4spec_rust::lang::data::typ::make::bool(),
                    iters: vec![]
                })
                .slot
        )
        .is_none()
    );
}

#[test]
fn test_cons_tail_preserves_value_type_with_default_span() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = exp(ast::ExpKind::Cons(Box::new(id_exp("h")), Box::new(id_exp("t"))));
    let (exp, ctx, mut layout) = prepare_exp(&global, exp);

    let value = {
        let values = vec![value(&mut arena, true), value(&mut arena, false)];
        list(&mut arena, values)
    };
    let ctx = ok(assign_exp(&mut arena, ctx, &exp, value));
    let tail = binding(&ctx, &mut layout, "t", vec![]);
    assert_eq!(tail.note, value.note);
    assert_eq!(arena.span(&tail).clone(), Span::default());
    assert!((binding(&ctx, &mut layout, "h", vec![]) == get::list(&arena, &value).unwrap()[0]));
    assert!(!get::bool(&arena, &get::list(&arena, &tail).unwrap()[0]).unwrap());
}

#[test]
#[should_panic(expected = "cons pattern must match a non-empty list")]
fn test_cons_assignment_requires_a_matching_list() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = exp(ast::ExpKind::Cons(Box::new(id_exp("h")), Box::new(id_exp("t"))));
    let (exp, ctx, _) = prepare_exp(&global, exp);
    let value = list(&mut arena, vec![]);
    let _ = assign_exp(&mut arena, ctx, &exp, value);
}

#[test]
#[should_panic(expected = "assignment arity mismatch")]
fn test_assignment_requires_equal_counts() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exps = vec![
        id_exp("x"),
        p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("y")), note: typ::make::bool().node, span: span(9)),
    ];
    let mut layout = FrameLayout::default();
    let exps = exps
        .into_iter()
        .map(|exp_source| exp_source.prepare(&mut layout))
        .collect::<Vec<_>>();
    let ctx = Context::new(&global).localize_with_layout(&layout.into());
    let _ = assign_exps(&mut arena, ctx, &exps, &[]);
}

#[test]
#[should_panic(expected = "option pattern must match the value")]
fn test_option_assignment_requires_a_matching_option() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = exp(ast::ExpKind::Opt(None));
    let (exp, ctx, _) = prepare_exp(&global, exp);
    let value_inner = value(&mut arena, true);
    let value_opt = make::opt(
        &mut arena,
        typ::make::opt(typ::make::bool()).node.into(),
        Some(value_inner),
        span(8),
    )
    .unwrap();
    let _ = assign_exp(&mut arena, ctx, &exp, value_opt);
}

#[test]
#[should_panic(expected = "assignment pattern must match the value")]
fn test_assignment_requires_a_matching_value_kind() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let (exp, ctx, _) = prepare_exp(&global, exp(ast::ExpKind::Tuple(vec![])));
    let value_bool = value(&mut arena, true);
    let _ = assign_exp(&mut arena, ctx, &exp, value_bool);
}

#[test]
fn test_function_argument_shares_caller_definition_without_caller_values() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let func = Rc::new(Callable::prepare(ast::MetaFuncDef::Extern(ast::ExternFunc {
        id: id("f"),
        tparams: vec![],
        params: vec![],
        typ: typ::make::bool(),
        hints: vec![],
    })));
    let mut layout = FrameLayout::default();
    let slot_secret = layout.resolve_var(p4spec_rust::lang::il::ast::Var {
        id: id("secret"),
        typ: p4spec_rust::lang::data::typ::make::bool(),
        iters: vec![],
    });
    let callee = {
        let global_caller = Global::load(vec![]).unwrap();
        let mut caller =
            Context::new(&global_caller).localize_with_layout(&Rc::new(layout.clone()));
        caller.add_func(id("f"), func.clone()).unwrap();
        caller.add_value_at_slot(slot_secret.slot, value(&mut arena, true));
        let arg = phrase!(node: ast::ArgKind::Def(id("alias")), span: span(4));
        let arg = arg.prepare(&mut FrameLayout::default());
        let func_value =
            make::func(&mut arena, id("f"), vec![], vec![], typ::make::bool(), span(8)).unwrap();
        let callee = ok(assign_args(
            &mut arena,
            &caller,
            Context::new(&global).localize_with_layout(&layout.into()),
            &[arg],
            &[func_value],
        ));
        assert!(Rc::ptr_eq(
            caller.find_func_with_scope(&id("f")).unwrap().1,
            callee.find_func_with_scope(&id("alias")).unwrap().1,
        ));
        assert!(caller.find_func_opt(&id("alias")).is_none());
        callee
    };
    assert!(Rc::ptr_eq(&func, callee.find_func_with_scope(&id("alias")).unwrap().1));
    assert_eq!(callee.find_func_with_scope(&id("alias")).unwrap(), (Scope::Local, &func));
    assert!(callee.find_value_at_slot(slot_secret.slot).is_none());
}

#[test]
fn test_case_and_struct_assignments_follow_argument_order() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    use p4spec_rust::lang::common::notation::{atom::Atom, mixfix::Mixfix};
    let atom = |name: &str| phrase!(node: Atom::Keyword(name.to_owned()), span: span(3));
    let case_exp = exp(ast::ExpKind::Case(Box::new(Mixfix::Seq(vec![
        Mixfix::Atom(atom("LEFT")),
        Mixfix::Arg(id_exp("x")),
        Mixfix::Arg(id_exp("y")),
    ]))));
    let case_value = {
        let value_case = Mixfix::Seq(vec![
            Mixfix::Atom(atom("RIGHT")),
            Mixfix::Arg(value(&mut arena, true)),
            Mixfix::Arg(value(&mut arena, false)),
        ]);
        make::case(&mut arena, (typ::make::bool()).node.clone().into(), value_case, span(8))
    }
    .unwrap();
    let struct_exp = exp(ast::ExpKind::Str(vec![
        ast::ExpField { atom: atom("a"), exp: id_exp("x") },
        ast::ExpField { atom: atom("b"), exp: id_exp("y") },
    ]));
    let struct_value = {
        let fields =
            vec![(atom("b"), value(&mut arena, true)), (atom("a"), value(&mut arena, false))];
        make::structure(&mut arena, (typ::make::bool()).node.clone().into(), fields, span(8))
    }
    .unwrap();
    for (exp, value) in [(case_exp, case_value), (struct_exp, struct_value)] {
        let (exp, ctx, mut layout) = prepare_exp(&global, exp);
        let ctx = ok(assign_exp(&mut arena, ctx, &exp, value));
        assert!(get::bool(&arena, &binding(&ctx, &mut layout, "x", vec![])).unwrap());
        assert!(!get::bool(&arena, &binding(&ctx, &mut layout, "y", vec![])).unwrap());
    }
}

#[test]
fn test_empty_iteration_creates_empty_collections_for_every_binding() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = iter(
        exp(ast::ExpKind::Tuple(vec![id_exp("x"), id_exp("y")])),
        ast::Iter::List,
        vec![var("x", vec![]), var("y", vec![])],
    );
    let (exp, ctx, mut layout) = prepare_exp(&global, exp);

    let ctx = ok({
        let value = list(&mut arena, vec![]);
        assign_exp(&mut arena, ctx, &exp, value)
    });
    for name in ["x", "y"] {
        assert!(
            get::list(&arena, &binding(&ctx, &mut layout, name, vec![ast::Iter::List]))
                .unwrap()
                .is_empty()
        );
    }
}

fn assign_wrong_iteration_kind(iter_kind: ast::Iter) -> Result<(), Failure> {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = iter(exp(ast::ExpKind::Tuple(vec![id_exp("x")])), iter_kind, vec![var("x", vec![])]);
    let (exp, ctx, _) = prepare_exp(&global, exp);
    let value = value(&mut arena, true);
    assign_exp(&mut arena, ctx, &exp, value).map(|_| ())
}

#[test]
#[should_panic(expected = "iteration assignment value must be an option")]
fn test_option_assignment_typed_kind_precondition() {
    let _ = assign_wrong_iteration_kind(ast::Iter::Opt);
}

#[test]
#[should_panic(expected = "iteration assignment value must be a list")]
fn test_list_assignment_typed_kind_precondition() {
    let _ = assign_wrong_iteration_kind(ast::Iter::List);
}

#[test]
fn test_optional_destructuring_preserves_outer_scalars() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let exp = iter(
        exp(ast::ExpKind::Tuple(vec![id_exp("x"), id_exp("y")])),
        ast::Iter::Opt,
        vec![var("x", vec![]), var("y", vec![])],
    );
    let (exp, mut ctx, mut layout) = prepare_exp(&global, exp);
    ctx.add_value_at_slot(layout.resolve_var(var("x", vec![])).slot, value(&mut arena, false));

    let value_inner = value(&mut arena, true);
    let value_tuple = tuple(&mut arena, vec![value_inner, value_inner]);
    let value_opt = make::opt(
        &mut arena,
        typ::make::opt(typ::make::bool()).node.into(),
        Some(value_tuple),
        span(8),
    )
    .unwrap();
    let ctx = ok(assign_exp(&mut arena, ctx, &exp, value_opt));
    assert!(!get::bool(&arena, &binding(&ctx, &mut layout, "x", vec![])).unwrap());
    assert!(
        ctx.find_value_at_slot(
            layout
                .resolve_var(p4spec_rust::lang::il::ast::Var {
                    id: id("y"),
                    typ: p4spec_rust::lang::data::typ::make::bool(),
                    iters: vec![]
                })
                .slot
        )
        .is_none()
    );
    for name in ["x", "y"] {
        assert_eq!(
            get::opt(&arena, &binding(&ctx, &mut layout, name, vec![ast::Iter::Opt])).unwrap(),
            Some(value_inner)
        );
    }
}
