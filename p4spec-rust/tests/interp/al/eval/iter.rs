//! Iterated evaluation preserves input scopes and diagnostic spans

use p4spec_rust::interp::shared::context::{ReadContext, WriteContext};
use p4spec_rust::interp::shared::prepare::Prepare;
use p4spec_rust::runtime::envs::interp::shared::frame::FrameLayout;
use p4spec_rust::{
    interp::{
        al::{
            AlInterp, Config,
            context::{Context, Global},
        },
        shared::eval::iter::map,
        shared::{
            backtrack::Backtrack,
            error::{ContextErrorKind, ErrorKind, RuntimeErrorKind},
        },
    },
    lang::{
        al::ast,
        common::source::{Position, Span},
        data::{
            typ,
            value::{get, make},
        },
    },
    phrase,
    runner::{NullExtern, NullInterface, Runner},
};

fn id(name: &str, line: usize) -> ast::Id {
    phrase!(node: name.to_owned(), span: Span::new(
        Position::new("context.watsup", line, 0),
        Position::new("context.watsup", line, 1),
    ))
}

fn var(name: &str, iters: Vec<ast::Iter>) -> ast::Var {
    ast::Var { id: id(name, 1), typ: typ::make::bool(), iters }
}

#[test]
fn test_map_opt_requires_agreement_and_preserves_parent() {
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let mut runner = runner.context();
    let vars = [var("x", vec![ast::Iter::List]), var("y", vec![])];
    let typ = typ::make::bool();
    let span = id("iteration", 9).span;
    let mut layout = FrameLayout::default();
    let exp_iter = ast::ExpIter { iter: ast::Iter::Opt, vars: vars.to_vec() }.prepare(&mut layout);
    let typ_result = typ::make::iter(typ::make::bool(), exp_iter.iter)
        .node
        .into();
    let mut ctx = Context::new(runner.spec()).localize_with_layout(&layout.into());
    let value = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
    for var in &exp_iter.vars {
        ctx.add_value(
            ctx.find_var_slot_iterated(var, ast::Iter::Opt).slot,
            make::opt(runner.arena_mut(), typ.node.clone().into(), Some(value), Span::default())
                .unwrap(),
        );
    }
    let value_opt = map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |runner, ctx_sub| {
        for var in &exp_iter.vars {
            assert!(get::bool(runner.arena(), ctx_sub.find_value(var.slot).unwrap()).unwrap());
        }
        Backtrack::Ok(value)
    })
    .finish()
    .unwrap();
    assert_eq!(get::opt(runner.arena(), &value_opt).unwrap(), Some(value));
    assert_eq!(runner.arena().typ(&value_opt), &typ_result);
    assert_eq!(*runner.arena().span(&value_opt), Span::default());
    for var in &exp_iter.vars {
        assert!(ctx.find_value(var.slot).is_none());
    }
    ctx.add_value(
        ctx.find_var_slot_iterated(&exp_iter.vars[1], ast::Iter::Opt)
            .slot,
        make::opt(runner.arena_mut(), typ.node.clone().into(), None, Span::default()).unwrap(),
    );
    let Backtrack::Err(errors) =
        map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |_, _| panic!("mixed optionality"))
    else {
        panic!("expected optionality mismatch");
    };
    assert_eq!(*errors[0].kind, ErrorKind::Context(ContextErrorKind::OptionalityMismatch));
    assert_eq!(errors[0].span, span);
    ctx.add_value(
        ctx.find_var_slot_iterated(&exp_iter.vars[0], ast::Iter::Opt)
            .slot,
        make::opt(runner.arena_mut(), typ.node.clone().into(), None, Span::default()).unwrap(),
    );
    let value_none =
        map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |_, _| panic!("absent inputs"))
            .finish()
            .unwrap();
    assert!(get::opt(runner.arena(), &value_none).unwrap().is_none());
    let value_empty = map(
        &mut runner,
        &ctx,
        &span,
        &typ_result,
        &ast::ExpIter { iter: ast::Iter::Opt, vars: vec![] }.prepare(&mut FrameLayout::default()),
        |_, _| Backtrack::Ok(value),
    )
    .finish()
    .unwrap();
    assert_eq!(get::opt(runner.arena(), &value_empty).unwrap(), Some(value));
}

#[test]
fn test_map_list_transposes_in_order_without_leaking_bindings() {
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let mut runner = runner.context();
    let vars = [var("x", vec![]), var("y", vec![])];
    let span = id("iteration", 9).span;
    let mut layout = FrameLayout::default();
    let exp_iter = ast::ExpIter { iter: ast::Iter::List, vars: vars.to_vec() }.prepare(&mut layout);
    let typ_result = typ::make::iter(typ::make::bool(), exp_iter.iter)
        .node
        .into();
    let mut ctx = Context::new(runner.spec()).localize_with_layout(&layout.into());
    for (var, values) in exp_iter.vars.iter().zip([[true, false], [false, true]]) {
        let values = values
            .into_iter()
            .map(|b| make::bool(runner.arena_mut(), b, Span::default()).unwrap())
            .collect();
        ctx.add_value(
            ctx.find_var_slot_iterated(var, ast::Iter::List).slot,
            make::list(runner.arena_mut(), typ::make::bool().node.into(), values, Span::default())
                .unwrap(),
        );
    }
    let mut rows = Vec::new();
    let value_list = map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |runner, ctx_sub| {
        rows.push(
            exp_iter
                .vars
                .iter()
                .map(|var| {
                    get::bool(runner.arena(), ctx_sub.find_value(var.slot).unwrap()).unwrap()
                })
                .collect::<Vec<_>>(),
        );
        Backtrack::Ok(*ctx_sub.find_value(exp_iter.vars[0].slot).unwrap())
    })
    .finish()
    .unwrap();
    assert_eq!(runner.arena().typ(&value_list), &typ_result);
    assert_eq!(*runner.arena().span(&value_list), Span::default());
    let values = get::list(runner.arena(), &value_list).unwrap();
    assert_eq!(rows, [vec![true, false], vec![false, true]]);
    assert_eq!(
        values
            .iter()
            .map(|value| get::bool(runner.arena(), value).unwrap())
            .collect::<Vec<_>>(),
        [true, false]
    );
    assert!(ctx.find_value(exp_iter.vars[0].slot).is_none());
    let mut count = 0;
    let result = map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |_, _| {
        count += 1;
        Backtrack::Unmatch(vec![])
    });
    assert!(matches!(result, Backtrack::Unmatch(_)));
    assert_eq!(count, 1);
    ctx.add_value(
        ctx.find_var_slot_iterated(&exp_iter.vars[1], ast::Iter::List)
            .slot,
        make::list(runner.arena_mut(), typ::make::bool().node.into(), vec![], Span::default())
            .unwrap(),
    );
    let Backtrack::Err(errors) =
        map(&mut runner, &ctx, &span, &typ_result, &exp_iter, |_, _| panic!("unequal lengths"))
    else {
        panic!("expected iteration length mismatch");
    };
    assert!(matches!(
        *errors[0].kind,
        ErrorKind::Context(ContextErrorKind::IterationLengthMismatch { expected: 2, actual: 0 })
    ));
    assert_eq!(errors[0].span, span);
    let value_empty = map(
        &mut runner,
        &ctx,
        &span,
        &typ_result,
        &ast::ExpIter { iter: ast::Iter::List, vars: vec![] }.prepare(&mut FrameLayout::default()),
        |_, _| panic!("no inputs"),
    )
    .finish()
    .unwrap();
    assert!(get::list(runner.arena(), &value_empty).unwrap().is_empty());
}

#[test]
fn test_iteration_rejects_wrong_value_kind_at_variable_span() {
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let mut runner = runner.context();
    let var = var("x", vec![]);
    let mut layout = FrameLayout::default();
    let exp_iter =
        ast::ExpIter { iter: ast::Iter::Opt, vars: vec![var.clone()] }.prepare(&mut layout);
    let typ_result = typ::make::iter(typ::make::bool(), exp_iter.iter)
        .node
        .into();
    let mut ctx = Context::new(runner.spec()).localize_with_layout(&layout.into());
    ctx.add_value(
        ctx.find_var_slot_iterated(&exp_iter.vars[0], ast::Iter::Opt)
            .slot,
        make::bool(runner.arena_mut(), true, Span::default()).unwrap(),
    );
    let Backtrack::Err(errors) =
        map(&mut runner, &ctx, &id("iteration", 9).span, &typ_result, &exp_iter, |_, _| {
            panic!("wrong input kind")
        })
    else {
        panic!("expected value kind error");
    };
    assert_eq!(errors[0].span, var.id.span);
    assert!(matches!(*errors[0].kind, ErrorKind::Runtime(RuntimeErrorKind::Value(_))));
    let value = ctx
        .find_value(
            ctx.find_var_slot_iterated(&exp_iter.vars[0], exp_iter.iter)
                .slot,
        )
        .unwrap();
    assert!(get::bool(runner.arena(), value).unwrap());
}
