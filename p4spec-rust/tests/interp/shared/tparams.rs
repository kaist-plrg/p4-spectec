//! Type argument binding across AL, SL, and PL
//!
//! Type parameters shadow global definitions in both input checks and bodies.

use crate::interp::report::ReportExt;
use p4spec_rust::interp::shared::backtrack::Failure;
use p4spec_rust::{
    interp::{
        al::context as ctx_al,
        pl::context as ctx_pl,
        shared::{
            context::WriteContext,
            error::EntityKind,
            eval::assign::{assign_args, assign_def, assign_tparams},
        },
        sl::context as ctx_sl,
    },
    lang::{
        al,
        common::source::{Position, Span},
        data::{
            typ,
            value::{ValueArena, get, make},
        },
        pl, sl,
        traits::print::Print,
    },
    pass::{algo, elaborate, prosify, structure},
    phrase,
    runner::{self, BuiltinInterface, Config, Interpreter, NullExtern, Runner},
};

fn check_shadowing<Interp>(mut runner: Runner<Interp, BuiltinInterface, NullExtern>)
where
    Interp: Interpreter<BuiltinInterface, NullExtern, Error = Failure>,
{
    let value = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
    let value = runner
        .context()
        .call_func("ignore", &[typ::make::bool()], &[value])
        .unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
}

#[test]
fn type_arguments_shadow_globals_with_and_without_guards_in_all_interpreters() {
    let spec_el =
        crate::spec_fixture::parse("dec $ignore<X>(X) : nat\ndef $ignore<X>(X) = 1").unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let mut spec_al = algo::convert(spec_il).unwrap();
    let mut spec_sl = structure::convert(spec_al.clone(), false).unwrap();
    let mut spec_pl = prosify::convert(spec_sl.clone()).unwrap();
    // Add the collision after conversion so each interpreter sees the same name
    let id = phrase!(node: "X".to_owned(), span: Span::default());
    let typdef_al = al::ast::TypDef::Extern(al::ast::ExternTyp { id: id.clone(), hints: vec![] });
    let typdef_sl = sl::ast::TypDef::Extern(sl::ast::ExternTyp { id: id.clone(), hints: vec![] });
    let typdef_pl = pl::ast::TypDef::Extern(pl::ast::ExternTyp { id });
    spec_al.push(phrase!(node: al::ast::DefKind::Typ(typdef_al), span: Span::default()));
    spec_sl.push(phrase!(node: sl::ast::DefKind::Typ(typdef_sl), span: Span::default()));
    spec_pl.push(pl::annot::Annotated::new(
        phrase!(node: pl::ast::DefKind::Typ(typdef_pl), span: Span::default()),
    ));
    // Exercise binding in both input checks and function bodies
    for guard in [false, true] {
        let config = Config::new(false, false, guard);
        check_shadowing(runner::build_al(spec_al.clone(), config, NullExtern).unwrap());
        check_shadowing(runner::build_sl(spec_sl.clone(), config, NullExtern).unwrap());
        check_shadowing(runner::build_pl(spec_pl.clone(), config, NullExtern).unwrap());
    }
}

/// Checks internal arity preconditions and duplicate local type bindings.
fn check_binding_errors<Ctx: WriteContext>(ctx: Ctx) {
    let span_call = Span::new(Position::new("call", 1, 1), Position::new("call", 1, 8));
    let span_param = Span::new(Position::new("decl", 2, 3), Position::new("decl", 2, 4));
    let tparam = phrase!(node: "X".to_owned(), span: span_param.clone());
    // Internal type binding only receives calls with validated arity
    for targs in [vec![], vec![typ::make::bool(), typ::make::bool()]] {
        assert!(
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                assign_tparams(ctx.clone(), std::slice::from_ref(&tparam), &targs, &span_call)
            }))
            .is_err(),
            "internal type argument arity is validated before binding"
        );
    }
    // Binding the same parameter twice remains a local duplicate
    let Ok(ctx) =
        assign_tparams(ctx, std::slice::from_ref(&tparam), &[typ::make::bool()], &span_call)
    else {
        panic!("expected a successful type parameter binding")
    };
    let Err(Failure::Fatal(errors)) =
        assign_tparams(ctx, &[tparam], &[typ::make::bool()], &span_call)
    else {
        panic!("expected a duplicate type parameter error")
    };
    assert!(errors.children.is_empty());
    assert_eq!(errors.span(), span_param);
    assert_eq!(
        errors.diagnostic().message,
        p4spec_rust::interp::shared::error::context::binding_repeated(
            EntityKind::Type,
            "X".to_owned()
        )
        .message
    );
}

#[test]
fn internal_type_argument_arity_is_an_invariant_in_all_interpreters() {
    let global_al = ctx_al::Global::load(vec![]).unwrap();
    let global_sl = ctx_sl::Global::load(vec![]).unwrap();
    let global_pl = ctx_pl::Global::load(vec![]).unwrap();
    check_binding_errors(ctx_al::Context::new(&global_al));
    check_binding_errors(ctx_sl::Context::new(&global_sl));
    check_binding_errors(ctx_pl::Context::new(&global_pl));
}

/// Checks malformed bindings and unresolved raw references at assignment.
fn check_argument_invariants<Ctx: WriteContext>(ctx: Ctx) {
    let mut arena = ValueArena::default();
    let value = make::bool(&mut arena, true, Span::default()).unwrap();
    let id = phrase!(node: "f".to_owned(), span: Span::default());
    // Malformed IR violates the assignment precondition
    let panic_args = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        assign_args(&mut arena, &ctx, ctx.clone(), &[], &[value])
    }))
    .is_err();
    let panic_def = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        assign_def(&arena, &ctx, ctx.clone(), &id, value)
    }))
    .is_err();
    assert_eq!((panic_args, panic_def), (true, true));
    // Raw function references still report unknown names as diagnostics
    let value =
        make::func(&mut arena, id.clone(), vec![], vec![], typ::make::bool(), Span::default())
            .unwrap();
    let Err(Failure::Fatal(report)) = assign_def(&arena, &ctx, ctx.clone(), &id, value) else {
        panic!("unknown raw function reference must remain a diagnostic")
    };
    assert!(report.find_code("runtime/binding-undefined").is_some());
}

#[test]
fn internal_argument_bindings_require_validated_ir_in_all_interpreters() {
    let global_al = ctx_al::Global::load(vec![]).unwrap();
    let global_sl = ctx_sl::Global::load(vec![]).unwrap();
    let global_pl = ctx_pl::Global::load(vec![]).unwrap();
    check_argument_invariants(ctx_al::Context::new(&global_al));
    check_argument_invariants(ctx_sl::Context::new(&global_sl));
    check_argument_invariants(ctx_pl::Context::new(&global_pl));
}
