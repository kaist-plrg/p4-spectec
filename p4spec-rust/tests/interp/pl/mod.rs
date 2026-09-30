use crate::interp::report::ReportExt;
use p4spec_rust::{
    interp::pl::{Config, PlInterp, context::Global},
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{get, make},
        },
        pl::ast,
        traits::print::Print,
    },
    pass::{algo, elaborate, prosify, structure},
    runner::{BuiltinInterface, NullExtern, Runner, Spec},
};

fn spec(source: &str) -> ast::Spec {
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al, false).unwrap();
    prosify::convert(spec_sl).unwrap()
}

fn runner_spec(spec_pl: ast::Spec) -> Runner<PlInterp, BuiltinInterface, NullExtern> {
    Runner::new(
        Global::load(spec_pl).unwrap(),
        PlInterp::new(Config::new(false, false, true)),
        p4spec_rust::interface::p4(&Spec::Pl(Vec::new())),
        NullExtern,
    )
}

fn runner(source: &str) -> Runner<PlInterp, BuiltinInterface, NullExtern> {
    runner_spec(spec(source))
}

#[test]
fn executes_defined_function() {
    let mut runner = runner("dec $answer() : nat\ndef $answer() = 42");
    let value = runner.context().call_func("answer", &[], &[]).unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "42");
}

#[test]
fn executes_relation_dispatch_and_otherwise_function_block() {
    let mut runner = runner(
        r#"
var n : nat
relation Step: nat ~> nat
  hint(input %0)
rule Step/positive: n ~> $(n + 1)
  -- if $(n > 0)
rule Step/zero: n ~> 9
  -- otherwise
dec $use(nat) : nat
def $use(n) = n_result
  -- Step: n ~> n_result
"#,
    );
    for (num, expected) in [(0, "9"), (2, "3")] {
        let value = p4spec_rust::lang::data::value::make::nat(
            runner.arena_mut(),
            num.into(),
            p4spec_rust::lang::common::source::Span::default(),
        )
        .unwrap();
        let value = runner.context().call_func("use", &[], &[value]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), expected);
    }
}

#[test]
fn prose_hints_do_not_change_function_results() {
    let plain = "dec $answer() : nat\ndef $answer() = 42";
    let annotated = "dec $answer() : nat\n  hint(prose_in \"ignored\")\ndef $answer() = 42";
    let spec_annotated = spec(annotated);
    assert!(
        spec_annotated
            .iter()
            .any(|def| def.hints.node.prose_in.is_some())
    );
    for source in [plain, annotated] {
        let mut runner = runner(source);
        let value = runner.context().call_func("answer", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "42");
    }
}

#[test]
fn otherwise_block_handles_recoverable_mismatch() {
    let mut runner = runner(
        r#"
dec $unavailable(nat) : nat
def $unavailable(0) = 0
var n : nat
dec $fallback(nat) : nat
def $fallback(n) = $unavailable(n)
def $fallback(n) = 7
  -- otherwise
"#,
    );
    let value = p4spec_rust::lang::data::value::make::nat(
        runner.arena_mut(),
        1.into(),
        p4spec_rust::lang::common::source::Span::default(),
    )
    .unwrap();
    let value = runner
        .context()
        .call_func("fallback", &[], &[value])
        .unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7");
}

const CHECKED_LIST_SOURCE: &str = r#"
var ns : nat*
var x : nat
var y : nat
dec $fallback(nat*) : nat
def $fallback(ns) = x
  -- if ns <: nat*
  -- if [ x, y ] = ns
def $fallback(ns) = 7
  -- otherwise
"#;

#[test]
fn checked_list_binding_mismatch_reaches_otherwise() {
    let mut runner = runner(CHECKED_LIST_SOURCE);
    for (len, expected) in [(1, "7"), (2, "1")] {
        let value = make::nat(runner.arena_mut(), 1.into(), Span::default()).unwrap();
        let typ = typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List);
        let value =
            make::list(runner.arena_mut(), typ.node.into(), vec![value; len], Span::default())
                .unwrap();
        let value = runner
            .context()
            .call_func("fallback", &[], &[value])
            .unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), expected);
    }
}

#[test]
#[should_panic(expected = "assignment arity mismatch")]
fn checked_subtype_binding_requires_its_pattern_shape_guard() {
    let mut spec_pl = spec(CHECKED_LIST_SOURCE);
    let func = spec_pl
        .iter_mut()
        .find_map(|def| match &mut def.node.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func))
                if func.id.node == "fallback" =>
            {
                Some(func)
            }
            _ => None,
        })
        .unwrap();
    let instr_check = match &func.block[0].node.node {
        ast::InstrKind::If(instr_sub) => match &instr_sub.exp.node.node {
            ast::ExpKind::Sub(exp_r, typ, subcheck) => match &instr_sub.block[0].node.node {
                ast::InstrKind::CheckLetMatch(instr_match) => ast::CheckLetSubInstr {
                    typ: typ.clone(),
                    subcheck: subcheck.clone(),
                    exp_l: instr_match.exp_l.clone(),
                    exp_r: exp_r.as_ref().clone(),
                    block: instr_match.block.clone(),
                },
                _ => panic!("expected a checked list binding"),
            },
            _ => panic!("expected a subtype condition"),
        },
        _ => panic!("expected a subtype condition"),
    };
    // A subtype guard cannot replace the list length guard from CheckLetMatch
    func.block[0].node.node = ast::InstrKind::CheckLetSub(instr_check);

    let mut runner = runner_spec(spec_pl);
    let item = p4spec_rust::lang::data::value::make::nat(
        runner.arena_mut(),
        1.into(),
        p4spec_rust::lang::common::source::Span::default(),
    )
    .unwrap();
    let value = p4spec_rust::lang::data::value::make::list(
        runner.arena_mut(),
        p4spec_rust::lang::data::typ::make::iter(
            p4spec_rust::lang::data::typ::make::nat(),
            p4spec_rust::lang::common::Iter::List,
        )
        .node
        .into(),
        vec![item],
        p4spec_rust::lang::common::source::Span::default(),
    )
    .unwrap();
    let value = runner
        .context()
        .call_func("fallback", &[], &[value])
        .unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7");
}

#[test]
fn shorthand_failures_keep_specific_diagnostics() {
    let mut spec_sub = spec(
        r#"
var n : int
dec $subtype(int) : nat
def $subtype(n) = 1
  -- if n <: nat
"#,
    );
    let func_sub = spec_sub
        .iter_mut()
        .find_map(|def| match &mut def.node.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func))
                if func.id.node == "subtype" =>
            {
                Some(func)
            }
            _ => None,
        })
        .unwrap();
    let instr_check = match &func_sub.block[0].node.node {
        ast::InstrKind::If(instr) => match &instr.exp.node.node {
            ast::ExpKind::Sub(exp_r, typ, subcheck) => ast::CheckLetSubInstr {
                typ: typ.clone(),
                subcheck: subcheck.clone(),
                exp_l: exp_r.as_ref().clone(),
                exp_r: exp_r.as_ref().clone(),
                block: instr.block.clone(),
            },
            _ => panic!("expected a subtype condition"),
        },
        _ => panic!("expected a subtype condition"),
    };
    func_sub.block[0].node.node = ast::InstrKind::CheckLetSub(instr_check);
    let mut runner_sub = runner_spec(spec_sub);
    let value = make::int(runner_sub.arena_mut(), (-1).into(), Span::default()).unwrap();
    let error = runner_sub
        .context()
        .call_func("subtype", &[], &[value])
        .unwrap_err()
        .into_report()
        .render();
    assert!(error.contains("n is not a subtype of nat"), "{error}");

    let mut runner_match = runner(
        r#"
var ns : nat*
var x : nat
var y : nat
dec $matched(nat*) : nat
def $matched(ns) = x
  -- if [ x, y ] = ns
"#,
    );
    let item = make::nat(runner_match.arena_mut(), 1.into(), Span::default()).unwrap();
    let value = make::list(
        runner_match.arena_mut(),
        typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List)
            .node
            .into(),
        vec![item],
        Span::default(),
    )
    .unwrap();
    let error = runner_match
        .context()
        .call_func("matched", &[], &[value])
        .unwrap_err()
        .into_report()
        .render();
    assert!(error.contains("ns does not match the expected pattern"), "{error}");

    let mut spec_option = spec(
        r#"
var o : nat?
var n : nat
dec $option(nat?) : nat
def $option(o) = 0
"#,
    );
    let func_option = spec_option
        .iter_mut()
        .find_map(|def| match &mut def.node.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) if func.id.node == "option" => {
                Some(func)
            }
            _ => None,
        })
        .unwrap();
    let exp_id = |name: &str, typ: p4spec_rust::lang::data::typ::Typ| {
        p4spec_rust::annotated_note_phrase! {
            node: ast::ExpKind::Id(p4spec_rust::phrase! {
                node: name.to_owned(),
                span: Span::default(),
            }),
            note: typ.node,
            span: Span::default(),
        }
    };
    let block = std::mem::take(&mut func_option.block);
    func_option.block = vec![p4spec_rust::annotated_note_phrase! {
        node: ast::InstrKind::OptionGet(ast::OptionGetInstr {
            exp_l: exp_id("n", typ::make::nat()),
            exp_r: exp_id("o", typ::make::opt(typ::make::nat())),
            block,
        }),
        note: None,
        span: Span::default(),
    }];
    let mut runner_option = runner_spec(spec_option);
    let value = make::opt(
        runner_option.arena_mut(),
        typ::make::opt(typ::make::nat()).node.into(),
        None,
        Span::default(),
    )
    .unwrap();
    let error = runner_option
        .context()
        .call_func("option", &[], &[value])
        .unwrap_err()
        .into_report()
        .render();
    assert!(error.contains("o evaluated to an empty option"), "{error}");
}

mod control;
