use crate::interp::report::ReportExt;

use p4spec_rust::diagnostic::ReportKind;
use p4spec_rust::interp::shared::context::{ReadContext, WriteContext};
use p4spec_rust::interp::shared::prepare::Prepare;
use p4spec_rust::lang::data::value::ValueArena;
use std::rc::Rc;

use p4spec_rust::lang::traits::print::Print;
use p4spec_rust::{
    interp::al::{AlInterp, Config, context::Global},
    lang::{
        al::ast,
        common::source::Span,
        data::{
            typ,
            value::{Value, get, make},
        },
    },
    pass::{algo, elaborate},
    phrase,
    runner::{BuiltinInterface, InterpreterError, NullExtern, Runner},
};

fn spec(source: &str) -> ast::Spec {
    let spec_el = crate::spec_fixture::parse(source).expect("parse execution fixture");
    let spec_il = elaborate::convert(spec_el).expect("elaborate execution fixture");
    algo::convert(spec_il).expect("convert execution fixture")
}

fn make_runner(spec_al: ast::Spec, det: bool) -> Runner<AlInterp, BuiltinInterface, NullExtern> {
    Runner::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(false, det, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
        NullExtern,
    )
}

fn nat(arena: &mut ValueArena, n: u64) -> Value {
    make::nat(arena, n.into(), Span::default()).unwrap()
}

fn number(arena: &ValueArena, value: &Value) -> String {
    get::num(arena, value).unwrap().to_string()
}

#[test]
fn test_function_choice_preserves_order_and_counts_equal_successes() {
    for output in ["1", "2"] {
        let source = format!(
            "dec $pick() : nat\ndef $pick() = 1\ndef $pick() = {output}\ndef $pick() = 9\n  -- otherwise\n"
        );
        let spec_al = spec(&source);
        let mut runner = make_runner(spec_al.clone(), false);
        assert_eq!(
            {
                let value = &runner.context().call_func("pick", &[], &[]).unwrap();
                number(runner.arena(), value)
            },
            "1"
        );
        let mut runner = make_runner(spec_al, true);
        let error = runner
            .context()
            .call_func("pick", &[], &[])
            .unwrap_err()
            .into_report();
        assert!(matches!(error.kind, ReportKind::Frame { .. }));
        assert!(error.render().contains("non-deterministic"), "{error}");
    }
}

#[test]
fn test_table_rows_remain_sequential_in_deterministic_mode() {
    let mut spec_al = spec("dec $pick() : nat\ndef $pick() = 1\ndef $pick() = 2");
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = spec_al.remove(0).node else {
        panic!("defined function")
    };
    let table_rows = func
        .clauses
        .into_iter()
        .map(|clause| {
            phrase!(node: ast::TableRowKind {
            exps_signature: vec![], args: clause.node.args,
            exp: clause.node.exp, prems: clause.node.prems,
        }, span: clause.span)
        })
        .collect();
    // Overlapping rows cannot be produced by the table frontend
    let def = phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
        id: func.id, params: func.params, typ: func.typ, table_rows, hints: func.hints,
    })), span: Span::default());
    let mut runner = make_runner(vec![def], true);
    assert_eq!(
        {
            let value = &runner.context().call_func("pick", &[], &[]).unwrap();
            number(runner.arena(), value)
        },
        "1"
    );
}

#[test]
fn test_builtin_and_extern_failures_skip_otherwise() {
    let source = r#"
builtin dec $text_to_int(text) : int
extern dec $unavailable() : int
dec $recover() : int
def $recover() = $text_to_int("invalid")
def $recover() = +7
  -- otherwise
dec $fatal() : int
def $fatal() = $unavailable()
def $fatal() = +9
  -- otherwise
"#;
    for det in [false, true] {
        let mut runner = make_runner(spec(source), det);
        assert!(matches!(
            runner.context().call_func("recover", &[], &[]),
            Err(p4spec_rust::runner::InterpreterError::Fatal(_))
        ));
        let error = runner
            .context()
            .call_func("fatal", &[], &[])
            .unwrap_err()
            .into_report();
        assert!(matches!(error.kind, ReportKind::Frame { .. }));
        let message = error.render();
        assert!(message.contains("unavailable"), "{message}");
        assert!(message.contains("fatal"), "{message}");
        assert!(!message.contains("recover"), "{message}");
    }
}

#[test]
fn test_function_failures_keep_calls_without_clause_or_row_frames() {
    for table in [false, true] {
        for det in [false, true] {
            let mut spec_al = spec(
                "extern dec $unavailable() : nat\ndec $entry() : nat\ndef $entry() = 7\n  -- if $unavailable() = 0",
            );
            // Exercise the same failing premise in a clause and a table row
            if table {
                let def = spec_al.last_mut().unwrap();
                let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &def.node else {
                    panic!("defined function")
                };
                let table_rows = func
                    .clauses
                    .iter()
                    .map(|clause| {
                        phrase!(node: ast::TableRowKind {
                        exps_signature: vec![], args: clause.node.args.clone(),
                        exp: clause.node.exp.clone(), prems: clause.node.prems.clone(),
                    }, span: clause.span.clone())
                    })
                    .collect();
                def.node = ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
                    id: func.id.clone(),
                    params: func.params.clone(),
                    typ: func.typ.clone(),
                    table_rows,
                    hints: func.hints.clone(),
                }));
            }
            let mut runner = make_runner(spec_al, det);
            let failure = runner.context().call_func("entry", &[], &[]).unwrap_err();
            assert!(matches!(failure, InterpreterError::Fatal(_)), "{failure}");
            let text = failure.into_report().render();
            assert!(text.contains("while invoking $entry"), "{text}");
            assert!(text.contains("while invoking $unavailable"), "{text}");
            assert!(text.contains("while evaluating expression"), "{text}");
            assert!(!text.contains("while evaluating an if premise"), "{text}");
            assert!(!text.contains("while evaluating a function clause"), "{text}");
            assert!(!text.contains("while evaluating a table row"), "{text}");
        }
    }
}

#[test]
fn test_relation_failures_keep_causes_without_path_or_premise_frames() {
    for otherwise in [false, true] {
        let mut spec_al = spec(
            "relation Reject: nat |- nat\n  hint(input %0)\nrule Reject/one: 1 |- 0\n  -- if false",
        );
        // Exercise the same failure through the ordinary and otherwise paths
        if otherwise {
            let ast::DefKind::Rel(ast::RelDef::Defined(rel)) = &mut spec_al[0].node else {
                panic!("defined relation")
            };
            let mut group = rel.rule_groups.remove(0);
            rel.else_group = Some(phrase!(node: ast::ElseGroupKind {
                id: group.node.id,
                rule_match: group.node.rule_match,
                rule_path: group.node.rule_paths.remove(0),
            }, span: group.span));
        }
        // Sequential choice retains mismatches; deterministic choice discards them
        let mut runner = make_runner(spec_al, false);
        let value = nat(runner.arena_mut(), 1);
        let failure = runner.context().call_rel("Reject", &[value]).unwrap_err();
        assert!(matches!(failure, InterpreterError::Mismatch(_)), "{failure}");
        let error = failure.into_report();
        let cause = error.find_code("runtime/condition-unmet").unwrap();
        assert_eq!(cause.span().left.line, 4);
        let text = error.render();
        assert!(text.contains("while invoking Reject"), "{text}");
        assert!(!text.contains("while evaluating"), "{text}");
    }
}

#[test]
fn test_higher_order_alias_is_resolved_in_the_caller() {
    let source = r#"
var n : nat
dec $add(nat) : nat
def $add(n) = $(n + 3)
dec $apply(nat, def $f(nat) : nat) : nat
def $apply(n, def $f) = $f(n)
dec $forward(nat, def $g(nat) : nat) : nat
def $forward(n, def $g) = $apply(n, def $g)
dec $entry(nat) : nat
def $entry(n) = $forward(n, def $add)
"#;
    let mut runner = make_runner(spec(source), true);
    assert_eq!(
        {
            let value = &{
                let (name, targs, values) = ("entry", &[], &[nat(runner.arena_mut(), 4)]);
                runner.context().call_func(name, targs, values)
            }
            .unwrap();
            number(runner.arena(), value)
        },
        "7"
    );
}

const RELATION: &str = r#"
var n : nat
relation Step: nat ~> nat ~> nat
  hint(input %0)
rule Step/first: n ~> $(n + 1) ~> $(n + 2)
rule Step/second: n ~> $(n + 1) ~> $(n + 2)
"#;

#[test]
fn test_relation_premises_bind_outputs_and_else_recovers_unmatched_paths() {
    let source = r#"
var n : nat
relation Step: nat ~> nat ~> nat
  hint(input %0)
rule Step/positive: n ~> $(n + 1) ~> $(n + 2)
  -- if $(n > 0)
rule Step/zero: n ~> 8 ~> 9
  -- otherwise
dec $use(nat) : nat
def $use(n) = $(n_a * 10 + n_b)
  -- Step: n ~> n_a ~> n_b
"#;
    for det in [false, true] {
        let mut runner = make_runner(spec(source), det);
        assert_eq!(
            {
                let value = &{
                    let (name, targs, values) = ("use", &[], &[nat(runner.arena_mut(), 0)]);
                    runner.context().call_func(name, targs, values)
                }
                .unwrap();
                number(runner.arena(), value)
            },
            "89"
        );
        assert_eq!(
            {
                let value = &{
                    let (name, targs, values) = ("use", &[], &[nat(runner.arena_mut(), 2)]);
                    runner.context().call_func(name, targs, values)
                }
                .unwrap();
                number(runner.arena(), value)
            },
            "34"
        );
    }
}

#[test]
fn test_relation_outputs_follow_notation_order_and_equal_paths_are_ambiguous() {
    let spec_al = spec(RELATION);
    let mut runner = make_runner(spec_al.clone(), false);
    let values = {
        let (name, values) = ("Step", &[nat(runner.arena_mut(), 5)]);
        runner.context().call_rel(name, values)
    }
    .unwrap();
    assert_eq!(
        values
            .iter()
            .map(|value| number(runner.arena(), value))
            .collect::<Vec<_>>(),
        ["6", "7"]
    );
    let mut runner = make_runner(spec_al, true);
    let error = {
        let (name, values) = ("Step", &[nat(runner.arena_mut(), 5)]);
        runner.context().call_rel(name, values)
    }
    .unwrap_err()
    .into_report();
    assert!(matches!(error.kind, ReportKind::Frame { .. }));
    assert!(error.render().contains("non-deterministic"), "{error}");
}

#[test]
fn test_relation_fatal_failure_does_not_select_else() {
    let source = r#"
var n : nat
extern relation Broken: nat ~> nat
  hint(input %0)
relation Recover: nat ~> nat
  hint(input %0)
rule Recover/primary: n ~> n_result
  -- Broken: n ~> n_result
rule Recover/fallback: n ~> 9
  -- otherwise
"#;
    for det in [false, true] {
        let mut runner = make_runner(spec(source), det);
        let error = {
            let (name, values) = ("Recover", &[nat(runner.arena_mut(), 1)]);
            runner.context().call_rel(name, values)
        }
        .unwrap_err()
        .into_report();
        assert!(matches!(error.kind, ReportKind::Frame { .. }));
        let message = error.render();
        assert!(message.contains("Broken"), "{message}");
        assert!(message.contains("Recover"), "{message}");
    }
}

#[test]
fn test_iterated_premises_collect_results_and_do_not_leak_failed_bindings() {
    let source = r#"
var n : nat
dec $map(nat*) : nat*
def $map(n*) = n_result*
  -- if (n_result = $(n + 1))*
dec $fallback(nat*) : nat*
def $fallback(n*) = n_result*
  -- if (n_result = $(n + 1))*
  -- if false
def $fallback(n*) = n*
  -- otherwise
"#;
    let mut spec_al = spec(source);
    for def in &mut spec_al {
        let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node else {
            continue;
        };
        if func.id.node != "fallback" {
            continue;
        }
        let clause = &mut func.clauses[0];
        let ast::ArgKind::Exp(exp_l) = &clause.node.args[0].node else {
            panic!("expression argument")
        };
        // Force a branch-local overwrite before failure to expose scope leakage
        let prem = phrase!(node: ast::PremKind::Let(ast::LetPrem {
            exp_l: exp_l.as_ref().clone(), exp_r: clause.node.exp.clone(),
        }), span: clause.span.clone());
        clause.node.prems.insert(1, prem);
    }
    let mut runner = make_runner(spec_al, false);
    for values in [vec![], vec![nat(runner.arena_mut(), 2), nat(runner.arena_mut(), 4)]] {
        let value = make::list(
            runner.arena_mut(),
            (typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List))
                .node
                .clone()
                .into(),
            values.clone(),
            Span::default(),
        )
        .unwrap();
        let mapped = runner
            .context()
            .call_func("map", &[], std::slice::from_ref(&value))
            .unwrap();
        let expected = if values.is_empty() { vec![] } else { vec!["3", "5"] };
        assert_eq!(
            get::list(runner.arena(), &mapped)
                .unwrap()
                .iter()
                .map(|v| number(runner.arena(), v))
                .collect::<Vec<_>>(),
            expected
        );
        let fallback = runner
            .context()
            .call_func("fallback", &[], std::slice::from_ref(&value))
            .unwrap();
        assert_eq!(fallback.node, value.node);
    }
}

#[test]
fn test_iterated_premise_rows_read_parent_bindings_independently() {
    use p4spec_rust::{
        interp::al::{context::Context, eval::prem::eval_prem},
        lang::common::prim::num,
        note_phrase,
    };

    let id = |name: &str| phrase!(node: name.to_owned(), span: Span::default());
    let exp_id = |name: &str| p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id(name)), note: Rc::new(typ::make::nat().node), span: Span::default());
    let var = |name: &str| ast::Var { id: id(name), typ: typ::make::nat(), iters: vec![] };
    let prem_inner = phrase!(node: ast::PremKind::Let(ast::LetPrem {
        exp_l: exp_id("n_result"),
        exp_r: note_phrase!(
            node: ast::ExpKind::Bin(
                ast::BinOp::Num(num::BinOp::Add), ast::OpTyp::Nat,
                Box::new(exp_id("n_result")), Box::new(exp_id("n")),
            ),
            note: Rc::new(typ::make::nat().node), span: Span::default()
        ),
    }), span: Span::default());
    let prem = phrase!(node: ast::PremKind::Iter(ast::IterPrem {
        prem: Box::new(prem_inner),
        prem_iter: ast::PremIter {
            iter: ast::Iter::List,
            vars_bound: vec![var("n")], vars_bind: vec![var("n_result")],
        },
    }), span: Span::default());
    let mut var_bound = var("n");
    var_bound.iters.push(ast::Iter::List);
    let mut var_bind = var("n_result");
    var_bind.iters.push(ast::Iter::List);
    let prem_nested = phrase!(node: ast::PremKind::Iter(ast::IterPrem {
        prem: Box::new(prem.clone()),
        prem_iter: ast::PremIter {
            iter: ast::Iter::List,
            vars_bound: vec![var_bound], vars_bind: vec![var_bind],
        },
    }), span: Span::default());
    let mut layout = p4spec_rust::runtime::envs::interp::shared::frame::FrameLayout::default();
    let prem = prem.prepare(&mut layout);
    let prem_nested = prem_nested.prepare(&mut layout);
    let mut runner = make_runner(vec![], false);
    let mut runner = runner.context();
    let slot_n = layout.resolve_var(var("n"));
    let slot_result = layout.resolve_var(var("n_result"));
    let slot_n_list = layout.find_var_iterated(&slot_n, ast::Iter::List);
    let slot_result_list = layout.find_var_iterated(&slot_result, ast::Iter::List);
    let slot_n_nested = layout.find_var_iterated(&slot_n_list, ast::Iter::List);
    let slot_result_nested = layout.find_var_iterated(&slot_result_list, ast::Iter::List);
    let mut ctx = Context::new(runner.spec()).localize_with_layout(&Rc::new(layout.clone()));
    let value_parent = nat(runner.arena_mut(), 100);
    ctx.add_value_at_slot(slot_result.slot, value_parent);
    let values = (1..=3).map(|n| nat(runner.arena_mut(), n)).collect();
    let value = make::list(
        runner.arena_mut(),
        typ::make::list(typ::make::nat()).node.into(),
        values,
        Span::default(),
    )
    .unwrap();
    ctx.add_value_at_slot(slot_n_list.slot, value);
    let Ok(ctx_post) = eval_prem(&mut runner, ctx.clone(), &prem) else {
        panic!("iterated premise failed");
    };
    let value = ctx_post.find_value_at_slot(slot_result_list.slot).unwrap();
    let values: Vec<_> = get::list(runner.arena(), value)
        .unwrap()
        .iter()
        .map(|value| number(runner.arena(), value))
        .collect();
    assert_eq!(values, ["101", "102", "103"]);
    assert_eq!(ctx_post.find_value_at_slot(slot_result.slot).unwrap(), &value_parent);
    assert!(ctx_post.find_value_at_slot(slot_n.slot).is_none());
    assert!(ctx.find_value_at_slot(slot_result_list.slot).is_none());

    let rows = [vec![1, 2], vec![], vec![3]]
        .into_iter()
        .map(|row| {
            let values = row
                .into_iter()
                .map(|n| nat(runner.arena_mut(), n))
                .collect();
            make::list(
                runner.arena_mut(),
                typ::make::list(typ::make::nat()).node.into(),
                values,
                Span::default(),
            )
            .unwrap()
        })
        .collect();
    let value = make::list(
        runner.arena_mut(),
        typ::make::list(typ::make::list(typ::make::nat()))
            .node
            .into(),
        rows,
        Span::default(),
    )
    .unwrap();
    ctx.add_value_at_slot(slot_n_nested.slot, value);
    let Ok(ctx_post) = eval_prem(&mut runner, ctx.clone(), &prem_nested) else {
        panic!("nested iterated premise failed");
    };
    let value = ctx_post
        .find_value_at_slot(slot_result_nested.slot)
        .unwrap();
    let rows: Vec<Vec<_>> = get::list(runner.arena(), value)
        .unwrap()
        .iter()
        .map(|value| {
            get::list(runner.arena(), value)
                .unwrap()
                .iter()
                .map(|value| number(runner.arena(), value))
                .collect()
        })
        .collect();
    assert_eq!(rows, [vec!["101", "102"], vec![], vec!["103"]]);
    assert!(ctx_post.find_value_at_slot(slot_result_list.slot).is_none());
    assert!(ctx.find_value_at_slot(slot_result_nested.slot).is_none());
}

#[test]
fn test_optional_premises_collect_present_and_absent_bindings() {
    let source = r#"
var n : nat
dec $map_opt(nat?) : nat?
def $map_opt(n?) = n_result?
  -- if (n_result = $(n + 1))?
"#;
    let mut runner = make_runner(spec(source), true);
    for input in [None, Some(nat(runner.arena_mut(), 6))] {
        let present = input.is_some();
        let value = make::opt(
            runner.arena_mut(),
            (typ::make::opt(typ::make::nat())).node.clone().into(),
            input,
            Span::default(),
        )
        .unwrap();
        let output = runner
            .context()
            .call_func("map_opt", &[], &[value])
            .unwrap();
        assert_eq!(
            get::opt(runner.arena(), &output)
                .unwrap()
                .map(|value| number(runner.arena(), &value)),
            present.then(|| "7".to_owned())
        );
    }
}

#[test]
fn test_hold_and_not_hold_distinguish_unmatch_from_fatal_failure() {
    for external in [false, true] {
        let relation = if external {
            "extern relation Check: CHECK nat\n  hint(input %0)\n"
        } else {
            "relation Check: CHECK nat\n  hint(input %0)\nrule Check/zero: CHECK n\n  -- if n = 0\n"
        };
        let source = format!(
            r#"
var n : nat
{relation}
dec $hold(nat) : bool
def $hold(n) = true
  -- Check: CHECK n
def $hold(n) = false
  -- otherwise
dec $not_hold(nat) : bool
def $not_hold(n) = true
  -- Check: CHECK n
def $not_hold(n) = false
  -- otherwise
"#
        );
        let mut spec_al = spec(&source);
        for def in &mut spec_al {
            let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node else {
                continue;
            };
            let negate = func.id.node == "not_hold";
            let prem = &mut func.clauses[0].node.prems[0];
            let (id, not_exp) = match &prem.node {
                ast::PremKind::Rule(prem) => (prem.id.clone(), prem.not_exp.clone()),
                ast::PremKind::IfHold(prem) => (prem.id.clone(), prem.not_exp.clone()),
                other => panic!("relation premise: {other:?}"),
            };
            prem.node = if negate {
                ast::PremKind::IfNotHold(ast::IfNotHoldPrem { id, not_exp })
            } else {
                ast::PremKind::IfHold(ast::IfHoldPrem { id, not_exp })
            };
        }
        for det in [false, true] {
            let mut runner = make_runner(spec_al.clone(), det);
            for n in [0, 1] {
                for (name, expected) in [("hold", n == 0), ("not_hold", n != 0)] {
                    let result = {
                        let (name, targs, values) = (name, &[], &[nat(runner.arena_mut(), n)]);
                        runner.context().call_func(name, targs, values)
                    };
                    if external {
                        let error = result.unwrap_err().into_report();
                        assert!(matches!(error.kind, ReportKind::Frame { .. }));
                        assert!(error.render().contains("Check"), "{error}");
                    } else {
                        assert_eq!(get::bool(runner.arena(), &result.unwrap()).unwrap(), expected);
                    }
                }
            }
        }
    }
}

#[test]
fn test_native_function_calls_and_else_fallback() {
    let spec_al = spec(
        r#"
var n : nat
dec $inc(nat) : nat
def $inc(n) = n + 1
dec $choose(nat) : nat
def $choose(n) = 0
  -- if false
def $choose(n) = $inc(n)
  -- otherwise
"#,
    );
    let mut runner = make_runner(spec_al, false);
    assert_eq!(
        {
            let value = &{
                let (name, targs, values) = ("choose", &[], &[nat(runner.arena_mut(), 4)]);
                runner.context().call_func(name, targs, values)
            }
            .unwrap();
            number(runner.arena(), value)
        },
        "5"
    );
}

#[test]
fn test_program_evaluation_preserves_input_value() {
    let spec_al =
        spec("var n : nat\nrelation Pass: nat ~> nat\n  hint(input %0)\nrule Pass/pass: n ~> n");
    let mut runner = make_runner(spec_al, false);
    let program = nat(runner.arena_mut(), 8);
    let values = runner.eval_program("Pass", program).unwrap();
    assert!((values[0] == program));
}

#[test]
fn test_native_table_skips_unmatched_rows_and_reports_no_success() {
    let source = r"
syntax choice
syntax firstChoice = A
syntax secondChoice = B
syntax thirdChoice = C
syntax choice =
  | firstChoice
  | secondChoice
  | thirdChoice
dec $reject() : bool
def $reject() = true
  -- if false
tbl dec $select(choice) : bool
tbl def $select =
  | A => true
  | B => false
  | C => $reject()
dec $first() : bool
def $first() = $select(A)
dec $later() : bool
def $later() = $select(B)
dec $none() : bool
def $none() = $select(C)
";
    for det in [false, true] {
        let mut runner = make_runner(spec(source), det);
        assert!(
            {
                let value = &runner.context().call_func("first", &[], &[]).unwrap();
                get::bool(runner.arena(), value)
            }
            .unwrap()
        );
        assert!(
            !{
                let value = &runner.context().call_func("later", &[], &[]).unwrap();
                get::bool(runner.arena(), value)
            }
            .unwrap()
        );
        let error = runner
            .context()
            .call_func("none", &[], &[])
            .unwrap_err()
            .into_report();
        assert!(matches!(error.kind, ReportKind::Frame { .. }));
        let message = error.render();
        assert!(message.contains("select"), "{message}");
        assert!(message.contains("reject"), "{message}");
    }
}

#[test]
fn test_deterministic_calls_preserve_fatal_failure_after_first_success() {
    let spec_al = spec(
        r#"
extern dec $fail() : nat
dec $choose() : nat
def $choose() = 1
def $choose() = $fail()
def $choose() = 9
  -- otherwise
var n : nat
relation Choose: nat ~> nat
  hint(input %0)
rule Choose/first: n ~> 1
rule Choose/fatal: n ~> $fail()
rule Choose/else: n ~> 9
  -- otherwise
"#,
    );
    for det in [false, true] {
        let mut runner = make_runner(spec_al.clone(), det);
        let func = runner.context().call_func("choose", &[], &[]);
        let rel = {
            let (name, values) = ("Choose", &[nat(runner.arena_mut(), 0)]);
            runner.context().call_rel(name, values)
        };
        if det {
            assert!(func.unwrap_err().into_report().render().contains("fail"));
            assert!(rel.unwrap_err().into_report().render().contains("fail"));
        } else {
            assert_eq!(number(runner.arena(), &func.unwrap()), "1");
            assert_eq!(number(runner.arena(), &rel.unwrap()[0]), "1");
        }
    }
}

#[test]
fn test_type_bindings_survive_clause_selection_and_else() {
    let spec_al = spec(
        r#"
var b : bool
dec $identity<X>(X) : X
def $identity<X>(X) = X
dec $pick<X>(bool, X) : X
def $pick<X>(true, X) = $identity<X>(X)
  -- if false
def $pick<X>(true, X) = $identity<X>(X)
def $pick<X>(b, X) = $identity<X>(X)
  -- otherwise
"#,
    );
    for det in [false, true] {
        let mut runner = make_runner(spec_al.clone(), det);
        for condition in [true, false] {
            let value = nat(runner.arena_mut(), 7);
            let values =
                [make::bool(runner.arena_mut(), condition, Span::default()).unwrap(), value];
            let output = runner
                .context()
                .call_func("pick", &[typ::make::nat()], &values)
                .unwrap();
            assert!((output == value));
        }
    }
}

#[test]
fn test_public_guard_rejects_malformed_function_input() {
    let mut runner =
        make_runner(spec("var n : nat\ndec $ignore(nat) : nat\ndef $ignore(n) = 1"), false);
    let error = {
        let (name, targs, values) =
            ("ignore", &[], &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()]);
        runner.context().call_func(name, targs, values)
    }
    .unwrap_err()
    .into_report();
    assert!(error.render().contains("function argument of ignore"), "{error}");
}

struct Host {
    calls: Rc<std::cell::Cell<u64>>,
    value: fn(&mut ValueArena) -> Value,
    reenter: bool,
}

impl p4spec_rust::runner::Interface for Host {
    fn call_builtin(
        &mut self,
        arena: &mut ValueArena,
        _id: &ast::Id,
        _targs: &[ast::Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::InterfaceError> {
        self.calls.set(self.calls.get() + 1);
        Ok(((self.value)(arena), true))
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

impl p4spec_rust::runner::Extern for Host {
    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        self.calls.set(self.calls.get() + 1);
        let values = if self.reenter {
            ctx.call_rel("Step", values)?
        } else {
            vec![(self.value)(ctx.arena_mut())]
        };
        Ok((values, true))
    }

    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        targs: &[ast::Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        // The OCaml extern boundary erases type arguments
        assert!(targs.is_empty());
        self.calls.set(self.calls.get() + 1);
        let value = if self.reenter {
            let value = (self.value)(ctx.arena_mut());
            ctx.call_func("inner", &[], &[value])?
        } else {
            (self.value)(ctx.arena_mut())
        };
        Ok((value, true))
    }

    fn clear(&mut self) {
        self.calls.set(0);
    }
}

fn host(value: fn(&mut ValueArena) -> Value, reenter: bool) -> Host {
    Host { calls: Rc::new(std::cell::Cell::new(0)), value, reenter }
}

#[test]
fn test_guards_toggle_input_checks_and_substitute_type_arguments() {
    let spec_al = spec("var n : nat\ndec $ignore<X>(X) : nat\ndef $ignore<X>(X) = 1");
    for det in [false, true] {
        for guard in [false, true] {
            let mut runner = Runner::<AlInterp, _, _>::new(
                Global::load(spec_al.clone()).unwrap(),
                AlInterp::new(Config::new(false, det, guard)),
                p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
                NullExtern,
            );
            let invalid = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
            let result = runner.context().call_func(
                "ignore",
                &[typ::make::nat()],
                std::slice::from_ref(&invalid),
            );
            if guard {
                assert!(
                    result
                        .unwrap_err()
                        .into_report()
                        .render()
                        .contains("function argument of ignore")
                );
                assert!(
                    {
                        let (name, targs, values) = ("ignore", &[], &[nat(runner.arena_mut(), 1)]);
                        runner.context().call_func(name, targs, values)
                    }
                    .unwrap_err()
                    .into_report()
                    .render()
                    .contains("arity mismatch in type arguments")
                );
                assert!(
                    runner
                        .context()
                        .call_func("ignore", &[typ::make::nat()], &[])
                        .unwrap_err()
                        .into_report()
                        .find_code("runtime/function-input-arity-mismatch")
                        .is_some()
                );
            } else {
                assert_eq!(number(runner.arena(), &result.unwrap()), "1");
            }
            assert_eq!(
                {
                    let value = &runner
                        .context()
                        .call_func("ignore", &[typ::make::bool()], &[invalid])
                        .unwrap();
                    number(runner.arena(), value)
                },
                "1"
            );
        }
    }
}

#[test]
fn test_relation_input_guards_use_hint_order() {
    let mut spec_al = spec(
        "var n : nat\nvar b : bool\nrelation Pick: nat ~> bool ~> nat\n  hint(input %0 %1)\nrule Pick/pick: n ~> b ~> 7",
    );
    let rel = spec_al
        .iter_mut()
        .find_map(|def| match &mut def.node {
            ast::DefKind::Rel(ast::RelDef::Defined(rel)) => Some(rel),
            _ => None,
        })
        .expect("relation");
    rel.input_hint = p4spec_rust::lang::hints::input::InputHint::new(vec![
        p4spec_rust::phrase!(node: 1, span: Default::default()),
        p4spec_rust::phrase!(node: 0, span: Default::default()),
    ]);
    let mut runner = make_runner(spec_al, false);
    let boolean = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
    assert_eq!(
        {
            let value = &{
                let (name, values) = ("Pick", &[boolean, nat(runner.arena_mut(), 1)]);
                runner.context().call_rel(name, values)
            }
            .unwrap()[0];
            number(runner.arena(), value)
        },
        "7"
    );
    let error = {
        let (name, values) = ("Pick", &[nat(runner.arena_mut(), 1), boolean]);
        runner.context().call_rel(name, values)
    }
    .unwrap_err()
    .into_report();
    assert!(error.render().contains("relation input of Pick"));
}

#[test]
fn test_host_output_guards_are_fatal_and_substitute_return_types() {
    let source = r#"
builtin dec $builtin<X>() : X
extern dec $external<X>() : X
dec $pick<X>() : X
def $pick<X>() = $builtin<X>()
def $pick<X>() = $external<X>()
  -- otherwise
"#;
    let spec_al = spec(source);
    for det in [false, true] {
        for guard in [false, true] {
            let builtin = host(|arena| make::bool(arena, true, Span::default()).unwrap(), false);
            let external = host(|arena| make::bool(arena, false, Span::default()).unwrap(), false);
            let calls = external.calls.clone();
            let mut runner = Runner::<AlInterp, _, _>::new(
                Global::load(spec_al.clone()).unwrap(),
                AlInterp::new(Config::new(false, det, guard)),
                builtin,
                external,
            );
            for name in ["builtin", "external", "pick"] {
                let result = runner.context().call_func(name, &[typ::make::nat()], &[]);
                if guard {
                    let error = result.unwrap_err().into_report();
                    assert!(error.render().contains("return value of function"), "{error}");
                } else {
                    assert!(get::bool(runner.arena(), &result.unwrap()).is_ok());
                }
                assert!(
                    {
                        let value = &runner
                            .context()
                            .call_func(name, &[typ::make::bool()], &[])
                            .unwrap();
                        get::bool(runner.arena(), value)
                    }
                    .is_ok()
                );
            }
            assert_eq!(calls.get(), 2, "fatal builtin output must not select else");
        }
    }
}

#[test]
fn test_extern_relation_output_guards_preserve_call_span() {
    let source = "extern relation External: nat ~> bool\n  hint(input %0)\nvar n : nat\nvar b : bool\nrelation Entry: nat ~> bool\n  hint(input %0)\nrule Entry/entry: n ~> b\n  -- External: n ~> b";
    let spec_al = spec(source);
    let rel = spec_al
        .iter()
        .find_map(|def| match &def.node {
            ast::DefKind::Rel(ast::RelDef::Defined(rel)) => Some(rel),
            _ => None,
        })
        .expect("relation");
    let ast::PremKind::Rule(prem) = &rel.rule_groups[0].node.rule_paths[0].prems[0].node else {
        panic!("relation premise")
    };
    let span = prem.id.span.clone();

    fn find_output(
        error: &p4spec_rust::diagnostic::Report,
    ) -> Option<&p4spec_rust::diagnostic::Report> {
        if error.code() == Some("runtime/relation-output-type-mismatch") {
            Some(error)
        } else {
            error.children.iter().find_map(find_output)
        }
    }
    for guard in [false, true] {
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(spec_al.clone()).unwrap(),
            AlInterp::new(Config::new(false, false, guard)),
            p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
            host(|arena| nat(arena, 4), false),
        );
        let result = {
            let (name, values) = ("Entry", &[nat(runner.arena_mut(), 1)]);
            runner.context().call_rel(name, values)
        };
        if guard {
            let error = result.unwrap_err().into_report();
            assert_eq!(find_output(&error).expect("output guard error").span(), span);
            assert!(matches!(error.kind, ReportKind::Frame { .. }));
        } else {
            assert_eq!(number(runner.arena(), &result.unwrap()[0]), "4");
        }
    }
}

#[test]
fn test_uncached_extern_reentry_preserves_outer_scope_and_clear_policy() {
    let source = r#"
var n : nat
builtin dec $tick() : nat
extern dec $bridge(nat) : nat
dec $inner(nat) : nat
def $inner(n) = $(n + $tick())
dec $outer(nat) : nat
def $outer(n) = $(n + $bridge(n))
relation Step: nat ~> nat
  hint(input %0)
rule Step/step: n ~> $(n + 1)
extern relation Relay: nat ~> nat
  hint(input %0)
dec $ambiguous() : nat
def $ambiguous() = 1
def $ambiguous() = 2
"#;
    let builtin = host(|arena| nat(arena, 1), false);
    let external = host(|arena| nat(arena, 40), true);
    let calls_builtin = builtin.calls.clone();
    let calls_extern = external.calls.clone();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(false, true, true)),
        builtin,
        external,
    );
    for _ in 0..2 {
        for count in 1..=2 {
            assert_eq!(
                {
                    let value = &{
                        let (name, targs, values) = ("outer", &[], &[nat(runner.arena_mut(), 5)]);
                        runner.context().call_func(name, targs, values)
                    }
                    .unwrap();
                    number(runner.arena(), value)
                },
                "46"
            );
            assert_eq!(
                {
                    let value = &{
                        let (name, values) = ("Relay", &[nat(runner.arena_mut(), 8)]);
                        runner.context().call_rel(name, values)
                    }
                    .unwrap()[0];
                    number(runner.arena(), value)
                },
                "9"
            );
            assert_eq!(calls_builtin.get(), count);
            assert_eq!(calls_extern.get(), count * 2);
        }
        assert!(
            {
                let (name, targs, values) = (
                    "outer",
                    &[],
                    &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()],
                );
                runner.context().call_func(name, targs, values)
            }
            .unwrap_err()
            .into_report()
            .render()
            .contains("function argument of outer")
        );
        assert!(
            runner
                .context()
                .call_func("ambiguous", &[], &[])
                .unwrap_err()
                .into_report()
                .render()
                .contains("non-deterministic")
        );
        runner.reset();
        assert_eq!(calls_builtin.get(), 0);
        assert_eq!(calls_extern.get(), 0);
    }
}

#[test]
fn test_extern_reentry_uses_public_input_guards() {
    let source =
        "extern dec $bridge(nat) : nat\nvar n : nat\ndec $inner(nat) : nat\ndef $inner(n) = 7";
    for guard in [false, true] {
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            AlInterp::new(Config::new(false, false, guard)),
            p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
            host(|arena| make::bool(arena, true, Span::default()).unwrap(), true),
        );
        let result = {
            let (name, targs, values) = ("bridge", &[], &[nat(runner.arena_mut(), 1)]);
            runner.context().call_func(name, targs, values)
        };
        if guard {
            assert!(
                result
                    .unwrap_err()
                    .into_report()
                    .render()
                    .contains("function argument of inner")
            );
        } else {
            assert_eq!(number(runner.arena(), &result.unwrap()), "7");
        }
    }
}

#[test]
fn test_output_guard_after_success_is_fatal_only_in_deterministic_choice() {
    let source = "builtin dec $bad() : nat\ndec $pick() : nat\ndef $pick() = 1\ndef $pick() = $bad()\ndef $pick() = 9\n  -- otherwise";
    for det in [false, true] {
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            AlInterp::new(Config::new(false, det, true)),
            host(|arena| make::bool(arena, true, Span::default()).unwrap(), false),
            NullExtern,
        );
        let result = runner.context().call_func("pick", &[], &[]);
        if det {
            assert!(
                result
                    .unwrap_err()
                    .into_report()
                    .render()
                    .contains("return value of function bad")
            );
        } else {
            assert_eq!(number(runner.arena(), &result.unwrap()), "1");
        }
    }
}

#[test]
fn test_uncached_input_guards_are_limited_to_public_entries() {
    let source = "var n : nat\ndec $ignore(nat) : nat\ndef $ignore(n) = 1\ndec $entry() : nat\ndef $entry() = $ignore(0)";
    let mut spec_al = spec(source);
    let func = spec_al
        .iter_mut()
        .find_map(|def| match &mut def.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) if func.id.node == "entry" => {
                Some(func)
            }
            _ => None,
        })
        .unwrap();
    let ast::ExpKind::Call(_, _, args) = &mut func.clauses[0].node.exp.node else { panic!("call") };
    let ast::ArgKind::Exp(exp) = &mut args[0].node else { panic!("argument") };
    // Ill-typed AL distinguishes public entry guards from recursive calls
    exp.node = ast::ExpKind::Bool(true);
    exp.note = Rc::new(ast::TypKind::Bool);
    let mut runner = make_runner(spec_al, true);
    assert_eq!(
        {
            let value = &runner.context().call_func("entry", &[], &[]).unwrap();
            number(runner.arena(), value)
        },
        "1"
    );
    assert!(
        {
            let (name, targs, values) =
                ("ignore", &[], &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()]);
            runner.context().call_func(name, targs, values)
        }
        .unwrap_err()
        .into_report()
        .render()
        .contains("function argument of ignore")
    );
}

#[test]
fn test_guard_failure_keeps_its_source_span_through_extern_reentry() {
    fn find_guard(
        error: &p4spec_rust::diagnostic::Report,
    ) -> Option<&p4spec_rust::diagnostic::Report> {
        if error.code().is_some_and(|code| {
            matches!(
                code,
                "runtime/relation-input-type-mismatch"
                    | "runtime/relation-output-type-mismatch"
                    | "runtime/function-input-type-mismatch"
                    | "runtime/function-output-type-mismatch"
            )
        }) {
            Some(error)
        } else {
            error.children.iter().find_map(find_guard)
        }
    }

    let source = "builtin dec $bad() : nat\nextern dec $bridge(nat) : nat\nvar n : nat\ndec $inner(nat) : nat\ndef $inner(n) = $bad()";
    let spec_al = spec(source);
    let func = spec_al
        .iter()
        .find_map(|def| match &def.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) => Some(func),
            _ => None,
        })
        .unwrap();
    let ast::ExpKind::Call(id, _, _) = &func.clauses[0].node.exp.node else { panic!("call") };
    let span = id.span.clone();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(false, false, true)),
        host(|arena| make::bool(arena, true, Span::default()).unwrap(), false),
        host(|arena| nat(arena, 3), true),
    );
    let error = {
        let (name, targs, values) = ("bridge", &[], &[nat(runner.arena_mut(), 1)]);
        runner.context().call_func(name, targs, values)
    }
    .unwrap_err()
    .into_report();
    assert_eq!(find_guard(&error).expect("guard error").span(), span);
    assert!(matches!(error.kind, ReportKind::Frame { .. }));
    assert!(!error.children.is_empty());
}

#[test]
fn test_reentrant_public_guard_keeps_no_source_span() {
    let source = "extern dec $bridge(nat) : nat\nvar n : nat\ndec $inner(nat) : nat\ndef $inner(n) = 7\ndec $outer(nat) : nat\ndef $outer(n) = $bridge(n)";
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(false, false, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
        host(|arena| make::bool(arena, true, Span::default()).unwrap(), true),
    );
    let error = {
        let (name, targs, values) = ("outer", &[], &[nat(runner.arena_mut(), 1)]);
        runner.context().call_func(name, targs, values)
    }
    .unwrap_err()
    .into_report();
    assert!(error.render().contains("function argument of inner"));
    assert_eq!(error.span(), Span::default());
}

// = Call caching

#[derive(Clone, Default)]
struct CacheHost {
    calls: Rc<std::cell::RefCell<std::collections::HashMap<String, usize>>>,
}

impl CacheHost {
    fn count(&self, name: &str) -> usize {
        self.calls.borrow().get(name).copied().unwrap_or(0)
    }

    fn record(&self, name: &str) {
        *self.calls.borrow_mut().entry(name.to_owned()).or_default() += 1;
    }
}

impl p4spec_rust::runner::Interface for CacheHost {
    fn call_builtin(
        &mut self,
        arena: &mut ValueArena,
        id: &ast::Id,
        _targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::InterfaceError> {
        self.record(&id.node);
        if id.node == "fail" {
            return Err(
                p4spec_rust::interface::builtin::BuiltinError::argument_invalid("failure").into()
            );
        }
        let value = values.first().copied().unwrap_or_else(|| nat(arena, 7));
        Ok((value, id.node == "impure"))
    }

    fn clear(&mut self) {
        self.calls.borrow_mut().clear();
    }
}

impl p4spec_rust::runner::Extern for CacheHost {
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        assert!(targs.is_empty());
        self.record(name);
        let value = if name == "bridge" { ctx.call_func("inner", &[], values)? } else { values[0] };
        Ok((value, false))
    }

    fn eval_rel<Interp, Iface>(
        &self,
        _ctx: &mut p4spec_rust::runner::RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: p4spec_rust::runner::Interface,
        Interp: p4spec_rust::runner::Interpreter<Iface, Self>,
    {
        self.record(name);
        Ok((values.to_vec(), false))
    }

    fn clear(&mut self) {
        self.calls.borrow_mut().clear();
    }
}

#[test]
fn test_cache_reuses_canonical_arguments_and_original_annotations() {
    let source = r#"
var ns : nat*
builtin dec $pure(nat*) : nat*
dec $pair(nat*, nat*) : (nat*, nat*)
def $pair(ns_1, ns_2) = ($pure(ns_1), $pure(ns_2))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(true, false, false)),
        host.clone(),
        NullExtern,
    );
    let value_l = nat(runner.arena_mut(), 7);
    let span = Span::new(
        p4spec_rust::lang::common::source::Position::new("right.p4", 50, 0),
        p4spec_rust::lang::common::source::Position::new("right.p4", 50, 2),
    );
    let value_r =
        Value { span: make::bool(runner.arena_mut(), false, span).unwrap().span, ..value_l };
    let value_r = Value {
        note: make::new(
            runner.arena_mut(),
            p4spec_rust::lang::data::value::ValueKind::Bool(false),
            typ::make::int().node.clone().into(),
            Span::default(),
        )
        .unwrap()
        .note,
        ..value_r
    };
    let typ = typ::make::iter(typ::make::nat(), p4spec_rust::lang::common::Iter::List).node;
    let value_l =
        make::list(runner.arena_mut(), typ.clone().into(), vec![value_l], Span::default()).unwrap();
    let value_r =
        make::list(runner.arena_mut(), typ.clone().into(), vec![value_r], Span::default()).unwrap();
    assert_ne!(value_l.node, value_r.node);
    for expected in [1, 2] {
        let value = runner
            .context()
            .call_func("pair", &[], &[value_l, value_r])
            .unwrap();
        assert_eq!(get::tuple(runner.arena(), &value).unwrap(), &[value_l, value_l]);
        assert_eq!(host.count("pure"), expected, "public entry clears memo tables");
    }
}

#[test]
fn test_cache_propagates_effects_and_aborts_on_host_failure() {
    for det in [false, true] {
        for operation in ["impure", "fail"] {
            let source = format!(
                r#"
var n : nat
builtin dec $pure(nat) : nat
builtin dec ${operation}(nat) : nat
dec $recover(nat) : nat
def $recover(n) = ${operation}(n)
def $recover(n) = $pure(n)
  -- otherwise
dec $pair(nat) : (nat, nat)
def $pair(n) = ($recover(n), $recover(n))
"#
            );
            let host = CacheHost::default();
            let mut runner = Runner::<AlInterp, _, _>::new(
                Global::load(spec(&source)).unwrap(),
                AlInterp::new(Config::new(true, det, false)),
                host.clone(),
                NullExtern,
            );
            let value = nat(runner.arena_mut(), 5);
            let result = runner.context().call_func("pair", &[], &[value]);
            if operation == "fail" {
                assert!(matches!(result, Err(p4spec_rust::runner::InterpreterError::Fatal(_))));
                assert_eq!(host.count(operation), 1);
            } else {
                assert_eq!(get::tuple(runner.arena(), &result.unwrap()).unwrap(), &[value, value]);
                assert_eq!(host.count(operation), 2, "tainted wrappers cannot be cached");
            }
            assert_eq!(host.count("pure"), 0);
        }
    }
}

#[test]
fn test_cache_memoizes_relations_but_not_direct_extern_calls() {
    let source = r#"
var n : nat
extern dec $probe(nat) : nat
extern relation Probe: nat ~> nat
  hint(input %0)
relation Step: nat ~> nat
  hint(input %0)
rule Step/step: n ~> $probe(n)
dec $pair(nat) : (nat, nat, nat, nat)
def $pair(n) = (n_1, n_2, n_3, n_4)
  -- Step: n ~> n_1
  -- Step: n ~> n_2
  -- Probe: n ~> n_3
  -- Probe: n ~> n_4
dec $direct(nat) : (nat, nat)
def $direct(n) = ($probe(n), $probe(n))
"#;
    for cache in [false, true] {
        let host = CacheHost::default();
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(spec(source)).unwrap(),
            AlInterp::new(Config::new(cache, false, false)),
            host.clone(),
            host.clone(),
        );
        let value = nat(runner.arena_mut(), 5);
        let result = runner.context().call_func("pair", &[], &[value]).unwrap();
        assert_eq!(get::tuple(runner.arena(), &result).unwrap(), &[value; 4]);
        assert_eq!(host.count("probe"), if cache { 1 } else { 2 });
        assert_eq!(host.count("Probe"), 2);
        runner.reset();
        let value = nat(runner.arena_mut(), 5);
        runner.context().call_func("direct", &[], &[value]).unwrap();
        assert_eq!(host.count("probe"), 2);
    }
}

#[test]
fn test_cache_reentry_clears_results_and_preserves_outer_effects() {
    for operation in ["pure", "impure"] {
        let source = format!(
            r#"
var n : nat
builtin dec $pure(nat) : nat
builtin dec $impure(nat) : nat
extern dec $bridge(nat) : nat
dec $inner(nat) : nat
def $inner(n) = $pure(n)
dec $outer(nat) : (nat, nat, nat)
def $outer(n) = ($pure(n), $bridge(${operation}(n)), $pure(n))
dec $pair(nat) : ((nat, nat, nat), (nat, nat, nat))
def $pair(n) = ($outer(n), $outer(n))
"#
        );
        let host = CacheHost::default();
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(spec(&source)).unwrap(),
            AlInterp::new(Config::new(true, false, false)),
            host.clone(),
            host.clone(),
        );
        let value = nat(runner.arena_mut(), 5);
        runner.context().call_func("pair", &[], &[value]).unwrap();
        let impure = operation == "impure";
        assert_eq!(host.count("bridge"), if impure { 2 } else { 1 });
        assert_eq!(host.count("pure"), if impure { 3 } else { 2 });
        assert_eq!(host.count("impure"), if impure { 2 } else { 0 });
    }
}

#[test]
fn test_cache_excludes_local_and_higher_order_calls() {
    let source = r#"
var n : nat
extern dec $probe(nat) : nat
dec $forward(nat) : nat
def $forward(n) = $probe(n)
dec $apply(nat, def $f(nat) : nat) : (nat, nat)
def $apply(n, def $f) = ($f(n), $f(n))
dec $pair(nat) : ((nat, nat), (nat, nat))
def $pair(n) = ($apply(n, def $forward), $apply(n, def $forward))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(true, false, false)),
        host.clone(),
        host.clone(),
    );
    let value = nat(runner.arena_mut(), 5);
    runner.context().call_func("pair", &[], &[value]).unwrap();
    assert_eq!(host.count("probe"), 4);
}

#[test]
fn test_cache_omits_type_arguments_and_runner_reset_discards_results() {
    let source = r#"
builtin dec $pure<X>(nat) : nat
dec $pair() : (nat, nat)
def $pair() = ($pure<nat>(7), $pure<bool>(7))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(true, false, true)),
        host.clone(),
        NullExtern,
    );
    let id = phrase!(node: "pair".to_owned(), span: Span::default());
    let value = {
        let mut ctx_runner = runner.context();
        let ctx = p4spec_rust::interp::al::context::Context::new(ctx_runner.spec());
        p4spec_rust::interp::al::eval::call::invoke_func(&mut ctx_runner, &ctx, &id, &[], &[])
            .map_err(InterpreterError::into_report)
            .unwrap()
    };
    assert_eq!(host.count("pure"), 1);
    assert_eq!(get::tuple(runner.arena(), &value).unwrap().len(), 2);
    runner.reset();
    let value_new = {
        let mut ctx_runner = runner.context();
        let ctx = p4spec_rust::interp::al::context::Context::new(ctx_runner.spec());
        p4spec_rust::interp::al::eval::call::invoke_func(&mut ctx_runner, &ctx, &id, &[], &[])
            .map_err(InterpreterError::into_report)
            .unwrap()
    };
    assert_eq!(get::tuple(runner.arena(), &value_new).unwrap().len(), 2);
    assert_eq!(host.count("pure"), 1, "reset also discards internal call results");
}

#[test]
fn test_program_reset_isolates_cached_values_between_arenas() {
    let source = "var n : nat\nbuiltin dec $pure(nat) : nat\ndec $pair(nat) : (nat, nat)\ndef $pair(n) = ($pure(n), $pure(n))";
    let host = CacheHost::default();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(true, false, false)),
        host.clone(),
        NullExtern,
    );
    let id = phrase!(node: "pair".to_owned(), span: Span::default());
    for num in [7, 9, 7] {
        runner.reset();
        let value = nat(runner.arena_mut(), num);
        let value_pair = {
            let mut ctx_runner = runner.context();
            let ctx = p4spec_rust::interp::al::context::Context::new(ctx_runner.spec());
            p4spec_rust::interp::al::eval::call::invoke_func(
                &mut ctx_runner,
                &ctx,
                &id,
                &[],
                &[value],
            )
            .map_err(InterpreterError::into_report)
            .unwrap()
        };
        assert_eq!(get::tuple(runner.arena(), &value_pair).unwrap(), &[value, value]);
        assert_eq!(host.count("pure"), 1, "one cached call within each independent program");
    }
}

#[test]
fn test_cache_distinguishes_call_names_and_argument_order() {
    let source = r#"
var n : nat
builtin dec $pure(nat, nat) : nat
builtin dec $other(nat, nat) : nat
dec $use(nat, nat) : (nat, nat, nat, nat)
def $use(n_1, n_2) = ($pure(n_1, n_2), $pure(n_2, n_1), $pure(n_1, n_2), $other(n_1, n_2))
"#;
    let host = CacheHost::default();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec(source)).unwrap(),
        AlInterp::new(Config::new(true, false, true)),
        host.clone(),
        NullExtern,
    );
    let value_l = nat(runner.arena_mut(), 5);
    let value_r = nat(runner.arena_mut(), 7);
    let value = runner
        .context()
        .call_func("use", &[], &[value_l, value_r])
        .unwrap();
    assert_eq!(get::tuple(runner.arena(), &value).unwrap(), &[value_l, value_r, value_l, value_l]);
    assert_eq!(host.count("pure"), 2);
    assert_eq!(host.count("other"), 1);
}

#[test]
fn test_cache_keys_follow_serde_external_payload_equality() {
    use p4spec_rust::util::json::json;
    use serde_json::json;
    let source = r#"
extern syntax state
var s : state
builtin dec $pure(state) : state
dec $pair(state, state) : (state, state)
def $pair(s_1, s_2) = ($pure(s_1), $pure(s_2))
"#;
    let mut fields_a = serde_json::Map::new();
    fields_a.insert("a".to_owned(), json!(1));
    fields_a.insert("b".to_owned(), json!([1, 2]));
    let mut fields_b = serde_json::Map::new();
    fields_b.insert("b".to_owned(), json!([1, 2]));
    fields_b.insert("a".to_owned(), json!(1));
    for (json_a, json_b, count) in [
        (json::Object(fields_a), json::Object(fields_b), 1),
        (json!(-0.0), json!(0.0), 1),
        (json!(1_i64), json!(1_u64), 1),
        (json!(0), json!(0.0), 2),
        (json!(1), json!(1.0), 2),
        (json!([1, 2]), json!([2, 1]), 2),
        (json!(9_007_199_254_740_992_u64), json!(9_007_199_254_740_993_u64), 2),
        (json!(i64::MAX), json!(i64::MAX as u64 + 1), 2),
        (json!(u64::MAX - 1), json!(u64::MAX), 2),
    ] {
        let host = CacheHost::default();
        let mut runner = Runner::new(
            Global::load(spec(source)).unwrap(),
            AlInterp::new(Config::new(true, false, false)),
            host.clone(),
            NullExtern,
        );
        let id = phrase!(node: "state".to_owned(), span: Span::default());
        let typ = typ::make::var(id, vec![]).node;
        let typ = std::rc::Rc::new(typ);
        let value_a =
            make::external(runner.arena_mut(), typ.clone(), json_a.into(), Span::default())
                .unwrap();
        let span_b = Span::new(
            p4spec_rust::lang::common::source::Position::new("state.p4", 7, 0),
            p4spec_rust::lang::common::source::Position::new("state.p4", 7, 1),
        );
        let value_b = make::external(runner.arena_mut(), typ, json_b.into(), span_b).unwrap();
        let value = runner
            .context()
            .call_func("pair", &[], &[value_a, value_b])
            .unwrap();
        let values = get::tuple(runner.arena(), &value).unwrap();
        assert_eq!(host.count("pure"), count);
        assert_eq!(values, &[value_a, if count == 1 { value_a } else { value_b }]);
    }
}
