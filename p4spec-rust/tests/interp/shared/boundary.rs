//! Public call validation and host output checks across interpreter stages
//!
//! Names and arities remain diagnostics with every cache and guard setting.

use crate::interp::report::ReportExt;
use p4spec_rust::{
    interp::shared::backtrack::Failure,
    lang::{
        data::{
            typ,
            value::{Value, get, make},
        },
        il::ast::Typ,
        traits::print::Print,
    },
    pass::{algo, elaborate, prosify, structure},
    runner::{self, Extern, Interface, Interpreter, RunnerContext},
};

const SOURCE: &str = r#"
var n : nat
relation Identity: nat |- nat
  hint(input %0)
rule Identity/one: n |- n
extern relation Host: nat |- nat
  hint(input %0)
extern relation Ready: READY
dec $ignore<X>(nat) : nat
def $ignore<X>(n) = 7
extern dec $bridge() : nat
dec $use(nat) : nat
def $use(n) = n_result
  -- Host: n |- n_result
dec $map_opt(nat?) : nat?
def $map_opt(n?) = n_result?
  -- if (n_result = $(n + 1))?
var i : int
dec $increment(int) : int
def $increment(i) = $(i + +1)
dec $less(int) : bool
def $less(i) = $(i < +1)
extern relation HostOpt: nat |- nat?
  hint(input %0)
dec $use_opt(nat) : nat?
def $use_opt(n) = $map_opt(n_out?)
  -- HostOpt: n |- n_out?
"#;

#[derive(Clone, Copy)]
struct Host {
    outputs: usize,
    reenter: bool,
}

impl Extern for Host {
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        // Host reentry supplies raw arguments to the public entry
        ctx.call_func("ignore", &[], &[])
            .map(|value| (value, false))
            .map_err(Into::into)
    }

    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        if self.reenter {
            ctx.call_rel("Identity", &[])
                .map(|values| (values, false))
                .map_err(Into::into)
        } else {
            Ok((vec![values[0]; self.outputs], false))
        }
    }

    fn clear(&mut self) {}
}

fn assert_fatal(failure: Failure, code: &str) {
    let Failure::Fatal(report) = failure else { panic!("expected fatal diagnostic") };
    assert!(report.find_code(code).is_some(), "{}", report.render());
}

macro_rules! runners {
    ($cache:expr, $guard:expr, $host:expr, |$runner:ident| $body:block) => {{
        let spec_el = crate::spec_fixture::parse(SOURCE).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();
        let spec_sl = structure::convert(spec_al.clone(), false).unwrap();
        let spec_pl = prosify::convert(spec_sl.clone()).unwrap();
        let config = runner::Config::new($cache, false, $guard);
        macro_rules! check {
            ($build:ident, $spec:expr) => {{
                let mut $runner = runner::$build($spec, config, $host).unwrap();
                $body
            }};
        }
        check!(build_al, spec_al);
        check!(build_sl, spec_sl);
        check!(build_pl, spec_pl);
    }};
}

#[test]
fn zero_arity_default_hints_reach_the_host() {
    for cache in [false, true] {
        for guard in [false, true] {
            runners!(cache, guard, runner::NullExtern, |runner| {
                assert_fatal(
                    runner.context().call_rel("Ready", &[]).unwrap_err(),
                    "runtime/extern-unconfigured",
                );
                let value = make::nat(runner.arena_mut(), 1u64.into(), Default::default()).unwrap();
                assert_fatal(
                    runner.context().call_rel("Ready", &[value]).unwrap_err(),
                    "runtime/relation-input-arity-mismatch",
                );
            });
        }
    }
}

#[test]
fn optional_inputs_are_rejected_at_the_public_boundary() {
    runners!(true, true, Host { outputs: 1, reenter: false }, |runner| {
        let value = make::bool(runner.arena_mut(), false, Default::default()).unwrap();
        assert_fatal(
            runner
                .context()
                .call_func("map_opt", &[], &[value])
                .unwrap_err(),
            "runtime/function-input-type-mismatch",
        );
    });
}

#[test]
fn optional_host_outputs_are_rejected_before_evaluation() {
    runners!(true, true, Host { outputs: 1, reenter: false }, |runner| {
        let value = make::nat(runner.arena_mut(), 1u64.into(), Default::default()).unwrap();
        assert_fatal(
            runner
                .context()
                .call_func("use_opt", &[], &[value])
                .unwrap_err(),
            "runtime/relation-output-type-mismatch",
        );
    });
}

#[test]
fn valid_optional_inputs_preserve_presence_and_payloads() {
    for cache in [false, true] {
        for guard in [false, true] {
            runners!(cache, guard, Host { outputs: 1, reenter: false }, |runner| {
                let value_nat =
                    make::nat(runner.arena_mut(), 3u64.into(), Default::default()).unwrap();
                for value_inner in [None, Some(value_nat)] {
                    let value = make::opt(
                        runner.arena_mut(),
                        typ::make::opt(typ::make::nat()).node.into(),
                        value_inner,
                        Default::default(),
                    )
                    .unwrap();
                    let value = runner
                        .context()
                        .call_func("map_opt", &[], &[value])
                        .unwrap();
                    let value_result = get::opt(runner.arena(), &value).unwrap();
                    assert_eq!(value_result.is_some(), value_inner.is_some());
                    if let Some(value) = value_result {
                        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "4");
                    }
                }
            });
        }
    }
}

#[test]
fn unguarded_optional_inputs_require_option_values() {
    use std::panic::{AssertUnwindSafe, catch_unwind};

    runners!(false, false, Host { outputs: 1, reenter: false }, |runner| {
        let value = make::bool(runner.arena_mut(), false, Default::default()).unwrap();
        assert!(
            catch_unwind(AssertUnwindSafe(|| {
                let _ = runner.context().call_func("map_opt", &[], &[value]);
            }))
            .is_err()
        );
    });
}

#[test]
fn mixed_numeric_representations_remain_runtime_errors() {
    runners!(true, true, Host { outputs: 1, reenter: false }, |runner| {
        let value = make::nat(runner.arena_mut(), 1u64.into(), Default::default()).unwrap();
        for name in ["increment", "less"] {
            assert_fatal(
                runner.context().call_func(name, &[], &[value]).unwrap_err(),
                "runtime/numeric-invalid",
            );
        }
    });
}

#[test]
fn public_names_and_arities_are_checked_with_every_cache_and_guard_setting() {
    for cache in [false, true] {
        for guard in [false, true] {
            runners!(cache, guard, Host { outputs: 1, reenter: false }, |runner| {
                let value = make::nat(runner.arena_mut(), 1.into(), Default::default()).unwrap();
                for values in [vec![], vec![value, value]] {
                    assert_fatal(
                        runner.context().call_rel("Identity", &values).unwrap_err(),
                        "runtime/relation-input-arity-mismatch",
                    );
                    assert_fatal(
                        runner.context().call_rel("Host", &values).unwrap_err(),
                        "runtime/relation-input-arity-mismatch",
                    );
                    assert_fatal(
                        runner
                            .context()
                            .call_func("ignore", &[typ::make::nat()], &values)
                            .unwrap_err(),
                        "runtime/function-input-arity-mismatch",
                    );
                }
                for targs in [vec![], vec![typ::make::nat(), typ::make::nat()]] {
                    assert_fatal(
                        runner
                            .context()
                            .call_func("ignore", &targs, &[value])
                            .unwrap_err(),
                        "runtime/type-argument-arity-mismatch",
                    );
                }
                assert_fatal(
                    runner.context().call_rel("Missing", &[]).unwrap_err(),
                    "runtime/binding-undefined",
                );
                assert_fatal(
                    runner.context().call_func("missing", &[], &[]).unwrap_err(),
                    "runtime/binding-undefined",
                );
                assert_fatal(
                    runner
                        .context()
                        .call_func("bridge", &[], &[value])
                        .unwrap_err(),
                    "runtime/function-input-arity-mismatch",
                );
            });
        }
    }
}

#[test]
fn public_input_guards_do_not_depend_on_cache_eligibility() {
    for cache in [false, true] {
        runners!(cache, true, Host { outputs: 1, reenter: false }, |runner| {
            let value = make::bool(runner.arena_mut(), true, Default::default()).unwrap();
            assert_fatal(
                runner.context().call_rel("Identity", &[value]).unwrap_err(),
                "runtime/relation-input-type-mismatch",
            );
            assert_fatal(
                runner
                    .context()
                    .call_func("ignore", &[typ::make::nat()], &[value])
                    .unwrap_err(),
                "runtime/function-input-type-mismatch",
            );
        });
    }
}

#[test]
fn host_relation_output_arity_is_checked_before_assignment() {
    for cache in [false, true] {
        for guard in [false, true] {
            for outputs in [0, 2] {
                runners!(cache, guard, Host { outputs, reenter: false }, |runner| {
                    let value =
                        make::nat(runner.arena_mut(), 1.into(), Default::default()).unwrap();
                    assert_fatal(
                        runner
                            .context()
                            .call_func("use", &[], &[value])
                            .unwrap_err(),
                        "runtime/relation-output-arity-mismatch",
                    );
                    assert_fatal(
                        runner.context().call_rel("Host", &[value]).unwrap_err(),
                        "runtime/relation-output-arity-mismatch",
                    );
                });
            }
        }
    }
}

#[test]
fn host_reentry_checks_raw_call_arities() {
    for cache in [false, true] {
        for guard in [false, true] {
            runners!(cache, guard, Host { outputs: 1, reenter: true }, |runner| {
                let value = make::nat(runner.arena_mut(), 1.into(), Default::default()).unwrap();
                assert_fatal(
                    runner.context().call_func("bridge", &[], &[]).unwrap_err(),
                    "runtime/type-argument-arity-mismatch",
                );
                assert_fatal(
                    runner.context().call_rel("Host", &[value]).unwrap_err(),
                    "runtime/relation-input-arity-mismatch",
                );
            });
        }
    }
}

#[test]
fn prepared_candidate_arities_are_invariants() {
    use p4spec_rust::lang::al::ast;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    let mut panics = Vec::new();
    for candidate in ["rule", "clause", "table row"] {
        let spec_el = crate::spec_fixture::parse(SOURCE).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let mut spec_al = algo::convert(spec_il).unwrap();
        // Corrupt candidate patterns while retaining the public signature
        for def in &mut spec_al {
            match &mut def.node {
                ast::DefKind::Rel(ast::RelDef::Defined(rel)) if candidate == "rule" => {
                    rel.rule_groups[0].node.rule_match.exps_input.clear();
                }
                ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func))
                    if func.id.node == "ignore" && candidate != "rule" =>
                {
                    func.clauses[0].node.args.clear();
                    if candidate == "table row" {
                        let clause = func.clauses[0].clone();
                        def.node =
                            ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
                                id: func.id.clone(),
                                params: func.params.clone(),
                                typ: func.typ.clone(),
                                hints: func.hints.clone(),
                                table_rows: vec![p4spec_rust::phrase!(
                                    node: ast::TableRowKind {
                                        exps_signature: vec![],
                                        args: clause.node.args,
                                        exp: clause.node.exp,
                                        prems: clause.node.prems,
                                    },
                                    span: clause.span
                                )],
                            }));
                    }
                }
                _ => {}
            }
        }
        let mut runner = runner::build_al(
            spec_al,
            runner::Config::new(false, false, false),
            Host { outputs: 1, reenter: false },
        )
        .unwrap();
        let value = make::nat(runner.arena_mut(), 1.into(), Default::default()).unwrap();
        panics.push(
            catch_unwind(AssertUnwindSafe(|| {
                if candidate == "rule" {
                    let _ = runner.context().call_rel("Identity", &[value]);
                } else {
                    let targs = if candidate == "clause" { vec![typ::make::nat()] } else { vec![] };
                    let _ = runner.context().call_func("ignore", &targs, &[value]);
                }
            }))
            .is_err(),
        );
    }
    assert_eq!(
        panics,
        vec![true, true, true],
        "rule, clause, and table row arities are invariants"
    );
}
