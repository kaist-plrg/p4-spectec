use super::*;
use crate::interp::report::ReportExt;
use p4spec_rust::diagnostic::ReportKind;
use p4spec_rust::{annotated_note_phrase, lang::common::prim::num::Number};

fn nat(num: u64) -> ast::Exp {
    annotated_note_phrase!(node: ast::ExpKind::Num(Number::Nat(num.into())), note: typ::make::nat().node, span: Span::default())
}

fn variable(name: &str) -> ast::Exp {
    annotated_note_phrase!(node: ast::ExpKind::Id(p4spec_rust::phrase!(node: name.to_owned(), span: Span::default())), note: typ::make::nat().node, span: Span::default())
}

fn instr<Tier>(instr_kind: ast::InstrKind<Tier>) -> ast::Instr<Tier> {
    annotated_note_phrase!(node: instr_kind, note: None, span: Span::default())
}

fn binding<Tier>(num: u64) -> ast::Instr<Tier> {
    instr(ast::InstrKind::Let(ast::LetInstr {
        exp_l: variable("n"),
        exp_r: nat(num),
        iter_instrs: vec![],
    }))
}

fn condition<Tier>(cond: bool, block: ast::Block<Tier>) -> ast::Instr<Tier> {
    instr(ast::InstrKind::If(ast::IfInstr {
        exp: annotated_note_phrase!(node: ast::ExpKind::Bool(cond), note: typ::make::bool().node, span: Span::default()),
        iter_exps: vec![],
        block,
        dangle: false,
    }))
}

fn returning(exp: ast::Exp) -> ast::Instr<ast::GroupInstr> {
    instr(ast::InstrKind::Tier(ast::TierInstr {
        tier: ast::GroupInstr::Return(ast::ReturnInstr { exp }),
    }))
}

fn backtrack(blocks: Vec<ast::GroupBlock>) -> ast::Instr<ast::GroupInstr> {
    instr(ast::InstrKind::Tier(ast::TierInstr {
        tier: ast::GroupInstr::Backtrack(ast::BacktrackInstr { blocks }),
    }))
}

fn function(block: ast::GroupBlock) -> ast::Spec {
    let mut spec_pl = spec("dec $entry() : nat\ndef $entry() = 0");
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut spec_pl[0].node.node else {
        panic!("defined function")
    };
    func.block = block;
    spec_pl
}

fn configured(spec_pl: ast::Spec, det: bool) -> Runner<PlInterp, BuiltinInterface, NullExtern> {
    Runner::new(
        Global::load(spec_pl).unwrap(),
        PlInterp::new(Config::new(false, det, true)),
        p4spec_rust::interface::p4(&Spec::Pl(vec![])),
        NullExtern,
    )
}

#[test]
fn nested_blocks_keep_their_bindings_local() {
    for det in [false, true] {
        let mut runner = configured(
            function(vec![
                binding(1),
                condition(true, vec![binding(2), condition(false, vec![returning(nat(0))])]),
                returning(variable("n")),
            ]),
            det,
        );
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1", "det={det}");
    }
}

#[test]
fn alternatives_start_with_the_same_bindings() {
    for det in [false, true] {
        let mut runner = configured(
            function(vec![
                binding(1),
                backtrack(vec![
                    vec![binding(2), condition(false, vec![])],
                    vec![returning(variable("n"))],
                ]),
            ]),
            det,
        );
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1", "det={det}");
    }
}

#[test]
fn alternatives_choose_the_first_conclusion_or_report_nondeterminism() {
    let spec_pl = function(vec![backtrack(vec![vec![returning(nat(1))], vec![returning(nat(2))]])]);
    let mut runner = configured(spec_pl.clone(), false);
    let value = runner.context().call_func("entry", &[], &[]).unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");

    let mut runner = configured(spec_pl, true);
    let error = runner
        .context()
        .call_func("entry", &[], &[])
        .unwrap_err()
        .into_report();
    assert!(
        error
            .render()
            .contains("nondeterministic instruction evaluation"),
        "{error}"
    );
}

#[test]
fn mismatches_abort_the_block_before_trying_the_next_alternative() {
    let mut spec_pl =
        spec("builtin dec $unavailable() : nat\ndec $entry() : nat\ndef $entry() = $unavailable()");
    let func = spec_pl
        .iter_mut()
        .find_map(|def| match &mut def.node.node {
            ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) => Some(func),
            _ => None,
        })
        .unwrap();
    let mut block = std::mem::take(&mut func.block);
    // This conclusion must not run after the unavailable builtin mismatches
    block.push(returning(nat(99)));
    func.block = vec![backtrack(vec![block, vec![returning(nat(7))]])];
    for det in [false, true] {
        let mut runner = configured(spec_pl.clone(), det);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7", "det={det}");
    }
}

#[test]
fn fatal_errors_abort_alternative_selection() {
    for det in [false, true] {
        let mut runner = configured(
            function(vec![backtrack(vec![
                vec![returning(variable("missing"))],
                vec![returning(nat(7))],
            ])]),
            det,
        );
        let error = runner
            .context()
            .call_func("entry", &[], &[])
            .unwrap_err()
            .into_report();
        assert!(error.render().contains("value `missing` is undefined"), "det={det}: {error}");
    }
}

#[test]
fn dispatch_alternatives_keep_their_bindings_local() {
    for det in [false, true] {
        let mut spec_pl =
            spec("var n : nat\nrelation Entry: nat ~> nat\n  hint(input %0)\nrule Entry: n ~> 0");
        let rel = spec_pl
            .iter_mut()
            .find_map(|def| match &mut def.node.node {
                ast::DefKind::Rel(ast::RelDef::Defined(rel)) => Some(rel),
                _ => None,
            })
            .unwrap();
        let instr_result = instr(ast::InstrKind::Tier(ast::TierInstr {
            tier: ast::GroupInstr::Result(ast::ResultInstr {
                rel_signature: rel.rel_signature.clone(),
                exps_output: vec![variable("n")],
            }),
        }));
        let instr_group = instr(ast::InstrKind::Tier(ast::TierInstr {
            tier: ast::DispatchInstr::Group(ast::RuleGroupInstr {
                id_rel: rel.id.clone(),
                id_group: rel.id.clone(),
                rel_signature: rel.rel_signature.clone(),
                exps_input: vec![],
                block: vec![instr_result],
            }),
        }));
        rel.block = vec![
            binding(1),
            instr(ast::InstrKind::Tier(ast::TierInstr {
                tier: ast::DispatchInstr::Route(ast::RouteInstr {
                    blocks: vec![
                        vec![condition(true, vec![binding(2), condition(false, vec![])])],
                        vec![instr_group],
                    ],
                }),
            })),
        ];
        let mut runner = configured(spec_pl, det);
        let value = make::nat(runner.arena_mut(), 0u64.into(), Span::default()).unwrap();
        let values = runner.context().call_rel("Entry", &[value]).unwrap();
        assert_eq!(get::num(runner.arena(), &values[0]).unwrap().to_string(), "1", "det={det}");
    }
}

#[test]
fn nested_instruction_traces_are_attached_once_in_both_tiers() {
    use p4spec_rust::lang::common::source::Position;

    // Collect instruction locations separately from expression and call traces
    fn instruction_spans(error: &p4spec_rust::diagnostic::Report, spans: &mut Vec<Span>) {
        if matches!(&error.kind, ReportKind::Frame { message, .. } if message.starts_with("evaluation of "))
            && error.span().left.file.as_ref() == "instruction_trace"
        {
            spans.push(error.span().clone());
        }
        for error in &error.children {
            instruction_spans(error, spans);
        }
    }

    let spans = (1..=4)
        .map(|line| {
            Span::new(
                Position::new("instruction_trace", line, 1),
                Position::new("instruction_trace", line, 2),
            )
        })
        .collect::<Vec<_>>();
    let mut spec_pl =
        spec("var n : nat\nrelation Entry: nat ~> nat\n  hint(input %0)\nrule Entry: n ~> 0");
    let rel = spec_pl
        .iter_mut()
        .find_map(|def| match &mut def.node.node {
            ast::DefKind::Rel(ast::RelDef::Defined(rel)) => Some(rel),
            _ => None,
        })
        .unwrap();
    // A dispatch condition enters a group whose condition reaches a fatal result
    let mut instr_result = instr(ast::InstrKind::Tier(ast::TierInstr {
        tier: ast::GroupInstr::Result(ast::ResultInstr {
            rel_signature: rel.rel_signature.clone(),
            exps_output: vec![variable("missing")],
        }),
    }));
    instr_result.node.span = spans[3].clone();
    let mut instr_condition = condition(true, vec![instr_result]);
    instr_condition.node.span = spans[2].clone();
    let mut instr_group = instr(ast::InstrKind::Tier(ast::TierInstr {
        tier: ast::DispatchInstr::Group(ast::RuleGroupInstr {
            id_rel: rel.id.clone(),
            id_group: rel.id.clone(),
            rel_signature: rel.rel_signature.clone(),
            exps_input: vec![variable("n")],
            block: vec![instr_condition],
        }),
    }));
    instr_group.node.span = spans[1].clone();
    let mut instr_dispatch = condition(true, vec![instr_group]);
    instr_dispatch.node.span = spans[0].clone();
    rel.block = vec![instr_dispatch];
    // Each instruction contributes one trace in enclosing-to-enclosed order
    for det in [false, true] {
        let mut runner = configured(spec_pl.clone(), det);
        let value = make::nat(runner.arena_mut(), 0u64.into(), Span::default()).unwrap();
        let error = runner
            .context()
            .call_rel("Entry", &[value])
            .unwrap_err()
            .into_report();
        assert!(error.render().contains("value `missing` is undefined"), "{error}");
        let mut spans_actual = vec![];
        instruction_spans(&error, &mut spans_actual);
        assert_eq!(spans_actual, spans, "det={det}");
    }
}

#[test]
fn nested_instructions_execute_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            // Isolate instruction evaluation from source construction and teardown
            let mut runner = stacker::grow(32 * 1024 * 1024, || {
                let mut instr_inner = returning(nat(7));
                for _ in 0..128 {
                    instr_inner = condition(true, vec![instr_inner]);
                }
                configured(function(vec![instr_inner]), false)
            });
            let value = runner.context().call_func("entry", &[], &[]).unwrap();
            assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7");
            stacker::grow(32 * 1024 * 1024, || drop(runner));
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn prepared_list_and_optional_bindings_execute_in_both_modes() {
    let source = r#"
var n : nat
dec $map(nat*) : nat*
def $map(n*) = n_result*
  -- if (n_result = $(n + 1))*
dec $map_opt(nat?) : nat?
def $map_opt(n?) = n_result?
  -- if (n_result = $(n + 1))?
"#;
    for det in [false, true] {
        let mut runner = configured(spec(source), det);
        let value_inner = make::nat(runner.arena_mut(), 6u64.into(), Span::default()).unwrap();
        for values in [vec![], vec![value_inner, value_inner]] {
            let len = values.len();
            let value = make::list(
                runner.arena_mut(),
                typ::make::list(typ::make::nat()).node.into(),
                values,
                Span::default(),
            )
            .unwrap();
            let value = runner.context().call_func("map", &[], &[value]).unwrap();
            let values = get::list(runner.arena(), &value).unwrap();
            assert_eq!(values.len(), len);
            assert!(
                values
                    .iter()
                    .all(|value| get::num(runner.arena(), value).unwrap().to_string() == "7")
            );
        }
        for value_opt in [None, Some(value_inner)] {
            let value = make::opt(
                runner.arena_mut(),
                typ::make::opt(typ::make::nat()).node.into(),
                value_opt,
                Span::default(),
            )
            .unwrap();
            let value = runner
                .context()
                .call_func("map_opt", &[], &[value])
                .unwrap();
            let value_opt_output = get::opt(runner.arena(), &value).unwrap();
            assert_eq!(
                value_opt_output.map(|value| get::num(runner.arena(), &value).unwrap().to_string()),
                value_opt.map(|_| "7".to_owned())
            );
        }
    }
}

#[test]
fn prepared_higher_order_parameters_keep_caller_aliases() {
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
    for det in [false, true] {
        let mut runner = configured(spec(source), det);
        let value = make::nat(runner.arena_mut(), 4u64.into(), Span::default()).unwrap();
        let value = runner.context().call_func("entry", &[], &[value]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7");
    }
}

#[test]
fn table_rows_remain_sequential_with_determinism_enabled() {
    let mut spec_pl = function(vec![]);
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &spec_pl[0].node.node else {
        panic!("defined function")
    };
    spec_pl[0].node.node = ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
        id: func.id.clone(),
        params: vec![],
        typ: func.typ.clone(),
        rows: [1, 2]
            .into_iter()
            .map(|num| ast::TableRow {
                exps_input: vec![],
                exp: nat(num),
                block: vec![returning(nat(num))],
            })
            .collect(),
    }));
    for det in [false, true] {
        let mut runner = configured(spec_pl.clone(), det);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
    }
}

#[test]
fn shorthand_bindings_keep_their_scope() {
    let list = |exp| annotated_note_phrase!(node: ast::ExpKind::List(vec![exp]), note: typ::make::list(typ::make::nat()).node, span: Span::default());
    let pattern = ast::Pattern::List(p4spec_rust::lang::il::ast::ListPattern::Fixed(1));
    let subcheck = Box::new(ast::Subcheck::Recurse(typ::make::nat()));
    let case = |exp, guard| {
        instr(ast::InstrKind::Case(ast::CaseInstr {
            exp,
            cases: vec![ast::Case { guard, block: vec![] }],
            dangle: false,
        }))
    };
    let instrs = vec![
        instr(ast::InstrKind::CheckLetSub(ast::CheckLetSubInstr {
            typ: typ::make::nat(),
            subcheck: subcheck.clone(),
            exp_l: variable("n"),
            exp_r: nat(2),
            block: vec![],
        })),
        instr(ast::InstrKind::CheckLetMatch(ast::CheckLetMatchInstr {
            pattern: pattern.clone(),
            exp_l: list(variable("n")),
            exp_r: list(nat(2)),
            block: vec![],
        })),
        instr(ast::InstrKind::OptionGet(ast::OptionGetInstr {
            exp_l: variable("n"),
            exp_r: annotated_note_phrase!(node: ast::ExpKind::Opt(Some(Box::new(nat(2)))), note: typ::make::opt(typ::make::nat()).node, span: Span::default()),
            block: vec![],
        })),
        case(nat(2), ast::Guard::CheckLetSub(typ::make::nat(), subcheck, variable("n"))),
        case(list(nat(2)), ast::Guard::CheckLetMatch(pattern, list(variable("n")))),
    ];
    for det in [false, true] {
        let mut outputs = vec![];
        for instr in &instrs {
            let mut runner = configured(
                function(vec![binding(1), instr.clone(), returning(variable("n"))]),
                det,
            );
            let value = runner.context().call_func("entry", &[], &[]).unwrap();
            outputs.push(get::num(runner.arena(), &value).unwrap().to_string());
        }
        assert_eq!(outputs, vec!["1"; instrs.len()], "det={det}");
    }
}

#[test]
fn prepared_expressions_keep_nested_hints_until_evaluation() {
    use p4spec_rust::{
        interp::pl::context::Context,
        lang::{
            hints::alter::{AlterationHint, Hole},
            pl::annot::Hints,
        },
        runtime::envs::interp::pl::ast_prepared as prepared,
    };

    // An invalid prose hole would fail if execution tried to render the hint
    let hints = Hints { prose: Some(AlterationHint::Hole(Hole::Num(999))), ..Hints::default() };
    let mut exp_inner = variable("n");
    exp_inner.hints = hints.clone();
    let exp_list = annotated_note_phrase!(
        node: ast::ExpKind::List(vec![exp_inner]),
        note: typ::make::list(typ::make::nat()).node,
        span: Span::default(),
        hints: hints.clone(),
    );
    let exp = annotated_note_phrase!(
        node: ast::ExpKind::Len(Box::new(exp_list)),
        note: typ::make::nat().node,
        span: Span::default(),
        hints: hints.clone(),
    );
    let mut instr_bind = binding(7);
    let ast::InstrKind::Let(binding) = &mut instr_bind.node.node else { unreachable!() };
    binding.exp_l.hints = hints.clone();
    binding.exp_r.hints = hints.clone();
    let spec_pl = function(vec![instr_bind, returning(exp)]);
    let global = Global::load(spec_pl.clone()).unwrap();

    let ctx = Context::new(&global);
    let id = p4spec_rust::phrase!(node: "entry".to_owned(), span: Span::default());
    let (_, callable) = ctx.find_func_with_scope(&id).unwrap();
    let prepared::MetaFuncDef::Defined(func) = &callable.def else { panic!("defined function") };
    let prepared::InstrKind::Let(binding) = &func.block[0].node.node else { panic!("binding") };
    assert_eq!(binding.exp_l.hints, hints);
    assert_eq!(binding.exp_r.hints, hints);
    let prepared::InstrKind::Tier(instr) = &func.block[1].node.node else { panic!("return") };
    let prepared::GroupInstr::Return(instr) = &instr.tier else { panic!("return") };
    assert_eq!(instr.exp.hints, hints);
    let prepared::ExpKind::Len(exp_list) = &instr.exp.node.node else { panic!("length") };
    assert_eq!(exp_list.hints, hints);
    let prepared::ExpKind::List(exps) = &exp_list.node.node else { panic!("list") };
    assert_eq!(exps[0].hints, hints);
    let prepared::ExpKind::Id(id_inner) = &exps[0].node.node else { panic!("variable") };
    let prepared::ExpKind::Id(id_bound) = &binding.exp_l.node.node else { panic!("variable") };
    assert_eq!(id_inner.slot, id_bound.slot);

    for det in [false, true] {
        let mut runner = configured(spec_pl.clone(), det);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
    }
}
