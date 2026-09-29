use super::*;
use crate::interp::report::{IntoReport, ReportExt};
use p4spec_rust::interp::shared::backtrack::Failure;
use p4spec_rust::interp::shared::context::{ReadContext, WriteContext};
use p4spec_rust::interp::shared::prepare::Prepare;
use p4spec_rust::{
    lang::{common::prim::num::Number, data::typ},
    note_phrase, phrase,
};

fn exp(num: u64) -> ast::Exp {
    note_phrase!(node: ast::ExpKind::Num(Number::Nat(num.into())), note: typ::make::nat().node, span: Span::default())
}
fn boolean(cond: bool) -> ast::Exp {
    note_phrase!(node: ast::ExpKind::Bool(cond), note: typ::make::bool().node, span: Span::default())
}
fn instr(exp_body: ast::Exp) -> ast::Instr {
    phrase!(node: ast::InstrKind::Return(ast::ReturnInstr { exp: exp_body }), span: Span::default())
}
fn with_block(block: ast::Block, det: bool) -> Runner<SlInterp, BuiltinInterface, NullExtern> {
    let mut spec_sl = spec("dec $entry() : nat\ndef $entry() = 0");
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut spec_sl[0].node else {
        panic!("function")
    };
    func.block = block;
    Runner::new(
        Global::load(spec_sl).unwrap(),
        SlInterp::new(Config::new(false, det, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
        NullExtern,
    )
}
fn message_has(error: &p4spec_rust::diagnostic::Report, code: &str) -> bool {
    error.find_code(code).is_some()
}

#[test]
fn raw_blocks_preserve_order_and_reject_two_identical_successes() {
    let block = vec![instr(exp(1)), instr(exp(1))];
    let mut runner = with_block(block.clone(), false);
    let value = runner.context().call_func("entry", &[], &[]).unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
    let mut runner = with_block(block, true);
    let error = runner
        .context()
        .call_func("entry", &[], &[])
        .unwrap_err()
        .into_report();
    assert!(message_has(&error, "runtime/instruction-nondeterministic"));
}

#[test]
fn case_selection_commits_to_the_first_matching_guard_before_body_fallthrough() {
    let block = vec![
        phrase!(node: ast::InstrKind::Case(ast::CaseInstr {
        exp: boolean(true),
        cases: vec![
            ast::Case { guard: ast::Guard::Bool(true), block: vec![] },
            ast::Case { guard: ast::Guard::Bool(true), block: vec![instr(exp(2))] },
        ], dangle: true,
    }), span: Span::default()),
        instr(exp(9)),
    ];
    for det in [false, true] {
        let mut runner = with_block(block.clone(), det);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "9");
    }
}

#[test]
fn all_case_guard_forms_dispatch_through_expression_semantics() {
    use p4spec_rust::lang::{
        common::prim::bool as bool_op,
        il::ast::{ListPattern, Subcheck},
    };
    let cases = [
        (boolean(false), ast::Guard::Bool(false)),
        (exp(7), ast::Guard::Cmp(ast::CmpOp::Bool(bool_op::CmpOp::Eq), ast::OpTyp::Nat, exp(7))),
        (exp(7), ast::Guard::Sub(typ::make::nat(), Box::new(Subcheck::Recurse(typ::make::nat())))),
        (
            note_phrase!(node: ast::ExpKind::List(vec![]), note: typ::make::list(typ::make::nat()).node, span: Span::default()),
            ast::Guard::Match(ast::Pattern::List(ListPattern::Nil)),
        ),
        (
            exp(7),
            ast::Guard::Mem(
                note_phrase!(node: ast::ExpKind::List(vec![exp(7)]), note: typ::make::list(typ::make::nat()).node, span: Span::default()),
            ),
        ),
    ];
    for (exp_guard, guard) in cases {
        let block = vec![
            phrase!(node: ast::InstrKind::Case(ast::CaseInstr { exp: exp_guard, cases: vec![ast::Case { guard, block: vec![instr(self::exp(5))] }], dangle: false }), span: Span::default()),
        ];
        let mut runner = with_block(block, false);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "5");
    }
}

#[test]
fn function_and_relation_tail_recursion_are_stack_bounded() {
    let source = r#"
var i : int
dec $count(int) : int
def $count(+0) = +0
def $count(i) = $count($(i - 1))
  -- if $(i > 0)
relation Count: int ~> int
  hint(input %0)
rule Count/zero: +0 ~> +0
rule Count/next: i ~> i_result
  -- if $(i > 0)
  -- Count: $(i - 1) ~> i_result
"#;
    // A small thread stack makes accidental Rust recursion fail promptly
    for det in [false, true] {
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(move || {
                let mut runner = runner(source, det);
                let value = make::int(runner.arena_mut(), 20_000.into(), Span::default()).unwrap();
                let value = runner.context().call_func("count", &[], &[value]).unwrap();
                assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "+0");
                let value = make::int(runner.arena_mut(), 20_000.into(), Span::default()).unwrap();
                let values = runner.context().call_rel("Count", &[value]).unwrap();
                assert_eq!(get::num(runner.arena(), &values[0]).unwrap().to_string(), "+0");
            })
            .unwrap()
            .join()
            .unwrap();
    }
}

#[test]
fn table_blocks_remain_sequential_even_with_determinism_enabled() {
    let mut spec_sl = spec("dec $entry() : nat\ndef $entry() = 0");
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = spec_sl.remove(0).node else {
        panic!("function")
    };
    let def = phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Table(ast::TableFunc {
        id: func.id, params: func.params, typ: func.typ, hints: vec![], table_rows: vec![
            ast::TableRow { exps_input: vec![], exp: exp(1), block: vec![instr(exp(1))] },
            ast::TableRow { exps_input: vec![], exp: exp(2), block: vec![instr(exp(2))] },
        ],
    })), span: Span::default());
    let mut runner = Runner::new(
        Global::load(vec![def]).unwrap(),
        SlInterp::new(Config::new(false, true, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
        NullExtern,
    );
    let value = runner.context().call_func("entry", &[], &[]).unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
}

#[test]
fn empty_body_and_wrong_terminal_flow_are_reported() {
    let mut runner = with_block(vec![], false);
    assert!(runner.context().call_func("entry", &[], &[]).is_err());
    let spec_sl =
        spec("var n : nat\nrelation Step: nat ~> nat\n  hint(input %0)\nrule Step/step: n ~> n");
    let ast::DefKind::Rel(ast::RelDef::Defined(rel)) = &spec_sl[1].node else { panic!("relation") };
    let mut runner = with_block(
        vec![
            phrase!(node: ast::InstrKind::Result(ast::ResultInstr { rel_signature: rel.rel_signature.clone(), exps: vec![exp(1)] }), span: Span::default()),
        ],
        false,
    );
    assert!(
        runner
            .context()
            .call_func("entry", &[], &[])
            .unwrap_err()
            .into_report()
            .render()
            .contains("function cannot produce a relation")
    );
}

#[test]
fn optional_and_list_conditions_preserve_empty_iteration_semantics() {
    for (iter, expected) in [(ast::Iter::Opt, "9"), (ast::Iter::List, "5")] {
        let id = phrase!(node: "n".to_owned(), span: Span::default());
        let var = ast::Var { id: id.clone(), typ: typ::make::nat(), iters: vec![] };
        let exp_id = p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id), note: typ::make::nat().node, span: Span::default());
        let exp_l = note_phrase!(node: ast::ExpKind::Iter(Box::new(exp_id), ast::ExpIter { iter, vars: vec![var.clone()] }), note: typ::make::iter(typ::make::nat(), iter).node, span: Span::default());
        let exp_r = note_phrase!(node: if iter == ast::Iter::Opt { ast::ExpKind::Opt(None) } else { ast::ExpKind::List(vec![]) }, note: typ::make::iter(typ::make::nat(), iter).node, span: Span::default());
        let instr_if = phrase!(node: ast::InstrKind::If(ast::IfInstr { exp: boolean(false), iter_exps: vec![ast::ExpIter { iter, vars: vec![var] }], block: vec![instr(exp(5))], dangle: true }), span: Span::default());
        let block = vec![
            phrase!(node: ast::InstrKind::Let(ast::LetInstr { exp_l, exp_r, iter_instrs: vec![], block: vec![instr_if, instr(exp(9))] }), span: Span::default()),
        ];
        let mut runner = with_block(block, false);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), expected);
    }
}

#[test]
fn loading_rejects_duplicate_execution_definitions() {
    for source in [
        "extern syntax opaque",
        "dec $entry() : nat\ndef $entry() = 1",
        "var n : nat\nrelation Step: nat ~> nat\n  hint(input %0)\nrule Step/step: n ~> n",
    ] {
        let mut spec_sl = spec(source);
        let def = spec_sl.last().unwrap().clone();
        spec_sl.push(def);
        assert!(
            Global::load(spec_sl).unwrap_err().into_report().code()
                == Some("runtime/binding-repeated")
        );
    }
}

#[test]
fn type_arguments_shadow_global_type_definitions() {
    let mut spec_sl = spec("dec $ignore<X>(X) : nat\ndef $ignore<X>(X) = 1");
    let id = phrase!(node: "X".to_owned(), span: Span::default());
    spec_sl.push(phrase!(node: ast::DefKind::Typ(ast::TypDef::Extern(ast::ExternTyp { id, hints: vec![] })), span: Span::default()));
    let mut runner = Runner::new(
        Global::load(spec_sl).unwrap(),
        SlInterp::new(Config::new(false, false, true)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Sl(Vec::new())),
        NullExtern,
    );
    let value = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
    let value = runner
        .context()
        .call_func("ignore", &[typ::make::bool()], &[value])
        .unwrap();
    assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "1");
}

#[test]
fn case_comparison_rhs_reads_the_enclosing_context() {
    use p4spec_rust::lang::common::prim::bool as bool_op;
    let exp_r: ast::Exp = note_phrase!(
        node: ast::ExpKind::Id(phrase!(node: "x".to_owned(), span: Span::default())),
        note: typ::make::nat().node, span: Span::default(),
    );
    let block_case = vec![phrase!(node: ast::InstrKind::Case(ast::CaseInstr {
        exp: exp(7),
        cases: vec![ast::Case {
            guard: ast::Guard::Cmp(ast::CmpOp::Bool(bool_op::CmpOp::Eq), ast::OpTyp::Nat, exp_r.clone()),
            block: vec![instr(exp_r.clone())],
        }],
        dangle: false,
    }), span: Span::default())];
    let block = vec![phrase!(node: ast::InstrKind::Let(ast::LetInstr {
        exp_l: exp_r, exp_r: exp(7), iter_instrs: vec![], block: block_case,
    }), span: Span::default())];
    for det in [false, true] {
        let mut runner = with_block(block.clone(), det);
        let value = runner.context().call_func("entry", &[], &[]).unwrap();
        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), "7");
    }
}

#[test]
fn optional_condition_preserves_remaining_iterator_order_and_outer_bindings() {
    use p4spec_rust::{
        interp::sl::{context::Context, eval::instr::eval_block, flow::Flow},
        lang::common::notation::mixfix::Mixfix,
    };
    for det in [false, true] {
        for (cond, expected) in [(true, "5"), (false, "9")] {
            let mut runner = with_block(vec![], det);
            let global = Global::load(spec(
                "var b : bool\nrelation True: bool ~> bool\n  hint(input %0)\nrule True/true: b ~> b\n  -- if b",
            ))
            .unwrap();
            let id = phrase!(node: "b".to_owned(), span: Span::default());
            let value_outer = make::bool(runner.arena_mut(), false, Span::default()).unwrap();
            let value_true = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
            let value_cond = make::bool(runner.arena_mut(), cond, Span::default()).unwrap();
            let typ_list = typ::make::list(typ::make::bool());
            let mut values = Vec::new();
            for value in [value_true, value_cond] {
                values.push(
                    make::list(
                        runner.arena_mut(),
                        typ_list.node.clone().into(),
                        vec![value],
                        Span::default(),
                    )
                    .unwrap(),
                );
            }
            let value = make::list(
                runner.arena_mut(),
                typ::make::list(typ_list).node.into(),
                values,
                Span::default(),
            )
            .unwrap();
            let var = ast::Var { id: id.clone(), typ: typ::make::bool(), iters: vec![] };
            let mut var_list = var.clone();
            var_list.iters.push(ast::Iter::List);
            let exp = p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id.clone()), note: typ::make::bool().node, span: Span::default());
            let iter_exps = vec![
                ast::ExpIter { iter: ast::Iter::List, vars: vec![var] },
                ast::ExpIter { iter: ast::Iter::List, vars: vec![var_list] },
                ast::ExpIter { iter: ast::Iter::Opt, vars: vec![] },
            ];
            for instr_cond in [
                ast::InstrKind::If(ast::IfInstr {
                    exp: exp.clone(),
                    iter_exps: iter_exps.clone(),
                    block: vec![instr(self::exp(5))],
                    dangle: true,
                }),
                ast::InstrKind::Hold(ast::HoldInstr {
                    id: phrase!(node: "True".to_owned(), span: Span::default()),
                    not_exp: Mixfix::Arg(exp),
                    iter_exps,
                    hold_case: ast::HoldCase::Hold(vec![instr(self::exp(5))], true),
                }),
            ] {
                let block = vec![phrase!(node: instr_cond, span: Span::default())];
                let mut layout =
                    p4spec_rust::runtime::envs::interp::shared::frame::FrameLayout::default();
                let block = block.prepare(&mut layout);
                let slot = layout.resolve_var(ast::Var {
                    id: id.clone(),
                    typ: typ::make::bool(),
                    iters: vec![],
                });
                let slot_nested = layout.resolve_var(ast::Var {
                    id: id.clone(),
                    typ: typ::make::bool(),
                    iters: vec![ast::Iter::List; 2],
                });
                let mut ctx =
                    Context::new(&global).localize_with_layout(&std::rc::Rc::new(layout.clone()));
                ctx.add_value_at_slot(slot.slot, value_outer);
                ctx.add_value_at_slot(slot_nested.slot, value);
                let flow = eval_block(
                    &mut runner.context(),
                    std::borrow::Cow::Borrowed(&ctx),
                    &block,
                    false,
                )
                .map_err(Failure::into_report)
                .unwrap();
                match flow {
                    Flow::Return(value) => {
                        assert_eq!(get::num(runner.arena(), &value).unwrap().to_string(), expected)
                    }
                    Flow::Cont(_) => assert_eq!(expected, "9"),
                    flow => panic!("unexpected flow: {flow:?}"),
                }
                assert_eq!(
                    *ctx.find_value_at_slot(
                        layout
                            .resolve_var(p4spec_rust::lang::il::ast::Var {
                                id: id.clone(),
                                typ: p4spec_rust::lang::data::typ::make::bool(),
                                iters: vec![]
                            })
                            .slot
                    )
                    .unwrap(),
                    value_outer
                );
            }
        }
    }
}
