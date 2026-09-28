use p4spec_rust::{
    lang::{
        al::ast as al,
        common::{
            notation::mixfix::Mixfix,
            source::{Position, Span},
        },
        hints::input::InputHint,
        sl::ast as sl,
    },
    pass::structure::convert,
};

fn span(int_line: usize) -> Span {
    let pos = Position::new("structure.watsup", int_line, 0);
    Span::new(pos.clone(), pos)
}

fn id(text: &str, int_line: usize) -> al::Id {
    p4spec_rust::phrase! {node: text.to_owned(), span: span(int_line)}
}

fn typ(int_line: usize) -> al::Typ {
    p4spec_rust::phrase! {node: al::TypKind::Bool, span: span(int_line)}
}

fn id_exp(text: &str, int_line: usize) -> al::Exp {
    p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id(text, int_line)), note: al::TypKind::Bool, span: span(int_line))
}

fn boolean(value: bool, int_line: usize) -> al::Exp {
    p4spec_rust::note_phrase! {node: al::ExpKind::Bool(value), note: al::TypKind::Bool, span: span(int_line)}
}

fn param(int_line: usize) -> al::Param {
    p4spec_rust::phrase! {node: al::ParamKind::Exp(typ(int_line)), span: span(int_line)}
}

fn clause(prems: Vec<al::Prem>, int_line: usize) -> al::Clause {
    p4spec_rust::phrase! {node: al::ClauseKind {args: vec![p4spec_rust::phrase! {node: al::ArgKind::Exp(Box::new(id_exp("x", int_line))), span: span(int_line)}], exp: id_exp("x", int_line + 1), prems}, span: span(int_line)}
}

fn function(clauses: Vec<al::Clause>, else_clause: Option<al::Clause>) -> al::Def {
    p4spec_rust::phrase! {node: al::DefKind::MetaFunc(al::MetaFuncDef::Defined(Box::new(al::DefinedFunc {id: id("f", 1), tparams: vec![], params: vec![param(2)], typ: typ(3), clauses, else_clause, hints: vec![]}))), span: span(1)}
}

fn function_sl(def_sl: &sl::Def) -> &sl::DefinedFunc {
    let def_kind_sl = &def_sl.node;
    let sl::DefKind::MetaFunc(def_func_sl) = def_kind_sl else { panic!("function") };
    let sl::MetaFuncDef::Defined(def_func_sl) = def_func_sl else { panic!("defined function") };
    def_func_sl
}

fn if_prem(int_line: usize) -> al::Prem {
    p4spec_rust::phrase! {node: al::PremKind::If(al::IfPrem {exp: id_exp("x", int_line)}), span: span(int_line)}
}

#[test]
fn test_nullary_relation_defaults_survive_structuring() {
    use p4spec_rust::{
        frontend::parse::parse_text,
        pass::{algo, elaborate},
    };

    let spec_el = parse_text(
        "nullary.watsup".into(),
        "extern relation External: _EXTERNAL\nrelation Empty: _EMPTY\nrelation Ready: _READY\nrule Ready: _READY\n",
    ).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    for without_rule_groups in [false, true] {
        let spec_sl = convert(spec_al.clone(), without_rule_groups).unwrap();
        assert_eq!(spec_sl.len(), 3);
        for def_sl in &spec_sl {
            let sl::DefKind::Rel(def_rel_sl) = &def_sl.node else { panic!("relation") };
            match def_rel_sl {
                sl::RelDef::Extern(def_rel_sl) => assert!(def_rel_sl.exps_input.is_empty()),
                sl::RelDef::Defined(def_rel_sl) => assert!(def_rel_sl.exps_input.is_empty()),
            }
        }
    }
}

#[test]
fn test_empty_function_generates_distinct_inputs_and_preserves_spans() {
    let mut def_al = function(vec![], None);
    let def_kind_al = &mut def_al.node;
    let al::DefKind::MetaFunc(def_func_kind_al) = def_kind_al else { unreachable!() };
    let al::MetaFuncDef::Defined(def_func_al) = def_func_kind_al else { unreachable!() };
    def_func_al.params.push(param(4));
    let spec_sl = convert(vec![def_al], true).unwrap();
    let def_func_sl = function_sl(&spec_sl[0]);
    assert_eq!(spec_sl[0].span, span(1));
    assert!(def_func_sl.block.is_empty());
    assert_eq!(def_func_sl.block_else, None);
    let sl::ParamKind::Exp(_, exp_a) = &def_func_sl.params[0].node else { panic!("input") };
    let sl::ParamKind::Exp(_, exp_b) = &def_func_sl.params[1].node else { panic!("input") };
    assert_ne!(exp_a.node, exp_b.node);
    assert_eq!(exp_a.span, span(2));
    assert_eq!(exp_b.span, span(4));
}

#[test]
fn test_explicit_fallback_changes_main_fallthrough_and_preserves_return() {
    for with_else in [false, true] {
        let def_al =
            function(vec![clause(vec![if_prem(7)], 5)], with_else.then(|| clause(vec![], 9)));
        let spec_sl = convert(vec![def_al], true).unwrap();
        let def_func_sl = function_sl(&spec_sl[0]);
        let sl::InstrKind::If(instr_if) = &def_func_sl.block[0].node else { panic!("if") };
        assert_eq!(instr_if.dangle, !with_else);
        assert_eq!(def_func_sl.block[0].span, span(7));
        assert_eq!(instr_if.block[0].span, span(6));
        assert_eq!(def_func_sl.block_else.is_some(), with_else);
    }
}

#[test]
fn test_relation_groups_and_result_signatures_follow_group_mode() {
    let exp_input = id_exp("x", 3);
    let exp_output = boolean(true, 8);
    let rule_match = al::RuleMatch {
        exps_signature: vec![exp_input.clone(), exp_output.clone()],
        exps_input: vec![exp_input],
        prems: vec![],
    };
    let rule_path =
        al::RulePath { id: id("path", 7), prems: vec![], exps_output: vec![exp_output.clone()] };
    let rule_group = p4spec_rust::phrase! {node: al::RuleGroupKind {id: id("group", 4), rule_match, rule_paths: vec![rule_path]}, span: span(4)};
    let not_typ = p4spec_rust::phrase! {node: Mixfix::Seq(vec![Mixfix::Arg(typ(2)), Mixfix::Arg(typ(2))]), span: span(2)};
    let def_al = p4spec_rust::phrase! {node: al::DefKind::Rel(al::RelDef::Defined(Box::new(al::DefinedRel {id: id("r", 1), not_typ: not_typ.clone(), input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]), rule_groups: vec![rule_group], else_group: None, hints: vec![]}))), span: span(1)};
    for without_rule_groups in [false, true] {
        let spec_sl = convert(vec![def_al.clone()], without_rule_groups).unwrap();
        let def_kind_sl = &spec_sl[0].node;
        let sl::DefKind::Rel(def_rel_kind_sl) = def_kind_sl else { panic!("relation") };
        let sl::RelDef::Defined(def_rel_sl) = def_rel_kind_sl else { panic!("defined relation") };
        let instr_result = if without_rule_groups {
            &def_rel_sl.block[0]
        } else {
            let sl::InstrKind::Group(instr_group) = &def_rel_sl.block[0].node else {
                panic!("group retained")
            };
            assert_eq!(instr_group.id, id("group", 4));
            assert_eq!(def_rel_sl.block[0].span, span(4));
            &instr_group.block[0]
        };
        let sl::InstrKind::Result(instr_result_kind) = &instr_result.node else { panic!("result") };
        assert_eq!(instr_result_kind.exps, vec![exp_output.clone()]);
        assert_eq!(instr_result_kind.rel_signature.not_typ, not_typ);
        assert_eq!(instr_result.span, span(8));
    }
}

#[test]
fn test_nested_iterators_are_internalized_inside_out() {
    let prem = if_prem(7);
    let prem = p4spec_rust::phrase! {node: al::PremKind::Iter(al::IterPrem {prem: Box::new(prem), prem_iter: al::PremIter {iter: al::Iter::Opt, vars_bound: vec![], vars_bind: vec![]}}), span: span(8)};
    let prem = p4spec_rust::phrase! {node: al::PremKind::Iter(al::IterPrem {prem: Box::new(prem), prem_iter: al::PremIter {iter: al::Iter::List, vars_bound: vec![], vars_bind: vec![]}}), span: span(9)};
    let spec_sl = convert(vec![function(vec![clause(vec![prem], 5)], None)], true).unwrap();
    let def_func_sl = function_sl(&spec_sl[0]);
    let sl::InstrKind::If(instr_if) = &def_func_sl.block[0].node else { panic!("if") };
    assert_eq!(
        instr_if.iter_exps,
        vec![
            al::ExpIter { iter: al::Iter::Opt, vars: vec![] },
            al::ExpIter { iter: al::Iter::List, vars: vec![] }
        ]
    );
    assert_eq!(def_func_sl.block[0].span, span(7));
}

#[test]
fn test_table_rows_keep_signature_output_pairing_and_disable_fallthrough() {
    let table_rows = [true, false].into_iter().enumerate().map(|(idx, value)| {
        let int_line = 10 + idx;
        let clause_al = clause(vec![if_prem(7)], int_line);
        let al::ClauseKind {args, prems, ..} = clause_al.node;
        p4spec_rust::phrase! {node: al::TableRowKind {exps_signature: vec![boolean(value, int_line)], args, exp: boolean(!value, int_line), prems}, span: span(int_line)}
    }).collect();
    let def_al = p4spec_rust::phrase! {node: al::DefKind::MetaFunc(al::MetaFuncDef::Table(al::TableFunc {id: id("t", 1), params: vec![param(2)], typ: typ(3), table_rows, hints: vec![]})), span: span(1)};
    let spec_sl = convert(vec![def_al], true).unwrap();
    let def_kind_sl = &spec_sl[0].node;
    let sl::DefKind::MetaFunc(def_func_kind_sl) = def_kind_sl else { panic!("function") };
    let sl::MetaFuncDef::Table(def_table_sl) = def_func_kind_sl else { panic!("table") };
    assert_eq!(def_table_sl.table_rows.len(), 2);
    for (idx, value) in [true, false].into_iter().enumerate() {
        let table_row_sl = &def_table_sl.table_rows[idx];
        assert_eq!(table_row_sl.exps_input, vec![boolean(value, 10 + idx)]);
        assert_eq!(table_row_sl.exp, boolean(!value, 10 + idx));
        let sl::InstrKind::If(instr_if) = &table_row_sl.block[0].node else { panic!("if") };
        assert!(!instr_if.dangle);
    }
}

#[test]
fn test_native_pipeline_preserves_groups_fallbacks_and_repeatability() {
    use p4spec_rust::{
        frontend::parse::parse_files,
        lang::traits::print::Print,
        pass::{algo, elaborate},
    };
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/structure/definitions.watsup");
    let spec_el = parse_files([&path]).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    for without_rule_groups in [false, true] {
        let spec_sl = convert(spec_al.clone(), without_rule_groups).unwrap();
        let spec_repeat_sl = convert(spec_al.clone(), without_rule_groups).unwrap();
        assert_eq!(spec_sl, spec_repeat_sl);
        let text_sl = Print::to_string(&spec_sl);
        assert!(!text_sl.is_empty());
        let mut num_groups = 0;
        let mut num_fallbacks = 0;
        for def_sl in &spec_sl {
            let def_kind_sl = &def_sl.node;
            if let sl::DefKind::Rel(def_rel_sl) = def_kind_sl
                && let sl::RelDef::Defined(def_rel_sl) = def_rel_sl
            {
                if def_rel_sl.id.node == "Empty" {
                    assert!(def_rel_sl.block.is_empty());
                    assert_eq!(def_rel_sl.exps_input.len(), 1);
                    assert_eq!(def_rel_sl.block_else, None);
                }
                num_fallbacks += usize::from(def_rel_sl.block_else.is_some());
                num_groups += count_groups(&def_rel_sl.block);
                if let Some(block_else) = &def_rel_sl.block_else {
                    num_groups += count_groups(block_else);
                }
            }
        }
        assert_eq!(num_fallbacks, 1);
        assert_eq!(num_groups, if without_rule_groups { 0 } else { 2 });
        assert_eq!(spec_sl.len(), spec_al.len());
    }
}

fn count_groups(block: &sl::Block) -> usize {
    block
        .iter()
        .map(|instr| count_instr_groups(&instr.node))
        .sum()
}

fn count_instr_groups(instr_kind: &sl::InstrKind) -> usize {
    match instr_kind {
        sl::InstrKind::Group(instr) => 1 + count_groups(&instr.block),
        sl::InstrKind::If(instr) => count_groups(&instr.block),
        sl::InstrKind::Case(instr) => instr
            .cases
            .iter()
            .map(|case| count_groups(&case.block))
            .sum(),
        sl::InstrKind::Let(instr) => count_groups(&instr.block),
        sl::InstrKind::Rule(instr) => count_groups(&instr.block),
        sl::InstrKind::Debug(instr) => count_instr_groups(&instr.instr.node),
        sl::InstrKind::Hold(instr) => count_hold_groups(&instr.hold_case),
        sl::InstrKind::Result(_) | sl::InstrKind::Return(_) => 0,
    }
}

fn count_hold_groups(hold_case: &sl::HoldCase) -> usize {
    match hold_case {
        sl::HoldCase::Both(block_a, block_b) => count_groups(block_a) + count_groups(block_b),
        sl::HoldCase::Hold(block, _) | sl::HoldCase::NotHold(block, _) => count_groups(block),
    }
}

#[test]
fn test_external_relation_fresh_inputs_follow_unsorted_hint_order() {
    let typ_text = p4spec_rust::phrase! {node: al::TypKind::Text, span: span(3)};
    let not_typ = p4spec_rust::phrase! {node: Mixfix::Seq(vec![Mixfix::Arg(typ(2)), Mixfix::Arg(typ_text)]), span: span(2)};
    let def_al = p4spec_rust::phrase! {node: al::DefKind::Rel(al::RelDef::Extern(Box::new(al::ExternRel {id: id("r", 1), not_typ, input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 1, span: Default::default()), p4spec_rust::phrase!(node: 0, span: Default::default())]), hints: vec![]}))), span: span(1)};
    let spec_sl = convert(vec![def_al], true).unwrap();
    let def_kind_sl = &spec_sl[0].node;
    let sl::DefKind::Rel(def_rel_sl) = def_kind_sl else { panic!("relation") };
    let sl::RelDef::Extern(def_rel_sl) = def_rel_sl else { panic!("extern relation") };
    assert_eq!(*def_rel_sl.exps_input[0].note, al::TypKind::Text);
    assert_eq!(*def_rel_sl.exps_input[1].note, al::TypKind::Bool);
    assert_eq!(def_rel_sl.exps_input[0].span, span(3));
    assert_eq!(def_rel_sl.exps_input[1].span, span(2));
}

#[test]
fn test_higher_order_parameters_have_independent_freshness_scope() {
    let mut def_al = function(vec![], None);
    let def_kind_al = &mut def_al.node;
    let al::DefKind::MetaFunc(def_func_kind_al) = def_kind_al else { unreachable!() };
    let al::MetaFuncDef::Defined(def_func_al) = def_func_kind_al else { unreachable!() };
    let param_def = p4spec_rust::phrase! {node: al::ParamKind::Def(id("g", 4), vec![id("T", 4)], vec![param(5), param(6)], typ(4)), span: span(4)};
    def_func_al.params.extend([param_def, param(7)]);
    let spec_sl = convert(vec![def_al], true).unwrap();
    let def_func_sl = function_sl(&spec_sl[0]);
    let sl::ParamKind::Exp(_, exp_a) = &def_func_sl.params[0].node else { panic!("input") };
    let sl::ParamKind::Def(id_def, tparams, params_inner, _) = &def_func_sl.params[1].node else {
        panic!("higher-order input")
    };
    let sl::ParamKind::Exp(_, exp_b) = &def_func_sl.params[2].node else { panic!("input") };
    let sl::ParamKind::Exp(_, exp_inner_a) = &params_inner[0].node else { panic!("input") };
    let sl::ParamKind::Exp(_, exp_inner_b) = &params_inner[1].node else { panic!("input") };
    use p4spec_rust::lang::traits::eq::SyntaxEq;
    assert!(exp_a.syntax_eq(exp_inner_a));
    assert!(exp_b.syntax_eq(exp_inner_b));
    assert!(!exp_a.syntax_eq(exp_b));
    assert_eq!(id_def, &id("g", 4));
    assert_eq!(tparams, &vec![id("T", 4)]);
    assert_eq!(exp_inner_a.span, span(5));
    assert_eq!(exp_inner_b.span, span(6));
}

#[test]
fn test_higher_order_argument_identity_ignores_spans() {
    let mut clause_al = clause(vec![], 5);
    clause_al.node.args =
        vec![p4spec_rust::phrase! {node: al::ArgKind::Def(id("g", 6)), span: span(6)}];
    let mut def_al = function(vec![clause_al], None);
    let al::DefKind::MetaFunc(al::MetaFuncDef::Defined(def_func_al)) = &mut def_al.node else {
        panic!("defined function")
    };
    def_func_al.params = vec![
        p4spec_rust::phrase! {node: al::ParamKind::Def(id("g", 2), vec![], vec![param(3)], typ(2)), span: span(2)},
    ];
    let spec_sl = convert(vec![def_al], true).unwrap();
    let def_func_sl = function_sl(&spec_sl[0]);
    let sl::ParamKind::Def(id_def, _, params, _) = &def_func_sl.params[0].node else {
        panic!("higher-order parameter")
    };
    assert_eq!(id_def, &id("g", 2));
    assert_eq!(params[0].span, span(3));
}

#[test]
fn test_debug_continuation_preserves_binding_rule_and_hold_payloads() {
    let prem_iter = al::PremIter {
        iter: al::Iter::List,
        vars_bound: vec![al::Var { id: id("x", 8), typ: typ(8), iters: vec![] }],
        vars_bind: vec![al::Var { id: id("y", 8), typ: typ(8), iters: vec![] }],
    };
    let prem_let = p4spec_rust::phrase! {node: al::PremKind::Let(al::LetPrem {exp_l: id_exp("y", 8), exp_r: id_exp("x", 8)}), span: span(8)};
    let prem_let = p4spec_rust::phrase! {node: al::PremKind::Iter(al::IterPrem {prem: Box::new(prem_let), prem_iter: prem_iter.clone()}), span: span(9)};
    let prems = vec![
        p4spec_rust::phrase! {node: al::PremKind::Debug(al::DebugPrem {exp: boolean(true, 7)}), span: span(7)},
        prem_let,
        p4spec_rust::phrase! {node: al::PremKind::Rule(al::RulePrem {id: id("r", 10), not_exp: Mixfix::Arg(id_exp("x", 10)), input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())])}), span: span(10)},
        p4spec_rust::phrase! {node: al::PremKind::IfHold(al::IfHoldPrem {id: id("r", 11), not_exp: Mixfix::Arg(id_exp("x", 11))}), span: span(11)},
        p4spec_rust::phrase! {node: al::PremKind::IfNotHold(al::IfNotHoldPrem {id: id("s", 12), not_exp: Mixfix::Arg(id_exp("x", 12))}), span: span(12)},
    ];
    let spec_sl = convert(vec![function(vec![clause(prems, 5)], None)], true).unwrap();
    let def_func_sl = function_sl(&spec_sl[0]);
    let sl::InstrKind::Debug(instr_debug) = &def_func_sl.block[0].node else { panic!("debug") };
    assert_eq!(instr_debug.exp, boolean(true, 7));
    assert_eq!(instr_debug.instr.span, span(8));
    let sl::InstrKind::Let(instr_let) = &instr_debug.instr.node else { panic!("let") };
    assert_eq!(instr_let.iter_instrs, vec![prem_iter]);
    assert_eq!(instr_let.exp_l, id_exp("y", 8));
    assert_eq!(instr_let.exp_r, id_exp("x", 8));
    let sl::InstrKind::Rule(instr_rule) = &instr_let.block[0].node else { panic!("rule") };
    assert_eq!(instr_rule.id, id("r", 10));
    assert_eq!(
        instr_rule.input_hint,
        InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())])
    );
    let sl::InstrKind::Hold(instr_hold) = &instr_rule.block[0].node else { panic!("hold") };
    let sl::HoldCase::Hold(block_hold, dangle) = &instr_hold.hold_case else {
        panic!("hold branch")
    };
    assert!(*dangle);
    let sl::InstrKind::Hold(instr_not_hold) = &block_hold[0].node else { panic!("not hold") };
    let sl::HoldCase::NotHold(block_not_hold, dangle) = &instr_not_hold.hold_case else {
        panic!("not-hold branch")
    };
    assert!(*dangle);
    assert_eq!(block_not_hold[0].span, span(6));
}

#[test]
fn test_structuring_type_failures_remain_reports() {
    use p4spec_rust::{
        frontend::parse::parse_files,
        pass::{algo, elaborate},
    };
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/structure/generic-subtype.watsup");
    let spec_el = parse_files([&path]).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    for without_rule_groups in [false, true] {
        let error = convert(spec_al.clone(), without_rule_groups).unwrap_err();
        let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &error.kind else {
            panic!("structuring diagnostic")
        };
        assert_eq!(diagnostic.source, "structure");
        assert_eq!(diagnostic.code.as_deref(), Some("structure/type-operation-invalid"));
        assert_eq!(diagnostic.severity, p4spec_rust::diagnostic::Severity::Error);
        assert_eq!(diagnostic.labels.len(), 1);
        assert_eq!(diagnostic.labels[0].style, p4spec_rust::diagnostic::LabelStyle::Primary);
        assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), path.to_str().unwrap());
        assert_eq!(diagnostic.labels[0].span.left.line, 4);
        assert!(error.children.is_empty());
    }
}

fn structure_fixture(name: &str) {
    use p4spec_rust::{
        frontend::parse::parse_files,
        pass::{algo, elaborate},
    };
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/structure")
        .join(format!("{name}.watsup"));
    let spec_el = parse_files([&path]).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    for without_rule_groups in [false, true] {
        convert(spec_al.clone(), without_rule_groups).unwrap();
    }
}

#[test]
fn test_crossed_input_positions_structure_successfully() {
    for name in [
        "crossed-inputs",
        "crossed-inputs-otherwise",
        "crossed-relation-inputs",
        "crossed-relation-inputs-otherwise",
    ] {
        structure_fixture(name);
    }
}

#[test]
fn test_crossed_nested_inputs_structure_successfully() {
    for name in ["crossed-nested-inputs", "crossed-nested-inputs-otherwise"] {
        structure_fixture(name);
    }
}

#[test]
fn test_total_case_extension_keeps_original_branches() {
    structure_fixture("total-list");
}
