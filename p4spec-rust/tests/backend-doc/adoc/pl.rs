use p4spec_rust::{
    backend_doc::adoc::pl::{
        self as adoc,
        doc::{doc::Subject, serialize::subject_name},
        render_def, render_spec,
    },
    lang::{
        common::{
            notation::{atom::Atom, mixfix::Mixfix},
            prim::{
                bool::UnOp as BoolUnOp,
                num::{BinOp as NumBinOp, Natural, Number},
            },
            source::Span,
        },
        data::typ,
        hints::{
            alter::{AlterationHint, Hole},
            input::InputHint,
        },
        pl::{annot::Hints, ast as pl},
    },
};

fn id(name: &str) -> pl::Id {
    p4spec_rust::phrase! { node: name.to_owned(), span: Span::default() }
}

fn exp_bool(value: bool) -> pl::Exp {
    p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Bool(value),
        note: pl::TypKind::Bool,
        span: Span::default(),
    }
}

fn exp_id(name: &str) -> pl::Exp {
    p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Id(id(name)),
        note: pl::TypKind::Bool,
        span: Span::default(),
    }
}

fn exp_nat(value: u64) -> pl::Exp {
    p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Num(Number::Nat(Natural::from(value))),
        note: pl::TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Nat),
        span: Span::default(),
    }
}

fn prose_hint(text_l: &str, hole: usize, text_r: &str) -> AlterationHint {
    AlterationHint::Seq(
        (!text_l.is_empty())
            .then(|| AlterationHint::Text(text_l.to_owned()))
            .into_iter()
            .chain(std::iter::once(AlterationHint::Hole(Hole::Num(hole))))
            .chain((!text_r.is_empty()).then(|| AlterationHint::Text(text_r.to_owned())))
            .collect(),
    )
}

fn return_exp_instr(
    exp: pl::Exp,
    fallthrough: Option<pl::Fallthrough>,
) -> pl::Instr<pl::GroupInstr> {
    p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Tier(pl::TierInstr {
            tier: pl::GroupInstr::Return(pl::ReturnInstr { exp }),
        }),
        note: fallthrough,
        span: Span::default(),
    }
}

fn return_instr(value: bool) -> pl::Instr<pl::GroupInstr> {
    return_exp_instr(exp_bool(value), Some(pl::Fallthrough::Fail))
}

fn defined_func_with(name: &str, params: Vec<pl::Param>, block: pl::GroupBlock) -> pl::Def {
    p4spec_rust::annotated! {
        node: p4spec_rust::phrase! {
            node: pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(pl::DefinedFunc {
                id: id(name),
                tparams: Vec::new(),
                params,
                typ: typ::make::bool(),
                block,
                block_else_opt: None,
            })),
            span: Span::default(),
        },
        hints: Default::default(),
    }
}

fn defined_func(name: &str, block: pl::GroupBlock) -> pl::Def {
    defined_func_with(name, Vec::new(), block)
}

fn backtrack_instr(blocks: Vec<pl::GroupBlock>) -> pl::Instr<pl::GroupInstr> {
    p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Tier(pl::TierInstr {
            tier: pl::GroupInstr::Backtrack(pl::BacktrackInstr { blocks }),
        }),
        note: None,
        span: Span::default(),
    }
}

#[test]
fn test_defined_function_renders_prose_body() {
    let def = defined_func("enabled", vec![return_instr(true)]);

    assert_eq!(
        render_def(&subject_name, &def),
        Some("xref:enabled[$enabled]\n\n return ``true``.".to_owned()),
    );
}

#[test]
fn test_function_hints_substitute_parameters_and_negative_calls() {
    let param = p4spec_rust::phrase! {
        node: pl::ParamKind::Exp(typ::make::bool(), Box::new(exp_id("flag"))),
        span: Span::default(),
    };
    let mut exp_call = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Call(
            id("enabled"),
            Vec::new(),
            vec![p4spec_rust::phrase! {
                node: pl::ArgKind::Exp(Box::new(exp_id("flag"))),
                span: Span::default(),
            }],
        ),
        note: pl::TypKind::Bool,
        span: Span::default(),
    };
    exp_call.hints.prose_false = Some(prose_hint("", 0, "is disabled"));
    let exp_not = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Un(pl::UnOp::Bool(BoolUnOp::Not), pl::OpTyp::Bool, Box::new(exp_call)),
        note: pl::TypKind::Bool,
        span: Span::default(),
    };
    let mut def = defined_func_with(
        "check",
        vec![param],
        vec![return_exp_instr(exp_not, Some(pl::Fallthrough::Fail))],
    );
    def.hints.prose_in = Some(prose_hint("checking whether", 0, ""));

    assert_eq!(
        render_def(&subject_name, &def).unwrap(),
        concat!(
            "xref:check[Checking whether ``flag``]",
            "\n\n",
            ". Return xref:enabled[``flag`` is disabled].",
            "+++<sub class=\"bk-mark\">[FAIL]</sub>+++",
        ),
    );
}

#[test]
fn test_relation_math_title_preserves_input_and_output_positions() {
    let atom_colon = p4spec_rust::phrase! { node: Atom::Colon, span: Span::default() };
    let signature = pl::RelSignature {
        not_typ: p4spec_rust::phrase! {
            node: Mixfix::Seq(vec![
                Mixfix::Arg(typ::make::bool()),
                Mixfix::Atom(atom_colon),
                Mixfix::Arg(typ::make::bool()),
            ]),
            span: Span::default(),
        },
        input_hint: InputHint::new(vec![p4spec_rust::phrase! { node: 0, span: Span::default() }]),
    };
    let def = p4spec_rust::annotated! {
        node: p4spec_rust::phrase! {
            node: pl::DefKind::Rel(pl::RelDef::Extern(pl::ExternRel {
                id: id("Check"),
                rel_signature: signature,
                exps_input: vec![exp_id("input")],
            })),
            span: Span::default(),
        },
        hints: Hints::default(),
    };

    assert_eq!(
        render_def(&subject_name, &def).unwrap(),
        "xref:Check[Check: ``input`` ``+:+`` ``%``]"
    );
}

#[test]
fn test_destruct_field_names_render_in_source_order() {
    let destruct = p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Destruct(pl::DestructInstr {
            bindings: vec![
                (Some("key".to_owned()), exp_id("k")),
                (Some("value".to_owned()), exp_id("v")),
                (None, exp_id("ignored")),
            ],
            exp: exp_id("entry"),
        }),
        note: Some(pl::Fallthrough::Fail),
        span: Span::default(),
    };
    let def = defined_func("fields", vec![destruct, return_instr(true)]);

    assert!(render_def(&subject_name, &def).unwrap().contains(
        ". Let ``k`` and ``v`` be the key and the value of ``entry``.+++<sub class=\"bk-mark\">[FAIL]</sub>+++"
    ));
}

#[test]
fn test_nested_backtracking_uses_local_arm_labels_and_fresh_block_counters() {
    let nested = backtrack_instr(vec![
        vec![return_exp_instr(exp_bool(false), Some(pl::Fallthrough::Next))],
        vec![return_instr(true)],
    ]);
    let outer = backtrack_instr(vec![
        vec![nested, return_exp_instr(exp_bool(false), Some(pl::Fallthrough::Next))],
        vec![return_instr(true)],
    ]);
    let def = defined_func("choice", vec![outer]);
    let rendered = render_def(&subject_name, &def).unwrap();

    assert!(rendered.contains("id=\"bk-choice-1-arm-1\""));
    assert!(rendered.contains("id=\"bk-choice-2-arm-1\""));
    assert!(rendered.contains("href=\"#bk-choice-2-arm-2\">→ b</a>"), "{rendered}",);
    assert!(rendered.contains("href=\"#bk-choice-1-arm-2\">→ 2</a>"), "{rendered}",);
}

#[test]
fn test_custom_function_anchor_is_used_by_fragment_api() {
    let def = defined_func("enabled", vec![return_instr(true)]);
    let anchor = |subject: &Subject| match subject {
        Subject::Function(id) => Some(format!("function-{id}")),
        Subject::Relation(id) => Some(format!("relation-{id}")),
    };

    assert!(
        render_def(&anchor, &def)
            .unwrap()
            .starts_with("xref:function-enabled[$enabled]")
    );
}

#[test]
fn test_full_render_is_deterministic_and_fragments_reset_counters() {
    let def = defined_func(
        "choice",
        vec![backtrack_instr(vec![vec![return_instr(false)], vec![return_instr(true)]])],
    );
    let spec = vec![def.clone(), def];
    let rendered_a = render_spec(&spec);
    let rendered_b = render_spec(&spec);

    assert_eq!(rendered_a, rendered_b);
    assert!(rendered_a.contains("id=\"bk-choice-1-arm-1\""));
    assert!(rendered_a.contains("id=\"bk-choice-2-arm-1\""));
}

#[test]
fn test_numeric_addition_uses_the_adoc_plus_attribute() {
    let exp_add = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Bin(
            pl::BinOp::Num(NumBinOp::Add),
            pl::OpTyp::Nat,
            Box::new(exp_nat(1)),
            Box::new(exp_nat(2)),
        ),
        note: pl::TypKind::Num(p4spec_rust::lang::common::prim::num::Typ::Nat),
        span: Span::default(),
    };
    let def = defined_func("add", vec![return_exp_instr(exp_add, None)]);

    assert!(
        render_def(&subject_name, &def)
            .unwrap()
            .contains("``1`` ``{plus}`` ``2``")
    );
}

#[test]
fn test_otherwise_anchor_follows_the_ordered_list_marker_space() {
    let mut def = defined_func("fallback", vec![return_exp_instr(exp_id("value"), None)]);
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(func)) = &mut def.node.node else {
        panic!("fixture is a defined function");
    };
    func.block_else_opt = Some(vec![return_instr(false)]);

    assert!(
        render_def(&subject_name, &def)
            .unwrap()
            .contains("\n\n. +++<span id=\"fallback-else\"></span>+++Otherwise:")
    );
}

#[test]
fn test_relation_dispatch_allocates_block_anchor_before_group_bodies() {
    let signature = pl::RelSignature {
        not_typ: p4spec_rust::phrase! {
            node: Mixfix::Arg(typ::make::bool()),
            span: Span::default(),
        },
        input_hint: InputHint::new(vec![p4spec_rust::phrase! { node: 0, span: Span::default() }]),
    };
    let group = p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Tier(pl::TierInstr {
            tier: pl::DispatchInstr::Group(pl::RuleGroupInstr {
                id_rel: id("Rel"),
                id_group: id("main"),
                rel_signature: signature.clone(),
                exps_input: vec![exp_id("input")],
                block: vec![backtrack_instr(vec![
                    vec![return_instr(false)],
                    vec![return_instr(true)],
                ])],
            }),
        }),
        note: None,
        span: Span::default(),
    };
    let route = p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Tier(pl::TierInstr {
            tier: pl::DispatchInstr::Route(pl::RouteInstr { blocks: vec![vec![group]] }),
        }),
        note: None,
        span: Span::default(),
    };
    let def = p4spec_rust::annotated! {
        node: p4spec_rust::phrase! {
            node: pl::DefKind::Rel(pl::RelDef::Defined(pl::DefinedRel {
                id: id("Rel"),
                rel_signature: signature,
                exps_input: vec![exp_id("input")],
                block: vec![route],
                block_else_opt: None,
            })),
            span: Span::default(),
        },
        hints: Hints::default(),
    };
    let rendered = render_def(&subject_name, &def).unwrap();

    assert!(rendered.contains("id=\"bk-Rel-2-arm-1\""), "{rendered}");
    assert!(rendered.contains("id=\"bk-Rel-1-arm-1\""), "{rendered}");
}

fn table_func() -> pl::TableFunc {
    pl::TableFunc {
        id: id("table"),
        params: ["x", "y"]
            .into_iter()
            .map(|name| {
                p4spec_rust::phrase! {
                    node: pl::ParamKind::Exp(typ::make::bool(), Box::new(exp_id(name))),
                    span: Span::default(),
                }
            })
            .collect(),
        typ: typ::make::bool(),
        rows: vec![pl::TableRow {
            exps_input: vec![exp_bool(false), exp_bool(true)],
            exp: exp_bool(true),
            block: vec![],
        }],
    }
}

fn meta_func_def(hints: Hints, func: pl::MetaFuncDef) -> pl::Def {
    p4spec_rust::annotated! {
        node: p4spec_rust::phrase! {
            node: pl::DefKind::MetaFunc(func),
            span: Span::default(),
        },
        hints: hints,
    }
}

#[test]
fn test_table_argument_tuple_always_occupies_one_column() {
    let def = meta_func_def(Hints::default(), pl::MetaFuncDef::Table(table_func()));
    let text = render_def(&subject_name, &def).unwrap();
    assert_eq!(
        text,
        concat!(
            "xref:table[$table(x, y)]:\n",
            "[cols=\"2\", options=\"header\"]\n",
            "|===\n| (``x``, ``y``) | Result \n\n",
            "| false, true | true\n\n|===",
        )
    );
}

#[test]
fn test_table_cells_use_the_enclosing_anchor_resolver() {
    let mut func = table_func();
    func.rows[0].exp = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Call(id("inner"), vec![], vec![]),
        note: pl::TypKind::Bool,
        span: Span::default(),
    };
    let anchor = |subject: &Subject| match subject {
        Subject::Function(id) | Subject::Relation(id) => Some(format!("custom-{id}")),
    };
    let def = meta_func_def(Hints::default(), pl::MetaFuncDef::Table(func));
    let text = render_def(&anchor, &def).unwrap();
    assert!(text.contains("| false, true | xref:custom-inner[$inner]"), "{text}");
    let text = render_def(&|_| None, &def).unwrap();
    assert!(text.contains("| false, true | $inner"), "{text}");
    assert!(!text.contains("xref:"), "{text}");
}

#[test]
fn test_function_header_suppresses_nested_pattern_links() {
    let mut exp = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Case(Box::new(Mixfix::Arg(exp_id("x")))),
        note: pl::TypKind::Var(id("Value"), vec![]),
        span: Span::default(),
    };
    exp.hints.prose = Some(prose_hint("value", 0, ""));
    let param = p4spec_rust::phrase! {
        node: pl::ParamKind::Exp(typ::make::bool(), Box::new(exp)),
        span: Span::default(),
    };
    let hints = Hints { prose_in: Some(prose_hint("checking", 0, "")), ..Hints::default() };
    let func = pl::ExternFunc {
        id: id("check"),
        tparams: vec![],
        params: vec![param],
        typ: typ::make::bool(),
    };
    let def = meta_func_def(hints, pl::MetaFuncDef::Extern(func));
    let text = render_def(&subject_name, &def).unwrap();
    assert_eq!(text, "xref:check[Checking value ``x``]");
}

#[test]
fn test_rulegroup_fragments_keep_distinct_arm_anchors() {
    let signature = pl::RelSignature {
        not_typ: p4spec_rust::phrase! {
            node: Mixfix::Arg(typ::make::bool()),
            span: Span::default(),
        },
        input_hint: InputHint::new(vec![p4spec_rust::phrase! { node: 0, span: Span::default() }]),
    };
    let block = vec![backtrack_instr(vec![
        vec![return_exp_instr(exp_bool(false), Some(pl::Fallthrough::Next))],
        vec![return_instr(true)],
    ])];
    let mut renderer = adoc::Renderer::new(&subject_name);
    let text_a = renderer.render_rulegroup(
        &Hints::default(),
        &id("Rel"),
        &signature,
        &[exp_bool(true)],
        &block,
    );
    let text_b = renderer.render_rulegroup(
        &Hints::default(),
        &id("Rel"),
        &signature,
        &[exp_bool(true)],
        &block,
    );
    assert!(text_a.contains("id=\"bk-Rel-1-arm-1\""), "{text_a}");
    assert!(text_b.contains("id=\"bk-Rel-2-arm-1\""), "{text_b}");
    assert!(text_b.contains("href=\"#bk-Rel-2-arm-2\">→ 2</a>"), "{text_b}");
    let text_fresh = adoc::Renderer::new(&subject_name).render_rulegroup(
        &Hints::default(),
        &id("Rel"),
        &signature,
        &[exp_bool(true)],
        &block,
    );
    assert_eq!(text_a, text_fresh);
}

#[test]
fn test_membership_guard_with_call_carries_the_fallthrough_label() {
    let exp_set = p4spec_rust::annotated_note_phrase! {
        node: pl::ExpKind::Call(id("g"), vec![], vec![]),
        note: pl::TypKind::Bool,
        span: Span::default(),
    };
    let case_instr = p4spec_rust::annotated_note_phrase! {
        node: pl::InstrKind::Case(pl::CaseInstr {
            exp: exp_id("x"),
            cases: vec![
                pl::Case {
                    guard: pl::Guard::Mem(exp_set),
                    block: vec![return_exp_instr(exp_bool(true), None)],
                },
                pl::Case {
                    guard: pl::Guard::Bool(true),
                    block: vec![return_exp_instr(exp_bool(false), None)],
                },
            ],
            dangle: true,
        }),
        note: Some(pl::Fallthrough::Fail),
        span: Span::default(),
    };
    let def = defined_func("f", vec![case_instr]);
    let text = render_def(&subject_name, &def).unwrap();
    assert_eq!(
        text,
        concat!(
            "xref:f[$f]\n\n",
            ". If ``x`` is in xref:g[``$g``]:+++<sub class=\"bk-mark\">[FAIL]</sub>+++ return ``true``.\n",
            ". Else if ``x``: return ``false``.",
        ),
    );
}
