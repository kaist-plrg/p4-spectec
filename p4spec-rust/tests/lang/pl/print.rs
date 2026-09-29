use p4spec_rust::{
    lang::common::source::{Position, Span},
    lang::{
        common::notation::mixfix::Mixfix,
        hints::{alter, input::InputHint},
        il, pl,
        traits::print::Print,
    },
};

fn span(name: &str) -> Span {
    Span::new(Position::new(name, 0, 0), Position::new(name, 0, 0))
}

fn id(name: &str) -> il::ast::Id {
    p4spec_rust::phrase! {
        node: name.to_owned(),
        span: span(name),
    }
}

fn typ() -> il::ast::Typ {
    p4spec_rust::phrase! {
        node: il::ast::TypKind::Bool,
        span: span("type"),
    }
}

fn id_exp(name: &str) -> pl::ast::Exp {
    pl::annot::Annotated {
        node: p4spec_rust::note_phrase! {
            node: pl::ast::ExpKind::Id(id(name)),
            note: il::ast::TypKind::Bool,
            span: span(name),
        },
        hints: pl::annot::Hints::default(),
    }
}

fn text(value: &str) -> pl::ast::Exp {
    pl::annot::Annotated {
        node: p4spec_rust::note_phrase! {
            node: pl::ast::ExpKind::Text(value.to_owned()),
            note: il::ast::TypKind::Text,
            span: span("text"),
        },
        hints: pl::annot::Hints::default(),
    }
}

fn signature() -> pl::ast::RelSignature {
    pl::ast::RelSignature {
        not_typ: p4spec_rust::phrase! {
            node: Mixfix::Arg(typ()),
            span: span("signature"),
        },
        input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
    }
}

fn group_instr(
    kind: pl::ast::InstrKind<pl::ast::GroupInstr>,
) -> pl::ast::Instr<pl::ast::GroupInstr> {
    pl::annot::Annotated {
        node: p4spec_rust::note_phrase! { node: kind, note: None, span: span("group-instruction") },
        hints: pl::annot::Hints::default(),
    }
}

fn dispatch_instr(
    kind: pl::ast::InstrKind<pl::ast::DispatchInstr>,
) -> pl::ast::Instr<pl::ast::DispatchInstr> {
    pl::annot::Annotated {
        node: p4spec_rust::note_phrase! { node: kind, note: None, span: span("dispatch-instruction") },
        hints: pl::annot::Hints::default(),
    }
}

#[test]
fn test_group_printer_escapes_text_and_omits_annotations_and_fallthrough() {
    let mut instr_a = group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
        tier: pl::ast::GroupInstr::Return(pl::ast::ReturnInstr { exp: text("line\n\"\\") }),
    }));
    instr_a.node.note = Some(pl::ast::Fallthrough::Next);
    instr_a.hints.prose = Some(
        p4spec_rust::phrase! { node: alter::AlterHintKind::Text("first prose".to_owned()), span: Default::default() },
    );

    let mut instr_b = instr_a.clone();
    instr_b.node.note = Some(pl::ast::Fallthrough::Fail);
    instr_b.node.span = span("other-source");
    instr_b.hints.prose = Some(
        p4spec_rust::phrase! { node: alter::AlterHintKind::Text("other prose".to_owned()), span: Default::default() },
    );

    assert_eq!(Print::to_string(&vec![instr_a]), "1. Return \"line\\n\\\"\\\\\"");
    assert_eq!(Print::to_string(&vec![instr_b]), "1. Return \"line\\n\\\"\\\\\"");
}

#[test]
fn test_shared_control_flow_renders_group_tier_at_nested_level() {
    let branch = group_instr(pl::ast::InstrKind::If(pl::ast::IfInstr {
        exp: id_exp("condition"),
        iter_exps: Vec::new(),
        block: vec![group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
            tier: pl::ast::GroupInstr::Return(pl::ast::ReturnInstr { exp: id_exp("value") }),
        }))],
        dangle: true,
    }));
    assert_eq!(
        Print::to_string(&vec![branch]),
        concat!("1. If (condition), then\n\n", "  1. Return value\n\n", "1. Else Dangling",)
    );
}

#[test]
fn test_group_and_dispatch_backtracking_preserve_arm_order() {
    let backtrack = group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
        tier: pl::ast::GroupInstr::Backtrack(pl::ast::BacktrackInstr {
            blocks: vec![
                vec![group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
                    tier: pl::ast::GroupInstr::Return(pl::ast::ReturnInstr { exp: id_exp("a") }),
                }))],
                vec![group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
                    tier: pl::ast::GroupInstr::Return(pl::ast::ReturnInstr { exp: id_exp("b") }),
                }))],
            ],
        }),
    }));
    assert_eq!(
        Print::to_string(&vec![backtrack]),
        concat!("1. Block (2 arms)\n\n", "Arm 1:\n\n  1. Return a\n\n", "Arm 2:\n\n  1. Return b",)
    );

    let group = |name: &str| {
        vec![dispatch_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
            tier: pl::ast::DispatchInstr::Group(pl::ast::RuleGroupInstr {
                id_rel: id("relation"),
                id_group: id(name),
                rel_signature: signature(),
                exps_input: vec![id_exp(name)],
                block: vec![group_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
                    tier: pl::ast::GroupInstr::Result(pl::ast::ResultInstr {
                        rel_signature: signature(),
                        exps_output: Vec::new(),
                    }),
                }))],
            }),
        }))]
    };
    let route = dispatch_instr(pl::ast::InstrKind::Tier(pl::ast::TierInstr {
        tier: pl::ast::DispatchInstr::Route(pl::ast::RouteInstr {
            blocks: vec![group("first"), group("second")],
        }),
    }));
    assert_eq!(
        Print::to_string(&vec![route]),
        concat!(
            "1. Block (2 arms)\n\n",
            "Arm 1:\n\n  1. Group first: first\n\n",
            "    1. The relation holds\n\n",
            "Arm 2:\n\n  1. Group second: second\n\n",
            "    1. The relation holds",
        )
    );
}
