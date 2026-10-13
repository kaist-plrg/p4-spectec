//! Synthetic AL entry functions for encoded programs
//!
//! The meta specification's Entry invokes main() in its input script.
//! An added main calls the selected single-input relation and returns "pass".
//! Relation notation, input positions, and output types come from the source AST;
//! literal expressions retain the parsed program's types and spans.

use super::{Result, error};
use crate::{
    lang::{
        al::ast as al,
        common::source::Span,
        data::{
            arena::Arena,
            notation::tree::Mixfix,
            value::flat::{Value, ValueKind},
        },
        hints::input,
    },
    note_phrase, phrase,
};
use std::rc::Rc;

/// Converts a runtime value into an AL literal expression.
fn value_as_exp(arena: &Arena, value: Value) -> Result<al::Exp> {
    let span = *arena.value.span(value.span);
    let typ = arena.value.typ(value.note).clone();
    let exp_kind = match arena.value.kind(value.node) {
        ValueKind::Bool(value) => al::ExpKind::Bool(*value),
        ValueKind::Num(num) => al::ExpKind::Num(num.clone()),
        ValueKind::Text(text) => al::ExpKind::Text(text.clone()),
        ValueKind::Struct(value_fields) => al::ExpKind::Str(
            value_fields
                .iter()
                .map(|value_field| {
                    Ok(al::ExpField {
                        atom: value_field.atom.clone(),
                        exp: value_as_exp(arena, value_field.value)?,
                    })
                })
                .collect::<Result<_>>()?,
        ),
        ValueKind::Case(value_case) => {
            let exps = value_case
                .args()
                .iter()
                .map(|value| value_as_exp(arena, *value))
                .collect::<Result<_>>()?;
            let mixop = value_case.mixop().into_tree(&arena.mixop);
            al::ExpKind::Case(Box::new(
                Mixfix::new(Rc::new(mixop), exps).map_err(|cause| error(cause.to_string()))?,
            ))
        }
        ValueKind::Tuple(values) => al::ExpKind::Tuple(
            values
                .iter()
                .map(|value| value_as_exp(arena, *value))
                .collect::<Result<_>>()?,
        ),
        ValueKind::Opt(value) => al::ExpKind::Opt(
            value
                .map(|value| value_as_exp(arena, value).map(Box::new))
                .transpose()?,
        ),
        ValueKind::List(values) => al::ExpKind::List(
            values
                .iter()
                .map(|value| value_as_exp(arena, *value))
                .collect::<Result<_>>()?,
        ),
        ValueKind::Func(_) | ValueKind::Extern(_) => {
            return Err(error("cannot embed a function or extern value in a synthetic AL entry"));
        }
    };
    Ok(note_phrase!(node: exp_kind, note: typ, span: span))
}

/// Appends a main function that succeeds only if the selected relation succeeds.
pub(super) fn entry(
    arena: &Arena,
    spec_al: &al::Spec,
    name: &str,
    value: Value,
) -> Result<al::Spec> {
    // A synthetic entry must never shadow a source function
    for def_al in spec_al {
        if let al::DefKind::MetaFunc(func_al) = &def_al.node {
            let id = match func_al {
                al::MetaFuncDef::Extern(func) => &func.id,
                al::MetaFuncDef::Builtin(func) => &func.id,
                al::MetaFuncDef::Table(func) => &func.id,
                al::MetaFuncDef::Defined(func) => &func.id,
            };
            if id.node.as_ref() == "main" {
                return Err(error("cannot add a synthetic entry: the script already defines main"));
            }
        }
    }
    // Read the actual notation and direction of the configured entry
    let (id, not_typ, input_hint) = spec_al
        .iter()
        .find_map(|def_al| match &def_al.node {
            al::DefKind::Rel(al::RelDef::Defined(rel)) if rel.id.node.as_ref() == name => {
                Some((&rel.id, &rel.not_typ, &rel.input_hint))
            }
            al::DefKind::Rel(al::RelDef::Extern(rel)) if rel.id.node.as_ref() == name => {
                Some((&rel.id, &rel.not_typ, &rel.input_hint))
            }
            _ => None,
        })
        .ok_or_else(|| error(format!("entry relation {name} was not found")))?;
    let (typs_input, typs_output) = input::split(input_hint, not_typ.node.args().to_vec())
        .map_err(|cause| error(cause.to_string()))?;
    if typs_input.len() != 1 {
        return Err(error(format!("entry relation {name} must have exactly one input")));
    }
    // Bind each output at its declared type in the new function's empty scope
    let exps_output = typs_output
        .into_iter()
        .enumerate()
        .map(|(idx, typ)| {
            let id = phrase!(node: format!("result_{idx}").into(), span: Span::default());
            note_phrase!(node: al::ExpKind::Id(id), note: Rc::new(typ.node), span: Span::default())
        })
        .collect();
    let exps = input::combine(input_hint, vec![value_as_exp(arena, value)?], exps_output)
        .map_err(|cause| error(cause.to_string()))?;
    let not_exp = Mixfix::new(not_typ.node.mixop().clone(), exps)
        .map_err(|cause| error(cause.to_string()))?;
    let prem = phrase!(node: al::PremKind::Rule(al::RulePrem {
        id: id.clone(), not_exp, input_hint: input_hint.clone(),
    }), span: Span::default());
    // The success marker is evaluated only after the relation premise
    let exp = note_phrase!(node: al::ExpKind::Text("pass".into()), note: Rc::new(al::TypKind::Text), span: Span::default());
    let clause = phrase!(node: al::ClauseKind { args: vec![], exp, prems: vec![prem] }, span: Span::default());
    let def = phrase!(node: al::DefKind::MetaFunc(al::MetaFuncDef::Defined(Box::new(al::DefinedFunc {
        id: phrase!(node: "main".into(), span: Span::default()),
        tparams: vec![], params: vec![],
        typ: phrase!(node: al::TypKind::Text, span: Span::default()),
        clauses: vec![clause], else_clause: None, hints: vec![],
    }))), span: Span::default());
    let mut spec_patched_al = spec_al.clone();
    spec_patched_al.push(def);
    Ok(spec_patched_al)
}
