//! Synthetic AL entry functions for encoded programs
//!
//! The meta specification's Entry invokes main() in its input script.
//! An added main calls the selected single-input relation and returns "pass".
//! Relation notation, input positions, and output types come from the source AST;
//! literal expressions retain the parsed program's types and spans.

use super::{Result, error::entry_invalid, tower::Tower};
use crate::{
    interface::al::encode::Encode,
    lang::{
        al::ast as al,
        common::source::Span,
        data::{arena::Arena, notation::tree::Mixfix, value::flat::Value},
        hints::input,
    },
    note_phrase, phrase,
};
use std::rc::Rc;

impl Tower {
    /// Wraps an input in AL scripts from the target toward the native base.
    ///
    /// Each level is entered through its configured relation.
    /// Encoding errors remain separate from the subsequent program evaluation.
    pub fn patch(&self, arena: &mut Arena, program: Value) -> Result<Value> {
        // The base is never encoded; a direct tower consumes the input unchanged
        let Some((target, interms)) = self.levels[1..].split_last() else {
            return Ok(program);
        };
        // Patch the configured target relation around this input
        let mut value = Self::entry(arena, &target.spec_al, &target.config.rel, program)?
            .encode(arena)
            .map_err(|error| error.into_report())?;
        // Each intermediate interprets the script immediately above it
        for interm in interms.iter().rev() {
            value = Self::entry(arena, &interm.spec_al, &interm.config.rel, value)?
                .encode(arena)
                .map_err(|error| error.into_report())?;
        }
        Ok(value)
    }

    /// Appends a main function that succeeds only if the selected relation succeeds.
    ///
    /// For `Program_inst`, constructs the AL equivalent of this SpecTec sketch:
    ///
    /// ```text
    /// dec $main() : text
    /// def $main() = "pass"
    ///   -- Program_inst: |- <program literal> : result_0 result_1
    /// ```
    ///
    /// `<program literal>` denotes the expression produced by `value.to_exp(arena)`;
    /// it is embedded in the body, so `main` takes no arguments.
    /// `result_0` and `result_1` have types `globalInstLayer` and `store`.
    /// The relation binds these outputs; `main` discards them and returns `"pass"`
    /// only after the premise succeeds.
    ///
    /// Other relations use their own notation, input positions, and output types.
    fn entry(arena: &Arena, spec_al: &al::Spec, name: &str, value: Value) -> Result<al::Spec> {
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
                    return Err(entry_invalid(
                        "cannot add a synthetic entry: the script already defines main",
                    ));
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
            .ok_or_else(|| entry_invalid(format!("entry relation {name} was not found")))?;
        let (typs_input, typs_output) = input::split(input_hint, not_typ.node.args().to_vec())
            .map_err(|cause| entry_invalid(cause.to_string()))?;
        if typs_input.len() != 1 {
            return Err(entry_invalid(format!(
                "entry relation {name} must have exactly one input"
            )));
        }
        // Program_inst outputs: result_0 : globalInstLayer, result_1 : store
        let exps_output = typs_output
            .into_iter()
            .enumerate()
            .map(|(idx, typ)| {
                let id = phrase!(node: format!("result_{idx}").into(), span: Span::default());
                note_phrase!(node: al::ExpKind::Id(id), note: Rc::new(typ.node), span: Span::default())
            })
            .collect();
        // Arguments: <program literal>, result_0, result_1 in relation order
        let exps = input::combine(
            input_hint,
            vec![
                value
                    .to_exp(arena)
                    .map_err(|cause| entry_invalid(cause.to_string()))?,
            ],
            exps_output,
        )
        .map_err(|cause| entry_invalid(cause.to_string()))?;
        // Notation: |- <program literal> : result_0 result_1
        let not_exp = Mixfix::new(not_typ.node.mixop().clone(), exps)
            .map_err(|cause| entry_invalid(cause.to_string()))?;
        // Premise: -- Program_inst: |- <program literal> : result_0 result_1
        let prem = phrase!(node: al::PremKind::Rule(al::RulePrem {
            id: id.clone(), not_exp, input_hint: input_hint.clone(),
        }), span: Span::default());
        // The success marker is evaluated only after the relation premise
        let exp = note_phrase!(node: al::ExpKind::Text("pass".into()), note: Rc::new(al::TypKind::Text), span: Span::default());
        // Clause: def $main() = "pass" with the relation premise
        let clause = phrase!(node: al::ClauseKind { args: vec![], exp, prems: vec![prem] }, span: Span::default());
        // Function: dec $main() : text, with the clause above
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
}
