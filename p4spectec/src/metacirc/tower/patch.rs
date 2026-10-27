//! Synthetic entry functions for encoded programs
//!
//! The meta specification's `Eval_AL` and `Eval_IL` invoke main() in their input script.
//! An added main calls the selected single-input relation and returns "pass".
//! The main is added to the AL or the IL of a level,
//! as the level consuming it accepts AL or IL.
//! Relation notation, input positions, and output types come from the source AST;
//! literal expressions retain the parsed program's types and spans.

use super::super::{Result, error::entry_invalid};
use super::{PreparedLevel, Tower};
use crate::{
    interface::meta::shared::encode::Encode,
    lang::{
        al::ast as al,
        common::source::Span,
        data::{arena::Arena, notation::tree::Mixfix, value::flat::Value},
        hints::input::{self, InputHint},
        il::ast as il,
    },
    note_phrase, phrase,
    runner::Extern,
};
use std::rc::Rc;

// == Helpers

/// Rejects a script whose own function is named `main`.
fn check_main_free(id: &il::Id) -> Result<()> {
    if id.node.as_ref() == "main" {
        return Err(entry_invalid("cannot add a synthetic entry: the script already defines main"));
    }
    Ok(())
}

/// The success marker, evaluated only after the relation premise.
fn exp_pass() -> il::Exp {
    note_phrase!(node: il::ExpKind::Text("pass".into()), note: Rc::new(il::TypKind::Text), span: Span::default())
}

// == Entry construction

/// Patches level `idx` and encodes it in the form the level below accepts.
fn entry_encoded(
    arena: &mut Arena,
    levels: &[PreparedLevel],
    idx: usize,
    value: Value,
) -> Result<Value> {
    let level = &levels[idx];
    let name = &level.config().rel;
    let encoded = match level {
        PreparedLevel::Al { spec_al, .. } => entry_al(arena, spec_al, name, value)?.encode(arena),
        // An IL consumer converts the patched IL itself
        PreparedLevel::Il { spec_il, .. } => entry_il(arena, spec_il, name, value)?.encode(arena),
        PreparedLevel::Base { .. } => {
            return Err(entry_invalid("the base is never passed to another level"));
        }
    };
    encoded.map_err(|error| error.into_report())
}

/// Builds the entry relation's notation over the program literal and fresh outputs.
fn entry_not_exp(
    arena: &Arena,
    name: &str,
    not_typ: &il::NotTyp,
    input_hint: &InputHint,
    value: Value,
) -> Result<il::NotExp> {
    let (typs_input, typs_output) = input::split(input_hint, not_typ.node.args().to_vec())
        .map_err(|cause| entry_invalid(cause.to_string()))?;
    if typs_input.len() != 1 {
        return Err(entry_invalid(format!("entry relation {name} must have exactly one input")));
    }
    // Program_inst outputs: result_0 : globalInstLayer, result_1 : store
    let exps_output = typs_output
        .into_iter()
        .enumerate()
        .map(|(idx, typ)| {
            let id = phrase!(node: format!("result_{idx}").into(), span: Span::default());
            note_phrase!(node: il::ExpKind::Id(id), note: Rc::new(typ.node), span: Span::default())
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
    Mixfix::new(not_typ.node.mixop().clone(), exps)
        .map_err(|cause| entry_invalid(cause.to_string()))
}

/// Appends a main function to IL that succeeds only if the selected relation succeeds.
///
/// For `Program_inst`, constructs the IL equivalent of this SpecTec sketch:
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
fn entry_il(arena: &Arena, spec_il: &il::Spec, name: &str, value: Value) -> Result<il::Spec> {
    // A synthetic entry must never shadow a source function
    for def_il in spec_il {
        if let il::DefKind::MetaFunc(func_il) = &def_il.node {
            let id = match func_il {
                il::MetaFuncDef::Extern(func) => &func.id,
                il::MetaFuncDef::Builtin(func) => &func.id,
                il::MetaFuncDef::Table(func) => &func.id,
                il::MetaFuncDef::Defined(func) => &func.id,
            };
            check_main_free(id)?;
        }
    }
    // Read the actual notation and direction of the configured entry
    let (id, not_typ, input_hint) = spec_il
        .iter()
        .find_map(|def_il| match &def_il.node {
            il::DefKind::Rel(il::RelDef::Defined(rel)) if rel.id.node.as_ref() == name => {
                Some((&rel.id, &rel.not_typ, &rel.input_hint))
            }
            il::DefKind::Rel(il::RelDef::Extern(rel)) if rel.id.node.as_ref() == name => {
                Some((&rel.id, &rel.not_typ, &rel.input_hint))
            }
            _ => None,
        })
        .ok_or_else(|| entry_invalid(format!("entry relation {name} was not found")))?;
    let not_exp = entry_not_exp(arena, name, not_typ, input_hint, value)?;
    // Premise: -- Program_inst: |- <program literal> : result_0 result_1
    let prem = phrase!(node: il::PremKind::Rule(il::RulePrem {
        id: id.clone(), not_exp, input_hint: input_hint.clone(),
    }), span: Span::default());
    // Clause: def $main() = "pass" with the relation premise
    let clause = phrase!(node: il::ClauseKind {
        args: vec![], exp: exp_pass(), prems: vec![prem], otherwise_opt: None,
    }, span: Span::default());
    // Function: dec $main() : text, with the clause above
    let def = phrase!(node: il::DefKind::MetaFunc(il::MetaFuncDef::Defined(Box::new(il::DefinedFunc {
        id: phrase!(node: "main".into(), span: Span::default()),
        tparams: vec![], params: vec![],
        typ: phrase!(node: il::TypKind::Text, span: Span::default()),
        clauses: vec![clause], else_clause: None, hints: vec![],
    }))), span: Span::default());
    let mut spec_patched_il = spec_il.clone();
    spec_patched_il.push(def);
    Ok(spec_patched_il)
}

/// Appends the main function of `entry_il` to AL instead.
fn entry_al(arena: &Arena, spec_al: &al::Spec, name: &str, value: Value) -> Result<al::Spec> {
    // A synthetic entry must never shadow a source function
    for def_al in spec_al {
        if let al::DefKind::MetaFunc(func_al) = &def_al.node {
            let id = match func_al {
                al::MetaFuncDef::Extern(func) => &func.id,
                al::MetaFuncDef::Builtin(func) => &func.id,
                al::MetaFuncDef::Table(func) => &func.id,
                al::MetaFuncDef::Defined(func) => &func.id,
            };
            check_main_free(id)?;
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
    let not_exp = entry_not_exp(arena, name, not_typ, input_hint, value)?;
    // Premise: -- Program_inst: |- <program literal> : result_0 result_1
    let prem = phrase!(node: al::PremKind::Rule(al::RulePrem {
        id: id.clone(), not_exp, input_hint: input_hint.clone(),
    }), span: Span::default());
    // Clause: def $main() = "pass" with the relation premise
    let clause = phrase!(node: al::ClauseKind { args: vec![], exp: exp_pass(), prems: vec![prem] }, span: Span::default());
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

// == Tower patching

impl<Ext: Extern> Tower<Ext> {
    /// Wraps an input in scripts from the top level toward the native base.
    ///
    /// Each level is entered through its configured relation,
    /// and its script is encoded in the language of the level consuming it.
    /// Encoding errors remain separate from the subsequent program evaluation.
    pub fn patch(&self, arena: &mut Arena, program: Value) -> Result<Value> {
        // The base is never encoded; a direct tower consumes the input unchanged
        let mut value = program;
        // Each level interprets the script immediately above it
        for idx in (1..self.levels.len()).rev() {
            value = entry_encoded(arena, &self.levels, idx, value)?;
        }
        Ok(value)
    }
}
