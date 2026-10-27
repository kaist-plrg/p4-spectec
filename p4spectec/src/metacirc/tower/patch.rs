//! Synthetic entry relations for encoded programs
//!
//! The meta specification's `Eval_AL` and `Eval_IL` call the relation MAIN of their input script.
//! An added MAIN calls the selected single-input relation and outputs "pass".
//! The MAIN is added to the AL or the IL of a level,
//! as the level consuming it accepts AL or IL;
//! a script at the top level instead gets a MAIN that outputs its `$main()`.
//! Relation notation, input positions, and output types come from the source AST;
//! literal expressions retain the parsed program's types and spans.

use super::super::{Result, config::Lang, error::entry_invalid};
use super::{PreparedLevel, Tower};
use crate::{
    interface::meta::shared::encode::Encode,
    lang::{
        al::ast as al,
        common::{notation::atom::Atom, source::Span},
        data::{
            arena::Arena,
            notation::tree::{self as notation, Mixfix},
            value::flat::Value,
        },
        hints::input::{self, InputHint},
        il::ast as il,
    },
    note_phrase, phrase,
    runner::Extern,
};
use std::{path::PathBuf, rc::Rc};

// == Helpers

/// Rejects a script whose own relation is named `MAIN`.
fn check_main_free(id: &il::Id) -> Result<()> {
    if id.node.as_ref() == "MAIN" {
        return Err(entry_invalid("cannot add a synthetic entry: the script already defines MAIN"));
    }
    Ok(())
}

/// Rejects an IL script that already defines a relation `MAIN`.
fn check_main_free_il(spec_il: &il::Spec) -> Result<()> {
    for def_il in spec_il {
        match &def_il.node {
            il::DefKind::Rel(il::RelDef::Defined(rel)) => check_main_free(&rel.id)?,
            il::DefKind::Rel(il::RelDef::Extern(rel)) => check_main_free(&rel.id)?,
            _ => {}
        }
    }
    Ok(())
}

/// Rejects an AL script that already defines a relation `MAIN`.
fn check_main_free_al(spec_al: &al::Spec) -> Result<()> {
    for def_al in spec_al {
        match &def_al.node {
            al::DefKind::Rel(al::RelDef::Defined(rel)) => check_main_free(&rel.id)?,
            al::DefKind::Rel(al::RelDef::Extern(rel)) => check_main_free(&rel.id)?,
            _ => {}
        }
    }
    Ok(())
}

/// The success marker, evaluated only after the relation premise.
fn exp_pass() -> il::Exp {
    note_phrase!(node: il::ExpKind::Text("pass".into()), note: Rc::new(il::TypKind::Text), span: Span::default())
}

/// The call `$main()` of a script's own entry function returning `typ`.
fn exp_call_main(typ: &il::Typ) -> il::Exp {
    let id = phrase!(node: "main".into(), span: Span::default());
    note_phrase!(node: il::ExpKind::Call(id, vec![], vec![]), note: Rc::new(typ.node.clone()), span: Span::default())
}

/// The notation `|- typ` of MAIN, with its conclusion over `exp`.
fn main_not(typ: il::Typ, exp: il::Exp) -> (il::NotTyp, il::NotExp) {
    let atom = phrase!(node: Atom::Turnstile, span: Span::default());
    let not_typ = notation::make::infix(notation::make::seq(vec![]), atom, notation::make::arg(typ));
    let not_exp = Mixfix::new(Rc::clone(not_typ.mixop()), vec![exp])
        .expect("MAIN has one notation argument");
    (phrase!(node: not_typ, span: Span::default()), not_exp)
}

/// Builds `relation MAIN: |- typ` with the single rule `rule MAIN: |- exp -- prems`.
///
/// MAIN has no inputs, so its input hint is empty.
fn main_rel_il(typ: il::Typ, exp: il::Exp, prems: Vec<il::Prem>) -> il::Def {
    let (not_typ, not_exp) = main_not(typ, exp);
    let id_rule = phrase!(node: "".into(), span: Span::default());
    let rule = phrase!(node: il::RuleKind { id: id_rule.clone(), not_exp, prems }, span: Span::default());
    let rule_group = phrase!(node: il::RuleGroupKind { id: id_rule, rules: vec![rule] }, span: Span::default());
    phrase!(node: il::DefKind::Rel(il::RelDef::Defined(Box::new(il::DefinedRel {
        id: phrase!(node: "MAIN".into(), span: Span::default()),
        not_typ, input_hint: InputHint::new(vec![]),
        rule_groups: vec![rule_group], else_group: None, hints: vec![],
    }))), span: Span::default())
}

/// Builds the MAIN of `main_rel_il` in AL: no inputs to match, one path.
fn main_rel_al(typ: il::Typ, exp: il::Exp, prems: Vec<al::Prem>) -> al::Def {
    let (not_typ, _) = main_not(typ, exp.clone());
    let id_rule = phrase!(node: "".into(), span: Span::default());
    let rule_match = al::RuleMatch { exps_signature: vec![exp.clone()], exps_input: vec![], prems: vec![] };
    let rule_path = al::RulePath { id: id_rule.clone(), prems, exps_output: vec![exp] };
    let rule_group = phrase!(node: al::RuleGroupKind {
        id: id_rule, rule_match, rule_paths: vec![rule_path],
    }, span: Span::default());
    phrase!(node: al::DefKind::Rel(al::RelDef::Defined(Box::new(al::DefinedRel {
        id: phrase!(node: "MAIN".into(), span: Span::default()),
        not_typ, input_hint: InputHint::new(vec![]),
        rule_groups: vec![rule_group], else_group: None, hints: vec![],
    }))), span: Span::default())
}

/// The return type of a script's own `$main()`, which must take no arguments.
fn main_typ(func: Option<(&[il::TParam], &[il::Param], &il::Typ)>) -> Result<il::Typ> {
    match func {
        Some(([], [], typ)) => Ok(typ.clone()),
        _ => Err(entry_invalid("the script must define a function main without parameters")),
    }
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

/// Appends a MAIN relation to IL that succeeds only if the selected relation succeeds.
///
/// For `Program_inst`, constructs the IL equivalent of this SpecTec sketch:
///
/// ```text
/// relation MAIN: |- text
/// rule MAIN: |- "pass"
///   -- Program_inst: |- <program literal> : result_0 result_1
/// ```
///
/// `<program literal>` denotes the expression produced by `value.to_exp(arena)`;
/// it is embedded in the premise, so MAIN takes no inputs.
/// `result_0` and `result_1` have types `globalInstLayer` and `store`.
/// The premise binds these outputs; MAIN discards them and outputs `"pass"`
/// only after the premise succeeds.
///
/// Other relations use their own notation, input positions, and output types.
fn entry_il(arena: &Arena, spec_il: &il::Spec, name: &str, value: Value) -> Result<il::Spec> {
    // A synthetic entry must never shadow a source relation
    check_main_free_il(spec_il)?;
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
    // Relation: relation MAIN: |- text, with rule MAIN: |- "pass" over the premise
    let typ = phrase!(node: il::TypKind::Text, span: Span::default());
    let mut spec_patched_il = spec_il.clone();
    spec_patched_il.push(main_rel_il(typ, exp_pass(), vec![prem]));
    Ok(spec_patched_il)
}

/// Appends the MAIN relation of `entry_il` to AL instead.
fn entry_al(arena: &Arena, spec_al: &al::Spec, name: &str, value: Value) -> Result<al::Spec> {
    // A synthetic entry must never shadow a source relation
    check_main_free_al(spec_al)?;
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
    // Relation: relation MAIN: |- text, with rule MAIN: |- "pass" over the premise
    let typ = phrase!(node: il::TypKind::Text, span: Span::default());
    let mut spec_patched_al = spec_al.clone();
    spec_patched_al.push(main_rel_al(typ, exp_pass(), vec![prem]));
    Ok(spec_patched_al)
}

// == Script entry

/// Appends `relation MAIN: |- T` and `rule MAIN: |- $main()` to an IL script,
/// where `dec $main() : T` is the script's own entry function.
fn script_il(mut spec_il: il::Spec) -> Result<il::Spec> {
    check_main_free_il(&spec_il)?;
    let typ = main_typ(spec_il.iter().find_map(|def_il| match &def_il.node {
        il::DefKind::MetaFunc(il::MetaFuncDef::Defined(func)) if func.id.node.as_ref() == "main" => {
            Some((func.tparams.as_slice(), func.params.as_slice(), &func.typ))
        }
        _ => None,
    }))?;
    let exp = exp_call_main(&typ);
    spec_il.push(main_rel_il(typ, exp, vec![]));
    Ok(spec_il)
}

/// Appends the MAIN relation of `script_il` to an AL script instead.
fn script_al(mut spec_al: al::Spec) -> Result<al::Spec> {
    check_main_free_al(&spec_al)?;
    let typ = main_typ(spec_al.iter().find_map(|def_al| match &def_al.node {
        al::DefKind::MetaFunc(al::MetaFuncDef::Defined(func)) if func.id.node.as_ref() == "main" => {
            Some((func.tparams.as_slice(), func.params.as_slice(), &func.typ))
        }
        _ => None,
    }))?;
    let exp = exp_call_main(&typ);
    spec_al.push(main_rel_al(typ, exp, vec![]));
    Ok(spec_al)
}

/// Encodes specification paths as a top-level script entered through MAIN.
pub(in crate::metacirc) fn parse_script(
    arena: &mut Arena,
    lang: Lang,
    paths: &[PathBuf],
) -> Result<Value> {
    let encoded = match lang {
        Lang::Al => script_al(crate::algo(paths)?)?.encode(arena),
        Lang::Il => script_il(crate::elab(paths)?)?.encode(arena),
        Lang::P4 => return Err(entry_invalid("a P4 program is not a script")),
    };
    encoded.map_err(|error| error.into_report())
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
