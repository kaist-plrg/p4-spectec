//! Inputs of the native base's entry relation, encoded from the top level down
//!
//! The meta specification's entry relations, such as `Eval_AL_rel`, take a script,
//! the name of the script's entry, and the entry's inputs as meta values.
//! For `towers/al-p4.json`, the base calls
//! ``Eval_AL_rel: <P4 spec as AL> |- "Program_inst" `( <program as val> `) : val*``.
//! Each level above the base wraps the inputs of the level above it this way,
//! with the level encoded in the language of the level consuming it.
//! An AL or IL script at the top is entered at its function `main`, with no inputs,
//! so the level below it uses `Eval_AL_func` or `Eval_IL_func`.

use super::super::{Result, config::Lang};
use super::{PreparedLevel, Tower};
use crate::{
    interface::meta::shared::{
        Cache, Error,
        encode::{Encode, list},
    },
    lang::{
        common::source::Span,
        data::{arena::Arena, value::flat::Value},
        il::ast::Id,
    },
    phrase,
    runner::Extern,
};

/// The entry of an AL or IL script at the top of a tower.
const ENTRY_SCRIPT: &str = "main";

// == Helpers

/// Encodes the inputs of an entry relation entering `name` of a script.
///
/// `values` are inputs of `name`, encoded as meta values one level down.
fn entry_inputs(
    arena: &mut Arena,
    value_spec: Value,
    name: &str,
    values: &[Value],
) -> Result<Vec<Value>> {
    let id: Id = phrase!(node: name.into(), span: Span::default());
    let mut cache = Cache::new(false);
    let value_id = id
        .encode_cached(arena, &mut cache)
        .map_err(Error::into_report)?;
    let value_args = list(arena, &mut cache, "val", values).map_err(Error::into_report)?;
    Ok(vec![value_spec, value_id, value_args])
}

/// Encodes a level in the language of the level consuming it.
fn spec_encoded(arena: &mut Arena, level: &PreparedLevel) -> Result<Value> {
    let encoded = match level {
        PreparedLevel::Al { spec_al, .. } => spec_al.encode(arena),
        // An IL consumer converts the IL itself
        PreparedLevel::Il { spec_il, .. } => spec_il.encode(arena),
        // An EL consumer elaborates and converts the EL itself
        PreparedLevel::El { spec_el, .. } => spec_el.encode(arena),
        PreparedLevel::Base { .. } => unreachable!("the base is never passed to another level"),
    };
    encoded.map_err(Error::into_report)
}

// == Tower input

impl<Ext: Extern> Tower<Ext> {
    /// Encodes an input as the inputs of the native base's entry relation.
    ///
    /// Encoding errors remain separate from the subsequent evaluation.
    pub fn encode_input(&self, arena: &mut Arena, program: Value) -> Result<Vec<Value>> {
        let level_top = self.levels.last().expect("validated nonempty tower");
        // A P4 program is the top level's input; a script is a level above the top
        let mut values = match level_top.config().lang {
            Lang::P4 => vec![program],
            Lang::Al | Lang::Il | Lang::El => entry_inputs(arena, program, ENTRY_SCRIPT, &[])?,
        };
        // Each level above the base is entered through its configured relation
        for level in self.levels[1..].iter().rev() {
            let value_spec = spec_encoded(arena, level)?;
            values = entry_inputs(arena, value_spec, &level.config().rel, &values)?;
        }
        Ok(values)
    }
}
