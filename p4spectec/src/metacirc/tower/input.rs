//! Inputs of the native base's entry relation, encoded from the top level down
//!
//! The meta specification's entry relations, such as `Eval_AL_rel`, take a script,
//! the name of the script's entry, and the entry's inputs as meta values.
//! For `towers/al-p4.json`, the base calls
//! ``Eval_AL_rel: <P4 spec as AL> |- "Program_inst" `( <program as val> `) : val*``.
//! Each level above the base wraps the inputs of the level above it this way,
//! through the protocol of the boundary below it (`Protocol::encode_entry`),
//! with the level encoded in the language of the level consuming it.
//! An AL or IL script is one more level, above the top, entered at its function `main`
//! with no inputs, so the level below it uses `Eval_AL_func` or `Eval_IL_func`.

use super::super::{Result, config::Lang, error, protocol::Protocol};
use super::{Input, Script, Tower};
use crate::{
    lang::data::{arena::Arena, value::flat::Value},
    runner::Extern,
};

/// The entry of an AL or IL script above the top of a tower.
const ENTRY_SCRIPT: &str = "main";

// == Tower input

impl<Ext: Extern> Tower<Ext> {
    /// Encodes an input as the inputs of the native base's entry relation.
    ///
    /// Encoding errors remain separate from the subsequent evaluation.
    pub fn encode_input(&self, arena: &mut Arena, input: &Input) -> Result<Vec<Value>> {
        let lang_top = self
            .levels
            .last()
            .expect("validated nonempty tower")
            .config()
            .lang;
        // A P4 program is the top level's input; a script is a level above the top
        let (mut values, script) = match (input, lang_top) {
            (Input::P4(program), Lang::P4) => (vec![*program], None),
            (Input::Script(script @ Script::Al(_)), Lang::Al)
            | (Input::Script(script @ Script::Il(_)), Lang::Il) => {
                (vec![], Some((script.entered(), ENTRY_SCRIPT)))
            }
            // A KORE input enters spec-k through Krun, with a step bound
            (_, Lang::Kore) => {
                return Err(error::input_invalid("a KORE top level runs through krun"));
            }
            _ => {
                let message = format!("the top level accepts {lang_top:?}, not this input");
                return Err(error::input_invalid(message));
            }
        };
        // Each level above the base, and a script above the top,
        // is entered through the protocol of the boundary below it
        let entries = self.levels[1..]
            .iter()
            .map(|level| (level.entered(), level.config().rel.as_str()))
            .chain(script)
            .collect::<Vec<_>>();
        for ((above, name), protocol) in entries.into_iter().zip(&self.protocols).rev() {
            values = protocol.encode_entry(arena, above, name, &values)?;
        }
        Ok(values)
    }
}
