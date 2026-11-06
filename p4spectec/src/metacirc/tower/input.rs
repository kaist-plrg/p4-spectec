//! The program above the top level, and the inputs of the native base's entry
//!
//! `Tower::parse_input` reads the program in the language the top level accepts:
//! a P4 program, an AL or IL script, or a K definition with its initial term,
//! which the K library at the top reads (`Top::K`).
//!
//! The meta specification's entry relations, such as `Eval_AL_rel`, take a script,
//! the name of the script's entry, and the entry's inputs as meta values.
//! For `towers/al-p4.json`, the base calls
//! ``Eval_AL_rel: <P4 spec as AL> |- "Program_inst" `( <program as val> `) : val*``.
//! Each level above the base wraps the inputs of the level above it this way,
//! through the protocol of the boundary below it (`Protocol::encode_entry`),
//! with the level encoded in the language of the level consuming it.
//! A program above the top is one more level: an AL or IL script is entered
//! at its function `main` with no inputs, so the level below it uses
//! `Eval_AL_func` or `Eval_IL_func`; a K definition is entered with its initial
//! term and step bound, which spec-k's `Krun` rewrites.
//!
//! `Tower::print_output` prints what the base's entry gave, where the top
//! prints it: the run of a K definition, as krun prints it.

use super::super::{Result, config::Lang, error, protocol::Entered};
use super::{Input, Kore, Script, Top, Tower};
use crate::{
    interface::p4::parse::parse_file,
    lang::{
        common::{prim::num::Natural, source::Span},
        data::{
            arena::Arena,
            typ,
            value::flat::{self as value, Value},
        },
    },
    runner::Extern,
};
use std::path::PathBuf;

/// The entry of an AL or IL script above the top of a tower.
const ENTRY_SCRIPT: &str = "main";

/// The entry of a K definition above spec-k, which has none by name:
/// spec-k rewrites the initial term.
const ENTRY_KORE: &str = "";

// == Tower input

impl<Ext: Extern> Tower<Ext> {
    /// Reads the program at `paths` in the language the top level accepts.
    ///
    /// `depth` bounds the rewriting steps of a K definition, as `krun --depth` does,
    /// and applies to no other input.
    pub fn parse_input(
        &self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
        depth: Option<u64>,
    ) -> Result<Input> {
        if paths.is_empty() {
            return Err(error::input_invalid("a tower requires at least one program path"));
        }
        let lang_top = self.lang_top();
        if depth.is_some() && lang_top != Lang::Kore {
            return Err(error::input_invalid("a step bound applies only to a KORE input"));
        }
        match lang_top {
            Lang::Al => Ok(Input::Script(Script::Al(crate::algo(paths)?))),
            Lang::Il => Ok(Input::Script(Script::Il(crate::elab(paths)?))),
            // P4 inputs contain exactly one translation unit
            Lang::P4 => {
                let [path] = paths else {
                    return Err(error::input_invalid(
                        "a P4 top level requires exactly one program path",
                    ));
                };
                let program = parse_file(arena, includes, path).map_err(|e| e.into_report())?;
                Ok(Input::P4(program))
            }
            // The K library at the top reads the definition whose hooks it answers
            Lang::Kore => {
                let Some(Top::K(library)) = self.base.upper().map(|upper| &upper.top) else {
                    return Err(error::input_invalid("a KORE input requires spec-k as the base"));
                };
                let program = library
                    .parse_program(arena, paths)
                    .map_err(|e| e.into_report())?;
                let (definition, init) = value::get::tuple(arena, &program)
                    .and_then(value::get::two)
                    .map(|(definition, init)| (*definition, *init))
                    .map_err(|e| error::input_invalid(e.to_string()))?;
                Ok(Input::Kore(Kore { definition, init, depth }))
            }
        }
    }

    /// Encodes an input as the inputs of the native base's entry relation.
    ///
    /// Encoding errors remain separate from the subsequent evaluation.
    pub fn encode_input(&self, arena: &mut Arena, input: &Input) -> Result<Vec<Value>> {
        let lang_top = self.lang_top();
        // A P4 program is the top level's input; a script or a K definition is
        // a program above the top
        let (mut values, program) = match (input, lang_top) {
            (Input::P4(program), Lang::P4) => (vec![*program], None),
            (Input::Script(script @ Script::Al(_)), Lang::Al)
            | (Input::Script(script @ Script::Il(_)), Lang::Il) => {
                (vec![], Some((script.entered(), ENTRY_SCRIPT)))
            }
            (Input::Kore(Kore { definition, init, depth }), Lang::Kore) => {
                let value_depth = depth_value(arena, *depth)?;
                (vec![*init, value_depth], Some((Entered::Kore(*definition), ENTRY_KORE)))
            }
            _ => {
                let message = format!("the top level accepts {lang_top:?}, not this input");
                return Err(error::input_invalid(message));
            }
        };
        // Each level above the base, and a program above the top,
        // is entered through the protocol of the boundary below it
        let entries = self.levels[1..]
            .iter()
            .map(|level| (level.entered(), level.config().rel.as_str()))
            .chain(program)
            .collect::<Vec<_>>();
        let protocols = self.base.upper().map_or(&[][..], |upper| &upper.protocols);
        for ((above, name), protocol) in entries.into_iter().zip(protocols).rev() {
            values = protocol.encode_entry(arena, above, name, &values)?;
        }
        Ok(values)
    }

    /// Prints `outputs`, what the base's entry gave, if the top prints it:
    /// the run of a K definition, as krun prints it.
    pub fn print_output(&self, arena: &Arena, outputs: &[Value]) -> Result<Option<String>> {
        let Some(Top::K(library)) = self.base.upper().map(|upper| &upper.top) else {
            return Ok(None);
        };
        let runres = value::get::one(outputs).map_err(|e| error::input_invalid(e.to_string()))?;
        let text = library
            .print_runres(arena, *runres)
            .map_err(|e| e.into_report())?;
        Ok(Some(text))
    }

    /// Returns the language the top level accepts.
    fn lang_top(&self) -> Lang {
        self.levels
            .last()
            .expect("validated nonempty tower")
            .config()
            .lang
    }
}

/// Encodes a step bound as spec-k's `nat?`.
fn depth_value(arena: &mut Arena, depth: Option<u64>) -> Result<Value> {
    let value_depth = depth
        .map(|depth| value::make::nat(arena, Natural::from(depth), Span::default()))
        .transpose()
        .map_err(|e| error::input_invalid(e.to_string()))?;
    let typ_depth = typ::make::opt(typ::make::nat());
    value::make::opt(arena, typ_depth.node.into(), value_depth, Span::default())
        .map_err(|e| error::input_invalid(e.to_string()))
}
