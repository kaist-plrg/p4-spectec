//! Meta codec round trips registered in `suites/metacirc/metaparse.toml`
//!
//! For each registered specification, the elaborated IL and the converted AL
//! are encoded, decoded, and encoded again.
//! Decoding reconstructs omitted metadata canonically,
//! so the check compares the two encodings rather than the source syntax.

use std::{path::PathBuf, time::Instant};

use p4spectec::{
    interface::meta::shared,
    lang::{
        data::{arena::Arena, value::flat::Value},
        traits::print::Print,
    },
};

use serde::Deserialize;

use crate::harness::{self, Error, Result};

/// Registers specification sources whose IL and AL codecs must round-trip.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetaparseSuite {
    pub name: String,
    pub spec: Vec<PathBuf>,
}

/// Supplies the suites.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub suites: Vec<MetaparseSuite>,
}

/// Renders a meta value through its arena.
fn render(arena: &Arena, value: Value) -> String {
    Print::to_string(&value.view(arena))
}

/// Requires decode-then-encode to reproduce an encoding exactly.
fn check_fixpoint(
    name: &str,
    stage: &str,
    arena: &Arena,
    value_a: Value,
    value_b: Value,
) -> Result<()> {
    if render(arena, value_a) != render(arena, value_b) {
        return Err(Error::from(format!("metaparse {name}: {stage} encoding is not a fixpoint")));
    }
    Ok(())
}

/// Checks registered IL and AL codec round-trips.
pub fn run() -> Result<()> {
    let registration: Registration = harness::load("metacirc/metaparse")?;
    let start = Instant::now();
    for suite in &registration.suites {
        let mut arena = Arena::new();
        let invalid = |error: &dyn std::fmt::Display| {
            Error::from(format!("metaparse {}: {error}", suite.name))
        };
        // IL: elaborated specification
        let spec_il = p4spectec::elab(&suite.spec).map_err(|error| invalid(&error))?;
        let value_il = shared::encode::Encode::encode(&spec_il, &mut arena)
            .map_err(|error| invalid(&error))?;
        let spec_il_decoded: p4spectec::lang::il::ast::Spec =
            shared::decode::Decode::decode(&mut arena, value_il)
                .map_err(|error| invalid(&error))?;
        let value_il_again = shared::encode::Encode::encode(&spec_il_decoded, &mut arena)
            .map_err(|error| invalid(&error))?;
        check_fixpoint(&suite.name, "IL", &arena, value_il, value_il_again)?;
        // AL: natively converted specification
        let spec_al = p4spectec::algo(&suite.spec).map_err(|error| invalid(&error))?;
        let value_al = shared::encode::Encode::encode(&spec_al, &mut arena)
            .map_err(|error| invalid(&error))?;
        let spec_al_decoded: p4spectec::lang::al::ast::Spec =
            shared::decode::Decode::decode(&mut arena, value_al)
                .map_err(|error| invalid(&error))?;
        let value_al_again = shared::encode::Encode::encode(&spec_al_decoded, &mut arena)
            .map_err(|error| invalid(&error))?;
        check_fixpoint(&suite.name, "AL", &arena, value_al, value_al_again)?;
        eprintln!("metaparse: {} round-trips in IL and AL", suite.name);
    }
    eprintln!("metaparse: checked, elapsed={:.3}s", start.elapsed().as_secs_f64());
    Ok(())
}
