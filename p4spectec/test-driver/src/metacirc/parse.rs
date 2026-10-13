//! Parsing check for the AL program interface
//!
//! The check parses `spec-meta` without evaluating an AL, SL, or PL program.

use p4spectec::{interface, lang::data::arena::Arena, runner::Interface};

use super::{Result, source};

/// Parses the AL meta specification through the program interface.
pub fn run() -> Result<()> {
    super::snapshot::run()?;
    let mut arena = Arena::new();
    interface::al().parse_program(&mut arena, &[], &[source("spec-meta")])?;
    eprintln!("AL interface parsing: spec-meta");
    Ok(())
}
