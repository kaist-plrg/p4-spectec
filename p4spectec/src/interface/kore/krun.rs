//! Running a kompiled K definition with the K specification
//!
//! As the LLVM backend's interpreter does with krun's initial term:
//! `Krun: kdefinition |- kore nat? : runres` evaluates the term
//! and rewrites it until no rule applies, or up to a number of steps.

use std::{path::Path, rc::Rc, time::Instant};

use crate::lang::{
    common::{prim::num::Natural, source::Span},
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Config, NullExtern, RunError, Runner, Spec};

use super::KoreInterface;

/// A definition and an initial term read for a run.
pub struct Krun {
    pub arena: Arena,
    runner: Runner<KoreInterface, NullExtern>,
    /// `Krun`'s inputs: the definition, the initial term, the step limit.
    inputs: [Value; 3],
    /// The number of axioms in the definition.
    pub axioms: usize,
    /// Seconds to read the definition and the initial term.
    pub secs_load: f64,
}

impl Krun {
    /// Prepares the specification and reads a definition and an initial term.
    pub fn load(
        spec: Spec,
        config: Config,
        path_definition: &Path,
        path_term: &Path,
        depth: Option<u64>,
    ) -> Result<Self, RunError> {
        let mut arena = Arena::new();
        let interface = super::super::kore();
        let mut runner = Runner::new(&mut arena, spec, config, interface, NullExtern)
            .map_err(RunError::Build)?;

        // Read the definition, then the initial term against its symbols
        let time = Instant::now();
        let (value_definition, axioms) = runner
            .interface
            .load_definition(&mut arena, path_definition)
            .map_err(RunError::Parse)?;
        let value_term = runner
            .parse_program(&mut arena, &[], &[path_term.to_path_buf()])
            .map_err(RunError::Parse)?;
        let value_depth = depth_value(&mut arena, depth).map_err(RunError::Parse)?;
        let secs_load = time.elapsed().as_secs_f64();

        let inputs = [value_definition, value_term, value_depth];
        Ok(Self { arena, runner, inputs, axioms, secs_load })
    }

    /// Runs `Krun`; its `runres`.
    pub fn run(&mut self) -> Result<Value, RunError> {
        let values = self
            .runner
            .eval_rel(&mut self.arena, "Krun", &self.inputs)
            .result
            .map_err(RunError::Eval)?;
        let [result] = values[..] else { unreachable!("Krun has one output") };
        Ok(result)
    }
}

/// `nat?`: the step limit, if any.
fn depth_value(
    arena: &mut Arena,
    depth: Option<u64>,
) -> Result<Value, crate::runner::InterfaceError> {
    let span = Span::default();
    let to_error = |error: crate::lang::data::value::ValueError| {
        crate::runner::InterfaceError::from(crate::interface::builtin::BuiltinError::from(error))
    };
    let value_depth = depth
        .map(|n| {
            let n = Natural::try_from(num_bigint::BigInt::from(n)).expect("a u64 is natural");
            value::make::nat(arena, n, span)
        })
        .transpose()
        .map_err(to_error)?;
    let typ = Rc::new(typ::make::opt(typ::make::nat()).node);
    value::make::opt(arena, typ, value_depth, span).map_err(to_error)
}
