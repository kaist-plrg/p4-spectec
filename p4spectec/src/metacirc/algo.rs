//! Algorithmic conversion through the IL-to-AL specification
//!
//! `algo` elaborates the input specification and encodes its IL,
//! then runs `Algo_spec` of the IL-to-AL specification on it and decodes the AL.
//! The IL-to-AL specification itself is converted by the native `algo` pass;
//! that bootstrap is the only native conversion involved.
//! A missing derivation means the input has no AL, not that execution failed.

use std::path::{Path, PathBuf};

use super::{
    ExecutionMode, Result,
    error::{algo_rejected, input_invalid},
};
use crate::{
    interface::{
        self,
        meta::shared::{decode::Decode, encode::Encode},
    },
    lang::{al::ast as al, data::arena::Arena},
    pass::structure,
    runner::{self, InterpreterError, NullExtern, Runner, Spec},
};

/// Converts specification paths to AL by executing the IL-to-AL specification.
pub fn algo(
    paths: &[PathBuf],
    path_algo: &Path,
    mode: ExecutionMode,
    options: runner::Config,
) -> Result<al::Spec> {
    // Bootstrap the IL-to-AL specification with the native pass
    let spec_algo_al = crate::algo(&[path_algo.to_path_buf()])?;
    let spec_algo = match mode {
        ExecutionMode::Al => Spec::Al(spec_algo_al),
        ExecutionMode::Sl => Spec::Sl(structure::convert(spec_algo_al, true)?),
    };
    let mut arena = Arena::new();
    let mut runner = Runner::new(&mut arena, spec_algo, options, interface::al(), NullExtern)?;
    // Encode the elaborated input in the runner's arena
    let spec_il = crate::elab(paths)?;
    let value_il = spec_il
        .encode(&mut arena)
        .map_err(|error| error.into_report())?;
    let values = match runner.eval_rel(&mut arena, "Algo_spec", &[value_il]).result {
        Ok(values) => values,
        // No derivation: the specification has no algorithmic form
        Err(InterpreterError::Mismatch(_)) => {
            return Err(algo_rejected(paths));
        }
        Err(InterpreterError::Fatal(report)) => return Err(report),
    };
    let [value_al] = values.as_slice() else {
        return Err(input_invalid("Algo_spec must produce one AL specification"));
    };
    al::Spec::decode(&mut arena, *value_al).map_err(|error| error.into_report())
}
