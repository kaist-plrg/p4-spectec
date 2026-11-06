//! spec-k's protocol: a level written in K above spec-k
//!
//! spec-k, the K semantics written in P4-SpecTec, interprets KORE.
//! A K-in-ST tower is a single level: the K definition is the program above
//! the top, as a script is above spec-meta. `encode_entry` gives `Krun` the
//! definition, the initial term, and the step bound, and `host` carries
//! spec-k's host call, its extern relation `Call_hook`, up to the definition,
//! whose K library answers it (`Top::K`).
//! Stacking a level over a K definition, such as ST-in-K-in-ST, would use the rest:
//! `load` prepares the definition once (`Prepare`, a split of spec-k's `Start`),
//! `lower` runs one request as a K term to the end (`Run`),
//! and each depth keeps the configuration of the K level it runs.
//! That needs `PreparedLevel::Kore` and per-depth K state in `TowerInterpState`;
//! neither exists until stacking is implemented.

use crate::metacirc::protocol::{Below, Call, Entered, HostCall, Output, Protocol};
use crate::{
    lang::{
        common::source::Span,
        data::{arena::Arena, value::flat::Value},
    },
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError},
};

/// What a level stacked over spec-k needs, which is not implemented.
const STACKING: &str = "stacking a level over a K definition";

// == Protocol

/// The protocol of spec-k toward the K definition above it.
pub(in crate::metacirc) struct ProtocolKSt;

impl Protocol for ProtocolKSt {
    fn new(_cache: bool) -> Self {
        Self
    }

    /// Prepares the K definition above once, as C (`Prepare`).
    fn load(_below: &mut dyn Below, _above: &PreparedLevel) -> Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Encodes the inputs of `Krun`, spec-k's entry, into the K definition above:
    /// the definition, then `values`, the initial term and the step bound.
    ///
    /// A K definition has no named entry, so `name` is unused;
    /// spec-k rewrites the initial term.
    fn encode_entry(
        _arena: &mut Arena,
        above: Entered<'_>,
        _name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        let Entered::Kore(definition) = above else {
            return Err(error::config_invalid("spec-k enters only a K definition"));
        };
        Ok(std::iter::once(definition)
            .chain(values.iter().copied())
            .collect())
    }

    /// Runs `call`, a request of the K level above, as a K term under C (`Run`).
    fn lower(
        &mut self,
        _arena: &mut Arena,
        _value_ctx: Value,
        _call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Reads the final configuration of a `Run` as the output of `call`.
    fn raise(
        &mut self,
        _arena: &mut Arena,
        _output: Output,
        _call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Passes spec-k's extern relation `Call_hook` up as it is,
    /// since the K definition above reads spec-k's values; rejects any other.
    fn host(
        &mut self,
        _arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        match name {
            "Call_hook" => Ok(HostCall::ExternRel {
                id: crate::phrase!(node: name.into(), span: Span::default()),
                values: values.to_vec(),
            }),
            _ => Err(error::extern_unsupported(name).into()),
        }
    }

    /// Gives the outputs of the extern relation as they are.
    fn reply(
        &mut self,
        _arena: &mut Arena,
        _call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        let Eval { result, effect } = eval;
        Eval::new(result.map(Output::into_rel).map_err(Into::into), effect)
    }

    fn clear(&mut self) {}
}
