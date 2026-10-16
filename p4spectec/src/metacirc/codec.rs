//! Host-call arguments and results at one AL representation boundary
//!
//! Calls decode identifiers, types, and values through the shared AL codec.
//! Successful native calls become OK; exhausted alternatives become FAIL.
//! Fatal errors remain diagnostics, and every path retains the call's effects.

use crate::{
    interface::al::{self, Cache, decode::Decode, encode as encode_al},
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{Value, make},
        },
        il::ast::{Id, Typ},
    },
    runner::{Eval, ExternError, InterpreterError},
};

/// Decoding of host-call arguments.
pub(super) mod decode {
    use super::*;

    /// Decodes one checked AL representation without flattening its diagnostic.
    fn decode<T: Decode>(
        arena: &mut Arena,
        cache: &mut Cache,
        value: Value,
    ) -> Result<T, ExternError> {
        T::decode_cached(arena, cache, value)
            .map_err(|error| ExternError::from(error.into_report()))
    }

    /// Decodes the three arguments shared by builtin and extern function calls.
    pub(in crate::metacirc) fn func_args(
        arena: &mut Arena,
        cache: &mut Cache,
        values: &[Value],
    ) -> Result<(Id, Vec<Typ>, Vec<Value>), ExternError> {
        let [value_id, value_targs, value_args] = values else {
            return Err(ExternError::diagnostic_message(
                "meta function call requires three arguments",
            ));
        };
        Ok((
            decode(arena, cache, *value_id)?,
            decode(arena, cache, *value_targs)?,
            decode(arena, cache, *value_args)?,
        ))
    }

    /// Decodes an extern relation name and its input values.
    pub(in crate::metacirc) fn rel_args(
        arena: &mut Arena,
        cache: &mut Cache,
        values: &[Value],
    ) -> Result<(Id, Vec<Value>), ExternError> {
        let [value_id, value_args] = values else {
            return Err(ExternError::diagnostic_message(
                "meta relation call requires two arguments",
            ));
        };
        Ok((decode(arena, cache, *value_id)?, decode(arena, cache, *value_args)?))
    }
}

/// Encoding of native outcomes.
pub(super) mod encode {
    use super::*;

    /// Wraps a native outcome while preserving its effect even if encoding fails.
    fn result<T>(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<T>,
        text_typ: &str,
        encode: impl FnOnce(&mut Arena, &mut Cache, T) -> Result<Value, al::Error>,
    ) -> Eval<Vec<Value>, ExternError> {
        let result = match eval.result {
            // Encode successful outputs in the same arena as the decoded arguments
            Ok(output) => {
                encode(arena, cache, output).map_err(|error| ExternError::from(error.into_report()))
            }
            // An exhausted relation or function is a meta-level FAIL, not a host abort
            Err(InterpreterError::Mismatch(_)) => make::case_string! {
                arena: arena,
                shape: "FAIL",
                args: [],
                typ: text_typ,
                span: Span::default(),
            }
            .map_err(|error| ExternError::from(al::Error::from(error).into_report())),
            // Keep fatal failures outside the specification's backtracking protocol
            Err(InterpreterError::Fatal(report)) => Err(ExternError::from(report)),
        };
        Eval::new(result.map(|value| vec![value]), eval.effect)
    }

    /// Encodes a native function outcome as valres.
    pub(in crate::metacirc) fn func_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Value>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "valres", encode_al::ok_value_cached)
    }

    /// Encodes a native relation outcome as valsres.
    pub(in crate::metacirc) fn rel_result(
        arena: &mut Arena,
        cache: &mut Cache,
        eval: Eval<Vec<Value>>,
    ) -> Eval<Vec<Value>, ExternError> {
        result(arena, cache, eval, "valsres", |arena, cache, values| {
            encode_al::ok_values_cached(arena, cache, &values)
        })
    }
}
