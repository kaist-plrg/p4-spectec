//! The calls and answers of a base's `serve()` loop
//!
//! spec-meta-k (`spec-meta-k/3-execution/3.14-entry.k`) and spec-meta-redex
//! (`spec-meta-redex/3-execution/3.16-entry.rkt`) serve the same JSON text,
//! held in `text` values (`make::text`, `get::text`): calls and answers
//! for the level above, and the requests and responses of their host calls.
//! `json`, imported as `wire`, prints and reads the values and types inside.

use super::{
    Call, HostCall, Output, Values,
    json::{self as wire, Cache, Writer},
};
use crate::{
    diagnostic::Report,
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{self as value, Value, get, make},
        },
        il::ast::Typ,
    },
    metacirc::error,
    runner::{Eval, ExternError, InterpreterError},
    util::json::json,
};
use smallvec::smallvec;
use std::borrow::Cow;

/// The engine's call that loads a specification, `load(request)`, returning C.
pub(in crate::metacirc::protocol) const LOAD: &str = "load";
/// The engine's call that serves a request under C, `serve(C, request)`.
pub(in crate::metacirc::protocol) const SERVE: &str = "serve";
/// The host call of every request of the base.
pub(in crate::metacirc::protocol) const HOST_EVAL: &str = "host_eval";

// == The level above's side

/// Wraps `call` of the AL level above as `serve(C, request)` on the base.
pub(in crate::metacirc::protocol) fn lower(
    arena: &mut Arena,
    cache: &mut Cache,
    value_ctx: Value,
    call: &Call<'_>,
) -> Result<Call<'static>, InterpreterError> {
    let request = match call {
        Call::Rel { name, values } => request::call_rel(arena, cache, name, values),
        Call::Func { name, targs, values } => request::call_func(arena, cache, name, targs, values),
    };
    let value_request = make::text(arena, request, Span::default()).map_err(ExternError::from)?;
    let values = Values::Lowered(smallvec![value_ctx, value_request]);
    Ok(match call {
        Call::Rel { .. } => Call::Rel { name: SERVE, values },
        Call::Func { .. } => Call::Func { name: SERVE, targs: Cow::Borrowed(&[]), values },
    })
}

/// Unwraps the JSON answer of the base into the output of `call`.
///
/// An OK answer gives the outputs; UNMATCH is `InterpreterError::Mismatch`,
/// and ERROR is `InterpreterError::Fatal`, as `MetaProtocol::raise` does.
pub(in crate::metacirc::protocol) fn raise(
    arena: &mut Arena,
    cache: &mut Cache,
    output: Output,
    call: &Call<'_>,
) -> Result<Output, InterpreterError> {
    let value_answer = match output {
        Output::Rel(values) => *value::get::one(&values).map_err(ExternError::from)?,
        Output::Func(value) => value,
    };
    let payload = answer::ok(arena, value_answer, call.name())?;
    Ok(match call {
        Call::Rel { .. } => Output::Rel(wire::read_values(arena, cache, &payload)?),
        Call::Func { .. } => Output::Func(wire::read_value(arena, cache, &payload)?),
    })
}

/// Decodes a request of the base for the level above.
///
/// The engine passes each request it receives as
/// `upper.host_call(arena, "host_eval", &[request])`,
/// so `name` is `host_eval` and `values` is the one JSON text request.
pub(in crate::metacirc::protocol) fn host(
    arena: &mut Arena,
    cache: &mut Cache,
    name: &str,
    values: &[Value],
) -> Result<HostCall, ExternError> {
    if name != HOST_EVAL {
        return Err(error::extern_unsupported(name).into());
    }
    let json = wire::parse(get::text(arena, value::get::one(values)?)?)?;
    request::host(arena, cache, &json)
}

/// Encodes the outcome of a host call as the JSON text answering the request,
/// the one value that the engine passes back to the base.
///
/// A mismatch is `{"fail": null}`; a fatal failure stays outside the
/// specification's backtracking, for the engine to report.
pub(in crate::metacirc::protocol) fn reply(
    arena: &mut Arena,
    cache: &mut Cache,
    eval: Eval<Output>,
) -> Eval<Vec<Value>, ExternError> {
    let Eval { result, effect } = eval;
    let response = match result {
        Ok(output) => Ok(response::ok(arena, cache, &output)),
        Err(InterpreterError::Mismatch(_)) => Ok(response::FAIL.to_owned()),
        Err(InterpreterError::Fatal(report)) => Err(ExternError::from(report)),
    };
    let result = response.and_then(|response| {
        let value = make::text(arena, response, Span::default())?;
        Ok(vec![value])
    });
    Eval::new(result, effect)
}

// == The engine's side

/// The outputs of `Eval_AL` from its answer: the value of `$main()`.
pub(in crate::metacirc::protocol) fn program_outputs(
    arena: &mut Arena,
    value_answer: Value,
    name: &str,
) -> Result<Vec<Value>, InterpreterError> {
    let payload = answer::ok(arena, value_answer, name)?;
    Ok(vec![wire::read_value(arena, &mut Cache::new(false), &payload)?])
}

/// Checks the answer to a load request, `{"ok": []}`.
pub(in crate::metacirc::protocol) fn load_answer(
    arena: &mut Arena,
    value_answer: Value,
) -> Result<(), InterpreterError> {
    match answer::ok(arena, value_answer, LOAD)? {
        json::Array(items) if items.is_empty() => Ok(()),
        payload => Err(wire::unexpected("the answer to a load", &payload).into()),
    }
}

/// The response telling the base that a host call failed with `report`.
pub(in crate::metacirc::protocol) fn error_response(report: &Report) -> String {
    let mut writer = Writer::new();
    writer.object(|writer| {
        writer.key("error");
        writer.text(&report.to_string());
    });
    writer.finish()
}

/// The request that ends the base's `serve()` loop.
pub(in crate::metacirc::protocol) fn exit_request() -> String {
    r#"{"exit":null}"#.to_owned()
}

// == Requests

/// The calls of the `serve()` loop, with fields in sorted order, which K matches literally.
pub(in crate::metacirc::protocol) mod request {
    use super::*;

    pub(in crate::metacirc::protocol) const LOAD: &str = "load";
    pub(in crate::metacirc::protocol) const EVAL_AL: &str = "eval-al";

    /// `{"args": [val...], "call-rel": id}`.
    pub(super) fn call_rel(
        arena: &Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> String {
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key("args");
            wire::values(writer, arena, cache, values);
            writer.key("call-rel");
            writer.text(name);
        });
        writer.finish()
    }

    /// `{"args": [val...], "call-func": id, "targs": [typ...]}`.
    pub(super) fn call_func(
        arena: &Arena,
        cache: &mut Cache,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> String {
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key("args");
            wire::values(writer, arena, cache, values);
            writer.key("call-func");
            writer.text(name);
            writer.key("targs");
            writer.list(targs, wire::typ);
        });
        writer.finish()
    }

    /// Decodes a request of the base:
    /// `{"builtin" | "extern-func": id, "targs": [typ...], "args": [val...]}`
    /// or `{"extern-rel": id, "args": [val...]}`.
    pub(super) fn host(
        arena: &mut Arena,
        cache: &mut Cache,
        json: &json,
    ) -> Result<HostCall, ExternError> {
        let json::Object(fields) = json else {
            return Err(wire::unexpected("a request", json));
        };
        let field = |name: &str| {
            fields.get(name).ok_or_else(|| {
                ExternError::diagnostic_message(format!("the base's request has no field {name}"))
            })
        };
        let values = wire::read_values(arena, cache, field("args")?)?;
        match (fields.get("builtin"), fields.get("extern-func"), fields.get("extern-rel")) {
            (Some(id), None, None) => Ok(HostCall::Builtin {
                id: wire::read_id(id)?,
                targs: wire::read_typs(field("targs")?)?,
                values,
            }),
            (None, Some(id), None) => Ok(HostCall::ExternFunc {
                id: wire::read_id(id)?,
                targs: wire::read_typs(field("targs")?)?,
                values,
            }),
            (None, None, Some(id)) => Ok(HostCall::ExternRel { id: wire::read_id(id)?, values }),
            _ => Err(wire::unexpected("a builtin, extern-func, or extern-rel request", json)),
        }
    }
}

// == Responses and answers

/// The responses to the base's requests.
mod response {
    use super::*;

    pub(super) const FAIL: &str = r#"{"fail":null}"#;

    /// `{"ok": val}` for a function, `{"ok": [val...]}` for a relation.
    pub(super) fn ok(arena: &Arena, cache: &mut Cache, output: &Output) -> String {
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key("ok");
            match output {
                Output::Func(value) => wire::value(writer, arena, cache, *value),
                Output::Rel(values) => wire::values(writer, arena, cache, values),
            }
        });
        writer.finish()
    }
}

/// The answers of the base's `serve()` loop.
mod answer {
    use super::*;

    /// Selects the OK payload of the answer to `name`;
    /// `fail` allows another candidate, `error` aborts.
    pub(super) fn ok(
        arena: &Arena,
        value_answer: Value,
        name: &str,
    ) -> Result<json, InterpreterError> {
        let json = wire::parse(get::text(arena, &value_answer).map_err(ExternError::from)?)?;
        if let json::Object(fields) = &json
            && let [(key, payload)] = &fields.iter().collect::<Vec<_>>()[..]
        {
            match (key.as_str(), payload) {
                ("ok", _) => return Ok((*payload).clone()),
                ("fail", json::Null) => {
                    let report = *error::execution_unmatched(name);
                    return Err(InterpreterError::Mismatch(vec![report]));
                }
                ("error", json::String(_)) => {
                    return Err(InterpreterError::Fatal(error::execution_failed(name)));
                }
                _ => {}
            }
        }
        Err(wire::unexpected("an answer", &json).into())
    }
}
