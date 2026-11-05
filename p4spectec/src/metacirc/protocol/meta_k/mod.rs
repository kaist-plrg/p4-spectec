//! spec-meta-k's protocol: an AL level above a base written in K
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base,
//! on a `KEngine`. Every payload crossing this boundary is JSON text
//! held in a `text` value (`make::text`, `get::text`),
//! in the format of `spec-meta-k/3-execution/3.04.1-extern-json.k`;
//! `wire` prints and reads it, and `spec` prints an AL specification.
//! `load` passes the AL specification above to the base as `{"load": spec}`,
//! and the base returns C, its handle of the loaded specification;
//! `encode_entry` encodes the inputs of the base's entry.
//! `lower` wraps a call of the level above as a `call-rel` or `call-func` request
//! to spec-meta-k's `serve()` loop, the engine's `serve` with C,
//! and `raise` unwraps its answer;
//! `host` and `reply` decode a `#ffiCall` request and encode its outcome.

mod spec;
mod wire;

use super::{Below, Call, HostCall, Output, Protocol, Values};
use crate::{
    diagnostic::Report,
    lang::{
        al::ast as al,
        common::source::Span,
        data::{
            arena::Arena,
            notation::{AtomPhrase, flat::Mixop},
            value::flat::{self as value, Value, get, make},
        },
        il::ast::Typ,
    },
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError},
    util::json::json,
};
use smallvec::smallvec;
use std::{borrow::Cow, collections::HashMap};
use wire::Writer;

// == Protocol

/// The protocol of spec-meta-k toward the AL specification above it.
pub(in crate::metacirc) struct MetaKProtocol;

impl MetaKProtocol {
    /// The engine's call that loads a specification, `load(request)`, returning C.
    pub(in crate::metacirc) const LOAD: &str = "load";
    /// The engine's call that serves a request under C, `serve(C, request)`.
    pub(in crate::metacirc) const SERVE: &str = "serve";
    /// The host call of every `#ffiCall` request.
    pub(in crate::metacirc) const HOST_EVAL: &str = "host_eval";
    /// The entry calling a relation, `Eval_AL_rel(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_REL: &str = "Eval_AL_rel";
    /// The entry calling a function, `Eval_AL_func(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_FUNC: &str = "Eval_AL_func";
}

/// The codec state of a boundary above spec-meta-k.
///
/// Notation is converted once per spelling if enabled;
/// every reentry depth shares it, and `clear` runs between programs.
pub(in crate::metacirc) struct Cache {
    enabled: bool,
    /// A value's notation, as its atom rows print.
    mixops_out: HashMap<Mixop, String>,
    /// A notation by its atom rows, joined as `[Vec<AtomPhrase>]::to_mixop` prints them.
    mixops_in: HashMap<String, Mixop>,
    /// A struct field's atom by its spelling.
    atoms_in: HashMap<String, AtomPhrase>,
}

impl Cache {
    fn new(enabled: bool) -> Self {
        Self {
            enabled,
            mixops_out: HashMap::new(),
            mixops_in: HashMap::new(),
            atoms_in: HashMap::new(),
        }
    }

    /// Clears the memoized conversions between programs.
    pub(in crate::metacirc) fn clear(&mut self) {
        self.mixops_out.clear();
        self.mixops_in.clear();
        self.atoms_in.clear();
    }
}

impl Protocol for MetaKProtocol {
    type Cache = Cache;

    fn new_cache(&self, enabled: bool) -> Cache {
        Cache::new(enabled)
    }

    /// Loads the AL specification of `above` into spec-meta-k, returning C.
    ///
    /// C is the value the base returns for `{"load": spec}`, kept for the whole program.
    fn load(
        &self,
        below: &mut dyn Below,
        above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        let PreparedLevel::Al { spec_al, .. } = above else {
            let message = "spec-meta-k loads only a level passed to it as AL";
            return Err(ExternError::diagnostic_message(message).into());
        };
        let request = request::spec(request::LOAD, spec_al);
        let value_request =
            make::text(below.arena(), request, Span::default()).map_err(ExternError::from)?;
        let call = Call::Rel { name: Self::LOAD, values: Values::Borrowed(&[value_request]) };
        let values = below.call(call)?.into_rel();
        Ok(*value::get::one(&values).map_err(ExternError::from)?)
    }

    /// Encodes the inputs of the base's entry as JSON text.
    ///
    /// `Eval_AL_rel` and `Eval_AL_func` take the AL specification of `above`,
    /// the name of its configured relation, and `values`, its inputs:
    /// `[def...]`, `"id"`, and `[val...]`, which `KEngine::call_rel` passes on.
    fn encode_entry(
        &self,
        arena: &mut Arena,
        above: &PreparedLevel,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        let PreparedLevel::Al { spec_al, .. } = above else {
            let message = "spec-meta-k enters only a level passed to it as AL";
            return Err(error::config_invalid(message));
        };
        let mut writer = Writer::new();
        spec::spec(&mut writer, spec_al);
        let text_spec = writer.finish();
        let mut writer = Writer::new();
        writer.text(&above.config().rel);
        let text_id = writer.finish();
        let mut writer = Writer::new();
        wire::values(&mut writer, arena, &mut Cache::new(false), values);
        let text_args = writer.finish();
        [text_spec, text_id, text_args]
            .into_iter()
            .map(|text| {
                make::text(arena, text, Span::default())
                    .map_err(|error| error::input_invalid(error.to_string()))
            })
            .collect()
    }

    /// Wraps `call` of the AL level above as `serve(C, request)` on spec-meta-k.
    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        let request = match call {
            Call::Rel { name, values } => request::call_rel(arena, cache, name, values),
            Call::Func { name, targs, values } => {
                request::call_func(arena, cache, name, targs, values)
            }
        };
        let value_request =
            make::text(arena, request, Span::default()).map_err(ExternError::from)?;
        let values = Values::Lowered(smallvec![value_ctx, value_request]);
        Ok(match call {
            Call::Rel { .. } => Call::Rel { name: Self::SERVE, values },
            Call::Func { .. } => {
                Call::Func { name: Self::SERVE, targs: Cow::Borrowed(&[]), values }
            }
        })
    }

    /// Unwraps the JSON answer of spec-meta-k into the output of `call`.
    ///
    /// An OK answer gives the outputs; UNMATCH is `InterpreterError::Mismatch`,
    /// and ERROR is `InterpreterError::Fatal`, as `MetaProtocol::raise` does.
    fn raise(
        &self,
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

    /// Decodes a `#ffiCall` request of spec-meta-k for the level above.
    ///
    /// `KEngine` passes each request it receives from `host_eval` as
    /// `upper.host_call(arena, "host_eval", &[request])`,
    /// so `name` is `host_eval` and `values` is the one JSON text request.
    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        if name != Self::HOST_EVAL {
            return Err(error::extern_unsupported(name).into());
        }
        let json = wire::parse(get::text(arena, value::get::one(values)?)?)?;
        request::host(arena, cache, &json)
    }

    /// Encodes the outcome of a host call as the JSON text answering `#ffiCall`,
    /// the one value that `KEngine` passes back to `host_eval`.
    ///
    /// A mismatch is `{"fail": null}`; a fatal failure stays outside the
    /// specification's backtracking, for `KEngine` to report.
    fn reply(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        _call: &HostCall,
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
}

// == The engine's side

impl MetaKProtocol {
    /// The requests that run spec-meta-k's entry `name` on `values`,
    /// the JSON texts `spec`, `id`, and `args` that `encode_entry` made.
    ///
    /// Both entries load first, `{"load": spec}`; `Eval_AL_rel` then calls
    /// `{"args": args, "call-rel": id}`, and `Eval_AL_func`
    /// `{"args": args, "call-func": id, "targs": []}`.
    pub(in crate::metacirc) fn entry_requests(
        arena: &Arena,
        name: &str,
        values: &[Value],
    ) -> Result<[String; 2], InterpreterError> {
        let [value_spec, value_id, value_args] = values else {
            let message = format!("{name} of spec-meta-k takes a spec, an id, and inputs");
            return Err(ExternError::diagnostic_message(message).into());
        };
        let text_spec = get::text(arena, value_spec).map_err(ExternError::from)?;
        let text_id = get::text(arena, value_id).map_err(ExternError::from)?;
        let text_args = get::text(arena, value_args).map_err(ExternError::from)?;
        // Load the specification, as $load_ctx($empty_ctx, spec) does
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key(request::LOAD);
            writer.json(text_spec);
        });
        let request_load = writer.finish();
        // Call the entry's relation or function on the loaded specification
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key("args");
            writer.json(text_args);
            if name == Self::ENTRY_FUNC {
                writer.key("call-func");
                writer.json(text_id);
                writer.key("targs");
                writer.json("[]");
            } else {
                writer.key("call-rel");
                writer.json(text_id);
            }
        });
        Ok([request_load, writer.finish()])
    }

    /// The outputs of spec-meta-k's entry `name` from the answer to its call:
    /// the relation's values for `Eval_AL_rel`, the function's value for `Eval_AL_func`.
    pub(in crate::metacirc) fn entry_outputs(
        arena: &mut Arena,
        name: &str,
        value_answer: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        let payload = answer::ok(arena, value_answer, name)?;
        let mut cache = Cache::new(false);
        if name == Self::ENTRY_FUNC {
            Ok(vec![wire::read_value(arena, &mut cache, &payload)?])
        } else {
            Ok(wire::read_values(arena, &mut cache, &payload)?)
        }
    }

    /// Checks the answer to a load request, `{"ok": []}`.
    pub(in crate::metacirc) fn load_answer(
        arena: &mut Arena,
        value_answer: Value,
    ) -> Result<(), InterpreterError> {
        match answer::ok(arena, value_answer, Self::LOAD)? {
            json::Array(items) if items.is_empty() => Ok(()),
            payload => Err(wire::unexpected("the answer to a load", &payload).into()),
        }
    }

    /// The response telling spec-meta-k that a host call failed with `report`.
    pub(in crate::metacirc) fn error_response(report: &Report) -> String {
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key("error");
            writer.text(&report.to_string());
        });
        writer.finish()
    }

    /// The request that ends spec-meta-k's `serve()` loop.
    pub(in crate::metacirc) fn exit_request() -> String {
        r#"{"exit":null}"#.to_owned()
    }
}

// == Requests

/// The calls of the `serve()` loop, with fields in sorted order, which K matches literally.
mod request {
    use super::*;

    pub(super) const LOAD: &str = "load";

    /// `{key: spec}`.
    pub(super) fn spec(key: &str, spec_al: &al::Spec) -> String {
        let mut writer = Writer::new();
        writer.object(|writer| {
            writer.key(key);
            spec::spec(writer, spec_al);
        });
        writer.finish()
    }

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

    /// Decodes a request of spec-meta-k:
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
                ExternError::diagnostic_message(format!(
                    "spec-meta-k's request has no field {name}"
                ))
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

/// The responses to spec-meta-k's requests.
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

/// The answers of spec-meta-k's `serve()` loop.
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
