# ST-in-K: implementer guide

ST-in-K runs P4-SpecTec specifications on **spec-meta-k**, the AL semantics
written in K, instead of on spec-meta. The Rust side of the tower is already
built; you fill in a few marked places. This guide says which places, in what
order, and how to check each step. You do not need to know the rest of the
Rust code base.

K-in-ST is a separate task with its own guide (`k-in-st.md`). The two share no
code: do not use anything under `interface/kore/`, and K-in-ST will not use
yours.

## 1. The picture

```
   input: a P4 program + STF test (sim), or a P4 program / AL script (run-meta)
     |
+---------------------------+
| P4 specification (spec/)  |   top level            -- unchanged
+---------------------------+
| MetaKProtocol             |   boundary             -- YOU: protocol/meta_k/
|   AL calls <-> JSON text  |
+---------------------------+
| spec-meta-k (K)           |   base specification   -- YOU: spec-meta-k/
|   serve() loop, #ffiCall  |
+---------------------------+
| KEngine / KEngineState    |   one K thread per     -- YOU: engine/k/
| KRuntime / ffi.rs         |   reentry depth
+---------------------------+
```

* **Calls go down.** The P4 level calls a relation such as `Program_inst`.
  `MetaKProtocol::lower` turns it into a JSON request, and `KEngine` sends the
  request to the K thread running spec-meta-k's `serve()` loop. The JSON
  response comes back through `MetaKProtocol::raise`.
* **Host calls come up.** When spec-meta-k needs a builtin or a P4 extern, it
  calls `#ffiCall`, which lands in `host_eval` (in `ffi.rs`) on the K thread.
  `host_eval` hands the JSON request to the waiting `KEngine`. `KEngine` calls
  `upper.host_call(arena, "host_eval", &[request])`, and the tower decodes the
  request with `MetaKProtocol::host`, answers it, and encodes the answer with
  `MetaKProtocol::reply`. `KEngine` then passes the JSON answer back to the K
  thread.
* **Only JSON text crosses threads.** A Rust `Value` lives in an `Arena` on
  the calling thread. Never send one to the K thread.
* **Reentry.** A P4 extern such as `static_assert` may call the P4 level
  again. The tower then makes a new depth with a new `KEngineState`, which is
  a new K thread. Both K threads use the same `KRuntime`.

## 2. Your places

Add code only in these places; add new files inside the directories freely.

| place | what you fill |
|---|---|
| `spec-meta-k/` | spec-meta-k from #387, plus the `serve()` loop |
| `p4spectec/src/metacirc/engine/k/` | `KRuntime` (`runtime.rs`), `KEngineState` and `KEngine` (`mod.rs`), `host_eval` (`ffi.rs`) |
| `p4spectec/src/metacirc/protocol/meta_k/` | `MetaKProtocol` and `meta_k::Cache` |
| `METASQ.mk`, target `kompile` | the commands that compile spec-meta-k to a library |
| `towers/k.json`, `towers/k-p4.json`, `towers/k-p4-ok.json` | the library path (`kompiled`) if yours differs |
| `p4spectec/test-driver/suites/metacirc/run.toml`, `sim.toml` | delete the `pending` line of configuration `k` at the end |

Do not change any other file. If you think you have to, the template is
missing something: ask the template's owner to add it first.

`engine/k/ffi.rs` is the only file in the crate where `unsafe` is allowed.
Keep every `unsafe` block there.

## 3. Where #387's code goes

| #387 file | what it does | new place |
|---|---|---|
| `metacross/kast.rs`, `json.rs` | AL syntax and values to and from KAST/JSON | `protocol/meta_k/` |
| `spec-meta-k/*.k` (with `3.04.1-extern-json.k`, `3.04.2-extern-ffi.k`) | AL semantics, JSON codec, `#ffiCall` | `spec-meta-k/`, plus a `serve()` loop: `3.14-entry.k` runs one program; it must serve requests instead |
| `metacross/host.rs` | answers builtin and extern requests | not needed: the tower answers them (`upper.host_call`) |
| `metacross/ffi.rs` | `host_init`/`host_eval` for K calling Rust | `engine/k/ffi.rs`. The direction is reversed: Rust now loads K. Reuse the C string handling and the panic catching |
| `CROSS.mk` | kompile, the FFI library | `METASQ.mk` target `kompile` |
| `bin/metacross.rs`, `spec-meta-k/scripts/*` | krun drives a whole run | not needed: `metacirc run-meta` and `sim` drive |
| `metacross/sexp.rs`, `spec-meta-redex/` | PLT Redex | out of scope |

## 4. The functions to fill

Each function below returns `metacirc/unimplemented` today; its `///` comment
repeats the contract. "JSON text" always means a `text` value holding JSON in
the format of `spec-meta-k/3-execution/3.04.1-extern-json.k`:

```rust
use crate::lang::{common::source::Span, data::value::flat::{get, make}};

let value = make::text(arena, json_string, Span::default())?; // String -> Value
let json: &str = get::text(arena, &value)?;                    // Value -> &str
```

### `engine/k/runtime.rs`

* `KRuntime::load(path) -> Result<KRuntime>`: open the library with
  `libloading` and look up the entry points of the K LLVM backend's C API
  that you need. `path` comes from `kompiled` in the tower configuration. A
  missing file is already reported for you.

### `engine/k/mod.rs`

* `KEngineState::new(runtime) -> Result<KEngineState>`: start a thread that
  initializes K for itself (`kllvm_init`) and runs spec-meta-k's `serve()`
  loop. Keep a channel to it.
* `KEngine::call_program(arena, upper, effects, name, program)`: `name` is
  `Eval_AL` (from `rel` in the tower configuration), and `program` is the JSON
  text that `MetaKProtocol::encode_script` made. Send the request, then loop:
  * the thread sends a host request: call
    `upper.host_call(arena, "host_eval", &[request])`, take the one JSON text
    value it returns, and send that back to the thread;
  * the thread sends the response: return its outputs.
* `KEngine::call_rel` and `call_func`: the same loop, for the calls that
  `MetaKProtocol::lower` made.
* `clear` and `reset`: drop whatever the thread memoizes for one program.

### `engine/k/ffi.rs`

* `host_eval(request) -> response`: runs on the K thread. Read the C string,
  hand it to the `KEngine` that waits for this thread, block until the answer
  comes, and return a pointer to it that stays valid until the next call.
  `host_strlen` already works.

### `protocol/meta_k/mod.rs`

| function | input | output |
|---|---|---|
| `load(below, above)` | `above` is `PreparedLevel::Al { spec_al, .. }`, an AL specification | C: call `below.call(..)` with the JSON of `spec_al` (like `kast.rs`) and return what spec-meta-k answers |
| `encode_script(arena, script)` | `Script::Al(spec)`, the specification with a `main` added | the JSON text that `KEngine::call_program` sends to `Eval_AL` |
| `lower(arena, cache, C, call)` | `Call::Rel { name, values }` or `Call::Func { .. }` of the P4 level | a `Call` on spec-meta-k whose values are JSON text, for example `Call::Rel { name: "serve", values: [request] }` |
| `raise(arena, cache, output, call)` | the JSON response | `{"ok": [..]}` gives `Output::Rel(values)` or `Output::Func(value)`; `{"fail": null}` gives `InterpreterError::Mismatch(vec![])`; `{"error": msg}` gives `InterpreterError::Fatal(..)` |
| `host(arena, cache, "host_eval", [request])` | a JSON request of `#ffiCall` | `{"builtin": id, "targs", "args"}` gives `HostCall::Builtin`; `"extern-func"` gives `HostCall::ExternFunc`; `"extern-rel"` gives `HostCall::ExternRel` |
| `reply(arena, cache, call, eval)` | the outcome of that host call | the JSON response text, as a one-element `Vec<Value>` |

`protocol/meta.rs` (`MetaProtocol`) does the same job for spec-meta with
values instead of JSON. Read it as a worked example.

## 5. Steps

Build with `make metacirc`, which also copies the binary to `bin/metacirc`.
Before each commit, run `make fmt` and `make lint`.

1. **Compile spec-meta-k.** Bring `spec-meta-k/` over from #387, add the
   `serve()` loop, and write the `kompile` target in `METASQ.mk`.
   *Check:* `make kompile` creates the library that `towers/k.json` names.
2. **Load it.** Fill `KRuntime::load` and `KEngineState::new`.
   *Check:* `bin/metacirc run-meta towers/k.json -p testdata/spec/pos/run/<any>.watsup`
   now fails at `KEngine::call_program is not implemented yet`.
3. **Run AL scripts.** Fill `MetaKProtocol::encode_script`,
   `KEngine::call_program`, `host_eval`, `MetaKProtocol::host`, and `reply`.
   Delete the `pending` line of configuration `k` in `run.toml`.
   *Check:* `make test-metacirc-micro-k`.
4. **Run P4 programs.** Fill `MetaKProtocol::load`, `lower`, and `raise`,
   and `KEngine::call_rel` and `call_func`.
   *Check:* `bin/metacirc run-meta towers/k-p4.json -i p4c/p4include -p p4c/testdata/p4_16_samples/issue3531.p4`
   prints `passed`. This program also reenters, so it tests a second K thread.
   Then run `make test-metacirc-run-k`.
5. **Simulate.** Delete the `pending` line of configuration `k` in `sim.toml`.
   *Check:* `make test-metacirc-sim-k`.

## 6. Done when

* `run.toml` and `sim.toml` have no `pending` for configuration `k`.
* `make test-metacirc-micro-k`, `test-metacirc-run-k`, and
  `test-metacirc-sim-k` pass, using the same expected files as the native
  towers.
* `make fmt-check lint` are clean, and every changed file is in section 2.

Review covers (a) the template functions implemented, (b) the tests above
passing, and (c) a line-by-line reading of `spec-meta-k/` only.

## 7. Rust in five lines

* A `Value` is a handle into an `Arena`; pass `&mut Arena` to make values
  (`make::text`, `make::int`, ...) and `&Arena` to read them (`get::text`, ...).
* Errors are `Result`; `?` returns the error to the caller. Tower errors are
  `InterpreterError::Fatal(report)` (stop) or `InterpreterError::Mismatch`
  (try the next rule).
* `Eval::new(result, SideEffect::Pure)` wraps a result that has no effects.
* `make lint` runs clippy; fix what it reports in your files.
* `#[expect(dead_code, ...)]` on a field marks a slot that nothing reads yet;
  delete it when your code reads the field.
