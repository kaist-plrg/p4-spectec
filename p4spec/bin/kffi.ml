(* The K side of the K<->OCaml wire, reached through the C FFI.
 *
 * This is the in-process equivalent of `spectec-boot extern` (boot.ml's
 * `extern_command`): same JSON wire format, same dispatch, but answered from a
 * long-lived process rather than a fresh one per call.  The K side reaches it
 * through `spec-meta-k/ffi/shim.c`, which presents a plain C ABI outwards and
 * calls in here via `caml_callback`:
 *
 *   K rules  --#ffiCall-->  shim.c  --caml_callback-->  ml_init / ml_eval
 *
 * `Callback.register` is what makes these reachable by name: compiled OCaml
 * symbols are name-mangled with a build-dependent stamp and use OCaml's
 * internal calling convention, so C cannot call them directly.
 *
 * There are two entry points, and their order matters.  `ml_init` is called
 * once, with the target spec path, before anything else; it builds the runner.
 * `ml_eval` then answers requests against that runner and takes only the
 * request.
 *
 * Two things follow from being long-lived, and they are the whole point:
 *
 *   - The runner is built once.  Under `#system` each extern call
 *     re-elaborated the entire lower spec (~1.2 s); here it happens at
 *     startup, once.
 *   - Builtin state now persists across calls.  `Builtin.Call.Make (...) ()` is
 *     instantiated once (`interface.ml`), so `$fresh_typeId`'s counter --
 *     which under `#system` was reborn with every process, and therefore
 *     returned the same id every time -- now actually advances.
 *
 * The wire format is unchanged and documented in full at the head of
 * `p4spec/lib/interface/spectec/ali/extern.ml` and
 * `spec-meta-k/al/4-extern-json.k`. *)

open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error

(* Tune GC for the allocation-heavy meta-circular interpreter.
   Kept identical to boot.ml's so results stay comparable; note that unlike
   there, these settings now persist for the whole K run rather than for one
   short-lived process. *)

let () =
  Gc.set
    {
      (Gc.get ()) with
      Gc.minor_heap_size = 16 * 1024 * 1024;
      Gc.space_overhead = 2000;
    }

(* The one runner, built once at startup.

   `ml_init` is called by the C shim's `ml_init_c`, which K reaches through
   `initFFI()` at the head of the `<k>` cell -- before anything that could
   reach a builtin or an extern.  So by the time `eval` runs, this ref holds
   the runner for the target spec, and `eval` needs no spec path of its own.

   A run names exactly one target spec (the `$SPEC` configuration variable, in
   K's `<specdir>` cell), so one runner suffices.  Building it also calls
   `Interface.init`, which installs the `$print_` unparser for that spec --
   `Interface.P4.unparser` being a process-global ref, this is precisely why
   there is one runner and not a table of them.

   `Pass.structure` is itself already process-memoized (`pass/pass.ml`); what
   building once saves is the generative `P4.Make ()` plus `Runner_target.init`
   -- the ~1.2 s that the old per-call extern path paid every time. *)

let runner : (module RUNNER) option ref = ref None

let get_runner () : (module RUNNER) =
  match !runner with
  | Some runner -> runner
  | None ->
      raise
        (Interface.SpecTec_AL.Extern_error
           "runner not initialized: ml_init_c was never called")

(* One-time initialization, called by the shim's `ml_init_c`.

   Unlike `ml_eval` this is *not* total: it runs before any spec-level work,
   so a failure here (a bad spec path, an elaboration error) is a defect in
   the invocation rather than something a run could carry on past.  Letting it
   escape aborts the interpreter, which is the right outcome -- there is no
   configuration worth dumping yet. *)

let ml_init (path_spec : string) : unit =
  runner :=
    Some
      (Backend_boot.Build.build_target ~cache:true
         { layer = { specdir = path_spec; rel = "" }; interface = P4_interface })

(* Dispatch, mirroring boot.ml's `extern_command` exactly.

   `$print_` is the one builtin needing more than a registry lookup: it
   unparses against the spec.  boot.ml calls `Interface.P4.init` for it; here
   `ml_init` has already done that as part of `Runner_target.init`, so nothing
   extra is needed at call time. *)

let eval (str_request : string) : Yojson.Safe.t =
  let json_request = Yojson.Safe.from_string str_request in
  let request = Interface.SpecTec_AL.request_of_json json_request in
  match request with
  | Builtin (name, targs, args) ->
      let value =
        Interface.P4.call_builtin
          (fun _ -> ())
          Util.Source.(name $ no_region)
          targs args
      in
      Interface.SpecTec_AL.json_of_response value
  | ExternFunc (name, targs, args) -> (
      let (module Runner) = get_runner () in
      match Runner.Interp.eval_func name targs args with
      | Pass value -> Interface.SpecTec_AL.json_of_response value
      | Fail (at, msg) ->
          Format.eprintf "extern func %s failed: %s\n%!" name
            (string_of_error at msg);
          Interface.SpecTec_AL.json_of_response_fail ())
  | ExternRel (name, args) -> (
      let (module Runner) = get_runner () in
      match Runner.Interp.eval_rel name args with
      | Pass values -> Interface.SpecTec_AL.json_of_response_multi values
      | Fail (at, msg) ->
          Format.eprintf "extern rel %s failed: %s\n%!" name
            (string_of_error at msg);
          Interface.SpecTec_AL.json_of_response_fail ())

(* `ml_eval` must be TOTAL.  An OCaml exception escaping through
   `caml_callback` has no handler in the C shim and aborts the whole K
   interpreter with no configuration dump -- strictly worse than any error
   value, since the run's state is lost.  So every handler boot.ml has (which
   there `exit 1`s) returns a value here instead:

     {"error": "<diagnostic>"}

   This is a *third* reply shape, and the K side deliberately has no decoding
   rule for it.  That preserves the existing invariant: `{"fail": null}` is a
   recoverable spec-level outcome, while "the wire broke" sticks visibly in
   `<k>` -- only now the diagnostic is in the configuration dump rather than on
   a lost child's stderr.  It is also echoed to stderr, since a K run can be
   minutes long and the dump only arrives at the end.

   The reply string is built *inside* the `try`, so even a failure in
   `Yojson.Safe.to_string` is covered. *)

let ml_eval (str_request : string) : string =
  let fail (msg : string) : string =
    Format.eprintf "kffi: %s\n%!" msg;
    (* Hand-built rather than via Yojson, so this path cannot itself raise. *)
    Yojson.Safe.to_string (`Assoc [ ("error", `String msg) ])
  in
  try Yojson.Safe.to_string (eval str_request) with
  | Sys_error msg -> fail (Format.sprintf "File error: %s" msg)
  | Interface.SpecTec_AL.Extern_error msg ->
      fail (Format.sprintf "Extern error: %s" msg)
  | Yojson.Json_error msg -> fail (Format.sprintf "JSON error: %s" msg)
  | BuiltinError (at, msg) | InterpError (at, msg) ->
      fail (Format.sprintf "Builtin error: %s" (string_of_error at msg))
  | ParseError (at, msg) ->
      fail (Format.sprintf "Parse error: %s" (string_of_error at msg))
  | ElabError (at, msg) ->
      fail (Format.sprintf "Elaboration error: %s" (string_of_error at msg))
  (* The meta-interpreter's recursion now runs atop K's rewrite frames rather
     than on a fresh child stack, so this is reachable in a way it was not
     before.  Catching it turns a crash into a stuck term. *)
  | Stack_overflow -> fail "Stack overflow"
  | Out_of_memory -> fail "Out of memory"
  | e -> fail (Format.sprintf "Unknown error: %s" (Printexc.to_string e))

let () =
  Callback.register "ml_init" ml_init;
  Callback.register "ml_eval" ml_eval
