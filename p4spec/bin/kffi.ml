(* The K side of the K<->OCaml wire, reached through the C FFI.
 *
 * This is the in-process equivalent of `spectec-boot extern` (boot.ml's
 * `extern_command`): same JSON wire format, same dispatch, but answered from a
 * long-lived process rather than a fresh one per call.  The K side reaches it
 * through `spec-meta-k/ffi/shim.c`, which presents a plain C ABI outwards and
 * calls in here via `caml_callback2`:
 *
 *   K rules  --#ffiCall-->  shim.c  --caml_callback2-->  ml_eval
 *
 * `Callback.register` is what makes `ml_eval` reachable by name: compiled
 * OCaml symbols are name-mangled with a build-dependent stamp and use OCaml's
 * internal calling convention, so C cannot call them directly.
 *
 * Two things follow from being long-lived, and they are the whole point:
 *
 *   - `runners` memoizes the built runner per spec path.  Under `#system` each
 *     extern call re-elaborated the entire lower spec (~1.2 s); here it happens
 *     once.
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

(* Memoized runners, keyed by spec path.

   `Pass.structure` is itself already process-memoized (`pass/pass.ml`); what
   this table saves is the generative `P4.Make ()` plus `Runner_target.init` --
   the ~1.2 s of the extern path.

   Building a runner also calls `Interface.init`, which installs the `$print_`
   unparser for that spec, so no separate printer-init table is needed. *)

let runners : (string, (module RUNNER)) Hashtbl.t = Hashtbl.create 4

(* `Interface.P4.unparser` is a process-global ref, so the *last* spec built
   wins on `$print_`.  That is invisible while one run touches one spec, but a
   `noP4()` run reaches both `"spec"` (builtins) and `"examples/lower"`
   (externs), so say so out loud rather than let a wrong `$print_` be silent. *)

let path_spec_inited : string option ref = ref None

let build_runner (path_spec : string) : (module RUNNER) =
  match Hashtbl.find_opt runners path_spec with
  | Some runner -> runner
  | None ->
      (match !path_spec_inited with
      | Some path_prev when path_prev <> path_spec ->
          Format.eprintf
            "kffi: warning: building a runner for %s after %s; \
             $print_ now unparses against the former\n\
             %!"
            path_spec path_prev
      | _ -> ());
      let runner =
        Backend_boot.Build.build_target ~cache:true
          { layer = { specdir = path_spec; rel = "" }; interface = P4_interface }
      in
      path_spec_inited := Some path_spec;
      Hashtbl.replace runners path_spec runner;
      runner

(* Dispatch, mirroring boot.ml's `extern_command` exactly.

   `$print_` is the one builtin needing more than a registry lookup: it
   unparses against the spec.  boot.ml calls `Interface.P4.init` for it; here
   `build_runner` is called instead, which does that as part of
   `Runner_target.init` -- and memoizes, so the second `$print_` is free. *)

let eval (path_spec : string) (str_request : string) : Yojson.Safe.t =
  let json_request = Yojson.Safe.from_string str_request in
  let request = Interface.SpecTec_AL.request_of_json json_request in
  match request with
  | Builtin (name, targs, args) ->
      if name = "print_" then build_runner path_spec |> ignore;
      let value =
        Interface.P4.call_builtin
          (fun _ -> ())
          Util.Source.(name $ no_region)
          targs args
      in
      Interface.SpecTec_AL.json_of_response value
  | ExternFunc (name, targs, args) -> (
      let (module Runner) = build_runner path_spec in
      match Runner.Interp.eval_func name targs args with
      | Pass value -> Interface.SpecTec_AL.json_of_response value
      | Fail (at, msg) ->
          Format.eprintf "extern func %s failed: %s\n%!" name
            (string_of_error at msg);
          Interface.SpecTec_AL.json_of_response_fail ())
  | ExternRel (name, args) -> (
      let (module Runner) = build_runner path_spec in
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

let ml_eval (path_spec : string) (str_request : string) : string =
  let fail (msg : string) : string =
    Format.eprintf "kffi: %s\n%!" msg;
    (* Hand-built rather than via Yojson, so this path cannot itself raise. *)
    Yojson.Safe.to_string (`Assoc [ ("error", `String msg) ])
  in
  try Yojson.Safe.to_string (eval path_spec str_request) with
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

let () = Callback.register "ml_eval" ml_eval
