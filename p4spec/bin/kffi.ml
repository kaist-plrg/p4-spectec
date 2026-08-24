(* The K side of the K <-> OCaml wire, reached through the C FFI.
 *
 *   K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback-->
 *   ml_init / ml_eval
 *
 * The wire format is documented in full at the head of
 * `p4spec/lib/interface/spectec/ali/extern.ml` and
 * `spec-meta-k/al/4.1-extern-json.k`. *)

open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error

(* Tune GC for the allocation-heavy meta-circular interpreter. *)

let () =
  Gc.set
    {
      (Gc.get ()) with
      Gc.minor_heap_size = 16 * 1024 * 1024;
      Gc.space_overhead = 2000;
    }

(* The one runner, built once at startup. *)

let runner : (module RUNNER) option ref = ref None

let get_runner () : (module RUNNER) =
  match !runner with
  | Some runner -> runner
  | None ->
      raise
        (Interface.SpecTec_AL.Extern_error
           "runner not initialized: ml_init_c was never called")

(* One-time initialization, called by the shim's `ml_init_c`. *)

let ml_init (path_spec : string) : unit =
  runner :=
    Some
      (Backend_boot.Build.build_target ~cache:true
         { layer = { specdir = path_spec; rel = "" }; interface = P4_interface })

(* Dispatch, mirroring boot.ml's `extern_command`. *)

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

let ml_eval (str_request : string) : string =
  let fail (msg : string) : string =
    Format.eprintf "kffi: %s\n%!" msg;
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
  | e -> fail (Format.sprintf "Unknown error: %s" (Printexc.to_string e))

let () =
  Callback.register "ml_init" ml_init;
  Callback.register "ml_eval" ml_eval
