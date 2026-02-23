open Domain.Lib
open Lang
module Value = Runtime.Dynamic_Il.Value
module Dep = Runtime.Testgen_neg.Dep
open Handler

(* Cache flag *)

let cache = ref true
let cache_on () = cache := true
let cache_off () = cache := false
let is_cache_on () = !cache

(* Registered handlers *)

let handlers : (module HANDLER) list ref = ref []
let register (handlers_ : (module HANDLER) list) = handlers := handlers_

(* Initialization and finalization *)

let init_spec (spec : spec) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.init_spec spec) !handlers

let finish () : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.finish ()) !handlers

(* Backup and restore *)

let backup () : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.backup ()) !handlers

let restore () : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.restore ()) !handlers

(* Common events *)

let on_program (value_program : Value.t) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) -> H.on_program value_program)
        !handlers

let on_value (value : Value.t) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.on_value value) !handlers

let on_value_dependency (value : Value.t) (value_dep : Value.t)
    (label : Dep.Edges.label) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) ->
          H.on_value_dependency value value_dep label)
        !handlers

let on_rel_enter (rid : RId.t) (values_input : Value.t list) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) -> H.on_rel_enter rid values_input)
        !handlers

let on_rel_exit (rid : RId.t) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.on_rel_exit rid) !handlers

let on_func_enter (fid : FId.t) (values_input : Value.t list) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) -> H.on_func_enter fid values_input)
        !handlers

let on_func_exit (fid : FId.t) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.on_func_exit fid) !handlers

(* IL events *)

let on_prem (prem : Il.prem) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.on_prem prem) !handlers

(* SL events *)

let on_instr (instr : Sl.instr) : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.on_instr instr) !handlers

let on_instr_dangling (cond : bool) (pid : PId.t) (value : Value.t) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) -> H.on_instr_dangling cond pid value)
        !handlers
