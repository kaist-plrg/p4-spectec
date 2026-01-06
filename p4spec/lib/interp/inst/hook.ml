open Domain.Lib
open Lang
module Value = Runtime.Dynamic_Il.Value
open Handler

let handlers : (module HANDLER) list ref = ref []
let register (handlers_ : (module HANDLER) list) = handlers := handlers_

(* Initialization and finalization *)

let init spec : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.init spec) !handlers

let finish () : unit =
  match !handlers with
  | [] -> ()
  | _ -> List.iter (fun (module H : HANDLER) -> H.finish ()) !handlers

(* Common events *)

let on_value (value : Value.t) (value_handler : Value.t -> unit) : unit =
  match !handlers with
  | [] -> ()
  | _ ->
      List.iter
        (fun (module H : HANDLER) -> H.on_value value value_handler)
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
