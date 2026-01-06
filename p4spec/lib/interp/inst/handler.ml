open Domain.Lib
open Lang
module Value = Runtime.Dynamic_Il.Value
module ICov = Coverage.Instr.Single

type spec = IL of Il.spec | SL of Sl.spec

(* Handler signature *)

module type HANDLER = sig
  (* Initialization and finalization *)

  val init : spec -> unit
  val finish : unit -> unit

  (* Common events *)

  val on_value : Value.t -> (Value.t -> unit) -> unit
  val on_rel_enter : RId.t -> Value.t list -> unit
  val on_rel_exit : RId.t -> unit
  val on_func_enter : FId.t -> Value.t list -> unit
  val on_func_exit : FId.t -> unit

  (* IL events *)

  val on_prem : Il.prem -> unit

  (* SL events *)

  val on_instr : Sl.instr -> unit
end

(* Default handler *)

module Default : HANDLER = struct
  (* Initialization and finalization *)

  let init _ = ()
  let finish () = ()

  (* Common events *)

  let on_value _ _ = ()
  let on_rel_enter _ _ = ()
  let on_rel_exit _ = ()
  let on_func_enter _ _ = ()
  let on_func_exit _ = ()

  (* IL events *)

  let on_prem _ = ()

  (* SL events *)

  let on_instr _ = ()
end
