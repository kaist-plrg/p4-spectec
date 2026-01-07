open Domain.Lib
open Lang
module Value = Runtime.Dynamic_Il.Value
module Dep = Runtime.Testgen_neg.Dep
module Sim = Runtime.Sim.Simulator
module ICov = Coverage.Instr.Single

type spec = Sim.spec

(* Handler signature *)

module type HANDLER = sig
  (* Initialization and finalization *)

  val init_spec : spec -> unit
  val finish : unit -> unit

  (* Backup and restore *)

  val backup : unit -> unit
  val restore : unit -> unit

  (* Common events - values *)

  val on_program : Value.t -> unit
  val on_value : Value.t -> unit
  val on_value_dependency : Value.t -> Value.t -> Dep.Edges.label -> unit

  (* Common events - relations, functions, and iterations *)

  val on_rel_enter : RId.t -> Value.t list -> unit
  val on_rel_exit : RId.t -> unit
  val on_func_enter : FId.t -> Value.t list -> unit
  val on_func_exit : FId.t -> unit

  (* IL events *)

  val on_prem : Il.prem -> unit

  (* SL events *)

  val on_instr : Sl.instr -> unit
  val on_instr_dangling : bool -> PId.t -> Value.t -> unit
end

(* Default handler *)

module Default : HANDLER = struct
  (* Initialization and finalization *)

  let init_spec _ = ()
  let finish () = ()

  (* Backup and restore *)

  let backup () = ()
  let restore () = ()

  (* Common events *)

  let on_program _ = ()
  let on_value _ = ()
  let on_value_dependency _ _ _ = ()
  let on_rel_enter _ _ = ()
  let on_rel_exit _ = ()
  let on_func_enter _ _ = ()
  let on_func_exit _ = ()

  (* IL events *)

  let on_prem _ = ()

  (* SL events *)

  let on_instr _ = ()
  let on_instr_dangling _ _ _ = ()
end
