open Domain.Lib
open Runtime_static.Envs
module PEnv = Penv

type cond_style = If | ElseIf | Else | Check
type parent_def = None | Relation of RId.t

type t = {
  (* prose hints *)
  penv : PEnv.t;
  (* input hints *)
  ienv : IEnv.t;
  (* negation status, for HoldI, RuleI and CallE *)
  neg : bool;
  (* condition style, for IfI and Cases *)
  cond_style : cond_style option;
  (* current relation ID, for ResultI *)
  def : parent_def;
}

(* Helper functions for context manipulation *)

let create ?(penv = PEnv.empty) ?(ienv = IEnv.empty) () : t =
  { penv; ienv; neg = false; cond_style = None; def = None }

let init spec_sl : t =
  Collect.collect_spec spec_sl |> fun (penv, ienv) -> create ~penv ~ienv ()

let as_cond cond_style ctx = { ctx with cond_style = Some cond_style }
let clear_cond ctx = { ctx with cond_style = None }
let negate ctx = { ctx with neg = not ctx.neg }
let as_bool b ctx = { ctx with neg = b }
let in_rel r ctx = { ctx with def = Relation r }

let get_rel_id ctx =
  match ctx.def with Relation r -> r | None -> failwith "Not in relation"
