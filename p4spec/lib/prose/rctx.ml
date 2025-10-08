open Xl
open Domain.Lib
open Runtime_static.Envs
module PEnv = Penv

type mode = Prose | Code
type cond_style = If | ElseIf | Check
type def = None | Relation of RId.t

type t = {
  (* prose hints *)
  penv : PEnv.t;
  (* input hints *)
  ienv : IEnv.t;
  (* negation status, for HoldI, RuleI and CallE *)
  neg : bool;
  (* render mode *)
  mode : mode;
  (* indent level for instructions *)
  level : int;
  (* condition style, for IfI and Cases *)
  cond_style : cond_style option;
  (* current relation ID, for ResultI *)
  def : def;
}

(* Helper functions for context manipulation *)

let create ?(penv = PEnv.empty) ?(ienv = IEnv.empty) () : t =
  {
    penv;
    ienv;
    neg = false;
    mode = Prose;
    level = 0;
    cond_style = None;
    def = None;
  }

let init spec_sl : t =
  Collect.collect_spec spec_sl |> fun (penv, ienv) -> create ~penv ~ienv ()

let as_cond cond_style ctx = { ctx with cond_style = Some cond_style }
let clear_cond ctx = { ctx with cond_style = None }
let in_code ctx = { ctx with mode = Code }
let in_prose ctx = { ctx with mode = Prose }
let negate ctx = { ctx with neg = not ctx.neg }
let as_bool b ctx = { ctx with neg = b }
let increment_level ctx = { ctx with level = ctx.level + 1; cond_style = None }
let in_rel r ctx = { ctx with def = Relation r }

let bullet ctx : string (* = String.make (ctx.level + 1) '.' ^ " " *) =
  Format.asprintf "%s%s "
    (String.make ctx.level ' ')
    (String.make (ctx.level + 1) '.')

let unordered_bullet ctx : string =
  Format.asprintf "%s%s " (String.make (ctx.level * 2) ' ') "*"
