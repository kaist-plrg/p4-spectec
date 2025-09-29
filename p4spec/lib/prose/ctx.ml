open Xl
open Domain.Lib
open Runtime_static.Envs
module PEnv = Penv

type mode = Prose | Code
type cond_style = If | ElseIf | Check

type t = {
  (* prose hints *)
  penv : PEnv.t;
  (* input hints *)
  ienv : IEnv.t;
  (* Negation *)
  neg : bool;
  (* Render mode *)
  mode : mode;
  (* indent level *)
  level : int;
  (* only one if statement *)
  cond_style : cond_style option;
  (* relation signature for groups *)
  signature : (Mixop.t * int list) option;
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
    signature = None;
  }

let init spec_sl : t =
  Collect.collect_spec spec_sl |> fun (penv, ienv) -> create ~penv ~ienv ()

let with_signature ctx signature = { ctx with signature }
let as_cond cond_style ctx = { ctx with cond_style = Some cond_style }
let clear_cond ctx = { ctx with cond_style = None }
let in_code ctx = { ctx with mode = Code }
let in_prose ctx = { ctx with mode = Prose }
let increment_level ctx = { ctx with level = ctx.level + 1; cond_style = None }

let bullet ctx : string (* = String.make (ctx.level + 1) '.' ^ " " *) =
  Format.asprintf "%s%s "
    (String.make ctx.level ' ')
    (String.make (ctx.level + 1) '.')
