open Xl
open Domain.Lib
open Runtime_static.Envs
module PEnv = Penv

type mode = Prose | Code

type t = {
  (* prose hints *)
  penv : PEnv.t;
  (* input hints *)
  ienv : IEnv.t;
  (* Negation *)
  neg : bool;
  (* Rende mode *)
  mode : mode;
  (* indent level *)
  level : int;
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
    signature = None;
  }

let init spec_sl : t =
  Collect.collect_spec spec_sl |> fun (penv, ienv) -> create ~penv ~ienv ()

let with_level ctx level = { ctx with level }
let with_signature ctx signature = { ctx with signature }
let in_code ctx = { ctx with mode = Code }
let in_prose ctx = { ctx with mode = Prose }
let increment_level ctx = { ctx with level = ctx.level + 1 }

let bullet ctx : string (* = String.make (ctx.level + 1) '.' ^ " " *) =
  Format.asprintf "%s%s "
    (String.make ctx.level ' ')
    (String.make (ctx.level + 1) '.')
