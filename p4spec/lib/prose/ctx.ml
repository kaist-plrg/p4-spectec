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
  (* Top-down signal to emit code instead of prose *)
  mode : mode;
  (* indent level *)
  level : int;
  (* item index for numbering *)
  index : int;
  (* start index for continued indexing *)
  start_index : int option;
  (* relation signature for groups *)
  signature : (Mixop.t * int list) option;
  (* ?? *)
  short : bool;
}

(* Helper functions for context manipulation *)

let create ?(penv = PEnv.empty) ?(ienv = IEnv.empty) () : t =
  {
    penv;
    ienv;
    neg = false;
    mode = Prose;
    level = 0;
    index = 0;
    start_index = None;
    signature = None;
    short = false;
  }

let init spec_sl : t =
  Collect.collect_spec spec_sl |> fun (penv, ienv) -> create ~penv ~ienv ()

let with_level ctx level = { ctx with level }
let with_index ctx index = { ctx with index }
let with_start ctx start_index = { ctx with start_index = Some start_index }
let with_signature ctx signature = { ctx with signature }
let with_short ctx short = { ctx with short }
let reset_start ctx = { ctx with start_index = None }
let increment_level ctx = { ctx with level = ctx.level + 1 }
let increment_index ctx = { ctx with index = ctx.index + 1 }

let bullet ctx : string (* = String.make (ctx.level + 1) '.' ^ " " *) =
  Format.asprintf "%s%s "
    (String.make ctx.level ' ')
    (String.make (ctx.level + 1) '.')
