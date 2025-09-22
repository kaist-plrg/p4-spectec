open Xl
open Domain.Lib
open Runtime_static.Envs

type penv = { prose : Hintenv.t }

let empty_penv = { prose = Hintenv.empty }

type mode = Prose | Code

type t = {
  (* prose hints *)
  penv : penv;
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
  (* relation signature for groups *)
  signature : (Mixop.t * int list) option;
  (* ?? *)
  short : bool;
}

(* Helper functions for context manipulation *)

let create ?(penv = empty_penv) ?(ienv = IEnv.empty) () : t =
  {
    penv;
    ienv;
    neg = false;
    mode = Prose;
    level = 0;
    index = 0;
    signature = None;
    short = false;
  }

let with_level ctx level = { ctx with level }
let with_index ctx index = { ctx with index }
let with_signature ctx signature = { ctx with signature }
let with_short ctx short = { ctx with short }
let increment_level ctx = { ctx with level = ctx.level + 1 }
