(* Canonical wrapper-free LaTeX rendering for EL. *)

open Lang
open El
open Tex

(* Anchors *)

type anchors = Renderer.anchors

let anchors ~(func : string -> string option) ~(rel : string -> string option) :
    anchors =
  { func; rel }

(* Public rendering *)

let render_def ?(anchors : anchors option) (def : def) : string =
  Serialize.to_string (Renderer.tex_of_def_single ?anchors def)

let render_defs ?(anchors : anchors option) (defs : def list) : string =
  Serialize.to_string (Renderer.tex_of_defs ?anchors defs)
