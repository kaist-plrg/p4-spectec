(** Canonical wrapper-free LaTeX rendering for EL. *)

open Tex

(* Errors *)

exception LatexError = Util.Error.LatexError

let error_latex (at : Util.Source.region) (message : string) : 'a =
  Error.error at message

type anchors = Renderer.anchors

let anchors ~(func : string -> string option) ~(rel : string -> string option) :
    anchors =
  { func; rel }

(* Public rendering *)

let render_def ?(anchors : anchors option) (def : Ast.def) : string =
  Serialize.to_string (Renderer.tex_of_def_single ?anchors def)

let render_defs ?(anchors : anchors option) (defs : Ast.def list) : string =
  Serialize.to_string (Renderer.tex_of_defs ?anchors defs)
