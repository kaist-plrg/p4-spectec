(* Context for resolving anchors in prose and LaTeX renderers *)

type anchors = { func : string -> string option; rel : string -> string option }

type t = {
  anchors_prose : anchors;
  anchors_latex : anchors;
  anchors_emitted : (string, unit) Hashtbl.t;
}

(* Constructors *)

let empty_anchors : anchors =
  {
    func = (fun (_name : string) : string option -> None);
    rel = (fun (_name : string) : string option -> None);
  }

let make ~(anchors_prose : anchors) ~(anchors_latex : anchors) : t =
  { anchors_prose; anchors_latex; anchors_emitted = Hashtbl.create 128 }

let empty : t = make ~anchors_prose:empty_anchors ~anchors_latex:empty_anchors
let reset_anchors (context : t) : unit = Hashtbl.clear context.anchors_emitted

let claim_anchor (context : t) (anchor : string) : bool =
  if Hashtbl.mem context.anchors_emitted anchor then false
  else (
    Hashtbl.add context.anchors_emitted anchor ();
    true)
