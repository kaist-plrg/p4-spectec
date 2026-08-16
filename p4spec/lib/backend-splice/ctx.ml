(* Context for resolving anchors in prose and LaTeX renderers *)

type anchors = { func : string -> string option; rel : string -> string option }
type t = { anchors_prose : anchors; anchors_latex : anchors }

(* Constructors *)

let empty_anchors : anchors =
  {
    func = (fun (_name : string) : string option -> None);
    rel = (fun (_name : string) : string option -> None);
  }

let empty : t = { anchors_prose = empty_anchors; anchors_latex = empty_anchors }
