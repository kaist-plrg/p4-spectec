open Lang
open Splicer

module Key = struct
  type t = string

  let to_string (key : t) : string = key
  let to_anchor = to_string
  let compare = String.compare
  let parse (source : Source.t) : t list = Parser.parse_ids source
end

module Init : INIT with type key = Key.t and type value = El.def = struct
  type key = Key.t
  type value = El.def

  let init_def (def : El.def) : (key * value) option =
    match def.it with TableDefD (id, _) -> Some (id.it, def) | _ -> None

  let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
    spec_el |> List.filter_map init_def
end

(* Source splicer *)

module Source = struct
  module Value = struct
    type t = El.def

    let render (_context : Ctx.t) (values : t list) : string =
      values |> List.map Backend_adoc.El.render_def |> String.concat "\n\n"
  end

  module Config : CONFIG = struct
    let name = "table-source"
    let prefix = ""
    let suffix = "\n"
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* LaTeX splicer *)

module Latex = struct
  module Value = struct
    type t = El.def

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        El.Latex.anchors ~func:context.anchors_latex.func
          ~rel:context.anchors_latex.rel
      in
      El.Latex.render_defs ~anchors values
  end

  module Config = struct
    let name = "table-latex"
    let prefix = prefix_latex
    let suffix = suffix_latex
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.Annot.hints * Pl.tablefunc

  module Value = struct
    type t = prose

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Backend_adoc.Pl.anchors ~func:context.anchors_prose.func
          ~rel:context.anchors_prose.rel
      in
      values
      |> List.map (fun (hints, tablefunc) ->
             Backend_adoc.Pl.render_table_func_def ~anchors hints tablefunc)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : Pl.def) : (key * value) option =
      match def.node.it with
      | TableDecD tablefunc ->
          let id, _, _, _ = tablefunc in
          Some (id.it, (def.hints, tablefunc))
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Config : CONFIG = struct
    let name = "table-prose"
    let prefix = ""
    let suffix = "\n"
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
