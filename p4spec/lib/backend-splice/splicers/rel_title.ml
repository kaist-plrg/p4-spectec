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
    match def.it with
    | ExternRelD (id, _, _) | RelD (id, _, _) -> Some (id.it, def)
    | _ -> None

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
    let name = "relation-title-source"
    let prefix = prefix_source
    let suffix = suffix_source
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
        Backend_latex.El.anchors ~func:context.anchors_latex.func
          ~rel:context.anchors_latex.rel
      in
      Backend_latex.El.render_defs ~anchors values
  end

  module Config = struct
    let name = "relation-title-latex"
    let prefix = prefix_latex
    let suffix = suffix_latex

    let anchor (context : Ctx.t) (name : string) : string option =
      context.anchors_latex.rel name
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* Prose splicer *)

module Prose = struct
  type prose =
    | ExternP of Pl.Annot.hints * Pl.externrel
    | DefinedP of Pl.Annot.hints * Pl.rel

  module Value = struct
    type t = prose

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Backend_adoc.Pl.anchors ~func:context.anchors_prose.func
          ~rel:context.anchors_prose.rel
      in
      values
      |> List.map (fun value ->
             match value with
             | ExternP (hints, externrel) ->
                 Backend_adoc.Pl.render_extern_rel_def ~anchors hints externrel
             | DefinedP (hints, (id_rel, rel_signature, exps, _, _)) ->
                 Backend_adoc.Pl.render_rel_title_adoc ~anchors hints id_rel
                   rel_signature exps)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.node.it with
      | ExternRelD externrel ->
          let id, _, _ = externrel in
          Some (id.it, ExternP (def_pl.hints, externrel))
      | RelD rel ->
          let id, _, _, _, _ = rel in
          Some (id.it, DefinedP (def_pl.hints, rel))
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Config : CONFIG = struct
    let name = "relation-title-prose"
    let prefix = "[.sidebar-title]\n" ^ prefix_prose
    let suffix = suffix_prose

    let anchor (context : Ctx.t) (name : string) : string option =
      context.anchors_prose.rel name
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
