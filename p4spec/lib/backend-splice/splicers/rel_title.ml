open Lang
open Splicer
open Util.Source

module Key = struct
  type t = string

  let to_string (key : t) : string = key
  let to_anchor = to_string
  let compare = String.compare
  let parse (source : Source.t) : t list = Parser.parse_ids source
end

(* Source splicer *)

module Source = struct
  type source =
    | ExternS of El.id * El.nottyp * El.hint list
    | DefinedS of El.id * El.nottyp * El.hint list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let def =
               match value with
               | ExternS (id, nottyp, hints) ->
                   El.ExternRelD (id, nottyp, hints) $ no_region
               | DefinedS (id, nottyp, hints) ->
                   El.RelD (id, nottyp, hints) $ no_region
             in
             El.Render.render_def def)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : El.def) : (key * value) option =
      match def.it with
      | ExternRelD (id, nottyp, hints) ->
          let source = ExternS (id, nottyp, hints) in
          Some (id.it, source)
      | RelD (id, nottyp, hints) ->
          let source = DefinedS (id, nottyp, hints) in
          Some (id.it, source)
      | _ -> None

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      spec_el |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "relation-title-source"
    let prefix = prefix_source
    let suffix = suffix_source
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end

(* Prose splicer *)

module Prose = struct
  type prose =
    | ExternP of Pl.Annot.hints * Pl.externrel
    | DefinedP of Pl.Annot.hints * Pl.rel

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             match value with
             | ExternP (hints, externrel) ->
                 Pl.Render.render_extern_rel_def hints externrel
             | DefinedP (hints, (id_rel, rel_signature, exps, _, _)) ->
                 Pl.Render.render_rel_title_adoc hints id_rel rel_signature exps)
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

  module Anchor : ANCHOR = struct
    let name = "relation-title-prose"
    let prefix = "[.sidebar-title]\n" ^ prefix_prose
    let suffix = suffix_prose
    let header = true
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
