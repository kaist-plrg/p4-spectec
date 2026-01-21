open Lang
open Splicer
open Util.Source

module Key = struct
  type t = string

  let to_string (key : t) : string = key
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
             El.Print.string_of_def def)
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
  type prose = ExternP of Pl.rel_title | DefinedP of Pl.rel_title

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let rel_title =
               match value with
               | ExternP rel_title -> rel_title
               | DefinedP rel_title -> rel_title
             in
             Pl.Render.render_rel_title rel_title)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let id_of_rel_title = function
      | Pl.ProseRelTitle (`Hold (id_rel, _, _))
      | Pl.ProseRelTitle (`Yield (id_rel, _, _, _, _))
      | Pl.MathRelTitle (id_rel, _, _) ->
          id_rel

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.it with
      | ExternRelD rel_title ->
          let id = id_of_rel_title rel_title in
          let rel_title = ExternP rel_title in
          Some (id.it, rel_title)
      | RelD (rel_title, _) ->
          let id = id_of_rel_title rel_title in
          let rel_title = DefinedP rel_title in
          Some (id.it, rel_title)
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
