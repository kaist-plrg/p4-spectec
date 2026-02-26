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
  type source = El.id * El.tablecol list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let id, tablecols = value in
             let def =
               El.TableGroupD
                 ( id,
                   El.ExpP (El.BoolT $ no_region) $ no_region,
                   El.BoolT $ no_region,
                   tablecols,
                   [] )
               $ no_region
             in
             El.Print.string_of_def def)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : El.def) : (key * value) option =
      match def.it with
      | TableGroupD (id, _, _, tablecols, _) ->
          let value = (id, tablecols) in
          Some (id.it, value)
      | _ -> None

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      spec_el |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "table-source"
    let prefix = ""
    let suffix = "\n"
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.tablegroup

  module Value = struct
    type t = prose

    let render (tablegroups : t list) : string =
      tablegroups
      |> List.map (fun tablegroup -> Pl.Render.render_tablegroup_def tablegroup)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let id_of_group_title group_title =
      let id, _, _, _, _ = group_title in
      id

    let init_def (def : Pl.def) : (key * value) option =
      match def.it with
      | TableGroupD ({ title; _ } as tablegroup) ->
          let id = id_of_group_title title in
          Some (id.it, tablegroup)
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "table-prose"
    let prefix = ""
    let suffix = "\n"
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
