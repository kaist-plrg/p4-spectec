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
  type source = El.id * El.tablerow list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let id, tablerows = value in
             let def = El.TableDefD (id, tablerows) $ no_region in
             El.Print.string_of_def def)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : El.def) : (key * value) option =
      match def.it with
      | TableDefD (id, tablerows) ->
          let value = (id, tablerows) in
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
  type prose = Pl.tablefunc

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun value -> Pl.Render.render_table_func_def value)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let id_of_func_title = function
      | Pl.ProseFuncTitle (`Check (id_def, _, _))
      | Pl.ProseFuncTitle (`Yield (id_def, _, _))
      | Pl.MathFuncTitle (id_def, _, _) ->
          id_def

    let init_def (def : Pl.def) : (key * value) option =
      match def.it with
      | TableDecD tablefunc ->
          let func_title, _ = tablefunc in
          let id = id_of_func_title func_title in
          Some (id.it, tablefunc)
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
