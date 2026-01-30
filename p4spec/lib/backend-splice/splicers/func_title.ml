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
    | ExternS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list
    | BuiltinS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list
    | DefinedS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let def =
               match value with
               | ExternS (id, tparams, params, plaintyp, hints) ->
                   El.ExternDecD (id, tparams, params, plaintyp, hints)
                   $ no_region
               | BuiltinS (id, tparams, params, plaintyp, hints) ->
                   El.BuiltinDecD (id, tparams, params, plaintyp, hints)
                   $ no_region
               | DefinedS (id, tparams, params, plaintyp, hints) ->
                   El.FuncDecD (id, tparams, params, plaintyp, hints)
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
      | ExternDecD (id_func, tparams, params, plaintyp, hints) ->
          let value = ExternS (id_func, tparams, params, plaintyp, hints) in
          Some (id_func.it, value)
      | BuiltinDecD (id_func, tparams, params, plaintyp, hints) ->
          let value = BuiltinS (id_func, tparams, params, plaintyp, hints) in
          Some (id_func.it, value)
      | FuncDecD (id_func, tparams, params, plaintyp, hints) ->
          let value = DefinedS (id_func, tparams, params, plaintyp, hints) in
          Some (id_func.it, value)
      | _ -> None

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      spec_el |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "func-title-source"
    let prefix = prefix_source
    let suffix = suffix_source
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end

(* Prose splicer *)

module Prose = struct
  type prose =
    | ExternP of Pl.func_title
    | BuiltinP of Pl.func_title
    | DefinedP of Pl.func_title

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let func_title =
               match value with
               | ExternP func_title -> func_title
               | BuiltinP func_title -> func_title
               | DefinedP func_title -> func_title
             in
             Pl.Render.render_func_title func_title)
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

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.it with
      | ExternDecD func_title ->
          let id = id_of_func_title func_title in
          let value = ExternP func_title in
          Some (id.it, value)
      | BuiltinDecD func_title ->
          let id = id_of_func_title func_title in
          let value = BuiltinP func_title in
          Some (id.it, value)
      | FuncDecD (func_title, _) ->
          let id = id_of_func_title func_title in
          let value = DefinedP func_title in
          Some (id.it, value)
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "func-title-prose"
    let prefix = "[.sidebar-title]\n" ^ prefix_prose
    let suffix = suffix_prose
    let header = true
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
