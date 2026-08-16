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
    | ExternS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list
    | BuiltinS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list
    | DefinedS of
        El.id * El.tparam list * El.param list * El.plaintyp * El.hint list

  module Value = struct
    type t = source

    let render (_context : Ctx.t) (values : t list) : string =
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
             El.Render.render_def def)
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
    | ExternP of Pl.Annot.hints * Pl.externfunc
    | BuiltinP of Pl.Annot.hints * Pl.builtinfunc
    | DefinedP of Pl.Annot.hints * Pl.definedfunc

  module Value = struct
    type t = prose

    let render (_context : Ctx.t) (values : t list) : string =
      values
      |> List.map (fun value ->
             match value with
             | ExternP (hints, externfunc) ->
                 Pl.Render.render_extern_func_def hints externfunc
             | BuiltinP (hints, builtinfunc) ->
                 Pl.Render.render_builtin_func_def hints builtinfunc
             | DefinedP (hints, (id_func, tparams, params, _, _, _)) ->
                 Pl.Render.render_func_header hints id_func tparams params)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.node.it with
      | ExternDecD externfunc ->
          let id, _, _, _ = externfunc in
          Some (id.it, ExternP (def_pl.hints, externfunc))
      | BuiltinDecD builtinfunc ->
          let id, _, _, _ = builtinfunc in
          Some (id.it, BuiltinP (def_pl.hints, builtinfunc))
      | FuncDecD func ->
          let id, _, _, _, _, _ = func in
          Some (id.it, DefinedP (def_pl.hints, func))
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
