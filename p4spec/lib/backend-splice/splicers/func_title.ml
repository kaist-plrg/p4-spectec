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
    | ExternDecD (id_func, _, _, _, _)
    | BuiltinDecD (id_func, _, _, _, _)
    | FuncDecD (id_func, _, _, _, _) ->
        Some (id_func.it, def)
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
    let name = "func-title-source"
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
      match Backend_latex.El.render_defs ~anchors values with
      | Ok rendered -> rendered
      | Error error ->
          let at, msg = Backend_latex.to_region_msg error in
          Error.error at msg
  end

  module Config = struct
    let name = "func-title-latex"
    let prefix = prefix_latex
    let suffix = suffix_latex

    let anchor (context : Ctx.t) (name : string) : string option =
      context.anchors_latex.func name
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* Prose splicer *)

module Prose = struct
  type prose =
    | ExternP of Pl.Annot.hints * Pl.externfunc
    | BuiltinP of Pl.Annot.hints * Pl.builtinfunc
    | DefinedP of Pl.Annot.hints * Pl.definedfunc

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
             | ExternP (hints, externfunc) ->
                 Backend_adoc.Pl.render_extern_func_def ~anchors hints
                   externfunc
             | BuiltinP (hints, builtinfunc) ->
                 Backend_adoc.Pl.render_builtin_func_def ~anchors hints
                   builtinfunc
             | DefinedP (hints, (id_func, tparams, params, _, _, _)) ->
                 Backend_adoc.Pl.render_func_header ~anchors hints id_func
                   tparams params)
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

  module Config : CONFIG = struct
    let name = "func-title-prose"
    let prefix = "[.sidebar-title]\n" ^ prefix_prose
    let suffix = suffix_prose

    let anchor (context : Ctx.t) (name : string) : string option =
      context.anchors_prose.func name
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
