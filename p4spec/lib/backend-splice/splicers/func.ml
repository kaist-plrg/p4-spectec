open Lang
open Splicer

module Key = struct
  type t = string

  let to_string (key : t) : string = key
  let to_anchor = to_string
  let compare = String.compare
  let parse (source : Source.t) : t list = Parser.parse_ids source
end

module Init : INIT with type key = Key.t and type value = El.def list = struct
  type key = Key.t
  type value = El.def list

  let init_def (pairs : (key * value) list) (def : El.def) : (key * value) list
      =
    match def.it with
    | FuncDefD (id, _, _, _, _) ->
        let found, pairs =
          List.fold_left
            (fun (found, pairs) (key, value) ->
              if key = id.it then
                let pair = (key, value @ [ def ]) in
                (true, pair :: pairs)
              else (found, (key, value) :: pairs))
            (false, []) pairs
        in
        (if found then pairs else (id.it, [ def ]) :: pairs) |> List.rev
    | _ -> pairs

  let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
    List.fold_left init_def [] spec_el
end

(* Source splicer *)

module Source = struct
  module Value = struct
    type t = El.def list

    let render (_context : Ctx.t) (values : t list) : string =
      values
      |> List.map (fun value ->
             value
             |> List.map Backend_adoc.El.render_def
             |> String.concat "\n\n")
      |> String.concat "\n\n"
  end

  module Config : CONFIG = struct
    let name = "func-source"
    let prefix = prefix_source
    let suffix = suffix_source
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* LaTeX splicer *)

module Latex = struct
  module Value = struct
    type t = El.def list

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Backend_latex.El.anchors ~func:context.anchors_latex.func
          ~rel:context.anchors_latex.rel
      in
      match values |> List.concat |> Backend_latex.El.render_defs ~anchors with
      | Ok rendered -> rendered
      | Error error ->
          let at, msg = Backend_latex.to_region_msg error in
          Error.error at msg
  end

  module Config = struct
    let name = "func-latex"
    let prefix = prefix_latex
    let suffix = suffix_latex
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.Annot.hints * Pl.definedfunc

  module Value = struct
    type t = prose

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Backend_adoc.Pl.anchors ~func:context.anchors_prose.func
          ~rel:context.anchors_prose.rel
      in
      values
      |> List.map (fun (hints, func) ->
             Backend_adoc.Pl.render_defined_func_def ~anchors hints func)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.node.it with
      | FuncDecD func ->
          let id, _, _, _, _, _ = func in
          Some (id.it, (def_pl.hints, func))
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Config : CONFIG = struct
    let name = "func-prose"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
