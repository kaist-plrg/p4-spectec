open Lang
open Splicer

module Key = struct
  type t = string * string

  let to_string ((id_rel, id_rulegroup) : t) : string =
    Format.asprintf "%s/%s" id_rel id_rulegroup

  let to_anchor ((id_rel, id_rulegroup) : t) : string =
    Pl.Render.Fallthrough.anchor_of_group id_rel id_rulegroup

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b

  let parse (source : Source.t) : t list = [ Parser.parse_id_with_sub source ]
end

module Init : INIT with type key = Key.t and type value = El.def = struct
  type key = Key.t
  type value = El.def

  let init_def (def : El.def) : (key * value) option =
    match def.it with
    | RuleGroupD (id_rel, id_rulegroup, _) ->
        Some ((id_rel.it, id_rulegroup.it), def)
    | _ -> None

  let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
    spec_el |> List.filter_map init_def
end

(* Source splicer *)

module Source = struct
  module Value = struct
    type t = El.def

    let render (_context : Ctx.t) (values : t list) : string =
      values |> List.map El.Render.render_def |> String.concat "\n\n"
  end

  module Config : CONFIG = struct
    let name = "rulegroup-source"
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
        El.Latex.anchors ~func:context.anchors_latex.func
          ~rel:context.anchors_latex.rel
      in
      El.Latex.render_defs ~anchors values
  end

  module Config = struct
    let name = "rulegroup-latex"
    let prefix = prefix_latex
    let suffix = suffix_latex
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.Group.t

  module Value = struct
    type t = prose

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Pl.Render.anchors ~func:context.anchors_prose.func
          ~rel:context.anchors_prose.rel
      in
      values
      |> List.map (fun (group : t) ->
             Pl.Render.render_rulegroup ~anchors group.hints group.id_rulegroup
               group.id_rel group.rel_signature group.exps group.body)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : Pl.def) : (key * value) list =
      match def.node.it with
      | RelD (id_rel, _, _, block, _) ->
          block |> Pl.Group.collect_groups
          |> List.map (fun (group : Pl.Group.t) ->
                 ((id_rel.it, group.id_rulegroup.it), group))
      | _ -> []

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.concat_map init_def
  end

  module Config : CONFIG = struct
    let name = "rulegroup-prose"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let anchor (_context : Ctx.t) (name : string) : string option = Some name
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
