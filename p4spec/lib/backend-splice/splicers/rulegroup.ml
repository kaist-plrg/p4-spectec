open Lang
open Splicer
open Util.Source

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

(* Source splicer *)

module Source = struct
  type source = El.id * El.id * El.rule list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let id_rel, id_rulegroup, rules = value in
             let def =
               El.RuleGroupD (id_rel, id_rulegroup, rules) $ no_region
             in
             El.Print.string_of_def def)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : El.def) : (key * value) option =
      match def.it with
      | RuleGroupD (id_rel, id_rulegroup, rules) ->
          let value = (id_rel, id_rulegroup, rules) in
          Some ((id_rel.it, id_rulegroup.it), value)
      | _ -> None

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      spec_el |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "rulegroup-source"
    let prefix = prefix_source
    let suffix = suffix_source
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.Group.t

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun (group : t) ->
             Pl.Render.render_rulegroup group.hints group.id_rulegroup
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

  module Anchor : ANCHOR = struct
    let name = "rulegroup-prose"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let header = true
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
