open Lang
open Splicer

module Key = struct
  type t = string

  let to_string (key : t) : string = key
  let to_anchor = to_string
  let compare = String.compare
  let parse (source : Source.t) : t list = Parser.parse_ids source
end

module Prose = struct
  type prose = Pl.id * Pl.block_dispatch

  module Value = struct
    type t = prose

    let render (context : Ctx.t) (values : t list) : string =
      let anchors =
        Backend_adoc.Pl.anchors ~func:context.anchors_prose.func
          ~rel:context.anchors_prose.rel
      in
      values
      |> List.map (fun (id_rel, elseblock) ->
             Backend_adoc.Pl.render_rulegroup_else ~anchors id_rel elseblock)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.node.it with
      | RelD (id_rel, _, _, _, Some (_ :: _ as elseblock)) ->
          Some (id_rel.it, (id_rel, elseblock))
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Config : CONFIG = struct
    let name = "rulegroup-prose-else"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let anchor (_context : Ctx.t) (_name : string) : string option = None
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Config)
end
