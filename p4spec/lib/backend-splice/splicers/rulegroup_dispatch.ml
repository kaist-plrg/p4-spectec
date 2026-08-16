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

(* Prose splicer *)

module Prose = struct
  type prose = Pl.rel

  module Value = struct
    type t = prose

    let render (_context : Ctx.t) (values : t list) : string =
      values
      |> List.map Pl.Render.render_defined_rel_def_dispatch
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def_pl : Pl.def) : (key * value) option =
      match def_pl.node.it with
      | RelD rel ->
          let id_rel, _, _, _, _ = rel in
          Some (id_rel.it, rel)
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "rulegroup-dispatch-prose"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
