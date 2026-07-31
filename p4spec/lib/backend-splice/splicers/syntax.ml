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
    | ExternS of El.id * El.hint list
    | DefinedS of El.id * El.tparam list * El.deftyp * El.hint list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let def =
               match value with
               | ExternS (id, hints) -> El.ExternSynD (id, hints) $ no_region
               | DefinedS (id, tparams, deftyp, hints) ->
                   El.TypD (id, tparams, deftyp, hints) $ no_region
             in
             El.Render.render_def def)
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (def : El.def) : (key * value) option =
      match def.it with
      | ExternSynD (id, hints) ->
          let value = ExternS (id, hints) in
          Some (id.it, value)
      | TypD (id, tparams, deftyp, hints) ->
          let value = DefinedS (id, tparams, deftyp, hints) in
          Some (id.it, value)
      | _ -> None

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      spec_el |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "syntax"
    let prefix = "[source,bison]\n----\n"
    let suffix = "\n----"
    let header = true
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
