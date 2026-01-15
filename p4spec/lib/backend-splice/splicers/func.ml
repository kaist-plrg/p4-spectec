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
    (El.id * El.tparam list * El.arg list * El.exp * El.prem list) list

  module Value = struct
    type t = source

    let render (values : t list) : string =
      values
      |> List.map (fun value ->
             let defs =
               List.map
                 (fun (id, tparams, args, exp, prems) ->
                   El.FuncDefD (id, tparams, args, exp, prems) $ no_region)
                 value
             in
             defs |> List.map El.Print.string_of_def |> String.concat "\n\n")
      |> String.concat "\n\n"
  end

  module Init : INIT with type key = Key.t and type value = Value.t = struct
    type key = Key.t
    type value = Value.t

    let init_def (pairs : (key * value) list) (def : El.def) :
        (key * value) list =
      match def.it with
      | FuncDefD (id, tparams, args, exp, prems) ->
          let func = (id, tparams, args, exp, prems) in
          let found, pairs =
            List.fold_left
              (fun (found, pairs) (key, value) ->
                if key = id.it then
                  let pair = (key, value @ [ func ]) in
                  (true, pair :: pairs)
                else (found, (key, value) :: pairs))
              (false, []) pairs
          in
          (if found then pairs else (id.it, [ func ]) :: pairs) |> List.rev
      | _ -> pairs

    let init (spec_el : El.spec) (_spec_pl : Pl.spec) : (key * value) list =
      List.fold_left init_def [] spec_el
  end

  module Anchor : ANCHOR = struct
    let name = "func-source"
    let prefix = prefix_source
    let suffix = suffix_source
    let header = false
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end

(* Prose splicer *)

module Prose = struct
  type prose = Pl.func

  module Value = struct
    type t = prose

    let render (values : t list) : string =
      values
      |> List.map (fun value -> Pl.Render.render_defined_func_def value)
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
      | FuncDecD func ->
          let func_title, _ = func in
          let id = id_of_func_title func_title in
          Some (id.it, func)
      | _ -> None

    let init (_spec_el : El.spec) (spec_pl : Pl.spec) : (key * value) list =
      spec_pl |> List.filter_map init_def
  end

  module Anchor : ANCHOR = struct
    let name = "func-prose"
    let prefix = prefix_prose
    let suffix = suffix_prose
    let header = true
  end

  module Splicer : SPLICER = Make (Key) (Value) (Init) (Anchor)
end
