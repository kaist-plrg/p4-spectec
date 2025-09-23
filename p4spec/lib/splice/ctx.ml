open Util.Source

(* Context for splicer *)

module Map = Map.Make (String)

type t = { mutable syntax : El.Ast.def Map.t; anchors : Anchor.t list }

(* Initialization *)

let init_el (ctx : t) (spec_el : El.Ast.spec) : unit =
  let init_el' (def_el : El.Ast.def) : unit =
    match def_el.it with
    | TypD (id, _, _, _) -> ctx.syntax <- Map.add id.it def_el ctx.syntax
    | _ -> ()
  in
  List.iter init_el' spec_el

let init (spec_el : El.Ast.spec) : t =
  let anchors = [ Anchor.syntax ] in
  let ctx = { syntax = Map.empty; anchors } in
  init_el ctx spec_el;
  ctx

(* Finders *)

let find_syntax_defs (ctx : t) (targets : (string * string) list) :
    El.Ast.def list =
  let find_def (id : string) : El.Ast.def = Map.find id ctx.syntax in
  List.map (fun (id, _) -> find_def id) targets
