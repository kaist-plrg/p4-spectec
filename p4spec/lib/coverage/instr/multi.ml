open Domain.Lib
open Lang.Sl
open Util.Source

(* Instruction node *)

module Node = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status *)

  type status = Hit of string list | Miss

  (* Type *)

  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss }

  (* Equivalence *)

  let eq (node_a : t) (node_b : t) : bool =
    node_a.origin = node_b.origin && node_a.status = node_b.status

  (* Printer *)

  let to_string (node : t) : string =
    match node.status with Hit _ -> "H" | Miss -> "M"
end

(* Instruction node coverage map *)

module Cover = struct
  include MakeIIdEnv (Node)

  (* Constructor *)

  let is_ignored (hints : hint list) : bool =
    List.exists (fun (hint : hint) -> hint.hintid.it = "testgen_ignore") hints

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    let iid = instr.note.iid in
    let node = Node.init id in
    add iid node cover

  and init_instrs (cover : t) (id : id) (instrs : instr list) : t =
    List.fold_left (fun cover instr -> init_instr cover id instr) cover instrs

  let init_tablerow (cover : t) (id : id) (tablerow : tablerow) : t =
    let _, _, instrs = tablerow in
    init_instrs cover id instrs

  let init_tablerows (cover : t) (id : id) (tablerows : tablerow list) : t =
    List.fold_left
      (fun cover tablerow -> init_tablerow cover id tablerow)
      cover tablerows

  let init_def (cover : t) (def : def) : t =
    match def.it with
    | RelD (id, _, _, instrs, hints) when not (is_ignored hints) ->
        init_instrs cover id instrs
    | FuncDecD (id, _, _, _, instrs, hints) when not (is_ignored hints) ->
        init_instrs cover id instrs
    | TableDecD (id, _, _, tablerows, hints) when not (is_ignored hints) ->
        init_tablerows cover id tablerows
    | _ -> cover

  let init_spec (spec : spec) : t = List.fold_left init_def empty spec
end

(* Instruction node coverage *)

type t = Cover.t

(* Extension from single coverage *)

let extend (cover : t) (filename_p4 : string) (cover_single : Single.t) : t =
  Cover.mapi
    (fun (iid : iid) (node : Node.t) ->
      let node_single = Single.Cover.find iid cover_single in
      match node.status with
      | Hit filenames_p4 -> (
          match node_single.status with
          | Hit ->
              let filenames_p4 = filename_p4 :: filenames_p4 in
              { node with status = Hit filenames_p4 }
          | _ -> node)
      | Miss -> (
          match node_single.status with
          | Hit ->
              let filenames_p4 = [ filename_p4 ] in
              { node with status = Hit filenames_p4 }
          | _ -> node))
    cover

(* Constructor *)

let init (spec : spec) : t = Cover.init_spec spec
