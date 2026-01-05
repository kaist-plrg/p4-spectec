open Domain.Lib
open Lang.Sl
open Util.Source

(* Instruction node *)

module Node = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status *)

  type status = Hit | Miss

  (* Type *)

  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss }

  (* Equivalence *)

  let eq (node_a : t) (node_b : t) : bool =
    node_a.origin = node_b.origin && node_a.status = node_b.status

  (* Printer *)

  let to_string (node : t) : string =
    match node.status with Hit -> "H" | Miss -> "M"
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
    let cover = add iid node cover in
    match instr.it with
    | IfI (_, _, instrs_then, _) -> init_instrs cover id instrs_then
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (instrs_hold, instrs_nothold) ->
            let cover = init_instrs cover id instrs_hold in
            init_instrs cover id instrs_nothold
        | HoldH (instrs_hold, _) -> init_instrs cover id instrs_hold
        | NotHoldH (instrs_nothold, _) -> init_instrs cover id instrs_nothold)
    | CaseI (_, cases, _) ->
        let blocks = cases |> List.split |> snd in
        List.fold_left
          (fun cover instrs -> init_instrs cover id instrs)
          cover blocks
    | OtherwiseI instr -> init_instr cover id instr
    | GroupI (_, _, instrs_group) -> init_instrs cover id instrs_group
    | _ -> cover

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

(* Querying coverage *)

let is_hit (cover : t) (iid : iid) : bool =
  let node = Cover.find iid cover in
  match node.status with Hit -> true | Miss -> false

let is_miss (cover : t) (iid : iid) : bool =
  let node = Cover.find iid cover in
  match node.status with Hit -> false | Miss -> true

(* Hit *)

let hit (cover : t) (iid : iid) : t =
  match Cover.find_opt iid cover with
  | Some node when node.status = Node.Miss ->
      let hit_node = { node with Node.status = Node.Hit } in
      Cover.add iid hit_node cover
  | _ -> cover

(* Extending coverage *)

let extend (cover : t) (cover_extend : t) : t =
  Cover.fold
    (fun iid (node : Node.t) cover ->
      match node.status with Node.Hit -> hit cover iid | Node.Miss -> cover)
    cover_extend cover

(* Constructor *)

let init (spec : spec) : t = Cover.init_spec spec
let empty : t = Cover.empty
