open Domain.Lib
open Lang
open Sl
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
    Hints.Flag.init hints "cover_instr_ignore"

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    let iid = instr.note.iid in
    let node = Node.init id in
    let cover = add iid node cover in
    match instr.it with
    | IfI (_, _, block_then, _) -> init_block cover id block_then
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            let cover = init_block cover id block_hold in
            init_block cover id block_nothold
        | HoldH (block_hold, _) -> init_block cover id block_hold
        | NotHoldH (block_nothold, _) -> init_block cover id block_nothold)
    | CaseI (_, cases, _) ->
        let blocks = cases |> List.split |> snd in
        List.fold_left
          (fun cover block -> init_block cover id block)
          cover blocks
    | GroupI (_, _, _, block_group) -> init_block cover id block_group
    | LetI (_, _, _, block) -> init_block cover id block
    | RuleI (_, _, _, _, block) -> init_block cover id block
    | _ -> cover

  and init_block (cover : t) (id : id) (block : block) : t =
    List.fold_left (fun cover instr -> init_instr cover id instr) cover block

  let init_tablerow (cover : t) (id : id) (tablerow : tablerow) : t =
    let _, _, block = tablerow in
    init_block cover id block

  let init_tablerows (cover : t) (id : id) (tablerows : tablerow list) : t =
    List.fold_left
      (fun cover tablerow -> init_tablerow cover id tablerow)
      cover tablerows

  let init_def (cover : t) (def : def) : t =
    match def.it with
    | RelD (id, _, _, block, elseblock_opt, hints) when not (is_ignored hints)
      -> (
        let cover = init_block cover id block in
        match elseblock_opt with
        | Some elseblock -> init_block cover id elseblock
        | None -> cover)
    | FuncDecD (id, _, _, _, block, elseblock_opt, hints)
      when not (is_ignored hints) -> (
        let cover = init_block cover id block in
        match elseblock_opt with
        | Some elseblock -> init_block cover id elseblock
        | None -> cover)
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
