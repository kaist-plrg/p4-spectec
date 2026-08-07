open Ast
module Annot = Annot

(* Flat list of the dispatch block's GroupI leaves, each paired with the hints
   its wrapping instr carries (needed for that group's own title) *)

let collect_groups (block : block_dispatch) :
    (Annot.hints * instr_dispatch) list =
  let rec collect_instr (instr : instr_dispatch instr) :
      (Annot.hints * instr_dispatch) list =
    match instr.node.it with
    | IfI (_, _, block_then, _) -> collect_block block_then
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            collect_block block_hold @ collect_block block_nothold
        | HoldH (block_hold, _) -> collect_block block_hold
        | NotHoldH (block_nothold, _) -> collect_block block_nothold)
    | CaseI (_, cases, _) ->
        cases |> List.concat_map (fun (_, block) -> collect_block block)
    | LetI _ | DebugI _ | DestructI _ -> []
    | CheckLetSubI (_, _, _, block_then)
    | CheckLetMatchI (_, _, _, block_then)
    | OptionGetI (_, _, block_then) ->
        collect_block block_then
    | TierI (BlockI arms) -> arms |> List.concat_map collect_block
    | TierI (GroupI _ as group) -> [ (instr.hints, group) ]
  and collect_block (block : block_dispatch) :
      (Annot.hints * instr_dispatch) list =
    block |> List.concat_map collect_instr
  in
  collect_block block
