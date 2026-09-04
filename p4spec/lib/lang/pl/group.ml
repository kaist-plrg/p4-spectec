open Ast
module Annot = Annot

(* A rule group extracted from a dispatch block *)

type t = {
  hints : Annot.hints;
  id_rulegroup : id;
  id_rel : id;
  rel_signature : rel_signature;
  exps : exp list;
  body : block_group;
}

let collect_groups (block : block_dispatch) : t list =
  let rec collect_instr (instr : instr_dispatch instr) : t list =
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
    | CheckLetSubI (_, _, _, _, block_then)
    | CheckLetMatchI (_, _, _, block_then)
    | OptionGetI (_, _, block_then) ->
        collect_block block_then
    | TierI (RouteI arms) -> arms |> List.concat_map collect_block
    | TierI (GroupI (id_rulegroup, id_rel, rel_signature, exps, body)) ->
        [
          {
            hints = instr.hints;
            id_rulegroup;
            id_rel;
            rel_signature;
            exps;
            body;
          };
        ]
  and collect_block (block : block_dispatch) : t list =
    block |> List.concat_map collect_instr
  in
  collect_block block
