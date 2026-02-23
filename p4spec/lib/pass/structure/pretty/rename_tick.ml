open Domain.Lib
open Lang
open Ol.Ast
open Util.Source

(* Rename ticks in relation input expressions
   and function input arguments, which likely were
   introduced as fresh variables during anti-unification

   def $foo(n''') ...

   will be prettified to

   def $foo(n) ... *)

let count_trailing_ticks (id : Id.t) : int =
  let rec count_trailing_ticks (n_guess : int) =
    let ticks = String.make n_guess '\'' in
    if String.ends_with ~suffix:ticks id.it then
      count_trailing_ticks (n_guess + 1)
    else n_guess - 1
  in
  count_trailing_ticks 1

let strip_trailing_ticks (id : Id.t) : Id.t =
  let n_ticks = count_trailing_ticks id in
  if n_ticks = 0 then id
  else String.sub id.it 0 (String.length id.it - n_ticks) $ id.at

let find_rename_ticks (frees : IdSet.t) (id : Id.t) : Id.t option =
  let id_strip = strip_trailing_ticks id in
  let counts_overlap =
    frees |> IdSet.to_list
    |> List.filter_map (fun id_free ->
           if Id.eq (strip_trailing_ticks id_free) id_strip then
             Some (count_trailing_ticks id_free)
           else None)
  in
  let count_min =
    let rec find_count_min n =
      if List.mem n counts_overlap then find_count_min (n + 1) else n
    in
    find_count_min 0
  in
  let id_rename = id_strip.it ^ String.make count_min '\'' $ id.at in
  if Id.eq id id_rename then None else Some id_rename

let rec upstream_instr (frees : IdSet.t) (instr : instr) : instr =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let frees = Ol.Free.free_exp exp_cond |> IdSet.union frees in
      let block_then = upstream_block frees block_then in
      IfI (exp_cond, iterexps, block_then) $ instr.at
  | HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) ->
      let frees = Ol.Free.free_exps exps |> IdSet.union frees in
      let block_hold = upstream_block frees block_hold in
      let block_nothold = upstream_block frees block_nothold in
      HoldI (id, (mixop, exps), iterexps, block_hold, block_nothold) $ instr.at
  | CaseI (exp, cases, total) ->
      let frees = Ol.Free.free_exp exp |> IdSet.union frees in
      let cases =
        List.map
          (fun (guard, block) ->
            let frees = Ol.Free.free_guard guard |> IdSet.union frees in
            let block = upstream_block frees block in
            (guard, block))
          cases
      in
      CaseI (exp, cases, total) $ instr.at
  | GroupI (id, rel_signature, exps, block) ->
      let frees = Ol.Free.free_exps exps |> IdSet.union frees in
      let block = upstream_block frees block in
      GroupI (id, rel_signature, exps, block) $ instr.at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let frees_l = Ol.Free.free_exp exp_l in
      let frees, renamer =
        frees_l |> IdSet.to_list
        |> List.fold_left
             (fun (frees, renamer) id_l ->
               match find_rename_ticks frees id_l with
               | Some id_rename ->
                   let frees =
                     frees |> IdSet.remove id_l |> IdSet.add id_rename
                   in
                   let renamer = Re.Renamer.add id_l id_rename renamer in
                   (frees, renamer)
               | None ->
                   let frees = IdSet.add id_l frees in
                   (frees, renamer))
             (frees, Re.Renamer.empty)
      in
      let exp_l = Re.Renamer.rename_exp renamer exp_l in
      let iterinstrs = Re.Renamer.rename_iterinstrs_bind renamer iterinstrs in
      let frees = Ol.Free.free_exp exp_r |> IdSet.union frees in
      let block =
        Re.Renamer.rename_block renamer block |> upstream_block frees
      in
      LetI (exp_l, exp_r, iterinstrs, block) $ instr.at
  | RuleI (id, (mixop, exps), inputs, iterinstrs, block) ->
      let exps_input, exps_output = Hints.Input.split inputs exps in
      let frees_output = Ol.Free.free_exps exps_output in
      let frees, renamer =
        frees_output |> IdSet.to_list
        |> List.fold_left
             (fun (frees, renamer) id_output ->
               match find_rename_ticks frees id_output with
               | Some id_rename ->
                   let frees =
                     frees |> IdSet.remove id_output |> IdSet.add id_rename
                   in
                   let renamer = Re.Renamer.add id_output id_rename renamer in
                   (frees, renamer)
               | None ->
                   let frees = IdSet.add id_output frees in
                   (frees, renamer))
             (frees, Re.Renamer.empty)
      in
      let exps_output = Re.Renamer.rename_exps renamer exps_output in
      let iterinstrs = Re.Renamer.rename_iterinstrs_bind renamer iterinstrs in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let frees = Ol.Free.free_exps exps_input |> IdSet.union frees in
      let block =
        Re.Renamer.rename_block renamer block |> upstream_block frees
      in
      RuleI (id, (mixop, exps), inputs, iterinstrs, block) $ instr.at
  | _ -> instr

and upstream_block (frees : IdSet.t) (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let instr_h = upstream_instr frees instr_h in
      let block_t = upstream_block frees block_t in
      instr_h :: block_t

let apply_rel
    ((exps_match, block, elseblock_opt) : exp list * block * elseblock option) :
    exp list * block * elseblock option =
  let frees_match = Ol.Free.free_exps exps_match in
  let frees, exps_match, block, elseblock_opt =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees, exps_match, block, elseblock_opt) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.add id_rename frees in
               let renamer = Re.Renamer.singleton id id_rename in
               let exps_match = Re.Renamer.rename_exps renamer exps_match in
               let block = Re.Renamer.rename_block renamer block in
               let elseblock_opt =
                 Option.map (Re.Renamer.rename_block renamer) elseblock_opt
               in
               (frees, exps_match, block, elseblock_opt)
           | None ->
               let frees = IdSet.add id frees in
               (frees, exps_match, block, elseblock_opt))
         (IdSet.empty, exps_match, block, elseblock_opt)
  in
  let block = upstream_block frees block in
  let elseblock_opt = Option.map (upstream_block frees) elseblock_opt in
  (exps_match, block, elseblock_opt)

let apply_func
    ((args_input, block, elseblock_opt) : arg list * block * elseblock option) :
    arg list * block * elseblock option =
  let frees_args = Ol.Free.free_args args_input in
  let frees, args_input, block, elseblock_opt =
    frees_args |> IdSet.to_list
    |> List.fold_left
         (fun (frees, args_input, block, elseblock_opt) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.add id_rename frees in
               let renamer = Re.Renamer.singleton id id_rename in
               let args_input = Re.Renamer.rename_args renamer args_input in
               let block = Re.Renamer.rename_block renamer block in
               let elseblock_opt =
                 Option.map (Re.Renamer.rename_block renamer) elseblock_opt
               in
               (frees, args_input, block, elseblock_opt)
           | None ->
               let frees = IdSet.add id frees in
               (frees, args_input, block, elseblock_opt))
         (IdSet.empty, args_input, block, elseblock_opt)
  in
  let block = upstream_block frees block in
  let elseblock_opt = Option.map (upstream_block frees) elseblock_opt in
  (args_input, block, elseblock_opt)
