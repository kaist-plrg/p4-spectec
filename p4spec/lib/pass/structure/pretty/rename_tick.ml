open Domain.Lib
module Mixfix = Domain.Mixfix
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
    frees |> IdSet.remove id |> IdSet.to_list
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
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let exps = Mixfix.args notexp in
      let frees = Ol.Free.free_exps exps |> IdSet.union frees in
      let block_hold = upstream_block frees block_hold in
      let block_nothold = upstream_block frees block_nothold in
      HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at
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
      upstream_let_instr frees instr.at exp_l exp_r iterinstrs block
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      upstream_rule_instr frees instr.at id notexp inputs iterinstrs block
  | _ -> instr

and upstream_let_instr (frees_upstream : IdSet.t) (at : region) (exp_l : exp)
    (exp_r : exp) (iterinstrs : iterinstr list) (block : block) : instr =
  let frees_l = Ol.Free.free_exp exp_l in
  let frees_r = Ol.Free.free_exp exp_r in
  let frees_block = Ol.Free.free_block block in
  let frees_downstream =
    frees_l |> IdSet.union frees_r |> IdSet.union frees_block
    |> IdSet.union frees_upstream
  in
  let _, renamer =
    frees_l |> IdSet.to_list
    |> List.fold_left
         (fun (frees, renamer) id_l ->
           match find_rename_ticks frees id_l with
           | Some id_rename ->
               let frees = frees |> IdSet.remove id_l |> IdSet.add id_rename in
               let renamer = Re.Renamer.add id_l id_rename renamer in
               (frees, renamer)
           | None ->
               let frees = IdSet.add id_l frees in
               (frees, renamer))
         (frees_downstream, Re.Renamer.empty)
  in
  let exp_l = Re.Renamer.rename_exp renamer exp_l in
  let iterinstrs = Re.Renamer.rename_iterinstrs_bind renamer iterinstrs in
  let frees_l = Ol.Free.free_exp exp_l in
  let frees_r = Ol.Free.free_exp exp_r in
  let frees_upstream =
    frees_l |> IdSet.union frees_r |> IdSet.union frees_upstream
  in
  let block =
    Re.Renamer.rename_block renamer block |> upstream_block frees_upstream
  in
  LetI (exp_l, exp_r, iterinstrs, block) $ at

and upstream_rule_instr (frees_upstream : IdSet.t) (at : region) (id : id)
    (notexp : notexp) (inputs : Hints.Input.t) (iterinstrs : iterinstr list)
    (block : block) : instr =
  let mixop, exps = Mixfix.split notexp in
  let exps_input, exps_output = Hints.Input.split inputs exps in
  let frees_input = Ol.Free.free_exps exps_input in
  let frees_output = Ol.Free.free_exps exps_output in
  let frees_block = Ol.Free.free_block block in
  let frees_downstream =
    frees_input |> IdSet.union frees_output |> IdSet.union frees_block
    |> IdSet.union frees_upstream
  in
  let _, renamer =
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
         (frees_downstream, Re.Renamer.empty)
  in
  let exps_output = Re.Renamer.rename_exps renamer exps_output in
  let iterinstrs = Re.Renamer.rename_iterinstrs_bind renamer iterinstrs in
  let exps = Hints.Input.combine inputs exps_input exps_output in
  let frees_exps = Ol.Free.free_exps exps in
  let frees_upstream = frees_exps |> IdSet.union frees_upstream in
  let block =
    Re.Renamer.rename_block renamer block |> upstream_block frees_upstream
  in
  RuleI (id, Mixfix.fill mixop exps, inputs, iterinstrs, block) $ at

and upstream_block (frees_upstream : IdSet.t) (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let instr_h = upstream_instr frees_upstream instr_h in
      let block_t = upstream_block frees_upstream block_t in
      instr_h :: block_t

let upstream_exps (exps_match : exp list) (block : block)
    (elseblock_opt : elseblock option) : exp list * block * elseblock option =
  let frees_exps = Ol.Free.free_exps exps_match in
  let frees_block = Ol.Free.free_block block in
  let frees_elseblock =
    elseblock_opt
    |> Option.map Ol.Free.free_block
    |> Option.value ~default:IdSet.empty
  in
  let frees_downstream =
    frees_exps |> IdSet.union frees_block |> IdSet.union frees_elseblock
  in
  let _, exps_match, block, elseblock_opt =
    frees_exps |> IdSet.to_list
    |> List.fold_left
         (fun (frees, exps_match, block, elseblock_opt) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.remove id frees |> IdSet.add id_rename in
               let renamer = Re.Renamer.singleton id id_rename in
               let exps_match = Re.Renamer.rename_exps renamer exps_match in
               let block = Re.Renamer.rename_block renamer block in
               let elseblock_opt =
                 Option.map (Re.Renamer.rename_block renamer) elseblock_opt
               in
               (frees, exps_match, block, elseblock_opt)
           | None -> (frees, exps_match, block, elseblock_opt))
         (frees_downstream, exps_match, block, elseblock_opt)
  in
  (exps_match, block, elseblock_opt)

let upstream_args (args_input : arg list) (block : block)
    (elseblock_opt : elseblock option) : arg list * block * elseblock option =
  let frees_args = Ol.Free.free_args args_input in
  let frees_block = Ol.Free.free_block block in
  let frees_elseblock =
    elseblock_opt
    |> Option.map Ol.Free.free_block
    |> Option.value ~default:IdSet.empty
  in
  let frees_downstream =
    frees_args |> IdSet.union frees_block |> IdSet.union frees_elseblock
  in
  let _, args_input, block, elseblock_opt =
    frees_args |> IdSet.to_list
    |> List.fold_left
         (fun (frees, args_input, block, elseblock_opt) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.remove id frees |> IdSet.add id_rename in
               let renamer = Re.Renamer.singleton id id_rename in
               let args_input = Re.Renamer.rename_args renamer args_input in
               let block = Re.Renamer.rename_block renamer block in
               let elseblock_opt =
                 Option.map (Re.Renamer.rename_block renamer) elseblock_opt
               in
               (frees, args_input, block, elseblock_opt)
           | None -> (frees, args_input, block, elseblock_opt))
         (frees_downstream, args_input, block, elseblock_opt)
  in
  (args_input, block, elseblock_opt)

let apply_rel
    ((exps_match, block, elseblock_opt) : exp list * block * elseblock option) :
    exp list * block * elseblock option =
  let exps_match, block, elseblock_opt =
    upstream_exps exps_match block elseblock_opt
  in
  let frees_upstream = Ol.Free.free_exps exps_match in
  let block = upstream_block frees_upstream block in
  let elseblock_opt =
    Option.map (upstream_block frees_upstream) elseblock_opt
  in
  (exps_match, block, elseblock_opt)

let apply_func
    ((args_input, block, elseblock_opt) : arg list * block * elseblock option) :
    arg list * block * elseblock option =
  let args_input, block, elseblock_opt =
    upstream_args args_input block elseblock_opt
  in
  let frees_upstream = Ol.Free.free_args args_input in
  let block = upstream_block frees_upstream block in
  let elseblock_opt =
    Option.map (upstream_block frees_upstream) elseblock_opt
  in
  (args_input, block, elseblock_opt)
