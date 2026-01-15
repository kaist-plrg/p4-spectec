open Domain.Lib
open Lang
open Ol.Ast
open Runtime.Dynamic_Sl
open Envs
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

let rec upstream (ihenv : IHEnv.t) (frees : IdSet.t) (instrs : instr list) :
    instr list =
  match instrs with
  | [] -> []
  | { it = IfI (exp_cond, iterexps, instrs_then); at; _ } :: instrs_t ->
      let frees = Ol.Free.free_exp exp_cond |> IdSet.union frees in
      let instrs_then = upstream ihenv frees instrs_then in
      let instr_h = IfI (exp_cond, iterexps, instrs_then) $ at in
      let frees = Ol.Free.free_instrs instrs_then |> IdSet.union frees in
      let instrs_t = upstream ihenv frees instrs_t in
      instr_h :: instrs_t
  | {
      it = HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold);
      at;
      _;
    }
    :: instrs_t ->
      let frees = Ol.Free.free_exps exps |> IdSet.union frees in
      let instrs_hold = upstream ihenv frees instrs_hold in
      let instrs_nothold = upstream ihenv frees instrs_nothold in
      let instr_h =
        HoldI (id, (mixop, exps), iterexps, instrs_hold, instrs_nothold) $ at
      in
      let frees = Ol.Free.free_instrs instrs_hold |> IdSet.union frees in
      let frees = Ol.Free.free_instrs instrs_nothold |> IdSet.union frees in
      let instrs_t = upstream ihenv frees instrs_t in
      instr_h :: instrs_t
  | { it = CaseI (exp, cases, total); at; _ } :: instrs_t ->
      let frees = Ol.Free.free_exp exp |> IdSet.union frees in
      let cases =
        List.map
          (fun (guard, block) ->
            let frees = Ol.Free.free_guard guard |> IdSet.union frees in
            let block = upstream ihenv frees block in
            (guard, block))
          cases
      in
      let instr_h = CaseI (exp, cases, total) $ at in
      let frees = Ol.Free.free_cases cases |> IdSet.union frees in
      let instrs_t = upstream ihenv frees instrs_t in
      instr_h :: instrs_t
  | { it = GroupI (id, rel_signature, exps, instrs_group); at; _ } :: instrs_t
    ->
      let frees = Ol.Free.free_exps exps |> IdSet.union frees in
      let instrs_group = upstream ihenv frees instrs_group in
      let instr_h = GroupI (id, rel_signature, exps, instrs_group) $ at in
      let frees = Ol.Free.free_instrs instrs_group |> IdSet.union frees in
      let instrs_t = upstream ihenv frees instrs_t in
      instr_h :: instrs_t
  | { it = LetI (exp_l, exp_r, iterinstrs); at; _ } :: instrs_t ->
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
                   let renamer = Renamer.add id_l id_rename renamer in
                   (frees, renamer)
               | None ->
                   let frees = IdSet.add id_l frees in
                   (frees, renamer))
             (frees, Renamer.empty)
      in
      let exp_l = Renamer.rename_exp renamer exp_l in
      let iterinstrs = Renamer.rename_iterinstrs_bind renamer iterinstrs in
      let instr_h = LetI (exp_l, exp_r, iterinstrs) $ at in
      let frees = Ol.Free.free_exp exp_r |> IdSet.union frees in
      let instrs_t =
        Renamer.rename_instrs ihenv renamer instrs_t |> upstream ihenv frees
      in
      instr_h :: instrs_t
  | { it = RuleI (id, (mixop, exps), iterinstrs); at; _ } :: instrs_t ->
      let inputs = IHEnv.find id ihenv in
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
                   let renamer = Renamer.add id_output id_rename renamer in
                   (frees, renamer)
               | None ->
                   let frees = IdSet.add id_output frees in
                   (frees, renamer))
             (frees, Renamer.empty)
      in
      let exps_output = Renamer.rename_exps renamer exps_output in
      let iterinstrs = Renamer.rename_iterinstrs_bind renamer iterinstrs in
      let exps = Hints.Input.combine inputs exps_input exps_output in
      let instr_h = RuleI (id, (mixop, exps), iterinstrs) $ at in
      let instrs_t =
        Renamer.rename_instrs ihenv renamer instrs_t |> upstream ihenv frees
      in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let frees = Ol.Free.free_instr instr_h |> IdSet.union frees in
      let instrs_t = upstream ihenv frees instrs_t in
      instr_h :: instrs_t

let apply_rel (ihenv : IHEnv.t) ((exps_match, instrs) : exp list * instr list) :
    exp list * instr list =
  let frees_match = Ol.Free.free_exps exps_match in
  let frees, exps_match, instrs =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees, exps_match, instrs) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.add id_rename frees in
               let renamer = Renamer.singleton id id_rename in
               let exps_match = Renamer.rename_exps renamer exps_match in
               let instrs = Renamer.rename_instrs ihenv renamer instrs in
               (frees, exps_match, instrs)
           | None ->
               let frees = IdSet.add id frees in
               (frees, exps_match, instrs))
         (IdSet.empty, exps_match, instrs)
  in
  let instrs = upstream ihenv frees instrs in
  (exps_match, instrs)

let apply_func (ihenv : IHEnv.t) ((args_input, instrs) : arg list * instr list)
    : arg list * instr list =
  let frees_args = Ol.Free.free_args args_input in
  let frees, args_input, instrs =
    frees_args |> IdSet.to_list
    |> List.fold_left
         (fun (frees, args_input, instrs) id ->
           match find_rename_ticks frees id with
           | Some id_rename ->
               let frees = IdSet.add id_rename frees in
               let renamer = Renamer.singleton id id_rename in
               let args_input = Renamer.rename_args renamer args_input in
               let instrs = Renamer.rename_instrs ihenv renamer instrs in
               (frees, args_input, instrs)
           | None ->
               let frees = IdSet.add id frees in
               (frees, args_input, instrs))
         (IdSet.empty, args_input, instrs)
  in
  let instrs = upstream ihenv frees instrs in
  (args_input, instrs)
