open Domain.Lib
open Lang
open Il
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
  let frees = IdSet.remove id frees in
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

let rename_ticks_rel (exps_match : exp list) (instrs : Ol.Ast.instr list) :
    exp list * Ol.Ast.instr list =
  let frees_match = Il.Free.free_exps exps_match in
  let frees_instrs = Ol.Free.free_instrs instrs in
  let _, exps_match, instrs =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees_instrs, exps_match, instrs) id_match ->
           match find_rename_ticks frees_instrs id_match with
           | Some id_rename ->
               let frees_instrs = IdSet.remove id_match frees_instrs in
               let frees_instrs = IdSet.add id_rename frees_instrs in
               let rename = Renamer.Rename.singleton id_match id_rename in
               let exps_match = Renamer.rename_exps rename exps_match in
               let instrs = Renamer.rename_instrs rename instrs in
               (frees_instrs, exps_match, instrs)
           | None -> (frees_instrs, exps_match, instrs))
         (frees_instrs, exps_match, instrs)
  in
  (exps_match, instrs)

let rename_ticks_func (args_input : arg list) (instrs : Ol.Ast.instr list) :
    arg list * Ol.Ast.instr list =
  let frees_match = Il.Free.free_args args_input in
  let frees_instrs = Ol.Free.free_instrs instrs in
  let _, args_input, instrs =
    frees_match |> IdSet.to_list
    |> List.fold_left
         (fun (frees_instrs, args_input, instrs) id_match ->
           match find_rename_ticks frees_instrs id_match with
           | Some id_rename ->
               let frees_instrs = IdSet.remove id_match frees_instrs in
               let frees_instrs = IdSet.add id_rename frees_instrs in
               let rename = Renamer.Rename.singleton id_match id_rename in
               let args_input = Renamer.rename_args rename args_input in
               let instrs = Renamer.rename_instrs rename instrs in
               (frees_instrs, args_input, instrs)
           | None -> (frees_instrs, args_input, instrs))
         (frees_instrs, args_input, instrs)
  in
  (args_input, instrs)

(* Prettify instructions *)

let pretty_rel (exps_match : exp list) (instrs : Ol.Ast.instr list) :
    exp list * Ol.Ast.instr list =
  rename_ticks_rel exps_match instrs

let pretty_func (args_input : arg list) (instrs : Ol.Ast.instr list) :
    arg list * Ol.Ast.instr list =
  rename_ticks_func args_input instrs
