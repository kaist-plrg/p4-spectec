open Lang
open Pass
open Util.Error
open Util.Source

(* Meta-language layers *)

let frontend filenames_spec =
  filenames_spec |> List.concat_map Frontend.Parse.parse_file

let elab spec_el = spec_el |> Elaborate.Elab.elab_spec
let algo spec_il = spec_il |> Algo.algo_spec
let structure spec_al = spec_al |> Structure.Struct.struct_spec

(* Spec statistics *)

let loc filenames =
  List.fold_left
    (fun loc filename ->
      let ic = open_in filename in
      let loc_file = ref 0 in
      (try
         while true do
           ignore (input_line ic);
           incr loc_file
         done
       with End_of_file -> ());
      close_in ic;
      loc + !loc_file)
    0 filenames

(* EL statistics *)

module ELConstruct = struct
  type t = int * int * int * int

  let count_defs defs : t =
    List.fold_left
      (fun (count_rels, count_rules, count_decs, count_syns) def ->
        match def.it with
        | El.ExternSynD _ | El.TypD _ ->
            (count_rels, count_rules, count_decs, count_syns + 1)
        | El.ExternRelD _ | El.RelD _ ->
            (count_rels + 1, count_rules, count_decs, count_syns)
        | El.RuleGroupD (_, _, rules) ->
            (count_rels, count_rules + List.length rules, count_decs, count_syns)
        | El.ExternDecD _ | El.BuiltinDecD _ | El.TableDecD _ | El.FuncDecD _ ->
            (count_rels, count_rules, count_decs + 1, count_syns)
        | _ -> (count_rels, count_rules, count_decs, count_syns))
      (0, 0, 0, 0) defs
end

let count_constructs_el spec_el = ELConstruct.count_defs spec_el

module ELPrem = struct
  type t = int * int

  let zero : t = (0, 0)
  let add (r_a, i_a) (r_b, i_b) : t = (r_a + r_b, i_a + i_b)

  let rec count_prem prem : t =
    match prem.it with
    | El.VarPr _ -> zero
    | El.RulePr _ -> (1, 0)
    | El.RuleNotPr _ -> (1, 0)
    | El.IfPr _ -> (0, 1)
    | El.ElsePr -> zero
    | El.IterPr (prem, _) -> count_prem prem
    | El.DebugPr _ -> zero

  let count_prems prems : t =
    List.fold_left
      (fun count prem ->
        let count_prem = count_prem prem in
        add count count_prem)
      zero prems

  let count_rule rule : t =
    let _, _, _, prems = rule.it in
    count_prems prems

  let count_rules rules : t =
    List.fold_left
      (fun count rule ->
        let count_rule = count_rule rule in
        add count count_rule)
      zero rules

  let count_def def : t =
    match def.it with
    | El.RuleGroupD (_, _, rules) -> count_rules rules
    | _ -> zero

  let count_defs defs : t =
    List.fold_left
      (fun count def ->
        let count_def = count_def def in
        add count count_def)
      zero defs
end

let count_prems_el spec_el = ELPrem.count_defs spec_el

(* IL statistics *)

module ILPrem = struct
  type t = int * int * int

  let zero : t = (0, 0, 0)
  let add (r_a, i_a, l_a) (r_b, i_b, l_b) : t = (r_a + r_b, i_a + i_b, l_a + l_b)

  let rec count_prem prem : t =
    match prem.it with
    (* Rule premise *)
    | Il.RulePr _ -> (1, 0, 0)
    (* If premise *)
    | Il.IfPr _ -> (0, 1, 0)
    (* If-hold and not-hold premises are specialized Rule premises, branching on relation results *)
    | Il.IfHoldPr _ | Il.IfNotHoldPr _ -> (1, 0, 0)
    (* Let premise *)
    | Il.LetPr _ -> (0, 0, 1)
    (* Iterated premise *)
    | Il.IterPr (prem, _) -> count_prem prem
    (* Debug premise is ignored *)
    | Il.DebugPr _ -> zero

  let count_prems prems : t =
    List.fold_left
      (fun count prem ->
        let count_prem = count_prem prem in
        add count count_prem)
      zero prems

  let count_rule rule : t =
    let _, _, prems = rule.it in
    count_prems prems

  let count_rules rules : t =
    List.fold_left
      (fun count rule ->
        let count_rule = count_rule rule in
        add count count_rule)
      zero rules

  let count_rulegroup rulegroup : t =
    let _, rules = rulegroup.it in
    count_rules rules

  let count_rulegroups rulegroups : t =
    List.fold_left
      (fun count rulegroup ->
        let count_rulegroup = count_rulegroup rulegroup in
        add count count_rulegroup)
      zero rulegroups

  let count_elsegroup elsegroup : t =
    let _, rule = elsegroup.it in
    count_rule rule

  let count_def def : t =
    match def.it with
    | Il.RelD (_, _, _, rulegroups, elsegroup_opt, _) ->
        let count_rulegroups = count_rulegroups rulegroups in
        let count_elsegroup =
          match elsegroup_opt with
          | Some elsegroup -> count_elsegroup elsegroup
          | None -> zero
        in
        add count_rulegroups count_elsegroup
    | _ -> zero

  let count_defs defs : t =
    List.fold_left
      (fun count def ->
        let count_def = count_def def in
        add count count_def)
      zero defs
end

let count_prems_il spec_il =
  let count_rule_prems, count_if_prems, count_let_prems =
    ILPrem.count_defs spec_il
  in
  (count_rule_prems, count_if_prems, count_let_prems)

(* AL statistics *)

module ALPrem = struct
  type t = int * int * int

  let zero : t = (0, 0, 0)
  let add (r_a, i_a, l_a) (r_b, i_b, l_b) : t = (r_a + r_b, i_a + i_b, l_a + l_b)

  let rec count_prem prem : t =
    match prem.it with
    (* Rule premise *)
    | Il.RulePr _ -> (1, 0, 0)
    (* If premise *)
    | Il.IfPr _ -> (0, 1, 0)
    (* If-hold and not-hold premises are specialized Rule premises, branching on relation results *)
    | Il.IfHoldPr _ | Il.IfNotHoldPr _ -> (1, 0, 0)
    (* Let premise *)
    | Il.LetPr _ -> (0, 0, 1)
    (* Iterated premise *)
    | Il.IterPr (prem, _) -> count_prem prem
    (* Debug premise is ignored *)
    | Il.DebugPr _ -> zero

  let count_prems prems : t =
    List.fold_left
      (fun count prem ->
        let count_prem = count_prem prem in
        add count count_prem)
      zero prems

  let count_rulematch rulematch : t =
    let _, _, prems_match = rulematch in
    count_prems prems_match

  let count_rulepath rulepath : t =
    let _, prems_path, _ = rulepath in
    count_prems prems_path

  let count_rulepaths rulepaths : t =
    List.fold_left
      (fun count rulepath ->
        let count_rulepath = count_rulepath rulepath in
        add count count_rulepath)
      zero rulepaths

  let count_rulegroup rulegroup : t =
    let _, rulematch, rulepaths = rulegroup.it in
    let count_rulematch = count_rulematch rulematch in
    let count_rulepaths = count_rulepaths rulepaths in
    add count_rulematch count_rulepaths

  let count_rulegroups rulegroups : t =
    List.fold_left
      (fun count rulegroup ->
        let count_rulegroup = count_rulegroup rulegroup in
        add count count_rulegroup)
      zero rulegroups

  let count_elsegroup elsegroup : t =
    let _, rulematch, rulepath = elsegroup.it in
    let count_rulematch = count_rulematch rulematch in
    let count_rulepath = count_rulepath rulepath in
    add count_rulematch count_rulepath

  let count_def def : t =
    match def.it with
    | Al.RelD (_, _, _, rulegroups, elsegroup_opt, _) ->
        let count_rulegroups = count_rulegroups rulegroups in
        let count_elsegroup =
          match elsegroup_opt with
          | Some elsegroup -> count_elsegroup elsegroup
          | None -> zero
        in
        add count_rulegroups count_elsegroup
    | _ -> zero

  let count_defs defs : t =
    List.fold_left
      (fun count def ->
        let count_def = count_def def in
        add count count_def)
      zero defs
end

let count_prems_al spec_al =
  let count_rule_prems, count_if_prems, count_let_prems =
    ALPrem.count_defs spec_al
  in
  (count_rule_prems, count_if_prems, count_let_prems)

(* SL statistics *)

module SLBranch = struct
  type t = int

  let rec count_instr instr : t =
    match instr.it with
    (* If instruction *)
    | Sl.IfI (_, _, block, _) -> count_block block
    (* Hold instruction is a specialized If, branching on relation results *)
    | Sl.HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | Sl.BothH (block_hold, block_nothold) ->
            1 + count_block block_hold + count_block block_nothold
        | Sl.HoldH (block_hold, _) -> count_block block_hold
        | Sl.NotHoldH (block_nothold, _) -> count_block block_nothold)
    (* Case instruction is essentially a parallelized If *)
    | Sl.CaseI (_, [], _) -> 0
    | Sl.CaseI (_, cases, _) ->
        List.fold_left
          (fun num_branches case ->
            let _, block = case in
            num_branches + count_block block)
          (List.length cases - 1)
          cases
    (* Group instruction itself is ignored *)
    | Sl.GroupI (_, _, _, block) -> count_block block
    (* Let instruction *)
    | Sl.LetI (_, _, _, block) -> count_block block
    (* Rule instruction *)
    | Sl.RuleI (_, _, _, _, block) -> count_block block
    (* Result/Return instruction is ignored *)
    | Sl.ResultI _ | Sl.ReturnI _ -> 0
    (* Debug instruction is ignored *)
    | Sl.DebugI _ -> 0

  and count_block block : t =
    List.fold_left
      (fun num_branches instr -> num_branches + count_instr instr)
      0 block

  let count_def def : t =
    match def.it with
    | Sl.RelD (_, _, _, block, elseblock_opt, _) ->
        let num_branches_block = count_block block in
        let num_branches_elseblock =
          match elseblock_opt with
          | Some elseblock -> count_block elseblock
          | None -> 0
        in
        num_branches_block + num_branches_elseblock
    | _ -> 0

  let count_defs defs : t =
    List.fold_left
      (fun num_branches def ->
        let num_branches_def = count_def def in
        num_branches + num_branches_def)
      0 defs
end

let count_inserted_branches_sl spec_sl = SLBranch.count_defs spec_sl

module SLInstr = struct
  type t = int * int * int

  let zero : t = (0, 0, 0)
  let add (r_a, i_a, l_a) (r_b, i_b, l_b) : t = (r_a + r_b, i_a + i_b, l_a + l_b)

  let rec count_instr instr : t =
    match instr.it with
    (* If instruction *)
    | Sl.IfI (_, _, block, _) ->
        let num_instrs_block = count_block block in
        add (0, 1, 0) num_instrs_block
    (* Hold instruction is a specialized If, branching on relation results *)
    | Sl.HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | Sl.BothH (block_hold, block_nothold) ->
            let num_instrs_block_hold = count_block block_hold in
            let num_instrs_block_nothold = count_block block_nothold in
            add (0, 1, 0) (add num_instrs_block_hold num_instrs_block_nothold)
        | Sl.HoldH (block_hold, _) ->
            let num_instrs_block_hold = count_block block_hold in
            add (0, 1, 0) num_instrs_block_hold
        | Sl.NotHoldH (block_nothold, _) ->
            let num_instrs_block_nothold = count_block block_nothold in
            add (0, 1, 0) num_instrs_block_nothold)
    (* Case instruction is essentially a parallelized If *)
    | Sl.CaseI (_, cases, _) ->
        List.fold_left
          (fun num_instrs case ->
            let _, block = case in
            let num_instrs_block = count_block block in
            (0, 1, 0) |> add num_instrs |> add num_instrs_block)
          zero cases
    (* Group instruction itself is ignored *)
    | Sl.GroupI (_, _, _, block) -> count_block block
    (* Let instruction *)
    | Sl.LetI (_, _, _, block) ->
        let num_instrs_block = count_block block in
        add (0, 0, 1) num_instrs_block
    (* Rule instruction *)
    | Sl.RuleI (_, _, _, _, block) ->
        let num_instrs_block = count_block block in
        add (1, 0, 0) num_instrs_block
    (* Result/Return instruction is ignored *)
    | Sl.ResultI _ | Sl.ReturnI _ -> zero
    (* Debug instruction is ignored *)
    | Sl.DebugI _ -> zero

  and count_block block : t =
    List.fold_left
      (fun num_instrs instr ->
        let num_instrs_instr = count_instr instr in
        add num_instrs num_instrs_instr)
      zero block

  let count_def def : t =
    match def.it with
    | Sl.RelD (_, _, _, block, elseblock_opt, _) ->
        let num_instrs_block = count_block block in
        let num_instrs_elseblock =
          match elseblock_opt with
          | Some elseblock -> count_block elseblock
          | None -> zero
        in
        add num_instrs_block num_instrs_elseblock
    | _ -> zero

  let count_defs defs : t =
    List.fold_left
      (fun num_instrs def ->
        let num_instrs_def = count_def def in
        add num_instrs num_instrs_def)
      zero defs
end

let count_instrs_sl spec_sl = SLInstr.count_defs spec_sl

(* Entry point *)

let () =
  let filenames = Array.to_list Sys.argv |> List.tl in
  if filenames = [] then (
    Printf.eprintf "Usage: perf <spec-file>...\n";
    exit 1);
  try
    let loc = loc filenames in
    Printf.printf "Total lines of code: %d\n" loc;
    let spec_el = filenames |> frontend in
    let num_rels_el, num_rules_el, num_decs_el, num_syns_el =
      count_constructs_el spec_el
    in
    Printf.printf "EL has [ %d ] rel\n" num_rels_el;
    Printf.printf "EL has [ %d ] rule\n" num_rules_el;
    Printf.printf "EL has [ %d ] dec\n" num_decs_el;
    Printf.printf "EL has [ %d ] syn\n" num_syns_el;
    let num_rule_prems_el, num_if_prems_el = count_prems_el spec_el in
    Printf.printf "EL has [ %d ] premises in rules\n"
      (num_rule_prems_el + num_if_prems_el);
    Printf.printf "   - Rule prems: %d\n" num_rule_prems_el;
    Printf.printf "   - If prems:   %d\n" num_if_prems_el;
    let spec_il = spec_el |> elab in
    let num_rule_prems_il, num_if_prems_il, num_let_prems_il =
      count_prems_il spec_il
    in
    Printf.printf "IL has [ %d ] premises in rules\n"
      (num_rule_prems_il + num_if_prems_il + num_let_prems_il);
    Printf.printf "   - Rule prems: %d\n" num_rule_prems_il;
    Printf.printf "   - If prems:   %d\n" num_if_prems_il;
    Printf.printf "   - Let prems:  %d\n" num_let_prems_il;
    let spec_al = spec_il |> algo in
    let num_rule_prems_al, num_if_prems_al, num_let_prems_al =
      count_prems_al spec_al
    in
    Printf.printf "AL has [ %d ] premises in rules\n"
      (num_rule_prems_al + num_if_prems_al + num_let_prems_al);
    Printf.printf "   - Rule prems: %d\n" num_rule_prems_al;
    Printf.printf "   - If prems:   %d\n" num_if_prems_al;
    Printf.printf "   - Let prems:  %d\n" num_let_prems_al;
    let spec_sl = spec_al |> structure in
    let num_rule_instrs_sl, num_if_instrs_sl, num_let_instrs_sl =
      count_instrs_sl spec_sl
    in
    let num_branches_sl = count_inserted_branches_sl spec_sl in
    Printf.printf "SL has [ %d ] instructions\n"
      (num_rule_instrs_sl + num_if_instrs_sl + num_let_instrs_sl);
    Printf.printf "   - Rule instructions: %d\n" num_rule_instrs_sl;
    Printf.printf "   - If instructions:   %d\n" num_if_instrs_sl;
    Printf.printf "   - Let instructions:  %d\n" num_let_instrs_sl;
    Printf.printf "SL has [ %d ] inserted branches\n" num_branches_sl
  with
  | ParseError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
  | ElabError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
  | StructError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
