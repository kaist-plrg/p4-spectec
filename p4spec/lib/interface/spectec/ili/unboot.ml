open Common.Unboot
open Common.Stub
open Lang
open Mixops
open Util.Source

(* Forward references for IL dispatch tables,
   populated after all sub-match functions are defined *)

let unboot_paramIL_mtchtbl : Il.param Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_prem_mtchtbl : Il.prem Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_defIL_mtchtbl : Il.def Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

(* Iter premises *)

let unboot_iterprem (value_iterprem : Value.t) : Il.iterprem =
  let values = Value.Get.(value_iterprem |>>! mop_iterprem) in
  let iter = Value.Get.nth 0 values |> unboot_iter in
  let varis_in = Value.Get.nth 1 values |> unboot_varis in
  let varis_out = Value.Get.nth 2 values |> unboot_varis in
  (iter, varis_in, varis_out)

(* Parameters *)

let rec unboot_paramIL (value_param : Value.t) : Il.param =
  Value.Get.mtch_dispatch value_param !unboot_paramIL_mtchtbl (fun _ _ ->
      error "@unboot_paramIL")

and unboot_exp_paramIL (at : region) (values : Value.t list) : Il.param =
  match values with
  | [ value_typ ] ->
      let typ = unboot_typ value_typ in
      Il.ExpP typ $ at
  | _ -> error "@unboot_exp_paramIL"

and unboot_def_paramIL (at : region) (values : Value.t list) : Il.param =
  match values with
  | [ value_id; value_tparams; value_paramILs; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let paramILs = unboot_paramILs value_paramILs in
      let typ = unboot_typ value_typ in
      Il.DefP (id, tparams, paramILs, typ) $ at
  | _ -> error "@unboot_def_paramIL"

and unboot_paramILs (value_paramILs : Value.t) : Il.param list =
  value_paramILs |> Value.Get.list |> List.map unboot_paramIL

(* Premises *)

and unboot_prem (value_prem : Value.t) : Il.prem =
  Value.Get.mtch_dispatch value_prem !unboot_prem_mtchtbl (fun _ _ ->
      error "@unboot_prem")

and unboot_prems (value_prems : Value.t) : Il.prem list =
  value_prems |> Value.Get.list |> List.map unboot_prem

and unboot_rel_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_exps_input; value_exps_output ] ->
      let id = unboot_id value_id in
      let exps_input = unboot_exps value_exps_input in
      let exps_output = unboot_exps value_exps_output in
      let notexp = stub_notexp exps_input exps_output in
      let input = stub_input_hint (List.length exps_input) in
      Il.RulePr (id, notexp, input) $ at
  | _ -> error "@unboot_rel_prem"

and unboot_if_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Il.IfPr exp $ at
  | _ -> error "@unboot_if_prem"

and unboot_ifhold_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_exps ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let notexp = stub_notexp exps [] in
      Il.IfHoldPr (id, notexp) $ at
  | _ -> error "@unboot_ifhold_prem"

and unboot_ifnothold_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_id; value_exps ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let notexp = stub_notexp exps [] in
      Il.IfNotHoldPr (id, notexp) $ at
  | _ -> error "@unboot_ifnothold_prem"

and unboot_let_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_exp_l; value_exp_r ] ->
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      Il.LetPr (exp_l, exp_r) $ at
  | _ -> error "@unboot_let_prem"

and unboot_iter_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_prem; value_iterprem ] ->
      let prem = unboot_prem value_prem in
      let iterprem = unboot_iterprem value_iterprem in
      Il.IterPr (prem, iterprem) $ at
  | _ -> error "@unboot_iter_prem"

and unboot_debug_prem (at : region) (values : Value.t list) : Il.prem =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Il.DebugPr exp $ at
  | _ -> error "@unboot_debug_prem"

(* Rule matching and paths *)

and unboot_rulmatch (value_rulmatch : Value.t) : Il.rulematch =
  let values = Value.Get.(value_rulmatch |>>! mop_rulematch) in
  let exps = Value.Get.nth 0 values |> unboot_exps in
  let prems = Value.Get.nth 1 values |> unboot_prems in
  (exps, exps, prems)

and unboot_rulpath (value_rulpath : Value.t) : Il.rulepath =
  let values = Value.Get.(value_rulpath |>>! mop_rulepath) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let exps = Value.Get.nth 1 values |> unboot_exps in
  let prems = Value.Get.nth 2 values |> unboot_prems in
  (id, prems, exps)

and unboot_rulpaths (value_rulpaths : Value.t) : Il.rulepath list =
  value_rulpaths |> Value.Get.list |> List.map unboot_rulpath

and unboot_rulgroup (value_rulgroup : Value.t) : Il.rulegroup =
  let at = value_rulgroup.at in
  let values = Value.Get.(value_rulgroup |>>! mop_rulegroup) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let rulmatch = Value.Get.nth 1 values |> unboot_rulmatch in
  let rulpaths = Value.Get.nth 2 values |> unboot_rulpaths in
  (id, rulmatch, rulpaths) $ at

and unboot_rulgroups (value_rulgroups : Value.t) : Il.rulegroup list =
  value_rulgroups |> Value.Get.list |> List.map unboot_rulgroup

and unboot_elsgroup (value_elsgroup : Value.t) : Il.elsegroup =
  let at = value_elsgroup.at in
  let values = Value.Get.(value_elsgroup |>>! mop_elsegroup) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let rulmatch = Value.Get.nth 1 values |> unboot_rulmatch in
  let rulpath = Value.Get.nth 2 values |> unboot_rulpath in
  (id, rulmatch, rulpath) $ at

and unboot_elsgroup_opt (value_elsgroup_opt : Value.t) : Il.elsegroup option =
  value_elsgroup_opt |> Value.Get.opt |> Option.map unboot_elsgroup

(* Clauses *)

and unboot_clause (value_clause : Value.t) : Il.clause =
  let at = value_clause.at in
  let values = Value.Get.(value_clause |>>! mop_clause) in
  let args = Value.Get.nth 0 values |> unboot_args in
  let exp = Value.Get.nth 1 values |> unboot_exp in
  let prems = Value.Get.nth 2 values |> unboot_prems in
  (args, exp, prems) $ at

and unboot_clauses (value_clauses : Value.t) : Il.clause list =
  value_clauses |> Value.Get.list |> List.map unboot_clause

and unboot_elsclause (value_elsclause : Value.t) : Il.elseclause =
  unboot_clause value_elsclause

and unboot_elsclause_opt (value_elsclause_opt : Value.t) : Il.elseclause option
    =
  value_elsclause_opt |> Value.Get.opt |> Option.map unboot_elsclause

(* Table rows *)

and unboot_tablerowIL (value_tablerowIL : Value.t) : Il.tablerow =
  let at = value_tablerowIL.at in
  let values = Value.Get.(value_tablerowIL |>>! mop_tablerowIL) in
  let args = Value.Get.nth 0 values |> unboot_args in
  let exp = Value.Get.nth 1 values |> unboot_exp in
  let prems = Value.Get.nth 2 values |> unboot_prems in
  ([], args, exp, prems) $ at

and unboot_tablerowILs (value_tablerowILs : Value.t) : Il.tablerow list =
  value_tablerowILs |> Value.Get.list |> List.map unboot_tablerowIL

(* Definitions *)

let unboot_defIL (value_def : Value.t) : Il.def =
  Value.Get.mtch_dispatch value_def !unboot_defIL_mtchtbl (fun _ _ ->
      error "@unboot_defIL")

and unboot_extern_typ_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Il.ExternTypD (id, []) $ at
  | _ -> error "@unboot_extern_typ_defIL"

and unboot_typ_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_deftyp ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let deftyp = unboot_deftyp value_deftyp in
      Il.TypD (id, tparams, deftyp, []) $ at
  | _ -> error "@unboot_typ_defIL"

and unboot_extern_rel_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_typs_input; value_typs_output ] ->
      let id = unboot_id value_id in
      let typs_input = unboot_typs value_typs_input in
      let typs_output = unboot_typs value_typs_output in
      let nottyp = stub_nottyp typs_input typs_output in
      let input = stub_input_hint (List.length typs_input) in
      Il.ExternRelD (id, nottyp, input, []) $ at
  | _ -> error "@unboot_extern_rel_defIL"

and unboot_rel_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [
   value_id;
   value_typs_input;
   value_typs_output;
   value_rulgroups;
   value_elsgroup;
  ] ->
      let id = unboot_id value_id in
      let typs_input = unboot_typs value_typs_input in
      let typs_output = unboot_typs value_typs_output in
      let nottyp = stub_nottyp typs_input typs_output in
      let input = stub_input_hint (List.length typs_input) in
      let rulgroups = unboot_rulgroups value_rulgroups in
      let elsgroup = unboot_elsgroup_opt value_elsgroup in
      Il.RelD (id, nottyp, input, rulgroups, elsgroup, []) $ at
  | _ -> error "@unboot_rel_defIL"

and unboot_extern_func_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_paramILs; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let paramILs = unboot_paramILs value_paramILs in
      let typ = unboot_typ value_typ in
      Il.ExternDecD (id, tparams, paramILs, typ, []) $ at
  | _ -> error "@unboot_extern_func_defIL"

and unboot_builtin_func_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_tparams; value_paramILs; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let paramILs = unboot_paramILs value_paramILs in
      let typ = unboot_typ value_typ in
      Il.BuiltinDecD (id, tparams, paramILs, typ, []) $ at
  | _ -> error "@unboot_builtin_func_defIL"

and unboot_table_func_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [ value_id; value_paramILs; value_typ; value_tablerowILs ] ->
      let id = unboot_id value_id in
      let paramILs = unboot_paramILs value_paramILs in
      let typ = unboot_typ value_typ in
      let tablerowILs = unboot_tablerowILs value_tablerowILs in
      Il.TableDecD (id, paramILs, typ, tablerowILs, []) $ at
  | _ -> error "@unboot_table_func_defIL"

and unboot_func_defIL (at : region) (values : Value.t list) : Il.def =
  match values with
  | [
   value_id;
   value_tparams;
   value_paramILs;
   value_typ;
   value_clauses;
   value_elsclause;
  ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let paramILs = unboot_paramILs value_paramILs in
      let typ = unboot_typ value_typ in
      let clauses = unboot_clauses value_clauses in
      let elsclause = unboot_elsclause_opt value_elsclause in
      Il.FuncDecD (id, tparams, paramILs, typ, clauses, elsclause, []) $ at
  | _ -> error "@unboot_func_defIL"

(* Specification *)

let unboot_scriptIL (value_scriptIL : Value.t) : Il.spec =
  let value_defILs = Value.Get.list value_scriptIL in
  List.map unboot_defIL value_defILs

(* Initialize IL dispatch tables after all handler functions are defined *)

let () =
  (* Parameters *)
  unboot_paramIL_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_exp_paramIL, unboot_exp_paramIL);
        (mop_def_paramIL, unboot_def_paramIL);
      ];
  (* Premises *)
  unboot_prem_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_rel_prem, unboot_rel_prem);
        (mop_if_prem, unboot_if_prem);
        (mop_if_hold_prem, unboot_ifhold_prem);
        (mop_if_nothold_prem, unboot_ifnothold_prem);
        (mop_let_prem, unboot_let_prem);
        (mop_iter_prem, unboot_iter_prem);
        (mop_debug_prem, unboot_debug_prem);
      ];
  (* Definitions *)
  unboot_defIL_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_extern_typ_defIL, unboot_extern_typ_defIL);
        (mop_typ_defIL, unboot_typ_defIL);
        (mop_extern_rel_defIL, unboot_extern_rel_defIL);
        (mop_rel_defIL, unboot_rel_defIL);
        (mop_extern_func_defIL, unboot_extern_func_defIL);
        (mop_builtin_func_defIL, unboot_builtin_func_defIL);
        (mop_table_func_defIL, unboot_table_func_defIL);
        (mop_func_defIL, unboot_func_defIL);
      ]
