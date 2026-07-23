open Common.Unboot
open Common.Stub
open Lang
open Mixops
open Util.Source

(* Forward references for IL dispatch tables,
   populated after all sub-match functions are defined *)

let unboot_param_mtchtbl : Il.param Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_prem_mtchtbl : Il.prem Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_def_mtchtbl : Al.def Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

(* Iter premises *)

let unboot_iterprem (value_iterprem : Value.t) : Il.iterprem =
  let values = Value.Get.(value_iterprem |>>! mop_iterprem) in
  let iter = Value.Get.nth 0 values |> unboot_iter in
  let varis_in = Value.Get.nth 1 values |> unboot_varis in
  let varis_out = Value.Get.nth 2 values |> unboot_varis in
  (iter, varis_in, varis_out)

(* Parameters *)

let rec unboot_param (value_param : Value.t) : Il.param =
  Value.Get.mtch_dispatch value_param !unboot_param_mtchtbl (fun _ _ ->
      error "@unboot_param")

and unboot_exp_param (at : region) (values : Value.t list) : Il.param =
  match values with
  | [ value_typ ] ->
      let typ = unboot_typ value_typ in
      Il.ExpP typ $ at
  | _ -> error "@unboot_exp_param"

and unboot_def_param (at : region) (values : Value.t list) : Il.param =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Il.DefP (id, tparams, params, typ) $ at
  | _ -> error "@unboot_def_param"

and unboot_params (value_params : Value.t) : Il.param list =
  value_params |> Value.Get.list |> List.map unboot_param

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

and unboot_rulmatch (value_rulmatch : Value.t) : Al.rulematch =
  let values = Value.Get.(value_rulmatch |>>! mop_rulematch) in
  let exps = Value.Get.nth 0 values |> unboot_exps in
  let prems = Value.Get.nth 1 values |> unboot_prems in
  (exps, exps, prems)

and unboot_rulpath (value_rulpath : Value.t) : Al.rulepath =
  let values = Value.Get.(value_rulpath |>>! mop_rulepath) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let exps = Value.Get.nth 1 values |> unboot_exps in
  let prems = Value.Get.nth 2 values |> unboot_prems in
  (id, prems, exps)

and unboot_rulpaths (value_rulpaths : Value.t) : Al.rulepath list =
  value_rulpaths |> Value.Get.list |> List.map unboot_rulpath

and unboot_rulgroup (value_rulgroup : Value.t) : Al.rulegroup =
  let at = value_rulgroup.at in
  let values = Value.Get.(value_rulgroup |>>! mop_rulegroup) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let rulmatch = Value.Get.nth 1 values |> unboot_rulmatch in
  let rulpaths = Value.Get.nth 2 values |> unboot_rulpaths in
  (id, rulmatch, rulpaths) $ at

and unboot_rulgroups (value_rulgroups : Value.t) : Al.rulegroup list =
  value_rulgroups |> Value.Get.list |> List.map unboot_rulgroup

and unboot_elsgroup (value_elsgroup : Value.t) : Al.elsegroup =
  let at = value_elsgroup.at in
  let values = Value.Get.(value_elsgroup |>>! mop_elsegroup) in
  let id = Value.Get.nth 0 values |> unboot_id in
  let rulmatch = Value.Get.nth 1 values |> unboot_rulmatch in
  let rulpath = Value.Get.nth 2 values |> unboot_rulpath in
  (id, rulmatch, rulpath) $ at

and unboot_elsgroup_opt (value_elsgroup_opt : Value.t) : Al.elsegroup option =
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

and unboot_tablerow (value_tablerow : Value.t) : Al.tablerow =
  let at = value_tablerow.at in
  let values = Value.Get.(value_tablerow |>>! mop_tablerow) in
  let args = Value.Get.nth 0 values |> unboot_args in
  let exp = Value.Get.nth 1 values |> unboot_exp in
  let prems = Value.Get.nth 2 values |> unboot_prems in
  ([], args, exp, prems) $ at

and unboot_tablerows (value_tablerows : Value.t) : Al.tablerow list =
  value_tablerows |> Value.Get.list |> List.map unboot_tablerow

(* Definitions *)

let unboot_def (value_def : Value.t) : Al.def =
  Value.Get.mtch_dispatch value_def !unboot_def_mtchtbl (fun _ _ ->
      error "@unboot_def")

and unboot_extern_typ_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Al.ExternTypD (id, []) $ at
  | _ -> error "@unboot_extern_typ_def"

and unboot_typ_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id; value_tparams; value_deftyp ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let deftyp = unboot_deftyp value_deftyp in
      Al.TypD (id, tparams, deftyp, []) $ at
  | _ -> error "@unboot_typ_def"

and unboot_extern_rel_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id; value_typs_input; value_typs_output ] ->
      let id = unboot_id value_id in
      let typs_input = unboot_typs value_typs_input in
      let typs_output = unboot_typs value_typs_output in
      let nottyp = stub_nottyp typs_input typs_output in
      let input = stub_input_hint (List.length typs_input) in
      Al.ExternRelD (id, nottyp, input, []) $ at
  | _ -> error "@unboot_extern_rel_def"

and unboot_rel_def (at : region) (values : Value.t list) : Al.def =
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
      Al.RelD (id, nottyp, input, rulgroups, elsgroup, []) $ at
  | _ -> error "@unboot_rel_def"

and unboot_extern_func_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Al.ExternDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_extern_func_def"

and unboot_builtin_func_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Al.BuiltinDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_builtin_func_def"

and unboot_table_func_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [ value_id; value_params; value_typ; value_tablerows ] ->
      let id = unboot_id value_id in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let tablerows = unboot_tablerows value_tablerows in
      Al.TableDecD (id, params, typ, tablerows, []) $ at
  | _ -> error "@unboot_table_func_def"

and unboot_func_def (at : region) (values : Value.t list) : Al.def =
  match values with
  | [
   value_id;
   value_tparams;
   value_params;
   value_typ;
   value_clauses;
   value_elsclause;
  ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let clauses = unboot_clauses value_clauses in
      let elsclause = unboot_elsclause_opt value_elsclause in
      Al.FuncDecD (id, tparams, params, typ, clauses, elsclause, []) $ at
  | _ -> error "@unboot_func_def"

(* Specification *)

let unboot_script (value_script : Value.t) : Al.spec =
  let value_defs = Value.Get.list value_script in
  List.map unboot_def value_defs

(* Initialize IL dispatch tables after all handler functions are defined *)

let () =
  (* Parameters *)
  unboot_param_mtchtbl :=
    Value.Get.build_mtchtbl
      [ (mop_exp_param, unboot_exp_param); (mop_def_param, unboot_def_param) ];
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
  unboot_def_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_extern_typ_def, unboot_extern_typ_def);
        (mop_typ_def, unboot_typ_def);
        (mop_extern_rel_def, unboot_extern_rel_def);
        (mop_rel_def, unboot_rel_def);
        (mop_extern_func_def, unboot_extern_func_def);
        (mop_builtin_func_def, unboot_builtin_func_def);
        (mop_table_func_def, unboot_table_func_def);
        (mop_func_def, unboot_func_def);
      ]
