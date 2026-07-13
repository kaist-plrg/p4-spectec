open Common.Stub
open Lang
open Mixops
open Util.Source

module Make (V : Runtime.Valrep.SAFE) = struct
  include Common.Unboot.Make (V)

  (* Forward references for IL dispatch tables,
     populated after all sub-match functions are defined *)

  let unboot_param_mtchtbl : Il.param Dispatch.mtchtbl ref =
    ref (Dispatch.MtchTbl.create 0)

  let unboot_prem_mtchtbl : Il.prem Dispatch.mtchtbl ref =
    ref (Dispatch.MtchTbl.create 0)

  let unboot_def_mtchtbl : Il.def Dispatch.mtchtbl ref =
    ref (Dispatch.MtchTbl.create 0)

  (* Iter premises *)

  let unboot_iterprem (value_iterprem : V.t) : Il.iterprem =
    let values =
      Dispatch.case_exact value_iterprem mop_iterprem Il_typs.typ_iterprem
    in
    let iter = V.Get.nth 0 values |> unboot_iter in
    let varis_in = V.Get.nth 1 values |> unboot_varis in
    let varis_out = V.Get.nth 2 values |> unboot_varis in
    (iter, varis_in, varis_out)

  (* Parameters *)

  let rec unboot_param (value_param : V.t) : Il.param =
    Dispatch.dispatch value_param Il_typs.typ_param !unboot_param_mtchtbl
      (fun _ _ -> error "@unboot_param")

  and unboot_exp_param (at : region) (values : V.t list) : Il.param =
    match values with
    | [ value_typ ] ->
        let typ = unboot_typ value_typ in
        Il.ExpP typ $ at
    | _ -> error "@unboot_exp_param"

  and unboot_def_param (at : region) (values : V.t list) : Il.param =
    match values with
    | [ value_id; value_tparams; value_params; value_typ ] ->
        let id = unboot_id value_id in
        let tparams = unboot_tparams value_tparams in
        let params = unboot_params value_params in
        let typ = unboot_typ value_typ in
        Il.DefP (id, tparams, params, typ) $ at
    | _ -> error "@unboot_def_param"

  and unboot_params (value_params : V.t) : Il.param list =
    value_params |> V.Get.list |> List.map unboot_param

  (* Premises *)

  and unboot_prem (value_prem : V.t) : Il.prem =
    Dispatch.dispatch value_prem Il_typs.typ_prem !unboot_prem_mtchtbl
      (fun _ _ -> error "@unboot_prem")

  and unboot_prems (value_prems : V.t) : Il.prem list =
    value_prems |> V.Get.list |> List.map unboot_prem

  and unboot_rel_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_id; value_exps_input; value_exps_output ] ->
        let id = unboot_id value_id in
        let exps_input = unboot_exps value_exps_input in
        let exps_output = unboot_exps value_exps_output in
        let notexp = stub_notexp exps_input exps_output in
        let input = stub_input_hint (List.length exps_input) in
        Il.RulePr (id, notexp, input) $ at
    | _ -> error "@unboot_rel_prem"

  and unboot_if_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_exp ] ->
        let exp = unboot_exp value_exp in
        Il.IfPr exp $ at
    | _ -> error "@unboot_if_prem"

  and unboot_ifhold_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_id; value_exps ] ->
        let id = unboot_id value_id in
        let exps = unboot_exps value_exps in
        let notexp = stub_notexp exps [] in
        Il.IfHoldPr (id, notexp) $ at
    | _ -> error "@unboot_ifhold_prem"

  and unboot_ifnothold_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_id; value_exps ] ->
        let id = unboot_id value_id in
        let exps = unboot_exps value_exps in
        let notexp = stub_notexp exps [] in
        Il.IfNotHoldPr (id, notexp) $ at
    | _ -> error "@unboot_ifnothold_prem"

  and unboot_let_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_exp_l; value_exp_r ] ->
        let exp_l = unboot_exp value_exp_l in
        let exp_r = unboot_exp value_exp_r in
        Il.LetPr (exp_l, exp_r) $ at
    | _ -> error "@unboot_let_prem"

  and unboot_iter_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_prem; value_iterprem ] ->
        let prem = unboot_prem value_prem in
        let iterprem = unboot_iterprem value_iterprem in
        Il.IterPr (prem, iterprem) $ at
    | _ -> error "@unboot_iter_prem"

  and unboot_debug_prem (at : region) (values : V.t list) : Il.prem =
    match values with
    | [ value_exp ] ->
        let exp = unboot_exp value_exp in
        Il.DebugPr exp $ at
    | _ -> error "@unboot_debug_prem"

  (* Rule matching and paths *)

  and unboot_rulmatch (value_rulmatch : V.t) : Il.rulematch =
    let values =
      Dispatch.case_exact value_rulmatch mop_rulematch Il_typs.typ_rulmatch
    in
    let exps = V.Get.nth 0 values |> unboot_exps in
    let prems = V.Get.nth 1 values |> unboot_prems in
    (exps, exps, prems)

  and unboot_rulpath (value_rulpath : V.t) : Il.rulepath =
    let values =
      Dispatch.case_exact value_rulpath mop_rulepath Il_typs.typ_rulpath
    in
    let id = V.Get.nth 0 values |> unboot_id in
    let exps = V.Get.nth 1 values |> unboot_exps in
    let prems = V.Get.nth 2 values |> unboot_prems in
    (id, prems, exps)

  and unboot_rulpaths (value_rulpaths : V.t) : Il.rulepath list =
    value_rulpaths |> V.Get.list |> List.map unboot_rulpath

  and unboot_rulgroup (value_rulgroup : V.t) : Il.rulegroup =
    let at = V.at value_rulgroup in
    let values =
      Dispatch.case_exact value_rulgroup mop_rulegroup Il_typs.typ_rulgroup
    in
    let id = V.Get.nth 0 values |> unboot_id in
    let rulmatch = V.Get.nth 1 values |> unboot_rulmatch in
    let rulpaths = V.Get.nth 2 values |> unboot_rulpaths in
    (id, rulmatch, rulpaths) $ at

  and unboot_rulgroups (value_rulgroups : V.t) : Il.rulegroup list =
    value_rulgroups |> V.Get.list |> List.map unboot_rulgroup

  and unboot_elsgroup (value_elsgroup : V.t) : Il.elsegroup =
    let at = V.at value_elsgroup in
    let values =
      Dispatch.case_exact value_elsgroup mop_elsegroup Il_typs.typ_elsgroup
    in
    let id = V.Get.nth 0 values |> unboot_id in
    let rulmatch = V.Get.nth 1 values |> unboot_rulmatch in
    let rulpath = V.Get.nth 2 values |> unboot_rulpath in
    (id, rulmatch, rulpath) $ at

  and unboot_elsgroup_opt (value_elsgroup_opt : V.t) : Il.elsegroup option =
    value_elsgroup_opt |> V.Get.opt |> Option.map unboot_elsgroup

  (* Clauses *)

  and unboot_clause (value_clause : V.t) : Il.clause =
    let at = V.at value_clause in
    let values =
      Dispatch.case_exact value_clause mop_clause Il_typs.typ_clause
    in
    let args = V.Get.nth 0 values |> unboot_args in
    let exp = V.Get.nth 1 values |> unboot_exp in
    let prems = V.Get.nth 2 values |> unboot_prems in
    (args, exp, prems) $ at

  and unboot_clauses (value_clauses : V.t) : Il.clause list =
    value_clauses |> V.Get.list |> List.map unboot_clause

  and unboot_elsclause (value_elsclause : V.t) : Il.elseclause =
    unboot_clause value_elsclause

  and unboot_elsclause_opt (value_elsclause_opt : V.t) : Il.elseclause option =
    value_elsclause_opt |> V.Get.opt |> Option.map unboot_elsclause

  (* Table rows *)

  and unboot_tablerow (value_tablerow : V.t) : Il.tablerow =
    let at = V.at value_tablerow in
    let values =
      Dispatch.case_exact value_tablerow mop_tablerow Il_typs.typ_tblrow
    in
    let args = V.Get.nth 0 values |> unboot_args in
    let exp = V.Get.nth 1 values |> unboot_exp in
    let prems = V.Get.nth 2 values |> unboot_prems in
    ([], args, exp, prems) $ at

  and unboot_tablerows (value_tablerows : V.t) : Il.tablerow list =
    value_tablerows |> V.Get.list |> List.map unboot_tablerow

  (* Definitions *)

  let unboot_def (value_def : V.t) : Il.def =
    Dispatch.dispatch value_def Il_typs.typ_defn !unboot_def_mtchtbl
      (fun _ _ -> error "@unboot_def")

  and unboot_extern_typ_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id ] ->
        let id = unboot_id value_id in
        Il.ExternTypD (id, []) $ at
    | _ -> error "@unboot_extern_typ_def"

  and unboot_typ_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id; value_tparams; value_deftyp ] ->
        let id = unboot_id value_id in
        let tparams = unboot_tparams value_tparams in
        let deftyp = unboot_deftyp value_deftyp in
        Il.TypD (id, tparams, deftyp, []) $ at
    | _ -> error "@unboot_typ_def"

  and unboot_extern_rel_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id; value_typs_input; value_typs_output ] ->
        let id = unboot_id value_id in
        let typs_input = unboot_typs value_typs_input in
        let typs_output = unboot_typs value_typs_output in
        let nottyp = stub_nottyp typs_input typs_output in
        let input = stub_input_hint (List.length typs_input) in
        Il.ExternRelD (id, nottyp, input, []) $ at
    | _ -> error "@unboot_extern_rel_def"

  and unboot_rel_def (at : region) (values : V.t list) : Il.def =
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
    | _ -> error "@unboot_rel_def"

  and unboot_extern_func_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id; value_tparams; value_params; value_typ ] ->
        let id = unboot_id value_id in
        let tparams = unboot_tparams value_tparams in
        let params = unboot_params value_params in
        let typ = unboot_typ value_typ in
        Il.ExternDecD (id, tparams, params, typ, []) $ at
    | _ -> error "@unboot_extern_func_def"

  and unboot_builtin_func_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id; value_tparams; value_params; value_typ ] ->
        let id = unboot_id value_id in
        let tparams = unboot_tparams value_tparams in
        let params = unboot_params value_params in
        let typ = unboot_typ value_typ in
        Il.BuiltinDecD (id, tparams, params, typ, []) $ at
    | _ -> error "@unboot_builtin_func_def"

  and unboot_table_func_def (at : region) (values : V.t list) : Il.def =
    match values with
    | [ value_id; value_params; value_typ; value_tablerows ] ->
        let id = unboot_id value_id in
        let params = unboot_params value_params in
        let typ = unboot_typ value_typ in
        let tablerows = unboot_tablerows value_tablerows in
        Il.TableDecD (id, params, typ, tablerows, []) $ at
    | _ -> error "@unboot_table_func_def"

  and unboot_func_def (at : region) (values : V.t list) : Il.def =
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
        Il.FuncDecD (id, tparams, params, typ, clauses, elsclause, []) $ at
    | _ -> error "@unboot_func_def"

  (* Specification *)

  let unboot_script (value_script : V.t) : Il.spec =
    let value_defs = V.Get.list value_script in
    List.map unboot_def value_defs

  (* Initialize IL dispatch tables after all handler functions are defined *)

  let () =
    (* Parameters *)
    unboot_param_mtchtbl :=
      Dispatch.build_mtchtbl
        [ (mop_exp_param, unboot_exp_param); (mop_def_param, unboot_def_param) ];
    (* Premises *)
    unboot_prem_mtchtbl :=
      Dispatch.build_mtchtbl
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
      Dispatch.build_mtchtbl
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
end
