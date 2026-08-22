open Common.Unboot
open Common.Stub
open Lang
open Mixops
open Stub
open Util.Source

(* Forward references for SL dispatch tables,
   populated after all sub-match functions are defined *)

let unboot_param_mtchtbl : Sl.param Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_holdcase_mtchtbl : Sl.holdcase Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_guard_mtchtbl : Sl.guard Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_instr_mtchtbl : Sl.instr Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

let unboot_def_mtchtbl : Sl.def Value.Get.mtchtbl ref =
  ref (Value.Get.MtchTbl.create 0)

(* Iter instrises *)

let unboot_iterinstr (value_iterinstr : Value.t) : Sl.iterinstr =
  let values = Value.Get.(value_iterinstr |>>! mop_iterinstr) in
  let iter = Value.Get.nth 0 values |> unboot_iter in
  let varis_in = Value.Get.nth 1 values |> unboot_varis in
  let varis_out = Value.Get.nth 2 values |> unboot_varis in
  (iter, varis_in, varis_out)

let unboot_iterinstrs (value_iterinstrs : Value.t) : Sl.iterinstr list =
  value_iterinstrs |> Value.Get.list |> List.map unboot_iterinstr

(* Parameters *)

let rec unboot_param (value_param : Value.t) : Sl.param =
  Value.Get.mtch_dispatch value_param !unboot_param_mtchtbl (fun _ _ ->
      error "@unboot_param")

and unboot_exp_param (at : region) (values : Value.t list) : Sl.param =
  match values with
  | [ value_typ; value_exp ] ->
      let typ = unboot_typ value_typ in
      let exp = unboot_exp value_exp in
      Sl.ExpP (typ, exp) $ at
  | _ -> error "@unboot_exp_param"

and unboot_def_param (at : region) (values : Value.t list) : Sl.param =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Sl.DefP (id, tparams, params, typ) $ at
  | _ -> error "@unboot_def_param"

and unboot_params (value_params : Value.t) : Sl.param list =
  value_params |> Value.Get.list |> List.map unboot_param

(* Instructions *)

let rec unboot_instr (value_instr : Value.t) : Sl.instr =
  Value.Get.mtch_dispatch value_instr !unboot_instr_mtchtbl (fun _ _ ->
      error "@unboot_instr")

and unboot_if_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_exp; value_iterexps; value_block ] ->
      let exp = unboot_exp value_exp in
      let iterexps = unboot_iterexps value_iterexps in
      let block = unboot_block value_block in
      let dangle = stub_dangle in
      Sl.IfI (exp, iterexps, block, dangle) $$ (at, stub_instr_note)
  | _ -> error "@unboot_if_instr"

and unboot_holdcase (value_holdcase : Value.t) : Sl.holdcase =
  Value.Get.mtch_dispatch value_holdcase !unboot_holdcase_mtchtbl (fun _ _ ->
      error "@unboot_holdcase")

and unboot_both_holdcase (_at : region) (values : Value.t list) : Sl.holdcase =
  match values with
  | [ value_block_hold; value_block_nothold ] ->
      let block_hold = unboot_block value_block_hold in
      let block_nothold = unboot_block value_block_nothold in
      Sl.BothH (block_hold, block_nothold)
  | _ -> error "@unboot_both_holdcase"

and unboot_hold_holdcase (_at : region) (values : Value.t list) : Sl.holdcase =
  match values with
  | [ value_block ] ->
      let block = unboot_block value_block in
      let dangle = stub_dangle in
      Sl.HoldH (block, dangle)
  | _ -> error "@unboot_hold_holdcase"

and unboot_nothold_holdcase (_at : region) (values : Value.t list) : Sl.holdcase
    =
  match values with
  | [ value_block ] ->
      let block = unboot_block value_block in
      let dangle = stub_dangle in
      Sl.NotHoldH (block, dangle)
  | _ -> error "@unboot_nothold_holdcase"

and unboot_hold_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_id; value_exps; value_iterexps; value_holdcase ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let notexp = stub_notexp exps [] in
      let iterexps = unboot_iterexps value_iterexps in
      let holdcase = unboot_holdcase value_holdcase in
      Sl.HoldI (id, notexp, iterexps, holdcase) $$ (at, stub_instr_note)
  | _ -> error "@unboot_hold_instr"

and unboot_guard (value_guard : Value.t) : Sl.guard =
  Value.Get.mtch_dispatch value_guard !unboot_guard_mtchtbl (fun _ _ ->
      error "@unboot_guard")

and unboot_bool_guard (_at : region) (values : Value.t list) : Sl.guard =
  match values with
  | [ value_bool ] ->
      let b = Value.Get.bool value_bool in
      Sl.BoolG b
  | _ -> error "@unboot_bool_guard"

and unboot_cmp_guard (_at : region) (values : Value.t list) : Sl.guard =
  let optyp_of_cmpop : Sl.cmpop -> Sl.optyp = function
    | `EqOp | `NeOp -> (`BoolT : Sl.optyp)
    | _ -> `NatT
  in
  match values with
  | [ value_cmpop; value_exp ] ->
      let cmpop = unboot_cmpop value_cmpop in
      let optyp = optyp_of_cmpop cmpop in
      let exp = unboot_exp value_exp in
      Sl.CmpG (cmpop, optyp, exp)
  | _ -> error "@unboot_cmp_guard"

and unboot_sub_guard (_at : region) (values : Value.t list) : Sl.guard =
  match values with
  | [ value_typ ] ->
      let typ = unboot_typ value_typ in
      Sl.SubG (typ, Il.RecurseSC typ)
  | _ -> error "@unboot_sub_guard"

and unboot_match_guard (_at : region) (values : Value.t list) : Sl.guard =
  match values with
  | [ value_pattern ] ->
      let pattern = unboot_pattern value_pattern in
      Sl.MatchG pattern
  | _ -> error "@unboot_match_guard"

and unboot_mem_guard (_at : region) (values : Value.t list) : Sl.guard =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Sl.MemG exp
  | _ -> error "@unboot_mem_guard"

and unboot_case (value_case : Value.t) : Sl.case =
  let values = Value.Get.(value_case |>>! mop_case) in
  let guard = Value.Get.nth 0 values |> unboot_guard in
  let block = Value.Get.nth 1 values |> unboot_block in
  (guard, block)

and unboot_cases (value_cases : Value.t) : Sl.case list =
  value_cases |> Value.Get.list |> List.map unboot_case

and unboot_case_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_exp; value_cases ] ->
      let exp = unboot_exp value_exp in
      let cases = unboot_cases value_cases in
      let dangle = stub_dangle in
      Sl.CaseI (exp, cases, dangle) $$ (at, stub_instr_note)
  | _ -> error "@unboot_case_instr"

and unboot_group_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_id; value_exps; value_typs_in; value_typs_out; value_block ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let typs_in = unboot_typs value_typs_in in
      let typs_out = unboot_typs value_typs_out in
      let nottyp = stub_nottyp typs_in typs_out in
      let inputs = stub_input_hint (List.length typs_in) in
      let block = unboot_block value_block in
      Sl.GroupI (id, (nottyp, inputs), exps, block) $$ (at, stub_instr_note)
  | _ -> error "@unboot_group_instr"

and unboot_let_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_exp_l; value_exp_r; value_iterinstrs; value_block ] ->
      let exp_l = unboot_exp value_exp_l in
      let exp_r = unboot_exp value_exp_r in
      let iterinstrs = unboot_iterinstrs value_iterinstrs in
      let block = unboot_block value_block in
      Sl.LetI (exp_l, exp_r, iterinstrs, block) $$ (at, stub_instr_note)
  | _ -> error "@unboot_let_instr"

and unboot_rule_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_id; value_exps_in; value_exps_out; value_iterinstrs; value_block ]
    ->
      let id = unboot_id value_id in
      let exps_in = unboot_exps value_exps_in in
      let exps_out = unboot_exps value_exps_out in
      let notexp = stub_notexp exps_in exps_out in
      let inputs = stub_input_hint (List.length exps_in) in
      let iterinstrs = unboot_iterinstrs value_iterinstrs in
      let block = unboot_block value_block in
      Sl.RuleI (id, notexp, inputs, iterinstrs, block) $$ (at, stub_instr_note)
  | _ -> error "@unboot_rule_instr"

and unboot_result_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_typs_in; value_typs_out; value_exps ] ->
      let typs_in = unboot_typs value_typs_in in
      let typs_out = unboot_typs value_typs_out in
      let nottyp = stub_nottyp typs_in typs_out in
      let inputs = stub_input_hint (List.length typs_in) in
      let exps = unboot_exps value_exps in
      Sl.ResultI ((nottyp, inputs), exps) $$ (at, stub_instr_note)
  | _ -> error "@unboot_result_instr"

and unboot_return_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_exp ] ->
      let exp = unboot_exp value_exp in
      Sl.ReturnI exp $$ (at, stub_instr_note)
  | _ -> error "@unboot_return_instr"

and unboot_debug_instr (at : region) (values : Value.t list) : Sl.instr =
  match values with
  | [ value_exp; value_instr ] ->
      let exp = unboot_exp value_exp in
      let instr = unboot_instr value_instr in
      Sl.DebugI (exp, instr) $$ (at, stub_instr_note)
  | _ -> error "@unboot_debug_instr"

and unboot_instrs (value_instrs : Value.t) : Sl.instr list =
  value_instrs |> Value.Get.list |> List.map unboot_instr

and unboot_block (value_block : Value.t) : Sl.block =
  value_block |> unboot_instrs

and unboot_block_opt (value_block_opt : Value.t) : Sl.block option =
  value_block_opt |> Value.Get.opt |> Option.map unboot_block

(* Table rows *)

let rec unboot_tablerow (value_tablerow : Value.t) : Sl.tablerow =
  let values = Value.Get.(value_tablerow |>>! mop_tablerow) in
  let exps = Value.Get.nth 0 values |> unboot_exps in
  let exp = Value.Get.nth 1 values |> unboot_exp in
  let block = Value.Get.nth 2 values |> unboot_block in
  (exps, exp, block)

and unboot_tablerows (value_tablerows : Value.t) : Sl.tablerow list =
  value_tablerows |> Value.Get.list |> List.map unboot_tablerow

(* Definitions *)

let unboot_def (value_def : Value.t) : Sl.def =
  Value.Get.mtch_dispatch value_def !unboot_def_mtchtbl (fun _ _ ->
      error "@unboot_def")

let unboot_extern_typ_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id ] ->
      let id = unboot_id value_id in
      Sl.ExternTypD (id, []) $ at
  | _ -> error "@unboot_extern_typ_def"

let unboot_typ_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id; value_tparams; value_deftyp ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let deftyp = unboot_deftyp value_deftyp in
      Sl.TypD (id, tparams, deftyp, []) $ at
  | _ -> error "@unboot_typ_def"

let unboot_extern_rel_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id; value_exps; value_typs_in; value_typs_out ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let typs_in = unboot_typs value_typs_in in
      let typs_out = unboot_typs value_typs_out in
      let nottyp = stub_nottyp typs_in typs_out in
      let inputs = stub_input_hint (List.length typs_in) in
      Sl.ExternRelD (id, (nottyp, inputs), exps, []) $ at
  | _ -> error "@unboot_extern_rel_def"

let unboot_rel_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [
   value_id;
   value_exps;
   value_typs_in;
   value_typs_out;
   value_block;
   value_elsblock;
  ] ->
      let id = unboot_id value_id in
      let exps = unboot_exps value_exps in
      let typs_in = unboot_typs value_typs_in in
      let typs_out = unboot_typs value_typs_out in
      let nottyp = stub_nottyp typs_in typs_out in
      let inputs = stub_input_hint (List.length typs_in) in
      let block = unboot_block value_block in
      let elseblock_opt = unboot_block_opt value_elsblock in
      Sl.RelD (id, (nottyp, inputs), exps, block, elseblock_opt, []) $ at
  | _ -> error "@unboot_rel_def"

let unboot_extern_func_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Sl.ExternDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_extern_func_def"

let unboot_builtin_func_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id; value_tparams; value_params; value_typ ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      Sl.BuiltinDecD (id, tparams, params, typ, []) $ at
  | _ -> error "@unboot_builtin_func_def"

let unboot_table_func_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [ value_id; value_params; value_typ; value_tablerows ] ->
      let id = unboot_id value_id in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let tablerows = unboot_tablerows value_tablerows in
      Sl.TableDecD (id, params, typ, tablerows, []) $ at
  | _ -> error "@unboot_table_func_def"

let unboot_func_def (at : region) (values : Value.t list) : Sl.def =
  match values with
  | [
   value_id; value_tparams; value_params; value_typ; value_block; value_elsblock;
  ] ->
      let id = unboot_id value_id in
      let tparams = unboot_tparams value_tparams in
      let params = unboot_params value_params in
      let typ = unboot_typ value_typ in
      let block = unboot_block value_block in
      let elseblock_opt = unboot_block_opt value_elsblock in
      Sl.FuncDecD (id, tparams, params, typ, block, elseblock_opt, []) $ at
  | _ -> error "@unboot_func_def"

(* Specification *)

let unboot_script (value_script : Value.t) : Sl.spec =
  let value_defs = Value.Get.list value_script in
  List.map unboot_def value_defs

(* Initialize SL dispatch tables after all handler functions are defined *)

let () =
  (* Parameters *)
  unboot_param_mtchtbl :=
    Value.Get.build_mtchtbl
      [ (mop_exp_param, unboot_exp_param); (mop_def_param, unboot_def_param) ];
  (* Instructions *)
  unboot_holdcase_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_both_holdcase, unboot_both_holdcase);
        (mop_hold_holdcase, unboot_hold_holdcase);
        (mop_nothold_holdcase, unboot_nothold_holdcase);
      ];
  unboot_guard_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_bool_guard, unboot_bool_guard);
        (mop_cmp_guard, unboot_cmp_guard);
        (mop_sub_guard, unboot_sub_guard);
        (mop_match_guard, unboot_match_guard);
        (mop_mem_guard, unboot_mem_guard);
      ];
  unboot_instr_mtchtbl :=
    Value.Get.build_mtchtbl
      [
        (mop_if_instr, unboot_if_instr);
        (mop_hold_instr, unboot_hold_instr);
        (mop_case_instr, unboot_case_instr);
        (mop_group_instr, unboot_group_instr);
        (mop_let_instr, unboot_let_instr);
        (mop_rule_instr, unboot_rule_instr);
        (mop_result_instr, unboot_result_instr);
        (mop_return_instr, unboot_return_instr);
        (mop_debug_instr, unboot_debug_instr);
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
