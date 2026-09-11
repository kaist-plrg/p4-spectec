open Common.Boot
open Lang
open Mixops
open Typs
open Util.Source

(* Parameters *)

let rec boot_param (param : Sl.param) : Value.t =
  let at = param.at in
  match param.it with
  | ExpP (typ, exp) ->
      let value_typ = boot_typ typ in
      let value_exp = boot_exp exp in
      Value.Make.(
        mop_exp_param <|! [ value_typ; value_exp ] <<|! typ_param <<<| at)
  | DefP (id, tparams, params, typ) ->
      let value_id = boot_id id in
      let value_tparams = boot_tparams tparams in
      let value_params = boot_params params in
      let value_typ = boot_typ typ in
      Value.Make.(
        mop_def_param
        <|! [ value_id; value_tparams; value_params; value_typ ]
        <<|! typ_param <<<| at)

and boot_params (params : Sl.param list) : Value.t =
  let values_params = List.map boot_param params in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_param) values_params

(* Iter instructions *)

and boot_iterinstr ((iter, vars_in, vars_out) : Sl.iterinstr) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars_in = boot_vars vars_in in
  let value_vars_out = boot_vars vars_out in
  Value.Make.(
    mop_iterinstr
    <|! [ value_iter; value_vars_in; value_vars_out ]
    <<|! typ_iterinstr)

and boot_iterinstrs (iterinstrs : Sl.iterinstr list) : Value.t =
  let values_iterinstrs = List.map boot_iterinstr iterinstrs in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_iterinstr) values_iterinstrs

(* Iter expressions *)

and boot_iterexps (iterexps : Sl.iterexp list) : Value.t =
  let values_iterexps = List.map boot_iterexp iterexps in
  Value.Make.list
    (Runtime.Type.Typ.Make.list Common.Typs.typ_iterexp)
    values_iterexps

(* Instructions *)

and boot_instr (instr : Sl.instr) : Value.t =
  let at = instr.at in
  match instr.it with
  | IfI (exp, iterexps, block, _) -> boot_if_instr at exp iterexps block
  | HoldI (id, notexp, iterexps, holdcase) ->
      boot_hold_instr at id notexp iterexps holdcase
  | CaseI (exp, cases, _) -> boot_case_instr at exp cases
  | GroupI (id, (nottyp, inputs), exps, block) ->
      boot_group_instr at id nottyp inputs exps block
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      boot_let_instr at exp_l exp_r iterinstrs block
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      boot_rule_instr at id notexp inputs iterinstrs block
  | ResultI ((nottyp, inputs), exps) -> boot_result_instr at nottyp inputs exps
  | ReturnI exp -> boot_return_instr at exp
  | DebugI (exp, instr) -> boot_debug_instr at exp instr

and boot_if_instr (at : region) (exp : Sl.exp) (iterexps : Sl.iterexp list)
    (block : Sl.block) : Value.t =
  let value_exp = boot_exp exp in
  let value_iterexps = boot_iterexps iterexps in
  let value_block = boot_block block in
  Value.Make.(
    mop_if_instr
    <|! [ value_exp; value_iterexps; value_block ]
    <<|! typ_instr <<<| at)

and boot_holdcase (holdcase : Sl.holdcase) : Value.t =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      let value_block_hold = boot_block block_hold in
      let value_block_nothold = boot_block block_nothold in
      Value.Make.(
        mop_both_holdcase
        <|! [ value_block_hold; value_block_nothold ]
        <<|! typ_holdcase)
  | HoldH (block_hold, _) ->
      let value_block_hold = boot_block block_hold in
      Value.Make.(mop_hold_holdcase <|! [ value_block_hold ] <<|! typ_holdcase)
  | NotHoldH (block_nothold, _) ->
      let value_block_nothold = boot_block block_nothold in
      Value.Make.(
        mop_nothold_holdcase <|! [ value_block_nothold ] <<|! typ_holdcase)

and boot_hold_instr (at : region) (id : Sl.id) (notexp : Sl.notexp)
    (iterexps : Sl.iterexp list) (holdcase : Sl.holdcase) : Value.t =
  let value_id = boot_id id in
  let exps = Mixfix.args notexp in
  let value_exps = boot_exps exps in
  let value_iterexps = boot_iterexps iterexps in
  let value_holdcase = boot_holdcase holdcase in
  Value.Make.(
    mop_hold_instr
    <|! [ value_id; value_exps; value_iterexps; value_holdcase ]
    <<|! typ_instr <<<| at)

and boot_guard (guard : Sl.guard) : Value.t =
  match guard with
  | BoolG b ->
      let value_b = Value.Make.bool b in
      Value.Make.(mop_bool_guard <|! [ value_b ] <<|! typ_guard)
  | CmpG (cmpop, _, exp) ->
      let value_cmpop = boot_cmpop cmpop in
      let value_exp = boot_exp exp in
      Value.Make.(mop_cmp_guard <|! [ value_cmpop; value_exp ] <<|! typ_guard)
  | SubG (typ, _) ->
      let value_typ = boot_typ typ in
      Value.Make.(mop_sub_guard <|! [ value_typ ] <<|! typ_guard)
  | MatchG pattern ->
      let value_pattern = boot_pattern pattern in
      Value.Make.(mop_match_guard <|! [ value_pattern ] <<|! typ_guard)
  | MemG exp ->
      let value_exp = boot_exp exp in
      Value.Make.(mop_mem_guard <|! [ value_exp ] <<|! typ_guard)

and boot_case (case : Sl.case) : Value.t =
  let guard, block = case in
  let value_guard = boot_guard guard in
  let value_block = boot_block block in
  Value.Make.(mop_case <|! [ value_guard; value_block ] <<|! typ_case)

and boot_cases (cases : Sl.case list) : Value.t =
  let values_cases = List.map boot_case cases in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_case) values_cases

and boot_case_instr (at : region) (exp : Sl.exp) (cases : Sl.case list) :
    Value.t =
  let value_exp = boot_exp exp in
  let value_cases = boot_cases cases in
  Value.Make.(
    mop_case_instr <|! [ value_exp; value_cases ] <<|! typ_instr <<<| at)

and boot_group_instr (at : region) (id : Sl.id) (nottyp : Sl.nottyp)
    (inputs : Hints.Input.t) (exps : Sl.exp list) (block : Sl.block) : Value.t =
  let value_id = boot_id id in
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split inputs typs in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  let value_exps = boot_exps exps in
  let value_block = boot_block block in
  Value.Make.(
    mop_group_instr
    <|! [ value_id; value_exps; value_typs_in; value_typs_out; value_block ]
    <<|! typ_instr <<<| at)

and boot_let_instr (at : region) (exp_l : Sl.exp) (exp_r : Sl.exp)
    (iterinstrs : Sl.iterinstr list) (block : Sl.block) : Value.t =
  let value_exp_l = boot_exp exp_l in
  let value_exp_r = boot_exp exp_r in
  let value_iterinstrs = boot_iterinstrs iterinstrs in
  let value_block = boot_block block in
  Value.Make.(
    mop_let_instr
    <|! [ value_exp_l; value_exp_r; value_iterinstrs; value_block ]
    <<|! typ_instr <<<| at)

and boot_rule_instr (at : region) (id : Sl.id) (notexp : Sl.notexp)
    (inputs : Hints.Input.t) (iterinstrs : Sl.iterinstr list) (block : Sl.block)
    : Value.t =
  let value_id = boot_id id in
  let exps = Mixfix.args notexp in
  let exps_in, exps_out = Hints.Input.split inputs exps in
  let value_exps_in = boot_exps exps_in in
  let value_exps_out = boot_exps exps_out in
  let value_iterinstrs = boot_iterinstrs iterinstrs in
  let value_block = boot_block block in
  Value.Make.(
    mop_rule_instr
    <|! [
          value_id; value_exps_in; value_exps_out; value_iterinstrs; value_block;
        ]
    <<|! typ_instr <<<| at)

and boot_result_instr (at : region) (nottyp : Sl.nottyp)
    (inputs : Hints.Input.t) (exps : Sl.exp list) : Value.t =
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split inputs typs in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  let value_exps = boot_exps exps in
  Value.Make.(
    mop_result_instr
    <|! [ value_typs_in; value_typs_out; value_exps ]
    <<|! typ_instr <<<| at)

and boot_return_instr (at : region) (exp : Sl.exp) : Value.t =
  let value_exp = boot_exp exp in
  Value.Make.(mop_return_instr <|! [ value_exp ] <<|! typ_instr <<<| at)

and boot_debug_instr (at : region) (exp : Sl.exp) (instr : Sl.instr) : Value.t =
  let value_exp = boot_exp exp in
  let value_instr = boot_instr instr in
  Value.Make.(
    mop_debug_instr <|! [ value_exp; value_instr ] <<|! typ_instr <<<| at)

and boot_instrs (instrs : Sl.instr list) : Value.t =
  let values_instrs = List.map boot_instr instrs in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_instr) values_instrs

and boot_block (block : Sl.block) : Value.t = boot_instrs block

and boot_elsblock_opt (elseblock_opt : Sl.elseblock option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt typ_block)
    (Option.map boot_block elseblock_opt)

(* Table rows *)

and boot_tablerow ((exps, exp, block) : Sl.tablerow) : Value.t =
  let value_exps = boot_exps exps in
  let value_exp = boot_exp exp in
  let value_block = boot_block block in
  Value.Make.(
    mop_tablerow <|! [ value_exps; value_exp; value_block ] <<|! typ_tblrow)

and boot_tablerows (tablerows : Sl.tablerow list) : Value.t =
  let values_tablerows = List.map boot_tablerow tablerows in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_tblrow) values_tablerows

(* Definitions *)

let rec boot_def (def : Sl.def) : Value.t option =
  let wrap_some value = Some value in
  let at = def.at in
  match def.it with
  | ExternTypD (id, _) -> boot_extern_typ_def at id |> wrap_some
  | TypD (id, tparams, deftyp, _) ->
      boot_typ_def at id tparams deftyp |> wrap_some
  | VarD _ -> None
  | ExternRelD (id, (nottyp, input), exps, _) ->
      boot_extern_rel_def at id nottyp input exps |> wrap_some
  | RelD (id, (nottyp, input), exps, block, elseblock_opt, _) ->
      boot_rel_def at id nottyp input exps block elseblock_opt |> wrap_some
  | ExternDecD (id, tparams, params, typ, _) ->
      boot_extern_func_def at id tparams params typ |> wrap_some
  | BuiltinDecD (id, tparams, params, typ, _) ->
      boot_builtin_func_def at id tparams params typ |> wrap_some
  | TableDecD (id, params, typ, tablerows, _) ->
      boot_table_func_def at id params typ tablerows |> wrap_some
  | FuncDecD (id, tparams, params, typ, block, elseblock_opt, _) ->
      boot_func_def at id tparams params typ block elseblock_opt |> wrap_some

and boot_extern_typ_def (at : region) (id : Sl.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.(mop_extern_typ_def <|! [ value_id ] <<|! typ_defn <<<| at)

and boot_typ_def (at : region) (id : Sl.id) (tparams : Sl.tparam list)
    (deftyp : Sl.deftyp) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_deftyp = boot_deftyp deftyp in
  Value.Make.(
    mop_typ_def
    <|! [ value_id; value_tparams; value_deftyp ]
    <<|! typ_defn <<<| at)

and boot_extern_rel_def (at : region) (id : Sl.id) (nottyp : Sl.nottyp)
    (input : Hints.Input.t) (exps : Sl.exp list) : Value.t =
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  Value.Make.(
    mop_extern_rel_def
    <|! [ value_id; value_exps; value_typs_in; value_typs_out ]
    <<|! typ_defn <<<| at)

and boot_rel_def (at : region) (id : Sl.id) (nottyp : Sl.nottyp)
    (input : Hints.Input.t) (exps : Sl.exp list) (block : Sl.block)
    (elseblock_opt : Sl.elseblock option) : Value.t =
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  let value_block = boot_block block in
  let value_elsblock = boot_elsblock_opt elseblock_opt in
  Value.Make.(
    mop_rel_def
    <|! [
          value_id;
          value_exps;
          value_typs_in;
          value_typs_out;
          value_block;
          value_elsblock;
        ]
    <<|! typ_defn <<<| at)

and boot_extern_func_def (at : region) (id : Sl.id) (tparams : Sl.tparam list)
    (params : Sl.param list) (typ : Sl.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_extern_func_def
    <|! [ value_id; value_tparams; value_params; value_typ ]
    <<|! typ_defn <<<| at)

and boot_builtin_func_def (at : region) (id : Sl.id) (tparams : Sl.tparam list)
    (params : Sl.param list) (typ : Sl.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_builtin_func_def
    <|! [ value_id; value_tparams; value_params; value_typ ]
    <<|! typ_defn <<<| at)

and boot_table_func_def (at : region) (id : Sl.id) (params : Sl.param list)
    (typ : Sl.typ) (tablerows : Sl.tablerow list) : Value.t =
  let value_id = boot_id id in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_tablerows = boot_tablerows tablerows in
  Value.Make.(
    mop_table_func_def
    <|! [ value_id; value_params; value_typ; value_tablerows ]
    <<|! typ_defn <<<| at)

and boot_func_def (at : region) (id : Sl.id) (tparams : Sl.tparam list)
    (params : Sl.param list) (typ : Sl.typ) (block : Sl.block)
    (elseblock_opt : Sl.elseblock option) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_params = boot_params params in
  let value_typ = boot_typ typ in
  let value_block = boot_block block in
  let value_elsblock = boot_elsblock_opt elseblock_opt in
  Value.Make.(
    mop_func_def
    <|! [
          value_id;
          value_tparams;
          value_params;
          value_typ;
          value_block;
          value_elsblock;
        ]
    <<|! typ_defn <<<| at)

(* Specification *)

let boot_spec (spec : Sl.spec) : Value.t =
  let values_def = List.map boot_def spec |> List.filter_map Fun.id in
  let typ_script = Runtime.Type.Typ.Make.var ("script" $ no_region) [] in
  Value.Make.list typ_script values_def
