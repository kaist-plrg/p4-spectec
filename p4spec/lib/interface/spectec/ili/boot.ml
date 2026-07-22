module Mixfix = Domain.Mixfix
open Lang
open Mixops
open Typs
open Util.Source

module Make (V : Runtime.Valrep.SAFE) = struct
  include Common.Boot.Make (V)

  (* Parameters *)

  let rec boot_param (param : Il.param) : V.t =
    let at = param.at in
    match param.it with
    | ExpP typ ->
        let value_typ = boot_typ typ in
        case_at at (mop_exp_param <|! [ value_typ ]) typ_param
    | DefP (id, tparams, params, typ) ->
        let value_id = boot_id id in
        let value_tparams = boot_tparams tparams in
        let value_params = boot_params params in
        let value_typ = boot_typ typ in
        case_at at
          (mop_def_param
          <|! [ value_id; value_tparams; value_params; value_typ ])
          typ_param

  and boot_params (params : Il.param list) : V.t =
    let values_params = List.map boot_param params in
    V.Make.list (Runtime.Type.Typ.Make.list typ_param) values_params

  (* Iter premises *)

  and boot_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : V.t =
    let value_iter = boot_iter iter in
    let value_vars_in = boot_vars vars_in in
    let value_vars_out = boot_vars vars_out in
    mop_iterprem
    <|! [ value_iter; value_vars_in; value_vars_out ]
    <<|! typ_iterprem

  (* Premises *)

  and boot_prem (prem : Il.prem) : V.t =
    let at = prem.at in
    match prem.it with
    | RulePr (id, notexp, input) -> boot_rel_prem at id notexp input
    | IfPr e -> boot_if_prem at e
    | IfHoldPr (id, notexp) -> boot_if_hold_prem at id notexp
    | IfNotHoldPr (id, notexp) -> boot_if_nothold_prem at id notexp
    | LetPr (el, er) -> boot_let_prem at el er
    | IterPr (p, ip) -> boot_iter_prem at p ip
    | DebugPr e -> boot_debug_prem at e

  and boot_rel_prem (at : region) (id : Il.id) (notexp : Il.notexp)
      (input : Hints.Input.t) : V.t =
    let exps = Mixfix.args notexp in
    let value_id = boot_id id in
    let exps_in, exps_out = Hints.Input.split input exps in
    let value_exps_in = boot_exps exps_in in
    let value_exps_out = boot_exps exps_out in
    case_at at
      (mop_rel_prem <|! [ value_id; value_exps_in; value_exps_out ])
      typ_prem

  and boot_if_prem (at : region) (e : Il.exp) : V.t =
    let value_exp = boot_exp e in
    case_at at (mop_if_prem <|! [ value_exp ]) typ_prem

  and boot_if_hold_prem (at : region) (id : Il.id) (notexp : Il.notexp) : V.t =
    let exps = Mixfix.args notexp in
    let value_id = boot_id id in
    let value_exps = boot_exps exps in
    case_at at (mop_if_hold_prem <|! [ value_id; value_exps ]) typ_prem

  and boot_if_nothold_prem (at : region) (id : Il.id) (notexp : Il.notexp) : V.t
      =
    let exps = Mixfix.args notexp in
    let value_id = boot_id id in
    let value_exps = boot_exps exps in
    case_at at (mop_if_nothold_prem <|! [ value_id; value_exps ]) typ_prem

  and boot_let_prem (at : region) (el : Il.exp) (er : Il.exp) : V.t =
    let value_el = boot_exp el in
    let value_er = boot_exp er in
    case_at at (mop_let_prem <|! [ value_el; value_er ]) typ_prem

  and boot_iter_prem (at : region) (p : Il.prem) (ip : Il.iterprem) : V.t =
    let value_prem = boot_prem p in
    let value_iterprem = boot_iterprem ip in
    case_at at (mop_iter_prem <|! [ value_prem; value_iterprem ]) typ_prem

  and boot_debug_prem (at : region) (e : Il.exp) : V.t =
    let value_exp = boot_exp e in
    case_at at (mop_debug_prem <|! [ value_exp ]) typ_prem

  and boot_prems (prems : Il.prem list) : V.t =
    let values_prems = List.map boot_prem prems in
    V.Make.list (Runtime.Type.Typ.Make.list typ_prem) values_prems

  (* Rule matching and paths *)

  and boot_rulmatch ((_, exps_input, prems) : Il.rulematch) : V.t =
    let value_exps = boot_exps exps_input in
    let value_prems = boot_prems prems in
    mop_rulematch <|! [ value_exps; value_prems ] <<|! typ_rulmatch

  and boot_rulpath ((id, prems, exps_output) : Il.rulepath) : V.t =
    let value_id = boot_id id in
    let value_exps_output = boot_exps exps_output in
    let value_prems = boot_prems prems in
    mop_rulepath
    <|! [ value_id; value_exps_output; value_prems ]
    <<|! typ_rulpath

  and boot_rulpaths (rulpaths : Il.rulepath list) : V.t =
    let values_rulpaths = List.map boot_rulpath rulpaths in
    V.Make.list (Runtime.Type.Typ.Make.list typ_rulpath) values_rulpaths

  and boot_rulgroup (rg : Il.rulegroup) : V.t =
    let at = rg.at in
    let id, rulmatch_, rulpaths = rg.it in
    let value_id = boot_id id in
    let value_rulmatch = boot_rulmatch rulmatch_ in
    let value_rulpaths = boot_rulpaths rulpaths in
    case_at at
      (mop_rulegroup <|! [ value_id; value_rulmatch; value_rulpaths ])
      typ_rulgroup

  and boot_rulgroups (rulgroups : Il.rulegroup list) : V.t =
    let values_rulgroups = List.map boot_rulgroup rulgroups in
    V.Make.list (Runtime.Type.Typ.Make.list typ_rulgroup) values_rulgroups

  and boot_elsgroup (elsegroup : Il.elsegroup) : V.t =
    let at = elsegroup.at in
    let id, rulmatch_, rulpath_ = elsegroup.it in
    let value_id = boot_id id in
    let value_rulmatch = boot_rulmatch rulmatch_ in
    let value_rulpath = boot_rulpath rulpath_ in
    case_at at
      (mop_elsegroup <|! [ value_id; value_rulmatch; value_rulpath ])
      typ_elsgroup

  and boot_elsgroup_opt (elsegroup_opt : Il.elsegroup option) : V.t =
    V.Make.opt
      (Runtime.Type.Typ.Make.opt typ_elsgroup)
      (Option.map boot_elsgroup elsegroup_opt)

  (* Clauses *)

  and boot_clause (clause : Il.clause) : V.t =
    let at = clause.at in
    let args, exp, prems = clause.it in
    let value_args = boot_args args in
    let value_exp = boot_exp exp in
    let value_prems = boot_prems prems in
    case_at at
      (mop_clause <|! [ value_args; value_exp; value_prems ])
      typ_clause

  and boot_clauses (clauses : Il.clause list) : V.t =
    let values_clauses = List.map boot_clause clauses in
    V.Make.list (Runtime.Type.Typ.Make.list typ_clause) values_clauses

  and boot_elsclause (elsclause : Il.elseclause) : V.t = boot_clause elsclause

  and boot_elsclause_opt (elsclause_opt : Il.elseclause option) : V.t =
    V.Make.opt
      (Runtime.Type.Typ.Make.opt
         (Runtime.Type.Typ.Make.var ("elsclause" $ no_region) []))
      (Option.map boot_elsclause elsclause_opt)

  (* Table rows *)

  and boot_tablerow (tablerow : Il.tablerow) : V.t =
    let at = tablerow.at in
    let _exps, args, exp, prems = tablerow.it in
    let value_args = boot_args args in
    let value_exp = boot_exp exp in
    let value_prems = boot_prems prems in
    case_at at
      (mop_clause <|! [ value_args; value_exp; value_prems ])
      typ_tblrow

  and boot_tablerows (tablerows : Il.tablerow list) : V.t =
    let values_tablerows = List.map boot_tablerow tablerows in
    V.Make.list (Runtime.Type.Typ.Make.list typ_tblrow) values_tablerows

  (* Definitions *)

  let rec boot_def (def : Il.def) : V.t option =
    let wrap_some value = Some value in
    let at = def.at in
    match def.it with
    | ExternTypD (id, _) -> boot_extern_typ_def at id |> wrap_some
    | TypD (id, tparams, deftyp, _) ->
        boot_typ_def at id tparams deftyp |> wrap_some
    | VarD _ -> None
    | ExternRelD (id, nottyp, input, _) ->
        boot_extern_rel_def at id nottyp input |> wrap_some
    | RelD (id, nottyp, input, rulgroups, elsegroup_opt, _) ->
        boot_rel_def at id nottyp input rulgroups elsegroup_opt |> wrap_some
    | ExternDecD (id, tparams, params, typ, _) ->
        boot_extern_func_def at id tparams params typ |> wrap_some
    | BuiltinDecD (id, tparams, params, typ, _) ->
        boot_builtin_func_def at id tparams params typ |> wrap_some
    | TableDecD (id, params, typ, tablerows, _) ->
        boot_table_func_def at id params typ tablerows |> wrap_some
    | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, _) ->
        boot_func_def at id tparams params typ clauses elseclause_opt
        |> wrap_some

  and boot_extern_typ_def (at : region) (id : Il.id) : V.t =
    let value_id = boot_id id in
    case_at at (mop_extern_typ_def <|! [ value_id ]) typ_defn

  and boot_typ_def (at : region) (id : Il.id) (tparams : Il.tparam list)
      (deftyp : Il.deftyp) : V.t =
    let value_id = boot_id id in
    let value_tparams = boot_tparams tparams in
    let value_deftyp = boot_deftyp deftyp in
    case_at at
      (mop_typ_def <|! [ value_id; value_tparams; value_deftyp ])
      typ_defn

  and boot_extern_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
      (input : Hints.Input.t) : V.t =
    let typs = Mixfix.args nottyp.it in
    let typs_in, typs_out = Hints.Input.split input typs in
    let value_id = boot_id id in
    let value_typs_in = boot_typs typs_in in
    let value_typs_out = boot_typs typs_out in
    case_at at
      (mop_extern_rel_def <|! [ value_id; value_typs_in; value_typs_out ])
      typ_defn

  and boot_rel_def (at : region) (id : Il.id) (nottyp : Il.nottyp)
      (input : Hints.Input.t) (rulgroups : Il.rulegroup list)
      (elsegroup_opt : Il.elsegroup option) : V.t =
    let typs = Mixfix.args nottyp.it in
    let typs_in, typs_out = Hints.Input.split input typs in
    let value_id = boot_id id in
    let value_typs_in = boot_typs typs_in in
    let value_typs_out = boot_typs typs_out in
    let value_rulgroups = boot_rulgroups rulgroups in
    let value_elsgroup = boot_elsgroup_opt elsegroup_opt in
    case_at at
      (mop_rel_def
      <|! [
            value_id;
            value_typs_in;
            value_typs_out;
            value_rulgroups;
            value_elsgroup;
          ])
      typ_defn

  and boot_extern_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
      (params : Il.param list) (typ : Il.typ) : V.t =
    let value_id = boot_id id in
    let value_tparams = boot_tparams tparams in
    let value_params = boot_params params in
    let value_typ = boot_typ typ in
    case_at at
      (mop_extern_func_def
      <|! [ value_id; value_tparams; value_params; value_typ ])
      typ_defn

  and boot_builtin_func_def (at : region) (id : Il.id)
      (tparams : Il.tparam list) (params : Il.param list) (typ : Il.typ) : V.t =
    let value_id = boot_id id in
    let value_tparams = boot_tparams tparams in
    let value_params = boot_params params in
    let value_typ = boot_typ typ in
    case_at at
      (mop_builtin_func_def
      <|! [ value_id; value_tparams; value_params; value_typ ])
      typ_defn

  and boot_table_func_def (at : region) (id : Il.id) (params : Il.param list)
      (typ : Il.typ) (tablerows : Il.tablerow list) : V.t =
    let value_id = boot_id id in
    let value_params = boot_params params in
    let value_typ = boot_typ typ in
    let value_tablerows = boot_tablerows tablerows in
    case_at at
      (mop_table_func_def
      <|! [ value_id; value_params; value_typ; value_tablerows ])
      typ_defn

  and boot_func_def (at : region) (id : Il.id) (tparams : Il.tparam list)
      (params : Il.param list) (typ : Il.typ) (clauses : Il.clause list)
      (elseclause_opt : Il.elseclause option) : V.t =
    let value_id = boot_id id in
    let value_tparams = boot_tparams tparams in
    let value_params = boot_params params in
    let value_typ = boot_typ typ in
    let value_clauses = boot_clauses clauses in
    let value_elsclause = boot_elsclause_opt elseclause_opt in
    case_at at
      (mop_func_def
      <|! [
            value_id;
            value_tparams;
            value_params;
            value_typ;
            value_clauses;
            value_elsclause;
          ])
      typ_defn

  (* Specification *)

  let boot_spec (spec : Il.spec) : V.t =
    let values_def = List.map boot_def spec |> List.filter_map Fun.id in
    let typ_script = Runtime.Type.Typ.Make.var ("script" $ no_region) [] in
    V.Make.list typ_script values_def
end
