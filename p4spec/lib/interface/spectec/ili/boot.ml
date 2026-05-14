open Common.Boot
open Lang
open Mixops
open Typs
open Util.Source

(* Parameters *)

let rec boot_paramIL (paramIL : Il.param) : Value.t =
  let at = paramIL.at in
  match paramIL.it with
  | ExpP typ ->
      let value_typ = boot_typ typ in
      Value.Make.(mop_exp_paramIL <|! [ value_typ ] <<|! typ_paramIL <<<| at)
  | DefP (id, tparams, paramILs, typ) ->
      let value_id = boot_id id in
      let value_tparams = boot_tparams tparams in
      let value_paramILs = boot_paramILs paramILs in
      let value_typ = boot_typ typ in
      Value.Make.(
        mop_def_paramIL
        <|! [ value_id; value_tparams; value_paramILs; value_typ ]
        <<|! typ_paramIL <<<| at)

and boot_paramILs (paramILs : Il.param list) : Value.t =
  let values_paramILs = List.map boot_paramIL paramILs in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_paramIL) values_paramILs

(* Iter premises *)

and boot_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : Value.t =
  let value_iter = boot_iter iter in
  let value_vars_in = boot_vars vars_in in
  let value_vars_out = boot_vars vars_out in
  Value.Make.(
    mop_iterprem
    <|! [ value_iter; value_vars_in; value_vars_out ]
    <<|! typ_iterprem)

(* Premises *)

and boot_prem (prem : Il.prem) : Value.t =
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
    (input : Hints.Input.t) : Value.t =
  let exps = Mixfix.args notexp in
  let value_id = boot_id id in
  let exps_in, exps_out = Hints.Input.split input exps in
  let value_exps_in = boot_exps exps_in in
  let value_exps_out = boot_exps exps_out in
  Value.Make.(
    mop_rel_prem
    <|! [ value_id; value_exps_in; value_exps_out ]
    <<|! typ_prem <<<| at)

and boot_if_prem (at : region) (e : Il.exp) : Value.t =
  let value_exp = boot_exp e in
  Value.Make.(mop_if_prem <|! [ value_exp ] <<|! typ_prem <<<| at)

and boot_if_hold_prem (at : region) (id : Il.id) (notexp : Il.notexp) : Value.t
    =
  let exps = Mixfix.args notexp in
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    mop_if_hold_prem <|! [ value_id; value_exps ] <<|! typ_prem <<<| at)

and boot_if_nothold_prem (at : region) (id : Il.id) (notexp : Il.notexp) :
    Value.t =
  let exps = Mixfix.args notexp in
  let value_id = boot_id id in
  let value_exps = boot_exps exps in
  Value.Make.(
    mop_if_nothold_prem <|! [ value_id; value_exps ] <<|! typ_prem <<<| at)

and boot_let_prem (at : region) (el : Il.exp) (er : Il.exp) : Value.t =
  let value_el = boot_exp el in
  let value_er = boot_exp er in
  Value.Make.(mop_let_prem <|! [ value_el; value_er ] <<|! typ_prem <<<| at)

and boot_iter_prem (at : region) (p : Il.prem) (ip : Il.iterprem) : Value.t =
  let value_prem = boot_prem p in
  let value_iterprem = boot_iterprem ip in
  Value.Make.(
    mop_iter_prem <|! [ value_prem; value_iterprem ] <<|! typ_prem <<<| at)

and boot_debug_prem (at : region) (e : Il.exp) : Value.t =
  let value_exp = boot_exp e in
  Value.Make.(mop_debug_prem <|! [ value_exp ] <<|! typ_prem <<<| at)

and boot_prems (prems : Il.prem list) : Value.t =
  let values_prems = List.map boot_prem prems in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_prem) values_prems

(* Rule matching and paths *)

and boot_rulmatch ((_, exps_input, prems) : Il.rulematch) : Value.t =
  let value_exps = boot_exps exps_input in
  let value_prems = boot_prems prems in
  Value.Make.(mop_rulematch <|! [ value_exps; value_prems ] <<|! typ_rulmatch)

and boot_rulpath ((id, prems, exps_output) : Il.rulepath) : Value.t =
  let value_id = boot_id id in
  let value_exps_output = boot_exps exps_output in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_rulepath
    <|! [ value_id; value_exps_output; value_prems ]
    <<|! typ_rulpath)

and boot_rulpaths (rulpaths : Il.rulepath list) : Value.t =
  let values_rulpaths = List.map boot_rulpath rulpaths in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_rulpath) values_rulpaths

and boot_rulgroup (rg : Il.rulegroup) : Value.t =
  let at = rg.at in
  let id, rulmatch_, rulpaths = rg.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch_ in
  let value_rulpaths = boot_rulpaths rulpaths in
  Value.Make.(
    mop_rulegroup
    <|! [ value_id; value_rulmatch; value_rulpaths ]
    <<|! typ_rulgroup <<<| at)

and boot_rulgroups (rulgroups : Il.rulegroup list) : Value.t =
  let values_rulgroups = List.map boot_rulgroup rulgroups in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_rulgroup) values_rulgroups

and boot_elsgroup (elsegroup : Il.elsegroup) : Value.t =
  let at = elsegroup.at in
  let id, rulmatch_, rulpath_ = elsegroup.it in
  let value_id = boot_id id in
  let value_rulmatch = boot_rulmatch rulmatch_ in
  let value_rulpath = boot_rulpath rulpath_ in
  Value.Make.(
    mop_elsegroup
    <|! [ value_id; value_rulmatch; value_rulpath ]
    <<|! typ_elsgroup <<<| at)

and boot_elsgroup_opt (elsegroup_opt : Il.elsegroup option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt typ_elsgroup)
    (Option.map boot_elsgroup elsegroup_opt)

(* Clauses *)

and boot_clause (clause : Il.clause) : Value.t =
  let at = clause.at in
  let args, exp, prems = clause.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_clause
    <|! [ value_args; value_exp; value_prems ]
    <<|! typ_clause <<<| at)

and boot_clauses (clauses : Il.clause list) : Value.t =
  let values_clauses = List.map boot_clause clauses in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_clause) values_clauses

and boot_elsclause (elsclause : Il.elseclause) : Value.t = boot_clause elsclause

and boot_elsclause_opt (elsclause_opt : Il.elseclause option) : Value.t =
  Value.Make.opt
    (Runtime.Type.Typ.Make.opt
       (Runtime.Type.Typ.Make.var ("elsclause" $ no_region) []))
    (Option.map boot_elsclause elsclause_opt)

(* Table rows *)

and boot_tablerowIL (tablerowIL : Il.tablerow) : Value.t =
  let at = tablerowIL.at in
  let _exps, args, exp, prems = tablerowIL.it in
  let value_args = boot_args args in
  let value_exp = boot_exp exp in
  let value_prems = boot_prems prems in
  Value.Make.(
    mop_clause
    <|! [ value_args; value_exp; value_prems ]
    <<|! typ_tblrowIL <<<| at)

and boot_tablerowILs (tablerowILs : Il.tablerow list) : Value.t =
  let values_tablerowILs = List.map boot_tablerowIL tablerowILs in
  Value.Make.list (Runtime.Type.Typ.Make.list typ_tblrowIL) values_tablerowILs

(* Definitions *)

let rec boot_defIL (defIL : Il.def) : Value.t option =
  let wrap_some value = Some value in
  let at = defIL.at in
  match defIL.it with
  | ExternTypD (id, _) -> boot_extern_typ_defIL at id |> wrap_some
  | TypD (id, tparams, deftyp, _) ->
      boot_typ_defIL at id tparams deftyp |> wrap_some
  | VarD _ -> None
  | ExternRelD (id, nottyp, input, _) ->
      boot_extern_rel_defIL at id nottyp input |> wrap_some
  | RelD (id, nottyp, input, rulgroups, elsegroup_opt, _) ->
      boot_rel_defIL at id nottyp input rulgroups elsegroup_opt |> wrap_some
  | ExternDecD (id, tparams, paramILs, typ, _) ->
      boot_extern_func_defIL at id tparams paramILs typ |> wrap_some
  | BuiltinDecD (id, tparams, paramILs, typ, _) ->
      boot_builtin_func_defIL at id tparams paramILs typ |> wrap_some
  | TableDecD (id, paramILs, typ, tablerowILs, _) ->
      boot_table_func_defIL at id paramILs typ tablerowILs |> wrap_some
  | FuncDecD (id, tparams, paramILs, typ, clauses, elseclause_opt, _) ->
      boot_func_defIL at id tparams paramILs typ clauses elseclause_opt
      |> wrap_some

and boot_extern_typ_defIL (at : region) (id : Il.id) : Value.t =
  let value_id = boot_id id in
  Value.Make.(mop_extern_typ_defIL <|! [ value_id ] <<|! typ_defnIL <<<| at)

and boot_typ_defIL (at : region) (id : Il.id) (tparams : Il.tparam list)
    (deftyp : Il.deftyp) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_deftyp = boot_deftyp deftyp in
  Value.Make.(
    mop_typ_defIL
    <|! [ value_id; value_tparams; value_deftyp ]
    <<|! typ_defnIL <<<| at)

and boot_extern_rel_defIL (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) : Value.t =
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  Value.Make.(
    mop_extern_rel_defIL
    <|! [ value_id; value_typs_in; value_typs_out ]
    <<|! typ_defnIL <<<| at)

and boot_rel_defIL (at : region) (id : Il.id) (nottyp : Il.nottyp)
    (input : Hints.Input.t) (rulgroups : Il.rulegroup list)
    (elsegroup_opt : Il.elsegroup option) : Value.t =
  let typs = Mixfix.args nottyp.it in
  let typs_in, typs_out = Hints.Input.split input typs in
  let value_id = boot_id id in
  let value_typs_in = boot_typs typs_in in
  let value_typs_out = boot_typs typs_out in
  let value_rulgroups = boot_rulgroups rulgroups in
  let value_elsgroup = boot_elsgroup_opt elsegroup_opt in
  Value.Make.(
    mop_rel_defIL
    <|! [
          value_id;
          value_typs_in;
          value_typs_out;
          value_rulgroups;
          value_elsgroup;
        ]
    <<|! typ_defnIL <<<| at)

and boot_extern_func_defIL (at : region) (id : Il.id) (tparams : Il.tparam list)
    (paramILs : Il.param list) (typ : Il.typ) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_paramILs = boot_paramILs paramILs in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_extern_func_defIL
    <|! [ value_id; value_tparams; value_paramILs; value_typ ]
    <<|! typ_defnIL <<<| at)

and boot_builtin_func_defIL (at : region) (id : Il.id)
    (tparams : Il.tparam list) (paramILs : Il.param list) (typ : Il.typ) :
    Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_paramILs = boot_paramILs paramILs in
  let value_typ = boot_typ typ in
  Value.Make.(
    mop_builtin_func_defIL
    <|! [ value_id; value_tparams; value_paramILs; value_typ ]
    <<|! typ_defnIL <<<| at)

and boot_table_func_defIL (at : region) (id : Il.id) (paramILs : Il.param list)
    (typ : Il.typ) (tablerowILs : Il.tablerow list) : Value.t =
  let value_id = boot_id id in
  let value_paramILs = boot_paramILs paramILs in
  let value_typ = boot_typ typ in
  let value_tablerowILs = boot_tablerowILs tablerowILs in
  Value.Make.(
    mop_table_func_defIL
    <|! [ value_id; value_paramILs; value_typ; value_tablerowILs ]
    <<|! typ_defnIL <<<| at)

and boot_func_defIL (at : region) (id : Il.id) (tparams : Il.tparam list)
    (paramILs : Il.param list) (typ : Il.typ) (clauses : Il.clause list)
    (elseclause_opt : Il.elseclause option) : Value.t =
  let value_id = boot_id id in
  let value_tparams = boot_tparams tparams in
  let value_paramILs = boot_paramILs paramILs in
  let value_typ = boot_typ typ in
  let value_clauses = boot_clauses clauses in
  let value_elsclause = boot_elsclause_opt elseclause_opt in
  Value.Make.(
    mop_func_defIL
    <|! [
          value_id;
          value_tparams;
          value_paramILs;
          value_typ;
          value_clauses;
          value_elsclause;
        ]
    <<|! typ_defnIL <<<| at)

(* Specification *)

let boot_specIL (specIL : Il.spec) : Value.t =
  let values_defIL = List.map boot_defIL specIL |> List.filter_map Fun.id in
  let typ_scriptIL = Runtime.Type.Typ.Make.var ("scriptIL" $ no_region) [] in
  Value.Make.list typ_scriptIL values_defIL
