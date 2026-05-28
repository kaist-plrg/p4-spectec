module Mixfix = Domain.Mixfix
open Ast
open Util.Print
open Util.Source

(* Numbers *)

let string_of_num = Il.Print.string_of_num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_typid typid = Il.Print.string_of_typid typid
let string_of_relid relid = Il.Print.string_of_relid relid
let string_of_ruleid ruleid = ruleid.it

let string_of_rulegroupid rulegroupid =
  Il.Print.string_of_rulegroupid rulegroupid

let string_of_rulepathid rulepathid = Il.Print.string_of_rulepathid rulepathid
let string_of_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let string_of_atom atom = Il.Print.string_of_atom atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

(* Variables *)

let string_of_var var = Il.Print.string_of_var var

(* Types *)

let string_of_typ typ = Il.Print.string_of_typ typ
let string_of_typs sep typs = String.concat sep (List.map string_of_typ typs)
let string_of_nottyp nottyp = Il.Print.string_of_nottyp nottyp
let string_of_deftyp deftyp = Il.Print.string_of_deftyp deftyp
let string_of_typfield typfield = Il.Print.string_of_typfield typfield

let string_of_typfields sep typfields =
  Il.Print.string_of_typfields sep typfields

let string_of_typcase typcase = Il.Print.string_of_typcase typcase
let string_of_typcases sep typcases = Il.Print.string_of_typcases sep typcases

(* Values *)

let string_of_value ?(short = false) ?(level = 0) value =
  Il.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let string_of_exp exp = Il.Print.string_of_exp exp
let string_of_exps sep exps = Il.Print.string_of_exps sep exps
let string_of_notexp notexp = Il.Print.string_of_notexp notexp
let string_of_iterexp iterexp = Il.Print.string_of_iterexp iterexp
let string_of_iterexps iterexps = Il.Print.string_of_iterexps iterexps

(* Patterns *)

let string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

let string_of_path path = Il.Print.string_of_path path

(* Parameters *)

let string_of_param param = Il.Print.string_of_param param
let string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

let string_of_tparam tparam = Il.Print.string_of_tparam tparam
let string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

let string_of_arg arg = Il.Print.string_of_arg arg
let string_of_args args = Il.Print.string_of_args args

(* Type arguments *)

let string_of_targ targ = Il.Print.string_of_targ targ
let string_of_targs targs = Il.Print.string_of_targs targs

(* Premises *)

let string_of_prem prem = Il.Print.string_of_prem prem
let string_of_prems ?(level = 0) prems = Il.Print.string_of_prems ~level prems

(* Rules *)
let string_of_rule rule =
  let ruleid, notexp, prems = rule.it in
  "rule " ^ string_of_ruleid ruleid ^ ": " ^ string_of_notexp notexp
  ^ string_of_prems ~level:2 prems

let string_of_rules rules =
  String.concat ""
    (List.map (fun rule -> "\n\n" ^ indent 2 ^ string_of_rule rule) rules)

let string_of_rulegroup rulegroup =
  let rulegroupid, rules = rulegroup.it in
  indent 1 ^ "rulegroup "
  ^ string_of_rulegroupid rulegroupid
  ^ string_of_rules rules

let string_of_rulegroups rulegroups =
  rulegroups |> List.map string_of_rulegroup |> String.concat "\n\n"

let string_of_elsegroup elsegroup =
  let elsegroupid, rule = elsegroup.it in
  indent 1 ^ "rulegroup "
  ^ string_of_rulegroupid elsegroupid
  ^ string_of_rules [ rule ]

let string_of_elsegroup_opt elsegroup_opt =
  match elsegroup_opt with
  | None -> ""
  | Some elsegroup ->
      "\n\n" ^ indent 1 ^ "elsegroup\n\n" ^ string_of_elsegroup elsegroup

(* Clause *)

let string_of_clause idx clause = Il.Print.string_of_clause idx clause
let string_of_clauses clauses = Il.Print.string_of_clauses clauses
let string_of_elseclause elseclause = Il.Print.string_of_elseclause elseclause

let string_of_elseclause_opt elseclause_opt =
  Il.Print.string_of_elseclause_opt elseclause_opt

(* Table rows *)

let string_of_tablerow tablerow =
  let args, exp = tablerow.it in
  "\n" ^ indent 2 ^ string_of_args args ^ " -> " ^ string_of_exp exp

let string_of_tablerows tablerows =
  String.concat ""
    (List.mapi
       (fun idx tablerow ->
         "\n" ^ indent 1 ^ "row " ^ string_of_int idx ^ " :"
         ^ string_of_tablerow tablerow)
       tablerows)

(* Hints *)

let string_of_hint hint = Il.Print.string_of_hint hint
let string_of_hints hints = Il.Print.string_of_hints hints

(* Definitions *)

let string_of_def def =
  match def.it with
  | ExternTypD (id, _) -> "extern syntax " ^ string_of_typid id
  | TypD (typid, tparams, deftyp, _) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | VarD (id, typ, _) -> "var " ^ string_of_varid id ^ " : " ^ string_of_typ typ
  | ExternRelD (relid, nottyp, _, _) ->
      "extern relation " ^ string_of_relid relid ^ ": "
      ^ string_of_nottyp nottyp
  | RelD (relid, nottyp, _, rulegroups, elsegroup_opt, _) ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_nottyp nottyp
      ^ "\n\n"
      ^ string_of_rulegroups rulegroups
      ^ string_of_elsegroup_opt elsegroup_opt
  | ExternDecD (defid, tparams, params, typ, _) ->
      "extern def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | BuiltinDecD (defid, tparams, params, typ, _) ->
      "builtin def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | TableDecD (defid, params, typ, tablerows, _) ->
      "tbl def " ^ string_of_defid defid ^ string_of_params params ^ " : "
      ^ string_of_typ typ ^ " ="
      ^ string_of_tablerows tablerows
  | FuncDecD (defid, tparams, params, typ, clauses, elseclause_opt, _) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " ="
      ^ string_of_clauses clauses
      ^ string_of_elseclause_opt elseclause_opt

let string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
