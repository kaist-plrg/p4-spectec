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

let string_of_rulegroupid rulegroupid =
  Il.Print.string_of_rulegroupid rulegroupid

let string_of_rulepathid rulepathid = rulepathid.it
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
let string_of_typs sep typs = Il.Print.string_of_typs sep typs
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

let rec string_of_ruleinput nottyp inputs exps_input =
  let mixop, typs = Mixfix.split nottyp.it in
  let exps_input = List.combine inputs exps_input in
  let exps =
    List.init (List.length typs) (fun idx ->
        match List.assoc_opt idx exps_input with
        | Some exp_input -> exp_input
        | None -> Il.VarE ("%" $ no_region) $$ (no_region, Il.TextT))
  in
  let notexp = Mixfix.fill mixop exps in
  string_of_notexp notexp

and string_of_ruleoutput nottyp inputs exps_output =
  let mixop, typs = Mixfix.split nottyp.it in
  let outputs =
    List.init (List.length typs) (fun idx ->
        if List.mem idx inputs then None else Some idx)
    |> List.filter_map Fun.id
  in
  let exps_output = List.combine outputs exps_output in
  match exps_output with
  | [] -> "-- the relation holds"
  | _ ->
      let exps =
        List.init (List.length typs) (fun idx ->
            match List.assoc_opt idx exps_output with
            | Some exp_output -> exp_output
            | None -> Il.VarE ("%" $ no_region) $$ (no_region, Il.TextT))
      in
      let notexp = Mixfix.fill mixop exps in
      "-- output: " ^ string_of_notexp notexp

and string_of_rulematch nottyp inputs rulematch =
  let exps_signature, exps_input, prems = rulematch in
  indent 2 ^ "(signature) "
  ^ string_of_ruleinput nottyp inputs exps_signature
  ^ "\n" ^ indent 2
  ^ string_of_ruleinput nottyp inputs exps_input
  ^ string_of_prems ~level:2 prems

and string_of_rulepath nottyp inputs rulepath =
  let rulepathid, prems, exps_output = rulepath in
  indent 2 ^ "rulepath "
  ^ string_of_rulepathid rulepathid
  ^ string_of_prems ~level:2 prems
  ^ "\n" ^ indent 2
  ^ string_of_ruleoutput nottyp inputs exps_output

and string_of_rulepaths nottyp inputs rulepaths =
  rulepaths
  |> List.map (string_of_rulepath nottyp inputs)
  |> String.concat "\n\n"

and string_of_rulegroup nottyp inputs rulegroup =
  let rulegroupid, rulematch, rulepaths = rulegroup.it in
  indent 1 ^ "rulegroup "
  ^ string_of_rulegroupid rulegroupid
  ^ "\n\n " ^ indent 1 ^ "match\n\n"
  ^ string_of_rulematch nottyp inputs rulematch
  ^ "\n\n " ^ indent 1 ^ "paths\n\n"
  ^ string_of_rulepaths nottyp inputs rulepaths

and string_of_rulegroups nottyp inputs rulegroups =
  rulegroups
  |> List.map (string_of_rulegroup nottyp inputs)
  |> String.concat "\n\n"

and string_of_elsegroup nottyp inputs elsegroup =
  let rulegroupid, rulematch, rulepath = elsegroup.it in
  indent 1 ^ "rulegroup "
  ^ string_of_rulegroupid rulegroupid
  ^ "\n\n " ^ indent 1 ^ "match\n\n"
  ^ string_of_rulematch nottyp inputs rulematch
  ^ "\n\n " ^ indent 1 ^ "paths\n\n"
  ^ string_of_rulepaths nottyp inputs [ rulepath ]

and string_of_elsegroup_opt nottyp inputs elsegroup_opt =
  match elsegroup_opt with
  | None -> ""
  | Some elsegroup ->
      "\n\n" ^ indent 1 ^ "elsegroup\n\n"
      ^ string_of_elsegroup nottyp inputs elsegroup

(* Clause *)

let string_of_clause idx clause = Il.Print.string_of_clause idx clause
let string_of_clauses clauses = Il.Print.string_of_clauses clauses
let string_of_elseclause elseclause = Il.Print.string_of_elseclause elseclause

let string_of_elseclause_opt elseclause_opt =
  Il.Print.string_of_elseclause_opt elseclause_opt

(* Table rows *)

let string_of_tablerow tablerow =
  let exps_signature, args, exp, prems = tablerow.it in
  "\n" ^ indent 2 ^ "(signature) "
  ^ string_of_exps ", " exps_signature
  ^ "\n" ^ indent 2 ^ string_of_args args ^ " -> " ^ string_of_exp exp
  ^ string_of_prems ~level:2 prems

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
  | RelD (relid, nottyp, inputs, rulegroups, elsegroup_opt, _) ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_nottyp nottyp
      ^ "\n\n"
      ^ string_of_rulegroups nottyp inputs rulegroups
      ^ string_of_elsegroup_opt nottyp inputs elsegroup_opt
  | ExternDecD (defid, tparams, params, typ, _) ->
      "extern def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | BuiltinDecD (defid, tparams, params, typ, _) ->
      "builtin def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ
  | TableDecD (defid, params, typ, tablerows, _) ->
      "tbldef " ^ string_of_defid defid ^ string_of_params params ^ " : "
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
