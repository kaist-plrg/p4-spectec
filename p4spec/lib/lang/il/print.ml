open Domain
open Xl
open Ast
open Util.Print
open Util.Source

(* Numbers *)

let string_of_num = Num.string_of_num

(* Texts *)

let string_of_text text = text

(* Identifiers *)

let string_of_varid varid = varid.it
let string_of_typid typid = typid.it
let string_of_relid relid = relid.it
let string_of_rulegroupid rulegroupid = rulegroupid.it
let string_of_rulepathid rulepathid = rulepathid.it
let string_of_defid defid = "$" ^ defid.it

(* Atoms *)

let string_of_atom ?(lower = true) atom =
  if lower then Atom.string_of_atom atom.it |> String.lowercase_ascii
  else Atom.string_of_atom atom.it

let string_of_atoms atoms =
  match atoms with
  | [] -> ""
  | _ -> atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Mixop.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = match iter with Opt -> "?" | List -> "*"

(* Variables *)

let string_of_var (id, _typ, iters) =
  string_of_varid id ^ String.concat "" (List.map string_of_iter iters)

(* Types *)

let rec string_of_typ typ =
  match typ.it with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT (typid, targs) -> string_of_typid typid ^ string_of_targs targs
  | TupleT typs -> "(" ^ string_of_typs ", " typs ^ ")"
  | IterT (typ, iter) -> string_of_typ typ ^ string_of_iter iter
  | FuncT -> "func"

and string_of_typs sep typs = String.concat sep (List.map string_of_typ typs)

and string_of_nottyp nottyp =
  let mixop, typs = nottyp.it in
  let len = List.length mixop + List.length typs in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth typs |> string_of_typ)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and string_of_deftyp deftyp =
  match deftyp.it with
  | PlainT typ -> string_of_typ typ
  | StructT typfields -> "{" ^ string_of_typfields ", " typfields ^ "}"
  | VariantT typcases -> "\n   | " ^ string_of_typcases "\n   | " typcases

and string_of_typfield typfield =
  let atom, typ = typfield in
  string_of_nottyp (([ [ atom ]; [] ], [ typ ]) $ no_region)

and string_of_typfields sep typfields =
  String.concat sep (List.map string_of_typfield typfields)

and string_of_typcase typcase =
  let nottyp, hints = typcase in
  string_of_nottyp nottyp ^ string_of_hints hints

and string_of_typcases sep typcases =
  String.concat sep (List.map string_of_typcase typcases)

(* Values *)

and string_of_value ?(short = false) ?(level = 0) value =
  match value.it with
  | BoolV b -> string_of_bool b
  | NumV n -> string_of_num n
  | TextV s -> String.escaped s
  | StructV [] -> "{}"
  | StructV valuefields when short ->
      Format.asprintf "{ .../%d }" (List.length valuefields)
  | StructV valuefields ->
      Format.sprintf "{\n%s\n%s}"
        (String.concat ";\n"
           (List.map
              (fun (atom, value) ->
                let indent = indent (level + 1) in
                Format.asprintf "%s%s %s" indent (string_of_atom atom)
                  (string_of_value ~short ~level:(level + 1) value))
              valuefields))
        (indent level)
  | CaseV (mixop, _) when short -> string_of_mixop mixop
  | CaseV (mixop, values) -> string_of_notval ~level (mixop, values)
  | TupleV values ->
      Format.asprintf "(%s)"
        (String.concat ", "
           (List.map (string_of_value ~short ~level:(level + 1)) values))
  | OptV (Some value) ->
      Format.asprintf "Some(%s)"
        (string_of_value ~short ~level:(level + 1) value)
  | OptV None -> "None"
  | ListV [] -> "[]"
  | ListV values when short -> Format.asprintf "[ .../%d ]" (List.length values)
  | ListV values ->
      Format.asprintf "[\n%s\n%s]"
        (String.concat ",\n"
           (List.map
              (fun value ->
                let indent = indent (level + 1) in
                indent ^ string_of_value ~short ~level:(level + 1) value)
              values))
        (indent level)
  | FuncV id -> string_of_defid id
  | ExternV _ -> "extern"

and string_of_notval ?(level = 0) notval =
  let mixop, values = notval in
  let len = List.length mixop + List.length values in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth values |> string_of_value ~level)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

(* Operators *)

and string_of_unop = function
  | #Bool.unop as op -> Bool.string_of_unop op
  | #Num.unop as op -> Num.string_of_unop op

and string_of_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

and string_of_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Expressions *)

and string_of_exp exp =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE varid -> string_of_varid varid
  | UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | UpCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | DownCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | SubE (exp, typ) -> string_of_exp exp ^ " <: " ^ string_of_typ typ
  | MatchE (exp, pattern) ->
      string_of_exp exp ^ " matches " ^ string_of_pattern pattern
  | TupleE es -> "(" ^ string_of_exps ", " es ^ ")"
  | CaseE notexp -> string_of_notexp notexp
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | OptE exp_opt -> "?(" ^ string_of_exps "" (Option.to_list exp_opt) ^ ")"
  | ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | ConsE (exp_h, exp_t) -> string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | MemE (exp_e, exp_s) -> string_of_exp exp_e ^ " <- " ^ string_of_exp exp_s
  | LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | IdxE (exp_b, exp_i) -> string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | IterE (exp, iterexp) -> string_of_exp exp ^ string_of_iterexp iterexp

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> string_of_exp)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and string_of_iterexp iterexp =
  let iter, vars = iterexp in
  string_of_iter iter ^ "{"
  ^ String.concat ", "
      (List.map
         (fun var ->
           let id, typ, iters = var in
           string_of_var var ^ " <- " ^ string_of_var (id, typ, iters @ [ iter ]))
         vars)
  ^ "}"

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern =
  match pattern with
  | CaseP mixop -> string_of_mixop mixop
  | ListP `Cons -> "_ :: _"
  | ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | ListP `Nil -> "[]"
  | OptP `Some -> "(_)"
  | OptP `None -> "()"

(* Paths *)

and string_of_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> string_of_atom atom
  | DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param =
  match param.it with
  | ExpP typ -> string_of_typ typ
  | DefP (defid, tparams, params, typ) ->
      string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ

and string_of_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map string_of_param params) ^ ")"

(* Type parameters *)

and string_of_tparam tparam = tparam.it

and string_of_tparams tparams =
  match tparams with
  | [] -> ""
  | tparams ->
      "<" ^ String.concat ", " (List.map string_of_tparam tparams) ^ ">"

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | ExpA exp -> string_of_exp exp
  | DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = string_of_typ targ

and string_of_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map string_of_targ targs) ^ ">"

(* Premises *)

and string_of_prem prem =
  match prem.it with
  | RulePr (id, notexp, _inputs) ->
      string_of_relid id ^ ": " ^ string_of_notexp notexp
  | IfPr exp -> "if " ^ string_of_exp exp
  | IfHoldPr (id, notexp) ->
      "if " ^ string_of_relid id ^ ": " ^ string_of_notexp notexp ^ " holds"
  | IfNotHoldPr (id, notexp) ->
      "if " ^ string_of_relid id ^ ": " ^ string_of_notexp notexp
      ^ " does not hold"
  | LetPr (exp_l, exp_r) ->
      "let " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IterPr (({ it = IterPr _; _ } as prem), iterprem) ->
      string_of_prem prem ^ string_of_iterprem iterprem
  | IterPr (prem, iterprem) ->
      "(" ^ string_of_prem prem ^ ")" ^ string_of_iterprem iterprem
  | DebugPr exp -> "debug " ^ string_of_exp exp

and string_of_prems ?(level = 0) prems =
  let indent = indent level in
  String.concat ""
    (List.map (fun prem -> "\n" ^ indent ^ "-- " ^ string_of_prem prem) prems)

and string_of_iterprem iterprem =
  let iter, vars_bound, vars_bind = iterprem in
  string_of_iter iter ^ "{"
  ^ String.concat ", "
      (List.map
         (fun var ->
           let id, typ, iters = var in
           string_of_var var ^ " <- " ^ string_of_var (id, typ, iters @ [ iter ]))
         vars_bound
      @ List.map
          (fun var ->
            let id, typ, iters = var in
            string_of_var var ^ " -> "
            ^ string_of_var (id, typ, iters @ [ iter ]))
          vars_bind)
  ^ "}"

and string_of_iterprems iterprems =
  iterprems |> List.map string_of_iterprem |> String.concat ""

(* Rules *)

and string_of_ruleinput nottyp inputs exps_input =
  let mixop, typs = nottyp.it in
  let exps_input = List.combine inputs exps_input in
  let exps =
    List.init (List.length typs) (fun idx ->
        match List.assoc_opt idx exps_input with
        | Some exp_input -> exp_input
        | None -> VarE ("%" $ no_region) $$ (no_region, TextT))
  in
  let notexp = (mixop, exps) in
  string_of_notexp notexp

and string_of_ruleoutput nottyp inputs exps_output =
  let mixop, typs = nottyp.it in
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
            | None -> VarE ("%" $ no_region) $$ (no_region, TextT))
      in
      let notexp = (mixop, exps) in
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

and string_of_clause idx clause =
  let args, exp, prems = clause.it in
  "clause " ^ string_of_int idx ^ " : " ^ string_of_args args ^ " = "
  ^ string_of_exp exp
  ^ string_of_prems ~level:1 prems

and string_of_clauses clauses =
  String.concat ""
    (List.mapi
       (fun idx clause -> "\n\n" ^ indent 1 ^ string_of_clause idx clause)
       clauses)

and string_of_elseclause elseclause = string_of_clause (-1) elseclause

and string_of_elseclause_opt elseclause_opt =
  match elseclause_opt with
  | None -> ""
  | Some elseclause -> "\n\n" ^ indent 1 ^ string_of_elseclause elseclause

(* Table rows *)

and string_of_tablerow tablerow =
  let exps_signature, args, exp, prems = tablerow.it in
  "\n" ^ indent 2 ^ "(signature) "
  ^ string_of_exps ", " exps_signature
  ^ "\n" ^ indent 2 ^ string_of_args args ^ " -> " ^ string_of_exp exp
  ^ string_of_prems ~level:2 prems

and string_of_tablerows tablerows =
  String.concat ""
    (List.mapi
       (fun idx tablerow ->
         "\n" ^ indent 1 ^ "row " ^ string_of_int idx ^ " :"
         ^ string_of_tablerow tablerow)
       tablerows)

(* Hints *)

and string_of_hint hint =
  " hint(" ^ hint.El.hintid.it ^ " " ^ El.Print.string_of_exp hint.hintexp ^ ")"

and string_of_hints hints = String.concat "" (List.map string_of_hint hints)

(* Definitions *)

let rec string_of_def def =
  match def.it with
  | ExternTypD (id, _) -> "extern syntax " ^ string_of_typid id
  | TypD (typid, tparams, deftyp, _) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
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
      "tbl def " ^ string_of_defid defid ^ string_of_params params ^ " : "
      ^ string_of_typ typ ^ " ="
      ^ string_of_tablerows tablerows
  | FuncDecD (defid, tparams, params, typ, clauses, elseclause_opt, _) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " ="
      ^ string_of_clauses clauses
      ^ string_of_elseclause_opt elseclause_opt

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
