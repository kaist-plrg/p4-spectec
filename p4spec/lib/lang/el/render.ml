open Domain
open Xl
open Ast
open Util.Source

(* Numbers *)

let render_num num = Num.string_of_num num

(* Texts *)

let render_text text = text

(* Identifiers *)

let render_varid id_var = id_var.it
let render_typid id_typ = id_typ.it
let render_relid id_rel = id_rel.it

let render_rulegroupid id_rulegroup =
  if id_rulegroup.it = "" then "" else "/" ^ id_rulegroup.it

let render_ruleid id_rule = if id_rule.it = "" then "" else "/" ^ id_rule.it
let render_defid id_def = "$" ^ id_def.it
let render_hintid id_hint = id_hint.it

(* Atoms *)

let render_atom atom = Atom.render_atom atom.it

(* Iterators *)

let render_iter iter = match iter with Opt -> "?" | List -> "*"

(* Types *)

let rec render_typ typ =
  match typ with
  | PlainT plaintyp -> render_plaintyp plaintyp
  | NotationT nottyp -> render_nottyp nottyp

and render_typs sep typs = String.concat sep (List.map render_typ typs)

and render_plaintyp plaintyp =
  match plaintyp.it with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT (typid, targs) -> render_typid typid ^ render_targs targs
  | ParenT plaintyp -> "(" ^ render_plaintyp plaintyp ^ ")"
  | TupleT plaintyps -> "(" ^ render_plaintyps ", " plaintyps ^ ")"
  | IterT (plaintyp, iter) -> render_plaintyp plaintyp ^ render_iter iter

and render_plaintyps sep plaintyps =
  String.concat sep (List.map render_plaintyp plaintyps)

and render_nottyp nottyp =
  match nottyp.it with
  | AtomT atom -> render_atom atom
  | SeqT typs -> render_typs " " typs
  | InfixT (typ_l, atom, typ_r) ->
      render_typ typ_l ^ " " ^ render_atom atom ^ " " ^ render_typ typ_r
  | BrackT (atom_l, typ, atom_r) ->
      "`" ^ render_atom atom_l ^ " " ^ render_typ typ ^ " " ^ render_atom atom_r

and render_nottyps sep nottyps =
  String.concat sep (List.map render_nottyp nottyps)

and render_deftyp deftyp =
  match deftyp.it with
  | PlainTD plaintyp -> " = " ^ render_plaintyp plaintyp
  | StructTD [] -> " = {}"
  | StructTD typfields -> " = {\n" ^ render_typfields ",\n" typfields ^ "\n}"
  | VariantTD typcases ->
      "\n   : " ^ render_typcases "\n   | " typcases ^ "\n   ;"

and render_typfield typfield =
  let atom, plaintyp, _hints = typfield in
  "  " ^ render_atom atom ^ " " ^ render_plaintyp plaintyp

and render_typfields sep typfields =
  String.concat sep (List.map render_typfield typfields)

and render_typcase typcase =
  let typ, _hints = typcase in
  match typ with
  | NotationT { it = SeqT typs; _ } -> (
      let width_max = 80 in
      let col_start = 5 in
      let indent = "       " in
      let col_indent = String.length indent in
      match typs with
      | [] -> ""
      | typ_h :: typs_t ->
          let buf = Buffer.create 64 in
          let s_h = render_typ typ_h in
          Buffer.add_string buf s_h;
          let col = ref (col_start + String.length s_h) in
          List.iter
            (fun typ ->
              let s = render_typ typ in
              if !col + 1 + String.length s > width_max then (
                Buffer.add_char buf '\n';
                Buffer.add_string buf indent;
                Buffer.add_string buf s;
                col := col_indent + String.length s)
              else (
                Buffer.add_char buf ' ';
                Buffer.add_string buf s;
                col := !col + 1 + String.length s))
            typs_t;
          Buffer.contents buf)
  | _ -> render_typ typ

and render_typcases sep typcases =
  String.concat sep (List.map render_typcase typcases)

(* Operators *)

and render_unop = function
  | #Bool.unop as op -> Bool.string_of_unop op
  | #Num.unop as op -> Num.string_of_unop op

and render_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

and render_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Expressions *)

and render_exp exp =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE (`DecOp, `Nat n) -> Bigint.to_string n
  | NumE (`HexOp, `Nat n) -> Format.asprintf "0x%X" (Bigint.to_int_exn n)
  | NumE (_, n) -> render_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE id -> render_varid id
  | UnE (unop, exp) -> render_unop unop ^ render_exp exp
  | BinE (exp_l, binop, exp_r) ->
      render_exp exp_l ^ " " ^ render_binop binop ^ " " ^ render_exp exp_r
  | CmpE (exp_l, cmpop, exp_r) ->
      render_exp exp_l ^ " " ^ render_cmpop cmpop ^ " " ^ render_exp exp_r
  | ArithE exp -> "$(" ^ render_exp exp ^ ")"
  | EpsE -> "eps"
  | ListE exps -> "[" ^ render_exps ", " exps ^ "]"
  | ConsE (exp_l, exp_r) -> render_exp exp_l ^ " :: " ^ render_exp exp_r
  | CatE (exp_l, exp_r) -> render_exp exp_l ^ " ++ " ^ render_exp exp_r
  | IdxE (exp_b, exp_i) -> render_exp exp_b ^ "[" ^ render_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      render_exp exp_b ^ "[" ^ render_exp exp_l ^ " : " ^ render_exp exp_h ^ "]"
  | LenE exp -> "|" ^ render_exp exp ^ "|"
  | MemE (exp_e, exp_s) -> render_exp exp_e ^ " <- " ^ render_exp exp_s
  | StrE fields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> render_atom atom ^ " " ^ render_exp exp)
             fields)
      ^ "}"
  | DotE (exp, atom) -> render_exp exp ^ "." ^ render_atom atom
  | UpdE (exp_b, path, exp_f) ->
      render_exp exp_b ^ "[" ^ render_path path ^ " = " ^ render_exp exp_f ^ "]"
  | ParenE exp -> "(" ^ render_exp exp ^ ")"
  | TupleE exps -> "(" ^ render_exps ", " exps ^ ")"
  | CallE (id, targs, args) ->
      render_defid id ^ render_targs targs ^ render_args args
  | IterE (exp, iter) -> render_exp exp ^ render_iter iter
  | SubE (exp, plaintyp) -> render_exp exp ^ " <: " ^ render_plaintyp plaintyp
  | AtomE atom -> render_atom atom
  | SeqE exps -> render_exps " " exps
  | InfixE (exp_l, atom, exp_r) ->
      render_exp exp_l ^ " " ^ render_atom atom ^ " " ^ render_exp exp_r
  | BrackE (atom_l, exp, atom_r) ->
      "`" ^ render_atom atom_l ^ render_exp exp ^ render_atom atom_r
  | HoleE (`Num i) -> "%" ^ string_of_int i
  | HoleE `Next -> "%"
  | HoleE `Rest -> "%%"
  | HoleE `None -> "!%"
  | FuseE (exp_l, exp_r) -> render_exp exp_l ^ "#" ^ render_exp exp_r
  | UnparenE exp -> "##" ^ render_exp exp
  | LatexE s -> "latex(" ^ String.escaped s ^ ")"

and render_exps sep exps = String.concat sep (List.map render_exp exps)

(* Paths *)

and render_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> render_path path ^ "[" ^ render_exp exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      render_path path ^ "[" ^ render_exp exp_l ^ " : " ^ render_exp exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> render_atom atom
  | DotP (path, atom) -> render_path path ^ "." ^ render_atom atom

(* Parameters *)

(* Type parameters *)

and render_tparam tparam = tparam.it

and render_tparams tparams =
  match tparams with
  | [] -> ""
  | tparams -> "<" ^ String.concat ", " (List.map render_tparam tparams) ^ ">"

(* Arguments *)

and render_arg arg =
  match arg.it with
  | ExpA exp -> render_exp exp
  | DefA id_def -> render_defid id_def

and render_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map render_arg args) ^ ")"

(* Type arguments *)

and render_targ targ = render_plaintyp targ

and render_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map render_targ targs) ^ ">"

(* Premises *)

and render_prem prem =
  match prem.it with
  | VarPr (id, plaintyp) -> render_varid id ^ " : " ^ render_plaintyp plaintyp
  | RulePr (id, exp) -> render_relid id ^ ": " ^ render_exp exp
  | RuleNotPr (id, exp) -> render_relid id ^ ":/ " ^ render_exp exp
  | IfPr exp -> "if " ^ render_exp exp
  | ElsePr -> "otherwise"
  | IterPr (({ it = IterPr _; _ } as prem), iter) ->
      render_prem prem ^ render_iter iter
  | IterPr (prem, iter) -> "(" ^ render_prem prem ^ ")" ^ render_iter iter
  | DebugPr exp -> "debug " ^ render_exp exp

and render_prems prems =
  String.concat "" (List.map (fun prem -> "\n    -- " ^ render_prem prem) prems)

(* Hints *)

and render_hint hint =
  "hint(" ^ render_hintid hint.hintid ^ " " ^ render_exp hint.hintexp ^ ")"

and render_hints hints = String.concat "\n   " (List.map render_hint hints)

(* Rules *)

let render_rule id_rulegroup rule =
  let id_rel, id_rule, exp, prems = rule.it in
  if id_rulegroup.it = id_rule.it then
    "rule " ^ render_relid id_rel
    ^ render_rulegroupid id_rulegroup
    ^ ":\n   " ^ render_exp exp ^ render_prems prems
  else
    "rule " ^ render_relid id_rel
    ^ render_rulegroupid id_rulegroup
    ^ render_ruleid id_rule ^ ":\n   " ^ render_exp exp ^ render_prems prems

let render_rules id_rulegroup rules =
  String.concat "\n\n" (List.map (render_rule id_rulegroup) rules)

(* Definitions *)

let render_type_def id_typ tparams deftyp _hints =
  render_typid id_typ ^ render_tparams tparams ^ render_deftyp deftyp

let render_relation_def id_rel nottyp hints =
  "relation " ^ render_relid id_rel ^ ":\n   " ^ render_nottyp nottyp
  ^ if hints = [] then "" else "\n   " ^ render_hints hints

let render_extern_relation_def id_rel nottyp hints =
  "extern relation " ^ render_relid id_rel ^ ":\n   " ^ render_nottyp nottyp
  ^ if hints = [] then "" else "\n   " ^ render_hints hints

let render_rulegroup_def _id_rel id_rulegroup rules =
  render_rules id_rulegroup rules

let render_def def =
  match def.it with
  | ExternSynD _ | SynD _ -> ""
  | TypD (id_typ, tparams, deftyp, hints) ->
      render_type_def id_typ tparams deftyp hints
  | VarD _ -> ""
  | ExternRelD (id_rel, nottyp, hints) ->
      render_extern_relation_def id_rel nottyp hints
  | RelD (id_rel, nottyp, hints) -> render_relation_def id_rel nottyp hints
  | RuleGroupD (id_rel, id_rulegroup, rules) ->
      render_rulegroup_def id_rel id_rulegroup rules
  | ExternDecD _ -> ""
  | BuiltinDecD _ -> ""
  | TableDecD _ -> ""
  | FuncDecD _ -> ""
  | TableDefD _ -> ""
  | FuncDefD _ -> ""
  | SepD -> "\n\n"
