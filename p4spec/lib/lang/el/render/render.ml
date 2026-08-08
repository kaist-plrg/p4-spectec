open Domain
open Xl
open Ast
open Util.Source

type atom_mode = SourceAtom | DisplayAtom

(* Documents *)

let ( ^^ ) = Doc.( ^^ )
let width = 80
let text = Doc.text
let space = text " "

(* Iterators *)

let doc_of_iter = function Opt -> text "?" | List -> text "*"

(* Identifiers *)

let doc_of_varid id_var = text id_var.it
let doc_of_typid id_typ = text id_typ.it
let doc_of_relid id_rel = text id_rel.it
let doc_of_defid id_def = text ("$" ^ id_def.it)
let doc_of_tparam id_tparam = text id_tparam.it

let doc_of_rule_suffix id_suffix =
  if id_suffix.it = "" then Doc.empty else text ("/" ^ id_suffix.it)

(* Atoms *)

let doc_of_atom atom_mode atom =
  let s =
    match atom_mode with
    | SourceAtom -> Atom.string_of_atom atom.it
    | DisplayAtom -> Atom.render_atom atom.it
  in
  text s

(* Lists *)

let doc_of_comma_list ~indent s_open s_close doc_of_item items =
  match items with
  | [] -> text (s_open ^ s_close)
  | _ ->
      Doc.group
        (text s_open
        ^^ Doc.nest indent
             (Doc.break ""
             ^^ Doc.join
                  (text "," ^^ Doc.break " ")
                  (List.map doc_of_item items))
        ^^ text s_close)

let doc_of_optional_comma_list ~indent s_open s_close doc_of_item items =
  match items with
  | [] -> Doc.empty
  | _ -> doc_of_comma_list ~indent s_open s_close doc_of_item items

(* Operators *)

let doc_of_bracket atom_mode doc atom_l atom_r =
  Doc.group
    (doc_of_atom atom_mode atom_l
    ^^ Doc.nest 2 (Doc.break " " ^^ doc)
    ^^ Doc.break " "
    ^^ doc_of_atom atom_mode atom_r)

let doc_of_infix doc_l op doc_r =
  Doc.group (doc_l ^^ Doc.nest 4 (Doc.break " " ^^ text op ^^ space ^^ doc_r))

let doc_of_unop = function
  | #Bool.unop as op -> text (Bool.string_of_unop op)
  | #Num.unop as op -> text (Num.string_of_unop op)

let string_of_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

let string_of_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Types *)

let rec doc_of_typ atom_mode = function
  | PlainT plaintyp -> doc_of_plaintyp atom_mode plaintyp
  | NotationT nottyp -> doc_of_nottyp atom_mode nottyp

and doc_of_plaintyp atom_mode plaintyp =
  match plaintyp.it with
  | BoolT -> text "bool"
  | NumT numtyp -> text (Num.string_of_typ numtyp)
  | TextT -> text "text"
  | VarT (id_typ, targs) -> doc_of_typid id_typ ^^ doc_of_targs atom_mode targs
  | ParenT plaintyp ->
      doc_of_comma_list ~indent:2 "(" ")"
        (doc_of_plaintyp atom_mode)
        [ plaintyp ]
  | TupleT plaintyps ->
      doc_of_comma_list ~indent:2 "(" ")" (doc_of_plaintyp atom_mode) plaintyps
  | IterT (plaintyp, iter) ->
      doc_of_plaintyp atom_mode plaintyp ^^ doc_of_iter iter

and doc_of_nottyp atom_mode nottyp =
  match nottyp.it with
  | AtomT atom -> doc_of_atom atom_mode atom
  | SeqT typs -> typs |> List.map (doc_of_typ atom_mode) |> Doc.flow
  | InfixT (typ_l, atom, typ_r) ->
      doc_of_infix
        (doc_of_typ atom_mode typ_l)
        (match atom_mode with
        | SourceAtom -> Atom.string_of_atom atom.it
        | DisplayAtom -> Atom.render_atom atom.it)
        (doc_of_typ atom_mode typ_r)
  | BrackT (atom_l, typ, atom_r) ->
      doc_of_bracket atom_mode (doc_of_typ atom_mode typ) atom_l atom_r

and doc_of_targ atom_mode targ = doc_of_plaintyp atom_mode targ

and doc_of_targs atom_mode targs =
  doc_of_optional_comma_list ~indent:2 "<" ">" (doc_of_targ atom_mode) targs

let doc_of_typfield atom_mode (atom, plaintyp, _hints) =
  Doc.group
    (doc_of_atom atom_mode atom ^^ space ^^ doc_of_plaintyp atom_mode plaintyp)

let doc_of_typcase atom_mode (typ, _hints) =
  Doc.nest 4 (doc_of_typ atom_mode typ)

let doc_of_deftyp atom_mode deftyp =
  match deftyp.it with
  | PlainTD plaintyp -> text " = " ^^ doc_of_plaintyp atom_mode plaintyp
  | StructTD [] -> text " = {}"
  | StructTD typfields ->
      text " = {"
      ^^ Doc.nest 2
           (Doc.line
           ^^ Doc.join
                (text "," ^^ Doc.line)
                (List.map (doc_of_typfield atom_mode) typfields))
      ^^ Doc.line ^^ text "}"
  | VariantTD [] -> Doc.line ^^ Doc.nest 4 (text ":" ^^ Doc.line ^^ text ";")
  | VariantTD (typcase_h :: typcases_t) ->
      Doc.nest 4
        (Doc.line ^^ text ": "
        ^^ doc_of_typcase atom_mode typcase_h
        ^^ Doc.concat
             (List.map
                (fun typcase ->
                  Doc.group
                    (Doc.break " " ^^ text "| "
                    ^^ doc_of_typcase atom_mode typcase))
                typcases_t)
        ^^ Doc.line ^^ text ";")

(* Expressions *)

let rec doc_of_exp atom_mode exp =
  match exp.it with
  | BoolE b -> text (string_of_bool b)
  | NumE (`DecOp, `Nat n) -> text (Bigint.to_string n)
  | NumE (`HexOp, `Nat n) -> text (Printf.sprintf "0x%X" (Bigint.to_int_exn n))
  | NumE (_, n) -> text (Num.string_of_num n)
  | TextE s -> text ("\"" ^ String.escaped s ^ "\"")
  | VarE id_var -> doc_of_varid id_var
  | UnE (op, exp) -> doc_of_unop op ^^ doc_of_exp atom_mode exp
  | BinE (exp_l, op, exp_r) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_l)
        (string_of_binop op)
        (doc_of_exp atom_mode exp_r)
  | CmpE (exp_l, op, exp_r) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_l)
        (string_of_cmpop op)
        (doc_of_exp atom_mode exp_r)
  | ArithE exp ->
      Doc.group
        (text "$("
        ^^ Doc.nest 2 (Doc.break "" ^^ doc_of_exp atom_mode exp)
        ^^ text ")")
  | EpsE -> text "eps"
  | ListE exps ->
      doc_of_comma_list ~indent:2 "[" "]" (doc_of_exp atom_mode) exps
  | ConsE (exp_l, exp_r) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_l)
        "::"
        (doc_of_exp atom_mode exp_r)
  | CatE (exp_l, exp_r) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_l)
        "++"
        (doc_of_exp atom_mode exp_r)
  | IdxE (exp_b, exp_i) ->
      doc_of_exp atom_mode exp_b
      ^^ Doc.group
           (text "["
           ^^ Doc.nest 2 (Doc.break "" ^^ doc_of_exp atom_mode exp_i)
           ^^ text "]")
  | SliceE (exp_b, exp_l, exp_h) ->
      doc_of_exp atom_mode exp_b
      ^^ Doc.group
           (text "["
           ^^ Doc.nest 2
                (Doc.break ""
                ^^ doc_of_infix
                     (doc_of_exp atom_mode exp_l)
                     ":"
                     (doc_of_exp atom_mode exp_h))
           ^^ text "]")
  | LenE exp -> text "|" ^^ doc_of_exp atom_mode exp ^^ text "|"
  | MemE (exp_e, exp_s) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_e)
        "<-"
        (doc_of_exp atom_mode exp_s)
  | StrE [] -> text "{}"
  | StrE fields ->
      Doc.group
        (text "{"
        ^^ Doc.nest 2
             (Doc.break ""
             ^^ Doc.join
                  (text "," ^^ Doc.break " ")
                  (List.map
                     (fun (atom, exp) ->
                       doc_of_atom atom_mode atom ^^ space
                       ^^ doc_of_exp atom_mode exp)
                     fields))
        ^^ Doc.break "" ^^ text "}")
  | DotE (exp, atom) ->
      doc_of_exp atom_mode exp ^^ text "." ^^ doc_of_atom atom_mode atom
  | UpdE (exp_b, path, exp_f) ->
      doc_of_exp atom_mode exp_b
      ^^ Doc.group
           (text "["
           ^^ Doc.nest 2
                (Doc.break "" ^^ doc_of_path atom_mode path ^^ text " = "
               ^^ doc_of_exp atom_mode exp_f)
           ^^ text "]")
  | ParenE exp ->
      Doc.group
        (text "("
        ^^ Doc.nest 2 (Doc.break "" ^^ doc_of_exp atom_mode exp)
        ^^ text ")")
  | TupleE exps ->
      doc_of_comma_list ~indent:2 "(" ")" (doc_of_exp atom_mode) exps
  | CallE (id_def, targs, args) ->
      Doc.group
        (doc_of_defid id_def
        ^^ doc_of_targs atom_mode targs
        ^^ doc_of_args atom_mode args)
  | IterE (exp, iter) -> doc_of_exp atom_mode exp ^^ doc_of_iter iter
  | SubE (exp, plaintyp) ->
      doc_of_infix (doc_of_exp atom_mode exp) "<:"
        (doc_of_plaintyp atom_mode plaintyp)
  | AtomE atom -> doc_of_atom atom_mode atom
  | SeqE exps -> exps |> List.map (doc_of_exp atom_mode) |> Doc.flow
  | InfixE (exp_l, atom, exp_r) ->
      doc_of_infix
        (doc_of_exp atom_mode exp_l)
        (match atom_mode with
        | SourceAtom -> Atom.string_of_atom atom.it
        | DisplayAtom -> Atom.render_atom atom.it)
        (doc_of_exp atom_mode exp_r)
  | BrackE (atom_l, exp, atom_r) ->
      doc_of_bracket atom_mode (doc_of_exp atom_mode exp) atom_l atom_r
  | HoleE (`Num i) -> text ("%" ^ string_of_int i)
  | HoleE `Next -> text "%"
  | HoleE `Rest -> text "%%"
  | HoleE `None -> text "!%"
  | FuseE (exp_l, exp_r) ->
      doc_of_exp atom_mode exp_l ^^ text "#" ^^ doc_of_exp atom_mode exp_r
  | UnparenE exp -> text "##" ^^ doc_of_exp atom_mode exp
  | LatexE s -> text ("latex(\"" ^ String.escaped s ^ "\")")

(* Paths *)

and doc_of_path atom_mode path =
  match path.it with
  | RootP -> Doc.empty
  | IdxP (path, exp) ->
      doc_of_path atom_mode path ^^ text "[" ^^ doc_of_exp atom_mode exp
      ^^ text "]"
  | SliceP (path, exp_l, exp_h) ->
      doc_of_path atom_mode path ^^ text "[" ^^ doc_of_exp atom_mode exp_l
      ^^ text " : " ^^ doc_of_exp atom_mode exp_h ^^ text "]"
  | DotP ({ it = RootP; _ }, atom) -> doc_of_atom atom_mode atom
  | DotP (path, atom) ->
      doc_of_path atom_mode path ^^ text "." ^^ doc_of_atom atom_mode atom

(* Arguments *)

and doc_of_arg atom_mode arg =
  match arg.it with
  | ExpA exp -> doc_of_exp atom_mode exp
  | DefA id_def -> text "def " ^^ doc_of_defid id_def

and doc_of_args atom_mode args =
  doc_of_comma_list ~indent:4 "(" ")" (doc_of_arg atom_mode) args

(* Parameters *)

let rec doc_of_param atom_mode param =
  match param.it with
  | ExpP plaintyp -> doc_of_plaintyp atom_mode plaintyp
  | DefP (id_def, tparams, params, plaintyp) ->
      Doc.group
        (text "def " ^^ doc_of_defid id_def ^^ doc_of_tparams tparams
        ^^ doc_of_params atom_mode params
        ^^ text " : "
        ^^ doc_of_plaintyp atom_mode plaintyp)

and doc_of_params atom_mode params =
  doc_of_optional_comma_list ~indent:4 "(" ")" (doc_of_param atom_mode) params

(* Type parameters *)

and doc_of_tparams tparams =
  doc_of_optional_comma_list ~indent:2 "<" ">" doc_of_tparam tparams

(* Premises *)

let doc_of_rel_prem atom_mode s_marker id_rel exp =
  Doc.group
    (doc_of_relid id_rel ^^ text s_marker
    ^^ Doc.nest 4 (Doc.break " " ^^ doc_of_exp atom_mode exp))

let rec doc_of_prem atom_mode prem =
  match prem.it with
  | VarPr (id_var, plaintyp) ->
      doc_of_varid id_var ^^ text " : " ^^ doc_of_plaintyp atom_mode plaintyp
  | RulePr (id_rel, exp) -> doc_of_rel_prem atom_mode ":" id_rel exp
  | RuleNotPr (id_rel, exp) -> doc_of_rel_prem atom_mode ":/" id_rel exp
  | IfPr exp -> text "if " ^^ doc_of_exp atom_mode exp
  | ElsePr -> text "otherwise"
  | IterPr (({ it = IterPr _; _ } as prem), iter) ->
      doc_of_prem atom_mode prem ^^ doc_of_iter iter
  | IterPr (prem, iter) ->
      text "(" ^^ doc_of_prem atom_mode prem ^^ text ")" ^^ doc_of_iter iter
  | DebugPr exp -> text "debug " ^^ doc_of_exp atom_mode exp

let doc_of_prems atom_mode prems =
  prems
  |> List.map (fun prem -> Doc.line ^^ text "-- " ^^ doc_of_prem atom_mode prem)
  |> Doc.concat

(* Rules *)

let doc_of_rule atom_mode rule =
  let id_rel, id_rule, exp, prems = rule.it in
  Doc.group
    (text "rule"
    ^^ Doc.nest 2
         (Doc.break " " ^^ doc_of_relid id_rel ^^ doc_of_rule_suffix id_rule)
    ^^ text ":")
  ^^ Doc.nest 2
       (Doc.line ^^ doc_of_exp atom_mode exp ^^ doc_of_prems atom_mode prems)

(* Tables *)

let doc_of_tablerow atom_mode tablerow =
  let exp_pattern, exp_body = tablerow.it in
  Doc.group
    (text "| "
    ^^ doc_of_exp atom_mode exp_pattern
    ^^ Doc.nest 4 (Doc.break " " ^^ text "=> " ^^ doc_of_exp atom_mode exp_body)
    )

(* Functions *)

let doc_of_func_dec s_prefix id_def tparams params plaintyp =
  Doc.group
    (text s_prefix ^^ doc_of_defid id_def ^^ doc_of_tparams tparams
    ^^ doc_of_params SourceAtom params
    ^^ Doc.nest 2
         (Doc.break " " ^^ text ": " ^^ doc_of_plaintyp SourceAtom plaintyp))

(* Definitions *)

let doc_of_def def =
  match def.it with
  | ExternSynD (id_typ, _hints) -> text "extern syntax " ^^ doc_of_typid id_typ
  | SynD syntaxes ->
      text "syntax "
      ^^ Doc.group
           (Doc.join
              (text "," ^^ Doc.break " ")
              (List.map
                 (fun (id_typ, tparams) ->
                   doc_of_typid id_typ ^^ doc_of_tparams tparams)
                 syntaxes))
  | TypD (id_typ, tparams, deftyp, _hints) ->
      doc_of_typid id_typ ^^ doc_of_tparams tparams
      ^^ doc_of_deftyp DisplayAtom deftyp
  | VarD (id_var, plaintyp, _hints) ->
      Doc.group
        (text "var " ^^ doc_of_varid id_var
        ^^ Doc.nest 2
             (Doc.break " " ^^ text ": " ^^ doc_of_plaintyp SourceAtom plaintyp)
        )
  | ExternRelD (id_rel, nottyp, _hints) ->
      text "extern relation " ^^ doc_of_relid id_rel ^^ text ":"
      ^^ Doc.nest 2 (Doc.line ^^ doc_of_nottyp SourceAtom nottyp)
  | RelD (id_rel, nottyp, _hints) ->
      text "relation " ^^ doc_of_relid id_rel ^^ text ":"
      ^^ Doc.nest 2 (Doc.line ^^ doc_of_nottyp SourceAtom nottyp)
  | RuleGroupD (_, _, [ rule ]) -> doc_of_rule SourceAtom rule
  | RuleGroupD (id_rel, id_group, rules) ->
      text "rulegroup " ^^ doc_of_relid id_rel
      ^^ doc_of_rule_suffix id_group
      ^^ text " {"
      ^^ Doc.concat
           (List.map
              (fun rule ->
                Doc.line ^^ Doc.nest 2 (Doc.line ^^ doc_of_rule SourceAtom rule))
              rules)
      ^^ Doc.line ^^ Doc.line ^^ text "}"
  | ExternDecD (id_def, tparams, params, plaintyp, _hints) ->
      doc_of_func_dec "extern dec " id_def tparams params plaintyp
  | BuiltinDecD (id_def, tparams, params, plaintyp, _hints) ->
      doc_of_func_dec "builtin dec " id_def tparams params plaintyp
  | TableDecD (id_def, params, plaintyp, _hints) ->
      doc_of_func_dec "tbl dec " id_def [] params plaintyp
  | FuncDecD (id_def, tparams, params, plaintyp, _hints) ->
      doc_of_func_dec "dec " id_def tparams params plaintyp
  | TableDefD (id_def, tablerows) ->
      text "tbl def " ^^ doc_of_defid id_def ^^ text " ="
      ^^ Doc.nest 2
           (Doc.concat
              (List.map
                 (fun tablerow ->
                   Doc.line ^^ doc_of_tablerow SourceAtom tablerow)
                 tablerows))
  | FuncDefD (id_def, tparams, args, exp, prems) ->
      Doc.group
        (text "def " ^^ doc_of_defid id_def ^^ doc_of_tparams tparams
        ^^ doc_of_args SourceAtom args
        ^^ Doc.nest 2 (Doc.break " " ^^ text "= " ^^ doc_of_exp SourceAtom exp)
        )
      ^^ Doc.nest 2 (doc_of_prems SourceAtom prems)
  | SepD -> Doc.line ^^ Doc.line

(* Rendering *)

let render_def def = Doc.render ~width (doc_of_def def)
