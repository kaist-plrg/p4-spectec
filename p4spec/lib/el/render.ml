open Xl
open Ast
open Util.Source

(* Numbers *)

let render_num num = Num.string_of_num num

(* Texts *)

let render_text text = text

(* Identifiers *)

let render_varid varid = varid.it
let render_typid typid = typid.it
let render_relid relid = relid.it
let render_ruleid ruleid = if ruleid.it = "" then "" else "/" ^ ruleid.it
let render_defid defid = "$" ^ defid.it

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
  | StructTD typfields -> "= {" ^ render_typfields ", " typfields ^ "}"
  | VariantTD [ typcase ] -> " = " ^ render_typcase typcase
  | VariantTD typcases ->
      "\n   : " ^ render_typcases "\n   | " typcases ^ "\n   ;"

and render_typfield typfield =
  let atom, plaintyp, _hints = typfield in
  render_atom atom ^ " " ^ render_plaintyp plaintyp

and render_typfields sep typfields =
  String.concat sep (List.map render_typfield typfields)

and render_typcase typcase =
  let typ, _hints = typcase in
  render_typ typ

and render_typcases sep typcases =
  String.concat sep (List.map render_typcase typcases)

(* Operators *)

(* Expressions *)

(* Paths *)

(* Parameters *)

(* Type parameters *)

and render_tparam tparam = tparam.it

and render_tparams tparams =
  match tparams with
  | [] -> ""
  | tparams -> "<" ^ String.concat ", " (List.map render_tparam tparams) ^ ">"

(* Arguments *)

and render_targ targ = render_plaintyp targ

and render_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map render_targ targs) ^ ">"

(* Type arguments *)

(* Premises *)

(* Rules *)

(* Definitions *)

let render_type_def typid tparams deftyp _hints =
  render_typid typid ^ render_tparams tparams ^ render_deftyp deftyp

let render_def def =
  match def.it with
  | SynD _ -> ""
  | TypD (typid, tparams, deftyp, hints) ->
      render_type_def typid tparams deftyp hints
  | VarD _ -> ""
  | RelD _ -> ""
  | RuleGroupD _ -> ""
  | DecD _ -> ""
  | DefD _ -> ""
  | SepD -> "\n\n"
