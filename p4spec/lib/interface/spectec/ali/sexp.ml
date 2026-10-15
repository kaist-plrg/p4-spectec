module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Al = Lang.Al
module Value = Runtime.Value
open Util.Source

(* Emitting an `Al.spec` as a Racket s-expression, for the Redex specification
   in spec-meta-redex/. The encoding is described in CROSS_REDEX.md, "Term
   encoding". *)

type t =
  | Sym of string
  | Str of string
  | Int of Bigint.t
  | Bool of bool
  | List of t list

let app (name : string) (args : t list) : t = List (Sym name :: args)

(* `x?` is a list of length 0 or 1 *)
let opt (f : 'a -> t) (x_opt : 'a option) : t =
  List (Option.to_list (Option.map f x_opt))

let list (f : 'a -> t) (xs : 'a list) : t = List (List.map f xs)

(* Printing, in Racket reader syntax *)

let add_string (buf : Buffer.t) (s : string) : unit =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Printf.bprintf buf "\\u%04x" (Char.code c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let rec add_sexp (buf : Buffer.t) (sexp : t) : unit =
  match sexp with
  | Sym s -> Buffer.add_string buf s
  | Str s -> add_string buf s
  | Int i -> Buffer.add_string buf (Bigint.to_string i)
  | Bool b -> Buffer.add_string buf (if b then "#t" else "#f")
  | List sexps ->
      Buffer.add_char buf '(';
      List.iteri
        (fun idx sexp ->
          if idx > 0 then Buffer.add_char buf ' ';
          add_sexp buf sexp)
        sexps;
      Buffer.add_char buf ')'

(* Identifiers *)

let sexp_of_id (id : Il.id) : t = Str id.it

(* Atoms *)

let sexp_of_atom (atom : Il.atom) : t = Str (Atom.string_of_atom atom.it)

(* Mixfix operators *)

let sexp_of_mixop (mixop : Il.mixop) : t =
  list (list sexp_of_atom) (Mixop.atoms_matrix mixop)

(* Iterators *)

let sexp_of_iter (iter : Il.iter) : t =
  match iter with Opt -> Sym "QUEST" | List -> Sym "STAR"

(* Types *)

let rec sexp_of_typ (typ : Il.typ) : t =
  match typ.it with
  | BoolT -> Sym "BOOL"
  | NumT `NatT -> Sym "NAT"
  | NumT `IntT -> Sym "INT"
  | TextT -> Sym "TEXT"
  | VarT (id, targs) -> app "VAR" [ sexp_of_id id; list sexp_of_typ targs ]
  | TupleT typs -> app "TUP" [ list sexp_of_typ typs ]
  | IterT (typ, iter) -> app "ITER" [ sexp_of_typ typ; sexp_of_iter iter ]
  | FuncT (_, _, _) -> Sym "FUNC"

(* Type parameters *)

let sexp_of_tparams (tparams : Il.tparam list) : t = list sexp_of_id tparams

(* Variables *)

let sexp_of_vari ((id, typ, iters) : Il.var) : t =
  List [ sexp_of_id id; sexp_of_typ typ; list sexp_of_iter iters ]

(* Defined types *)

let sexp_of_typfield ((atom, typ) : Il.typfield) : t =
  List [ sexp_of_atom atom; sexp_of_typ typ ]

let sexp_of_typcase (typcase : Il.typcase) : t =
  let nottyp, _, _ = typcase in
  let mixop, typs = Mixfix.split nottyp.it in
  List [ sexp_of_mixop mixop; list sexp_of_typ typs ]

let sexp_of_deftyp (deftyp : Il.deftyp) : t =
  match deftyp.it with
  | PlainT typ -> app "ALIAS" [ sexp_of_typ typ ]
  | StructT typfields -> app "STRUCT" [ list sexp_of_typfield typfields ]
  | VariantT typcases -> app "VARIANT" [ list sexp_of_typcase typcases ]

(* Values *)

let sexp_of_num (num : Il.num) : t =
  match num with `Nat n -> app "NAT" [ Int n ] | `Int i -> app "INT" [ Int i ]

let rec sexp_of_value (value : Il.value) : t =
  match value.it with
  | BoolV b -> app "BOOL" [ Bool b ]
  | NumV num -> sexp_of_num num
  | TextV t -> app "TEXT" [ Str t ]
  | StructV valuefields -> app "STR" [ list sexp_of_valuefield valuefields ]
  | CaseV valuecase -> app "INJ" [ sexp_of_valuecase valuecase ]
  | TupleV values -> app "TUP" [ list sexp_of_value values ]
  | OptV value_opt -> app "OPT" [ opt sexp_of_value value_opt ]
  | ListV values -> app "LIST" [ list sexp_of_value values ]
  | FuncV id -> app "FUNC" [ sexp_of_id id ]
  | ExternV json -> app "EXT" [ Str (Yojson.Safe.to_string json) ]

and sexp_of_valuefield ((atom, value) : Il.valuefield) : t =
  List [ sexp_of_atom atom; sexp_of_value value ]

and sexp_of_valuecase (valuecase : Il.valuecase) : t =
  let mixop, values = Mixfix.split valuecase in
  List [ sexp_of_mixop mixop; list sexp_of_value values ]

(* Operators *)

let sexp_of_unop (unop : Il.unop) : t =
  match unop with
  | `NotOp -> Sym "NOT"
  | `PlusOp -> Sym "PLUS"
  | `MinusOp -> Sym "MINUS"

let sexp_of_binop (binop : Il.binop) : t =
  match binop with
  | `AndOp -> Sym "AND"
  | `OrOp -> Sym "OR"
  | `ImplOp -> Sym "IMPL"
  | `EquivOp -> Sym "EQUIV"
  | `AddOp -> Sym "ADD"
  | `SubOp -> Sym "SUB"
  | `MulOp -> Sym "MUL"
  | `DivOp -> Sym "DIV"
  | `ModOp -> Sym "MOD"
  | `PowOp -> Sym "POW"

let sexp_of_cmpop (cmpop : Il.cmpop) : t =
  match cmpop with
  | `EqOp -> Sym "EQ"
  | `NeOp -> Sym "NE"
  | `LtOp -> Sym "LT"
  | `LeOp -> Sym "LE"
  | `GtOp -> Sym "GT"
  | `GeOp -> Sym "GE"

(* Patterns *)

let sexp_of_pattern (pattern : Il.pattern) : t =
  match pattern with
  | CaseP mixop -> app "INJ" [ sexp_of_mixop mixop ]
  | ListP `Cons -> Sym "CONS"
  | ListP (`Fixed n) -> app "FIXED" [ Int (Bigint.of_int n) ]
  | ListP `Nil -> Sym "NIL"
  | OptP `Some -> Sym "SOME"
  | OptP `None -> Sym "NONE"

(* Expressions *)

let rec sexp_of_exp (exp : Il.exp) : t =
  match exp.it with
  | BoolE b -> app "BOOL" [ Bool b ]
  | NumE num -> sexp_of_num num
  | TextE t -> app "TEXT" [ Str t ]
  | VarE id -> app "VAR" [ sexp_of_id id ]
  | UnE (unop, _, e) -> app "UN" [ sexp_of_unop unop; sexp_of_exp e ]
  | BinE (binop, _, el, er) ->
      app "BIN" [ sexp_of_binop binop; sexp_of_exp el; sexp_of_exp er ]
  | CmpE (cmpop, _, el, er) ->
      app "CMP" [ sexp_of_cmpop cmpop; sexp_of_exp el; sexp_of_exp er ]
  | UpCastE (typ, e) -> app "UPCAST" [ sexp_of_typ typ; sexp_of_exp e ]
  | DownCastE (typ, e) -> app "DOWNCAST" [ sexp_of_typ typ; sexp_of_exp e ]
  | SubE (e, typ) -> app "SUB" [ sexp_of_exp e; sexp_of_typ typ ]
  | MatchE (e, pattern) ->
      app "MATCH" [ sexp_of_exp e; sexp_of_pattern pattern ]
  | TupleE exps -> app "TUP" [ list sexp_of_exp exps ]
  | CaseE notexp -> app "INJ" [ sexp_of_expcase notexp ]
  | StrE expfields -> app "STR" [ list sexp_of_expfield expfields ]
  | OptE exp_opt -> app "OPT" [ opt sexp_of_exp exp_opt ]
  | ListE exps -> app "LIST" [ list sexp_of_exp exps ]
  | ConsE (eh, et) -> app "CONS" [ sexp_of_exp eh; sexp_of_exp et ]
  | CatE (el, er) -> app "CAT" [ sexp_of_exp el; sexp_of_exp er ]
  | MemE (ee, es) -> app "MEM" [ sexp_of_exp ee; sexp_of_exp es ]
  | LenE e -> app "LEN" [ sexp_of_exp e ]
  | DotE (e, atom) -> app "DOT" [ sexp_of_exp e; sexp_of_atom atom ]
  | IdxE (eb, ei) -> app "IDX" [ sexp_of_exp eb; sexp_of_exp ei ]
  | SliceE (eb, ei, en) ->
      app "SLICE" [ sexp_of_exp eb; sexp_of_exp ei; sexp_of_exp en ]
  | UpdE (eb, path, en) ->
      app "UPD" [ sexp_of_exp eb; sexp_of_path path; sexp_of_exp en ]
  | CallE (id, targs, args) ->
      app "CALL"
        [ sexp_of_id id; list sexp_of_typ targs; list sexp_of_arg args ]
  | IterE (e, iterexp) -> app "ITER" [ sexp_of_exp e; sexp_of_iterexp iterexp ]

and sexp_of_expfield ((atom, exp) : Il.atom * Il.exp) : t =
  List [ sexp_of_atom atom; sexp_of_exp exp ]

and sexp_of_expcase (notexp : Il.notexp) : t =
  let mixop, exps = Mixfix.split notexp in
  List [ sexp_of_mixop mixop; list sexp_of_exp exps ]

and sexp_of_iterexp ((iter, vars) : Il.iterexp) : t =
  List [ sexp_of_iter iter; list sexp_of_vari vars ]

(* Paths *)

and sexp_of_path (path : Il.path) : t =
  match path.it with
  | RootP -> Sym "ROOT"
  | IdxP (path, exp) -> app "IDX" [ sexp_of_path path; sexp_of_exp exp ]
  | SliceP (path, exp_i, exp_n) ->
      app "SLICE" [ sexp_of_path path; sexp_of_exp exp_i; sexp_of_exp exp_n ]
  | DotP (path, atom) -> app "DOT" [ sexp_of_path path; sexp_of_atom atom ]

(* Arguments *)

and sexp_of_arg (arg : Il.arg) : t =
  match arg.it with
  | ExpA e -> app "EXP" [ sexp_of_exp e ]
  | DefA id -> app "FUN" [ sexp_of_id id ]

(* Parameters *)

let rec sexp_of_param (param : Il.param) : t =
  match param.it with
  | ExpP typ -> app "EXP" [ sexp_of_typ typ ]
  | DefP (id, tparams, params, typ) ->
      app "FUN"
        [
          sexp_of_id id;
          sexp_of_tparams tparams;
          list sexp_of_param params;
          sexp_of_typ typ;
        ]

(* Premises *)

let rec sexp_of_prem (prem : Il.prem) : t =
  match prem.it with
  | RulePr (id, notexp, input) ->
      let exps = Mixfix.args notexp in
      let exps_in, exps_out = Lang.Hints.Input.split input exps in
      app "REL"
        [ sexp_of_id id; list sexp_of_exp exps_in; list sexp_of_exp exps_out ]
  | IfPr e -> app "IF" [ sexp_of_exp e ]
  | IfHoldPr (id, notexp) ->
      app "IFHOLD" [ sexp_of_id id; list sexp_of_exp (Mixfix.args notexp) ]
  | IfNotHoldPr (id, notexp) ->
      app "IFNOTHOLD" [ sexp_of_id id; list sexp_of_exp (Mixfix.args notexp) ]
  | LetPr (el, er) -> app "LET" [ sexp_of_exp el; sexp_of_exp er ]
  | IterPr (p, ip) -> app "ITER" [ sexp_of_prem p; sexp_of_iterprem ip ]
  | DebugPr e -> app "DEBUG" [ sexp_of_exp e ]

and sexp_of_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : t =
  List
    [ sexp_of_iter iter; list sexp_of_vari vars_in; list sexp_of_vari vars_out ]

(* Rule matching and paths *)

let sexp_of_rulmatch ((_, exps_input, prems) : Al.rulematch) : t =
  List [ list sexp_of_exp exps_input; list sexp_of_prem prems ]

let sexp_of_rulpath ((id, prems, exps_output) : Al.rulepath) : t =
  List [ sexp_of_id id; list sexp_of_exp exps_output; list sexp_of_prem prems ]

let sexp_of_rulgroup (rulegroup : Al.rulegroup) : t =
  let id, rulmatch_, rulpaths = rulegroup.it in
  List
    [ sexp_of_id id; sexp_of_rulmatch rulmatch_; list sexp_of_rulpath rulpaths ]

let sexp_of_elsgroup (elsegroup : Al.elsegroup) : t =
  let id, rulmatch_, rulpath_ = elsegroup.it in
  List [ sexp_of_id id; sexp_of_rulmatch rulmatch_; sexp_of_rulpath rulpath_ ]

(* Clauses and table rows *)

let sexp_of_clause (clause : Il.clause) : t =
  let args, exp, prems = clause.it in
  List [ list sexp_of_arg args; sexp_of_exp exp; list sexp_of_prem prems ]

let sexp_of_tblrow (tablerow : Al.tablerow) : t =
  let _exps, args, exp, prems = tablerow.it in
  List [ list sexp_of_arg args; sexp_of_exp exp; list sexp_of_prem prems ]

(* Definitions *)

let sexp_of_def (def : Al.def) : t option =
  match def.it with
  | ExternTypD (id, _) -> Some (app "EXTTYP" [ sexp_of_id id ])
  | TypD (id, tparams, deftyp, _) ->
      Some
        (app "TYP"
           [ sexp_of_id id; sexp_of_tparams tparams; sexp_of_deftyp deftyp ])
  | VarD _ -> None
  | ExternRelD (id, nottyp, input, _) ->
      let typs = Mixfix.args nottyp.it in
      let typs_in, typs_out = Lang.Hints.Input.split input typs in
      Some
        (app "EXTREL"
           [
             sexp_of_id id; list sexp_of_typ typs_in; list sexp_of_typ typs_out;
           ])
  | RelD (id, nottyp, input, rulgroups, elsegroup_opt, _) ->
      let typs = Mixfix.args nottyp.it in
      let typs_in, typs_out = Lang.Hints.Input.split input typs in
      Some
        (app "REL"
           [
             sexp_of_id id;
             list sexp_of_typ typs_in;
             list sexp_of_typ typs_out;
             list sexp_of_rulgroup rulgroups;
             opt sexp_of_elsgroup elsegroup_opt;
           ])
  | ExternDecD (id, tparams, params, typ, _) ->
      Some
        (app "EXTFUNC"
           [
             sexp_of_id id;
             sexp_of_tparams tparams;
             list sexp_of_param params;
             sexp_of_typ typ;
           ])
  | BuiltinDecD (id, tparams, params, typ, _) ->
      Some
        (app "BUILTINFUNC"
           [
             sexp_of_id id;
             sexp_of_tparams tparams;
             list sexp_of_param params;
             sexp_of_typ typ;
           ])
  | TableDecD (id, params, typ, tablerows, _) ->
      Some
        (app "TABLEFUNC"
           [
             sexp_of_id id;
             list sexp_of_param params;
             sexp_of_typ typ;
             list sexp_of_tblrow tablerows;
           ])
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, _) ->
      Some
        (app "FUNC"
           [
             sexp_of_id id;
             sexp_of_tparams tparams;
             list sexp_of_param params;
             sexp_of_typ typ;
             list sexp_of_clause clauses;
             opt sexp_of_clause elseclause_opt;
           ])

(* Specification, as a `script`, one definition per line *)

let string_of_spec_al (spec : Al.spec) : string =
  let buf = Buffer.create (1024 * 1024) in
  Buffer.add_char buf '(';
  List.iter
    (fun def ->
      Buffer.add_char buf '\n';
      add_sexp buf def)
    (List.filter_map sexp_of_def spec);
  Buffer.add_string buf "\n)";
  Buffer.contents buf

(* Value of a P4 program, as a `val` *)

let string_of_value (value : Value.t) : string =
  let buf = Buffer.create (1024 * 1024) in
  add_sexp buf (sexp_of_value value);
  Buffer.contents buf
