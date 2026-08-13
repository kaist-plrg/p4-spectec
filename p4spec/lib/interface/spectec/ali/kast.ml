module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Al = Lang.Al
module Value = Runtime.Value
open Util.Source

(* Emitting an `Al.spec` as KAST JSON, for the K specification in
   `spec-meta-k/`.

   The K definition is an *abstract* syntax: every watsup production is a
   constructor whose KORE label is pinned with `symbol(_)`.  Emitting a spec
   therefore amounts to naming, for each AL node, the K constructor that
   corresponds to it, and writing lists and options out as the cons lists and
   option constructors that `spec-meta-k` declares.

   This walks the `Al.spec` directly.  The alternative -- booting the spec to a
   `Value.t` first and emitting that -- goes through a representation in which
   every constructor has already been erased to a mixop, so the emitter has to
   *recover* which production built each value by looking its (sort, mixop) pair
   up in a table.  Walking the AST means the constructor is still in hand at the
   point its symbol is written, so there is nothing to recover and no table to
   keep in sync.

   The K symbols are those of `spec-meta-k/common/1-syntax.k` and
   `spec-meta-k/al/1-syntax.k`, whose productions mirror the watsup definitions
   in `spec-meta/`; `Ali.Boot` and `Common.Boot` build the same shape as values,
   so the two are read together when either changes. *)

(* KAST JSON, format version 4.

   `label` and `sort` are objects in version 4; version 3, where they are bare
   strings, is rejected by `kast`. *)

let version = 4

let json_of_klabel (name : string) : Yojson.Safe.t =
  `Assoc
    [
      ("node", `String "KLabel"); ("name", `String name); ("params", `List []);
    ]

let json_of_kapply (name : string) (args : Yojson.Safe.t list) : Yojson.Safe.t =
  `Assoc
    [
      ("node", `String "KApply");
      ("label", json_of_klabel name);
      ("arity", `Int (List.length args));
      ("args", `List args);
    ]

let json_of_ktoken (sort : string) (token : string) : Yojson.Safe.t =
  `Assoc
    [
      ("node", `String "KToken");
      ( "sort",
        `Assoc
          [
            ("node", `String "KSort");
            ("name", `String sort);
            ("params", `List []);
          ] );
      ("token", `String token);
    ]

(* Tokens.

   K's `String` literals carry their quotes inside the token, and escape with
   the same conventions as OCaml's `String.escaped` for the characters that
   occur in identifiers and atoms. *)

let json_of_bool (b : bool) : Yojson.Safe.t =
  json_of_ktoken "Bool" (if b then "true" else "false")

let json_of_int (i : Bigint.t) : Yojson.Safe.t =
  json_of_ktoken "Int" (Bigint.to_string i)

let json_of_string (s : string) : Yojson.Safe.t =
  json_of_ktoken "String" ("\"" ^ String.escaped s ^ "\"")

(* Cons lists.

   Watsup's `X*` becomes a two-constructor cons list in K, since K's own `List`
   is not a term sort the abstract syntax can nest.  `symbol` is the cons and
   `symbol_terminator` the empty list, both named after the element sort. *)

let json_of_klist (symbol : string) (symbol_terminator : string)
    (jsons : Yojson.Safe.t list) : Yojson.Safe.t =
  List.fold_right
    (fun json json_tail -> json_of_kapply symbol [ json; json_tail ])
    jsons
    (json_of_kapply symbol_terminator [])

(* Options.

   Watsup's `X?` becomes a two-constructor sort in K, since K has no generic
   option; both constructors are named after the element sort. *)

let json_of_kopt (symbol_none : string) (symbol_some : string)
    (json_opt : Yojson.Safe.t option) : Yojson.Safe.t =
  match json_opt with
  | None -> json_of_kapply symbol_none []
  | Some json -> json_of_kapply symbol_some [ json ]

(* Errors *)

exception Error of string

let error (fmt : ('a, Format.formatter, unit, 'b) format4) : 'a =
  Format.kasprintf (fun msg -> raise (Error msg)) fmt

(* Identifiers.

   `id` and `atom` are subsorts of `String` in K, so an injection is not written
   out: `kast` infers `String -> Id` and `String -> Atom`. *)

let json_of_id (id : Il.id) : Yojson.Safe.t = json_of_string id.it

(* Atoms.

   Operator atoms keep their quotes inside the string (`':'`), matching
   `Common.Boot.boot_atom`. *)

let json_of_atom (atom : Il.atom) : Yojson.Safe.t =
  json_of_string (Atom.string_of_atom atom.it)

let json_of_atoms (atoms : Il.atom list) : Yojson.Safe.t =
  json_of_klist "atomList" ".atomList" (List.map json_of_atom atoms)

(* Mixfix operators.

   A mixop is an atoms matrix: one row of atoms per notation position, exactly
   as `spec-meta-k/al/4-extern-json.k` puts it on the extern wire. *)

let json_of_mixop (mixop : Il.mixop) : Yojson.Safe.t =
  json_of_klist "mixop" ".mixop"
    (List.map json_of_atoms (Mixop.atoms_matrix mixop))

(* Iterators *)

let json_of_iter (iter : Il.iter) : Yojson.Safe.t =
  match iter with
  | Opt -> json_of_kapply "quest" []
  | List -> json_of_kapply "star" []

let json_of_iters (iters : Il.iter list) : Yojson.Safe.t =
  json_of_klist "iterList" ".iterList" (List.map json_of_iter iters)

(* Types *)

let rec json_of_typ (typ : Il.typ) : Yojson.Safe.t =
  match typ.it with
  | BoolT -> json_of_kapply "boolT" []
  | NumT `NatT -> json_of_kapply "natT" []
  | NumT `IntT -> json_of_kapply "intT" []
  | TextT -> json_of_kapply "textT" []
  | VarT (id, targs) ->
      json_of_kapply "varT" [ json_of_id id; json_of_typs targs ]
  | TupleT typs -> json_of_kapply "tupT" [ json_of_typs typs ]
  | IterT (typ, iter) ->
      json_of_kapply "iterT" [ json_of_typ typ; json_of_iter iter ]
  (* A function type carries no payload in the meta-language syntax: `funcT` is
     nullary, exactly as `boot_func_typ` drops the arrow's components. *)
  | FuncT (_, _, _) -> json_of_kapply "funcT" []

(* `targ` is an alias of `typ` in watsup, and K inlines it, so type arguments
   are emitted as a plain type list. *)
and json_of_typs (typs : Il.typ list) : Yojson.Safe.t =
  json_of_klist "typList" ".typList" (List.map json_of_typ typs)

(* Type parameters.  `tparam` is an alias of `id`. *)

let json_of_tparams (tparams : Il.tparam list) : Yojson.Safe.t =
  json_of_klist "tparamList" ".tparamList" (List.map json_of_id tparams)

(* Variables *)

let json_of_vari ((id, typ, iters) : Il.var) : Yojson.Safe.t =
  json_of_kapply "vari"
    [ json_of_id id; json_of_typ typ; json_of_iters iters ]

let json_of_varis (vars : Il.var list) : Yojson.Safe.t =
  json_of_klist "variList" ".variList" (List.map json_of_vari vars)

(* Defined types *)

let json_of_typfield ((atom, typ) : Il.typfield) : Yojson.Safe.t =
  json_of_kapply "typField" [ json_of_atom atom; json_of_typ typ ]

let json_of_typcase (typcase : Il.typcase) : Yojson.Safe.t =
  let nottyp, _, _ = typcase in
  let mixop, typs = Mixfix.split nottyp.it in
  json_of_kapply "typCase" [ json_of_mixop mixop; json_of_typs typs ]

let json_of_deftyp (deftyp : Il.deftyp) : Yojson.Safe.t =
  match deftyp.it with
  | PlainT typ -> json_of_kapply "aliasDT" [ json_of_typ typ ]
  | StructT typfields ->
      json_of_kapply "structDT"
        [
          json_of_klist "typFieldList" ".typFieldList"
            (List.map json_of_typfield typfields);
        ]
  | VariantT typcases ->
      json_of_kapply "variantDT"
        [
          json_of_klist "typCaseList" ".typCaseList"
            (List.map json_of_typcase typcases);
        ]

(* Values.

   `num` is a case of both `val` and `exp` in watsup and a subsort of both `Val`
   and `Exp` in K, so a number keeps the same constructor wherever it occurs. *)

let json_of_num (num : Il.num) : Yojson.Safe.t =
  match num with
  | `Nat n -> json_of_kapply "natN" [ json_of_int n ]
  | `Int i -> json_of_kapply "intN" [ json_of_int i ]

let rec json_of_value (value : Il.value) : Yojson.Safe.t =
  match value.it with
  | BoolV b -> json_of_kapply "boolV" [ json_of_bool b ]
  | NumV num -> json_of_num num
  | TextV t -> json_of_kapply "textV" [ json_of_string t ]
  | StructV valuefields ->
      json_of_kapply "strV"
        [
          json_of_klist "valFieldList" ".valFieldList"
            (List.map json_of_valuefield valuefields);
        ]
  | CaseV valuecase -> json_of_kapply "injV" [ json_of_valuecase valuecase ]
  | TupleV values -> json_of_kapply "tupV" [ json_of_values values ]
  | OptV value_opt ->
      json_of_kapply "optV"
        [ json_of_kopt "noVal" "someVal" (Option.map json_of_value value_opt) ]
  | ListV values -> json_of_kapply "listV" [ json_of_values values ]
  | FuncV id -> json_of_kapply "funcV" [ json_of_id id ]
  (* `extV` would need K's abstract `Json` sort, which has no K-reachable
     inhabitants, so an extern value cannot be written down as a term at all. *)
  | ExternV _ -> error "extern value cannot be emitted as a K Val"

and json_of_values (values : Il.value list) : Yojson.Safe.t =
  json_of_klist "valList" ".valList" (List.map json_of_value values)

and json_of_valuefield ((atom, value) : Il.valuefield) : Yojson.Safe.t =
  json_of_kapply "valField" [ json_of_atom atom; json_of_value value ]

and json_of_valuecase (valuecase : Il.valuecase) : Yojson.Safe.t =
  let mixop, values = Mixfix.split valuecase in
  json_of_kapply "valCase" [ json_of_mixop mixop; json_of_values values ]

(* Operators *)

let json_of_unop (unop : Il.unop) : Yojson.Safe.t =
  match unop with
  | `NotOp -> json_of_kapply "notOp" []
  | `PlusOp -> json_of_kapply "plusOp" []
  | `MinusOp -> json_of_kapply "minusOp" []

let json_of_binop (binop : Il.binop) : Yojson.Safe.t =
  match binop with
  | `AndOp -> json_of_kapply "andOp" []
  | `OrOp -> json_of_kapply "orOp" []
  | `ImplOp -> json_of_kapply "implOp" []
  | `EquivOp -> json_of_kapply "equivOp" []
  | `AddOp -> json_of_kapply "addOp" []
  | `SubOp -> json_of_kapply "subOp" []
  | `MulOp -> json_of_kapply "mulOp" []
  | `DivOp -> json_of_kapply "divOp" []
  | `ModOp -> json_of_kapply "modOp" []
  | `PowOp -> json_of_kapply "powOp" []

let json_of_cmpop (cmpop : Il.cmpop) : Yojson.Safe.t =
  match cmpop with
  | `EqOp -> json_of_kapply "eqOp" []
  | `NeOp -> json_of_kapply "neOp" []
  | `LtOp -> json_of_kapply "ltOp" []
  | `LeOp -> json_of_kapply "leOp" []
  | `GtOp -> json_of_kapply "gtOp" []
  | `GeOp -> json_of_kapply "geOp" []

(* Patterns *)

let json_of_pattern (pattern : Il.pattern) : Yojson.Safe.t =
  match pattern with
  | CaseP mixop -> json_of_kapply "injPat" [ json_of_mixop mixop ]
  | ListP `Cons -> json_of_kapply "consPat" []
  | ListP (`Fixed n) ->
      json_of_kapply "fixedPat" [ json_of_int (Bigint.of_int n) ]
  | ListP `Nil -> json_of_kapply "nilPat" []
  | OptP `Some -> json_of_kapply "somePat" []
  | OptP `None -> json_of_kapply "nonePat" []

(* Expressions *)

let rec json_of_exp (exp : Il.exp) : Yojson.Safe.t =
  match exp.it with
  | BoolE b -> json_of_kapply "boolE" [ json_of_bool b ]
  | NumE num -> json_of_num num
  | TextE t -> json_of_kapply "textE" [ json_of_string t ]
  | VarE id -> json_of_kapply "varE" [ json_of_id id ]
  | UnE (unop, _, e) ->
      json_of_kapply "unE" [ json_of_unop unop; json_of_exp e ]
  | BinE (binop, _, el, er) ->
      json_of_kapply "binE"
        [ json_of_binop binop; json_of_exp el; json_of_exp er ]
  | CmpE (cmpop, _, el, er) ->
      json_of_kapply "cmpE"
        [ json_of_cmpop cmpop; json_of_exp el; json_of_exp er ]
  | UpCastE (typ, e) ->
      json_of_kapply "upCastE" [ json_of_typ typ; json_of_exp e ]
  | DownCastE (typ, e) ->
      json_of_kapply "downCastE" [ json_of_typ typ; json_of_exp e ]
  | SubE (e, typ) -> json_of_kapply "subE" [ json_of_exp e; json_of_typ typ ]
  | MatchE (e, pattern) ->
      json_of_kapply "matchE" [ json_of_exp e; json_of_pattern pattern ]
  | TupleE exps -> json_of_kapply "tupE" [ json_of_exps exps ]
  | CaseE notexp -> json_of_kapply "injE" [ json_of_expcase notexp ]
  | StrE expfields ->
      json_of_kapply "strE"
        [
          json_of_klist "expFieldList" ".expFieldList"
            (List.map json_of_expfield expfields);
        ]
  | OptE exp_opt ->
      json_of_kapply "optE"
        [ json_of_kopt "noExp" "someExp" (Option.map json_of_exp exp_opt) ]
  | ListE exps -> json_of_kapply "listE" [ json_of_exps exps ]
  | ConsE (eh, et) ->
      json_of_kapply "consE" [ json_of_exp eh; json_of_exp et ]
  | CatE (el, er) -> json_of_kapply "catE" [ json_of_exp el; json_of_exp er ]
  | MemE (ee, es) -> json_of_kapply "memE" [ json_of_exp ee; json_of_exp es ]
  | LenE e -> json_of_kapply "lenE" [ json_of_exp e ]
  | DotE (e, atom) ->
      json_of_kapply "dotE" [ json_of_exp e; json_of_atom atom ]
  | IdxE (eb, ei) -> json_of_kapply "idxE" [ json_of_exp eb; json_of_exp ei ]
  | SliceE (eb, ei, en) ->
      json_of_kapply "sliceE"
        [ json_of_exp eb; json_of_exp ei; json_of_exp en ]
  | UpdE (eb, path, en) ->
      json_of_kapply "updE"
        [ json_of_exp eb; json_of_path path; json_of_exp en ]
  | CallE (id, targs, args) ->
      json_of_kapply "callE"
        [ json_of_id id; json_of_typs targs; json_of_args args ]
  | IterE (e, iterexp) ->
      json_of_kapply "iterE" [ json_of_exp e; json_of_iterexp iterexp ]

and json_of_exps (exps : Il.exp list) : Yojson.Safe.t =
  json_of_klist "expList" ".expList" (List.map json_of_exp exps)

and json_of_expfield ((atom, exp) : Il.atom * Il.exp) : Yojson.Safe.t =
  json_of_kapply "expField" [ json_of_atom atom; json_of_exp exp ]

and json_of_expcase (notexp : Il.notexp) : Yojson.Safe.t =
  let mixop, exps = Mixfix.split notexp in
  json_of_kapply "expCase" [ json_of_mixop mixop; json_of_exps exps ]

and json_of_iterexp ((iter, vars) : Il.iterexp) : Yojson.Safe.t =
  json_of_kapply "iterExp" [ json_of_iter iter; json_of_varis vars ]

(* Paths *)

and json_of_path (path : Il.path) : Yojson.Safe.t =
  match path.it with
  | RootP -> json_of_kapply "rootPath" []
  | IdxP (path, exp) ->
      json_of_kapply "idxPath" [ json_of_path path; json_of_exp exp ]
  | SliceP (path, exp_i, exp_n) ->
      json_of_kapply "slicePath"
        [ json_of_path path; json_of_exp exp_i; json_of_exp exp_n ]
  | DotP (path, atom) ->
      json_of_kapply "dotPath" [ json_of_path path; json_of_atom atom ]

(* Arguments *)

and json_of_arg (arg : Il.arg) : Yojson.Safe.t =
  match arg.it with
  | ExpA e -> json_of_kapply "expA" [ json_of_exp e ]
  | DefA id -> json_of_kapply "funA" [ json_of_id id ]

and json_of_args (args : Il.arg list) : Yojson.Safe.t =
  json_of_klist "argList" ".argList" (List.map json_of_arg args)

(* Parameters *)

let rec json_of_param (param : Il.param) : Yojson.Safe.t =
  match param.it with
  | ExpP typ -> json_of_kapply "expParam" [ json_of_typ typ ]
  | DefP (id, tparams, params, typ) ->
      json_of_kapply "funParam"
        [
          json_of_id id;
          json_of_tparams tparams;
          json_of_params params;
          json_of_typ typ;
        ]

and json_of_params (params : Il.param list) : Yojson.Safe.t =
  json_of_klist "paramList" ".paramList" (List.map json_of_param params)

(* Premises *)

let rec json_of_prem (prem : Il.prem) : Yojson.Safe.t =
  match prem.it with
  | RulePr (id, notexp, input) ->
      let exps = Mixfix.args notexp in
      let exps_in, exps_out = Lang.Hints.Input.split input exps in
      json_of_kapply "relPr"
        [ json_of_id id; json_of_exps exps_in; json_of_exps exps_out ]
  | IfPr e -> json_of_kapply "ifPr" [ json_of_exp e ]
  | IfHoldPr (id, notexp) ->
      json_of_kapply "ifHoldPr"
        [ json_of_id id; json_of_exps (Mixfix.args notexp) ]
  | IfNotHoldPr (id, notexp) ->
      json_of_kapply "ifNotHoldPr"
        [ json_of_id id; json_of_exps (Mixfix.args notexp) ]
  | LetPr (el, er) -> json_of_kapply "letPr" [ json_of_exp el; json_of_exp er ]
  | IterPr (p, ip) ->
      json_of_kapply "iterPr" [ json_of_prem p; json_of_iterprem ip ]
  | DebugPr e -> json_of_kapply "debugPr" [ json_of_exp e ]

and json_of_prems (prems : Il.prem list) : Yojson.Safe.t =
  json_of_klist "premList" ".premList" (List.map json_of_prem prems)

and json_of_iterprem ((iter, vars_in, vars_out) : Il.iterprem) : Yojson.Safe.t =
  json_of_kapply "iterPrem"
    [ json_of_iter iter; json_of_varis vars_in; json_of_varis vars_out ]

(* Rule matching and paths *)

let json_of_rulmatch ((_, exps_input, prems) : Al.rulematch) : Yojson.Safe.t =
  json_of_kapply "rulMatch" [ json_of_exps exps_input; json_of_prems prems ]

let json_of_rulpath ((id, prems, exps_output) : Al.rulepath) : Yojson.Safe.t =
  json_of_kapply "rulPath"
    [ json_of_id id; json_of_exps exps_output; json_of_prems prems ]

let json_of_rulgroup (rulegroup : Al.rulegroup) : Yojson.Safe.t =
  let id, rulmatch_, rulpaths = rulegroup.it in
  json_of_kapply "rulGroup"
    [
      json_of_id id;
      json_of_rulmatch rulmatch_;
      json_of_klist "rulPathList" ".rulPathList"
        (List.map json_of_rulpath rulpaths);
    ]

let json_of_elsgroup (elsegroup : Al.elsegroup) : Yojson.Safe.t =
  let id, rulmatch_, rulpath_ = elsegroup.it in
  json_of_kapply "elsGroup"
    [ json_of_id id; json_of_rulmatch rulmatch_; json_of_rulpath rulpath_ ]

(* Clauses and table rows.

   `clause` and `tblrow` share a notation in watsup but are distinct sorts, so
   they take distinct K constructors.  A table row's guard expressions are
   dropped, exactly as `boot_tablerow` drops them. *)

let json_of_clause (clause : Il.clause) : Yojson.Safe.t =
  let args, exp, prems = clause.it in
  json_of_kapply "clause"
    [ json_of_args args; json_of_exp exp; json_of_prems prems ]

let json_of_tblrow (tablerow : Al.tablerow) : Yojson.Safe.t =
  let _exps, args, exp, prems = tablerow.it in
  json_of_kapply "tblRow"
    [ json_of_args args; json_of_exp exp; json_of_prems prems ]

(* Definitions.

   `VarD` has no counterpart in the script syntax -- a `var` declaration is
   elaboration-time only -- so it contributes no definition, as in `boot_def`. *)

let json_of_def (def : Al.def) : Yojson.Safe.t option =
  match def.it with
  | ExternTypD (id, _) ->
      Some (json_of_kapply "extTypD" [ json_of_id id ])
  | TypD (id, tparams, deftyp, _) ->
      Some
        (json_of_kapply "typD"
           [ json_of_id id; json_of_tparams tparams; json_of_deftyp deftyp ])
  | VarD _ -> None
  | ExternRelD (id, nottyp, input, _) ->
      let typs = Mixfix.args nottyp.it in
      let typs_in, typs_out = Lang.Hints.Input.split input typs in
      Some
        (json_of_kapply "extRelD"
           [ json_of_id id; json_of_typs typs_in; json_of_typs typs_out ])
  | RelD (id, nottyp, input, rulgroups, elsegroup_opt, _) ->
      let typs = Mixfix.args nottyp.it in
      let typs_in, typs_out = Lang.Hints.Input.split input typs in
      Some
        (json_of_kapply "relD"
           [
             json_of_id id;
             json_of_typs typs_in;
             json_of_typs typs_out;
             json_of_klist "rulGroupList" ".rulGroupList"
               (List.map json_of_rulgroup rulgroups);
             json_of_kopt "noElsGroup" "someElsGroup"
               (Option.map json_of_elsgroup elsegroup_opt);
           ])
  | ExternDecD (id, tparams, params, typ, _) ->
      Some
        (json_of_kapply "extFuncD"
           [
             json_of_id id;
             json_of_tparams tparams;
             json_of_params params;
             json_of_typ typ;
           ])
  | BuiltinDecD (id, tparams, params, typ, _) ->
      Some
        (json_of_kapply "builtinFuncD"
           [
             json_of_id id;
             json_of_tparams tparams;
             json_of_params params;
             json_of_typ typ;
           ])
  | TableDecD (id, params, typ, tablerows, _) ->
      Some
        (json_of_kapply "tableFuncD"
           [
             json_of_id id;
             json_of_params params;
             json_of_typ typ;
             json_of_klist "tblRowList" ".tblRowList"
               (List.map json_of_tblrow tablerows);
           ])
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, _) ->
      Some
        (json_of_kapply "funcD"
           [
             json_of_id id;
             json_of_tparams tparams;
             json_of_params params;
             json_of_typ typ;
             json_of_klist "clauseList" ".clauseList"
               (List.map json_of_clause clauses);
             json_of_kopt "noElsClause" "someElsClause"
               (Option.map json_of_clause elseclause_opt);
           ])

(* Specification *)

let json_of_spec_al (spec : Al.spec) : Yojson.Safe.t =
  `Assoc
    [
      ("format", `String "KAST");
      ("version", `Int version);
      ( "term",
        json_of_klist "script" ".script"
          (List.filter_map json_of_def spec) );
    ]

let string_of_spec_al (spec : Al.spec) : string =
  spec |> json_of_spec_al |> Yojson.Safe.pretty_to_string

(* Values of an arbitrary spec, as the K sort `Val`.

   Everything above emits the *meta-language* script syntax, whose K sorts are
   those of `spec-meta-k`.  A target-level value cannot go through it: a P4
   program inhabits sorts (`p4program`, `declarationList`, ...) that the
   meta-language syntax knows nothing about.

   K's `Val` (`spec-meta-k/common/1-syntax.k`) is structural, though: `injV`
   carries its mixop as data rather than being resolved to a named constructor,
   so any value of any spec can be written down.  `json_of_value` above is
   exactly that structural walk, and it is what lets a P4 program reach the K
   definition: it is parsed by the OCaml P4 parser (`Interface.P4.parse_program`,
   which builds a `Value.t` directly), emitted here, and bound to the `<p4prog>`
   cell.

   Wrapped as `someP4(val)`, the inhabited case of the `<p4prog>` cell's sort.
   The wrapper is applied here rather than by the shell that drives this, because
   splicing one KORE term into another textually is not something `kast` offers:
   it parses a whole term of one sort.  Emitting the wrapper as part of the JSON
   means the term arrives at the right sort in one parse. *)

let json_of_p4_term (value : Value.t) : Yojson.Safe.t =
  `Assoc
    [
      ("format", `String "KAST");
      ("version", `Int version);
      ("term", json_of_kapply "someP4" [ json_of_value value ]);
    ]

let string_of_value (value : Value.t) : string =
  value |> json_of_p4_term |> Yojson.Safe.pretty_to_string
