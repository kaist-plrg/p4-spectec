module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Value = Runtime.Value
open Common.Mixops
open Mixops
open Util.Source

(* Emitting a booted `Value.t` as KAST JSON, for the K specification in
   `spec-meta-k/`.

   The K definition is an *abstract* syntax: every watsup production is a
   constructor whose KORE label is pinned with `symbol(_)`.  Emitting a value
   therefore amounts to naming, for each `CaseV`, the K constructor that
   corresponds to its mixop, and rebuilding `ListV` / `OptV` as the cons lists
   and option constructors that `spec-meta-k` declares.

   The mixop side of that mapping is taken from `Common.Mixops` and `Mixops`,
   the very tables `boot.ml` builds values with, so the two cannot drift: a
   mixop that is renamed there stops matching here and the emitter reports an
   unknown constructor instead of emitting a wrong label.

   Mixops alone do not identify a constructor, since the same notation is
   reused across sorts (`BOOL bool` is both a value and an expression;
   `clause` and `tblrow` share a notation entirely).  Every `CaseV` built by
   `boot.ml` is noted with the watsup sort it inhabits, so the table is keyed
   by the pair (sort, mixop). *)

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

(* Constructor table.

   Keyed by the watsup sort a value is noted with and the notation of its
   mixop; the payload is the `symbol(_)` of the corresponding `spec-meta-k`
   production.  `Mixop.string_of_mixop` is the canonical rendering of a mixop,
   so keying on it compares notations rather than atom phrases. *)

(* Watsup aliases are inlined in K, since K has no sort aliases: `targ = typ`
   and `elsclause = clause`.  The booter notes a value with whichever of the
   two names the position calls for (`boot_targ` re-notes a type as `targ`),
   so the alias is resolved before the constructor is looked up. *)

let unalias (sort : string) : string =
  match sort with "targ" -> "typ" | "elsclause" -> "clause" | _ -> sort

let key (sort : string) (mixop : Mixop.t) : string * string =
  (unalias sort, Mixop.string_of_mixop mixop)

let table : (string * string, string) Hashtbl.t = Hashtbl.create 128

let register (sort : string) (mixop : Mixop.t) (symbol : string) : unit =
  Hashtbl.replace table (key sort mixop) symbol

let () =
  (* Iterators: `common/1-syntax.k` *)
  register "iter" mop_quest "quest";
  register "iter" mop_star "star";
  (* Variables *)
  register "vari" mop_vari "vari";
  (* Types.  `optyp` and `numtyp` are subsorts of `Typ` in K, exactly as they
     are cases of `typ` in watsup, so their constructors are reachable under
     `typ` as well: `boot_targ` re-notes a type as `targ` whatever the
     narrower sort `boot_typ` gave it, and `targ` unaliases to `typ`. *)
  register "optyp" mop_bool_typ "boolT";
  register "optyp" mop_text_typ "textT";
  register "numtyp" mop_num_typ_nat "natT";
  register "numtyp" mop_num_typ_int "intT";
  register "typ" mop_bool_typ "boolT";
  register "typ" mop_text_typ "textT";
  register "typ" mop_num_typ_nat "natT";
  register "typ" mop_num_typ_int "intT";
  register "typ" mop_var_typ "varT";
  register "typ" mop_tuple_typ "tupT";
  register "typ" mop_iter_typ "iterT";
  register "typ" mop_func_typ "funcT";
  (* Defined types *)
  register "deftyp" mop_plain_deftyp "aliasDT";
  register "deftyp" mop_struct_deftyp "structDT";
  register "deftyp" mop_variant_deftyp "variantDT";
  register "typfield" mop_typfield "typField";
  register "typcase" mop_typcase "typCase";
  (* Numbers.  `num` is a case of both `val` and `exp` in watsup and a subsort
     of both `Val` and `Exp` in K, so a number keeps its own constructor
     whichever of the three sorts the booter noted it with: `boot_num_value`
     notes `num`, and `boot_num_exp` re-notes the same value as `exp`. *)
  register "num" mop_num_value_nat "natN";
  register "num" mop_num_value_int "intN";
  register "val" mop_num_value_nat "natN";
  register "val" mop_num_value_int "intN";
  register "exp" mop_num_value_nat "natN";
  register "exp" mop_num_value_int "intN";
  (* Values *)
  register "val" mop_bool_value "boolV";
  register "val" mop_text_value "textV";
  register "val" mop_struct_value "strV";
  register "val" mop_case_value "injV";
  register "val" mop_tuple_value "tupV";
  register "val" mop_opt_value "optV";
  register "val" mop_list_value "listV";
  register "val" mop_func_value "funcV";
  register "val" mop_extern_value "extV";
  register "valfield" mop_valuefield "valField";
  register "valcase" mop_valuecase "valCase";
  (* Unary, binary and comparison operators *)
  register "boolunop" mop_not_unop "notOp";
  register "numunop" mop_plus_unop "plusOp";
  register "numunop" mop_minus_unop "minusOp";
  register "boolbinop" mop_and_binop "andOp";
  register "boolbinop" mop_or_binop "orOp";
  register "boolbinop" mop_impl_binop "implOp";
  register "boolbinop" mop_equiv_binop "equivOp";
  register "numbinop" mop_add_binop "addOp";
  register "numbinop" mop_sub_binop "subOp";
  register "numbinop" mop_mul_binop "mulOp";
  register "numbinop" mop_div_binop "divOp";
  register "numbinop" mop_mod_binop "modOp";
  register "numbinop" mop_pow_binop "powOp";
  register "polycmpop" mop_eq_cmpop "eqOp";
  register "polycmpop" mop_ne_cmpop "neOp";
  register "numcmpop" mop_lt_cmpop "ltOp";
  register "numcmpop" mop_le_cmpop "leOp";
  register "numcmpop" mop_gt_cmpop "gtOp";
  register "numcmpop" mop_ge_cmpop "geOp";
  (* Arguments *)
  register "arg" mop_exp_arg "expA";
  register "arg" mop_def_arg "funA";
  (* Expressions *)
  register "exp" mop_bool_exp "boolE";
  register "exp" mop_text_exp "textE";
  register "exp" mop_var_exp "varE";
  register "exp" mop_un_exp "unE";
  register "exp" mop_bin_exp "binE";
  register "exp" mop_cmp_exp "cmpE";
  register "exp" mop_upcast_exp "upCastE";
  register "exp" mop_downcast_exp "downCastE";
  register "exp" mop_sub_exp "subE";
  register "exp" mop_match_exp "matchE";
  register "exp" mop_tuple_exp "tupE";
  register "exp" mop_case_exp "injE";
  register "exp" mop_struct_exp "strE";
  register "exp" mop_opt_exp "optE";
  register "exp" mop_list_exp "listE";
  register "exp" mop_cons_exp "consE";
  register "exp" mop_cat_exp "catE";
  register "exp" mop_mem_exp "memE";
  register "exp" mop_len_exp "lenE";
  register "exp" mop_dot_exp "dotE";
  register "exp" mop_idx_exp "idxE";
  register "exp" mop_slice_exp "sliceE";
  register "exp" mop_upd_exp "updE";
  register "exp" mop_call_exp "callE";
  register "exp" mop_iter_exp "iterE";
  register "expcase" mop_expcase "expCase";
  register "expfield" mop_expfield "expField";
  register "iterexp" mop_iterexp "iterExp";
  (* Paths *)
  register "path" mop_root_path "rootPath";
  register "path" mop_idx_path "idxPath";
  register "path" mop_slice_path "slicePath";
  register "path" mop_dot_path "dotPath";
  (* Patterns *)
  register "pattern" mop_case_pattern "injPat";
  register "listpattern" mop_list_cons_pattern "consPat";
  register "listpattern" mop_list_fixed_pattern "fixedPat";
  register "listpattern" mop_list_nil_pattern "nilPat";
  register "optpattern" mop_opt_some_pattern "somePat";
  register "optpattern" mop_opt_none_pattern "nonePat";
  (* Parameters: `al/1-syntax.k` *)
  register "param" mop_exp_param "expParam";
  register "param" mop_def_param "funParam";
  (* Premises *)
  register "iterprem" mop_iterprem "iterPrem";
  register "prem" mop_rel_prem "relPr";
  register "prem" mop_if_prem "ifPr";
  register "prem" mop_if_hold_prem "ifHoldPr";
  register "prem" mop_if_nothold_prem "ifNotHoldPr";
  register "prem" mop_let_prem "letPr";
  register "prem" mop_iter_prem "iterPr";
  register "prem" mop_debug_prem "debugPr";
  (* Rules, clauses and table rows.  `clause` and `tblrow` share a notation,
     which is why the sort is part of the key. *)
  register "rulmatch" mop_rulematch "rulMatch";
  register "rulpath" mop_rulepath "rulPath";
  register "rulgroup" mop_rulegroup "rulGroup";
  register "elsgroup" mop_elsegroup "elsGroup";
  register "clause" mop_clause "clause";
  register "tblrow" mop_tablerow "tblRow";
  (* Definitions *)
  register "defn" mop_extern_typ_def "extTypD";
  register "defn" mop_typ_def "typD";
  register "defn" mop_extern_rel_def "extRelD";
  register "defn" mop_rel_def "relD";
  register "defn" mop_extern_func_def "extFuncD";
  register "defn" mop_builtin_func_def "builtinFuncD";
  register "defn" mop_table_func_def "tableFuncD";
  register "defn" mop_func_def "funcD"

(* List sorts.

   A `ListV` carries `IterT (typ_elem, List)` as its note, except for the
   mixop of a case, whose note is the sort `mixop` itself.  Both the cons
   label and the terminator of the corresponding K list are looked up by the
   element sort. *)

let lists : (string, string * string) Hashtbl.t = Hashtbl.create 32

let register_list (sort_elem : string) (symbol : string)
    (symbol_terminator : string) : unit =
  Hashtbl.replace lists sort_elem (symbol, symbol_terminator)

let () =
  register_list "atom" "atomList" ".atomList";
  register_list "atomList" "mixop" ".mixop";
  register_list "typ" "typList" ".typList";
  (* `targ` is an alias of `typ`, and `tparam` of `id`; K inlines both. *)
  register_list "targ" "typList" ".typList";
  register_list "tparam" "tparamList" ".tparamList";
  register_list "typfield" "typFieldList" ".typFieldList";
  register_list "typcase" "typCaseList" ".typCaseList";
  register_list "iter" "iterList" ".iterList";
  register_list "vari" "variList" ".variList";
  register_list "val" "valList" ".valList";
  register_list "valfield" "valFieldList" ".valFieldList";
  register_list "exp" "expList" ".expList";
  register_list "expfield" "expFieldList" ".expFieldList";
  register_list "arg" "argList" ".argList";
  register_list "param" "paramList" ".paramList";
  register_list "prem" "premList" ".premList";
  register_list "rulpath" "rulPathList" ".rulPathList";
  register_list "rulgroup" "rulGroupList" ".rulGroupList";
  register_list "clause" "clauseList" ".clauseList";
  register_list "tblrow" "tblRowList" ".tblRowList";
  register_list "defn" "script" ".script"

(* Option sorts.

   Watsup's `X?` becomes a two-constructor sort in K, since K has no generic
   option.  Keyed, like lists, by the sort of the element. *)

let opts : (string, string * string) Hashtbl.t = Hashtbl.create 16

let register_opt (sort_elem : string) (symbol_none : string)
    (symbol_some : string) : unit =
  Hashtbl.replace opts sort_elem (symbol_none, symbol_some)

let () =
  register_opt "val" "noVal" "someVal";
  register_opt "exp" "noExp" "someExp";
  register_opt "elsgroup" "noElsGroup" "someElsGroup";
  (* `elsclause` is an alias of `clause`; `boot_elsclause_opt` notes the option
     with the alias, and K inlines it. *)
  register_opt "elsclause" "noElsClause" "someElsClause"

(* Errors *)

exception Error of string

let error (fmt : ('a, Format.formatter, unit, 'b) format4) : 'a =
  Format.kasprintf (fun msg -> raise (Error msg)) fmt

(* The sort a value is noted with.

   Every `CaseV`, list and option built by `boot.ml` is noted with the watsup
   sort it inhabits; anything else is a booter change this emitter has not
   been taught about. *)

let sort_of_typ (typ : Il.typ') : string =
  match typ with
  | Il.VarT (id, []) -> id.it
  | _ -> error "not a sort: %s" (Il.Print.string_of_typ (typ $ no_region))

let sort_of_elem (typ : Il.typ') : string =
  match typ with
  | Il.IterT (typ, Il.List) -> sort_of_typ typ.it
  | Il.IterT (typ, Il.Opt) -> sort_of_typ typ.it
  (* Watsup aliases of an iterated type are noted with the alias, not with the
     type it abbreviates: `mixop = atom**` and `script = defn*`. *)
  | Il.VarT (id, []) when id.it = "mixop" -> "atomList"
  | Il.VarT (id, []) when id.it = "script" -> "defn"
  | _ ->
      error "not an iterated sort: %s"
        (Il.Print.string_of_typ (typ $ no_region))

(* Values *)

let rec json_of_value (value : Value.t) : Yojson.Safe.t =
  match value.it with
  | Il.BoolV b -> json_of_bool b
  | Il.NumV (`Nat n) -> json_of_int n
  | Il.NumV (`Int i) -> json_of_int i
  (* `id` and `atom` are subsorts of `String` in K, so an injection is not
     written out: `kast` infers `String -> Id` and `String -> Atom`. *)
  | Il.TextV s -> json_of_string s
  | Il.CaseV valuecase -> json_of_valuecase value.note.typ valuecase
  | Il.ListV values -> json_of_list value.note.typ values
  | Il.OptV value_opt -> json_of_opt value.note.typ value_opt
  | Il.StructV _ ->
      error "struct value: the meta-language script syntax has no struct"
  | Il.TupleV _ ->
      error "tuple value: the meta-language script syntax has no tuple"
  | Il.FuncV _ -> error "function value in a booted script"
  | Il.ExternV _ -> error "extern value in a booted script"

and json_of_valuecase (typ : Il.typ') (valuecase : Il.valuecase) : Yojson.Safe.t
    =
  let sort = sort_of_typ typ in
  let mixop, values = Mixfix.split valuecase in
  match Hashtbl.find_opt table (key sort mixop) with
  | Some symbol -> json_of_kapply symbol (List.map json_of_value values)
  | None ->
      error "no K constructor for %s of sort %s"
        (Mixop.string_of_mixop mixop)
        sort

and json_of_list (typ : Il.typ') (values : Value.t list) : Yojson.Safe.t =
  let sort_elem = sort_of_elem typ in
  match Hashtbl.find_opt lists sort_elem with
  | Some (symbol, symbol_terminator) ->
      List.fold_right
        (fun value json_tail ->
          json_of_kapply symbol [ json_of_value value; json_tail ])
        values
        (json_of_kapply symbol_terminator [])
  | None -> error "no K list for element sort %s" sort_elem

and json_of_opt (typ : Il.typ') (value_opt : Value.t option) : Yojson.Safe.t =
  let sort_elem = sort_of_elem typ in
  match Hashtbl.find_opt opts sort_elem with
  | Some (symbol_none, symbol_some) -> (
      match value_opt with
      | None -> json_of_kapply symbol_none []
      | Some value -> json_of_kapply symbol_some [ json_of_value value ])
  | None -> error "no K option for element sort %s" sort_elem

(* Scripts *)

let json_of_script (value_script : Value.t) : Yojson.Safe.t =
  `Assoc
    [
      ("format", `String "KAST");
      ("version", `Int version);
      ("term", json_of_value value_script);
    ]

let string_of_script (value_script : Value.t) : string =
  value_script |> json_of_script |> Yojson.Safe.pretty_to_string

(* Values of an arbitrary spec, as the K sort `Val`.

   Everything above emits the *meta-language* script syntax: `json_of_value`
   dispatches on the watsup sort a value is noted with, and looks the
   constructor up in `table`.  That is exactly what a target-level value cannot
   go through — a P4 program inhabits sorts (`p4program`, `declarationList`, …)
   that the table knows nothing about, and `json_of_value` rejects `StructV` and
   `TupleV` outright since the script syntax has neither.

   K's `Val` (`spec-meta-k/common/1-syntax.k`) is structural, though: `injV`
   carries its mixop as data rather than being resolved to a named constructor,
   so any value of any spec can be written down without a table.  This second
   encoder therefore never consults `value.note.typ` and never touches `table`;
   it is a plain structural walk.

   That is what lets a P4 program reach the K definition: it is parsed by the
   OCaml P4 parser (`Interface.P4.parse_program`, which builds a `Value.t`
   directly), emitted here as a `Val`, and bound to the `<p4prog>` cell. *)

let rec json_of_val (value : Value.t) : Yojson.Safe.t =
  match value.it with
  | Il.BoolV b -> json_of_kapply "boolV" [ json_of_bool b ]
  | Il.NumV (`Nat n) -> json_of_kapply "natN" [ json_of_int n ]
  | Il.NumV (`Int i) -> json_of_kapply "intN" [ json_of_int i ]
  | Il.TextV s -> json_of_kapply "textV" [ json_of_string s ]
  | Il.StructV valuefields ->
      json_of_kapply "strV" [ json_of_valfields valuefields ]
  | Il.CaseV valuecase ->
      let mixop, values = Mixfix.split valuecase in
      json_of_kapply "injV"
        [
          json_of_kapply "valCase"
            [ json_of_mixop mixop; json_of_vals values ];
        ]
  | Il.TupleV values -> json_of_kapply "tupV" [ json_of_vals values ]
  | Il.OptV None -> json_of_kapply "optV" [ json_of_kapply "noVal" [] ]
  | Il.OptV (Some value) ->
      json_of_kapply "optV"
        [ json_of_kapply "someVal" [ json_of_val value ] ]
  | Il.ListV values -> json_of_kapply "listV" [ json_of_vals values ]
  (* Neither is reachable from the P4 parser, which builds only cases and
     text/number leaves.  `extV` would need K's abstract `Json` sort, which has
     no K-reachable inhabitants, so an extern value cannot be written down as a
     term at all. *)
  | Il.FuncV _ -> error "function value cannot be emitted as a K Val"
  | Il.ExternV _ -> error "extern value cannot be emitted as a K Val"

and json_of_vals (values : Value.t list) : Yojson.Safe.t =
  List.fold_right
    (fun value json_tail ->
      json_of_kapply "valList" [ json_of_val value; json_tail ])
    values
    (json_of_kapply ".valList" [])

and json_of_valfields (valuefields : (Il.atom * Value.t) list) : Yojson.Safe.t =
  List.fold_right
    (fun (atom_field, value_field) json_tail ->
      let json_field =
        json_of_kapply "valField"
          [
            json_of_string (Atom.string_of_atom atom_field.it);
            json_of_val value_field;
          ]
      in
      json_of_kapply "valFieldList" [ json_field; json_tail ])
    valuefields
    (json_of_kapply ".valFieldList" [])

(* A mixop is an atoms matrix: one row of atoms per notation position, exactly
   as `spec-meta-k/al/4-extern-json.k` puts it on the extern wire.  Operator
   atoms keep their quotes inside the string (`':'`), matching `boot_atom`. *)

and json_of_mixop (mixop : Mixop.t) : Yojson.Safe.t =
  List.fold_right
    (fun atoms json_tail ->
      json_of_kapply "mixop" [ json_of_atoms atoms; json_tail ])
    (Mixop.atoms_matrix mixop)
    (json_of_kapply ".mixop" [])

and json_of_atoms (atoms : Il.atom list) : Yojson.Safe.t =
  List.fold_right
    (fun (atom : Il.atom) json_tail ->
      json_of_kapply "atomList"
        [ json_of_string (Atom.string_of_atom atom.it); json_tail ])
    atoms
    (json_of_kapply ".atomList" [])

(* Wrapped as `someP4(val)`, the inhabited case of the `<p4prog>` cell's sort.

   The wrapper is applied here rather than by the shell that drives this,
   because splicing one KORE term into another textually is not something
   `kast` offers: it parses a whole term of one sort.  Emitting the wrapper as
   part of the JSON means the term arrives at the right sort in one parse. *)

let json_of_p4_term (value : Value.t) : Yojson.Safe.t =
  `Assoc
    [
      ("format", `String "KAST");
      ("version", `Int version);
      ("term", json_of_kapply "someP4" [ json_of_val value ]);
    ]

let string_of_value (value : Value.t) : string =
  value |> json_of_p4_term |> Yojson.Safe.pretty_to_string
