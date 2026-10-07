(* Loading KORE into values of the K spec (spec-k/1-syntax.watsup).

   Only syntactic clean-up happens here, matching
   what the LLVM backend's matching/Parser.scala does before compiling rules:
   - drop axioms that are not executed (logical properties, simplifications)
     and equations of hooked symbols;
   - split rewrite rules and equations into left-hand side, requires, and
     right-hand side, dropping the negated condition kompile adds to owise;
   - read domain values of hooked sorts (INT.Int, BOOL.Bool, STRING.String,
     FLOAT.Float in canonical form, BYTES.Bytes);
   - expand \left-assoc and \right-assoc. *)

open Ast
module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Atom = Domain.Atom
open Util.Source

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* Information about a definition used while loading and unparsing *)

type symbol = {
  params : string list;
  args : sort list;
  result : sort;
  hook : string option;
  is_function : bool;
  is_binder : bool;
  is_anywhere : bool;
}

type info = {
  sort_hooks : (string, string) Hashtbl.t; (* sort -> hook of a hooked sort *)
  sort_attrs : (string, attr list) Hashtbl.t;
  symbols : (string, symbol) Hashtbl.t;
}

let info_of_definition (defn : definition) : info =
  let info =
    {
      sort_hooks = Hashtbl.create 64;
      sort_attrs = Hashtbl.create 64;
      symbols = Hashtbl.create 512;
    }
  in
  List.iter
    (fun (m : module_) ->
      List.iter
        (function
          | SortDecl { name; attrs; _ } ->
              Hashtbl.replace info.sort_attrs name attrs;
              Option.iter
                (Hashtbl.replace info.sort_hooks name)
                (string_attr "hook" attrs)
          | SymbolDecl { name; params; args; result; attrs; _ } ->
              Hashtbl.replace info.symbols name
                {
                  params;
                  args;
                  result;
                  hook = string_attr "hook" attrs;
                  is_function = has_attr "function" attrs;
                  is_binder = has_attr "binder" attrs;
                  is_anywhere = has_attr "anywhere" attrs;
                }
          | _ -> ())
        m.sentences)
    defn.modules;
  info

let sort_hook (info : info) (s : sort) : string option =
  match s with
  | Sort (name, _) -> Hashtbl.find_opt info.sort_hooks name
  | SortVar _ -> None

(* Value construction *)

let typ (name : string) = Typ.Make.var (name $ no_region) []
let typ_sort = typ "sort"
let typ_pattern = typ "pattern"
let typ_attr = typ "attr"
let text_as (name : string) (s : string) = Value.Make.((text s) #@@ name)

let case (mixop : string) (args : Value.t list) (name : string) =
  Value.Make.(mixop <| args <<| name)

let list (t : Typ.t) (vs : Value.t list) = Value.Make.list (Typ.Make.list t) vs
let opt (t : Typ.t) (v : Value.t option) = Value.Make.opt (Typ.Make.opt t) v

let rec value_of_sort (s : sort) : Value.t =
  match s with
  | Sort (name, sorts) ->
      case "SORT sortid sort*"
        [ text_as "sortid" name; list typ_sort (List.map value_of_sort sorts) ]
        "sort"
  | SortVar name -> case "SORTVAR sortid" [ text_as "sortid" name ] "sort"

(* Domain values of hooked sorts are read here; others stay text *)

let value_of_dv (info : info) (s : sort) (text : string) : Value.t =
  match sort_hook info s with
  | Some "INT.Int" ->
      let text =
        if String.length text > 0 && text.[0] = '+' then
          String.sub text 1 (String.length text - 1)
        else text
      in
      let i =
        try Bigint.of_string text
        with _ -> error "bad integer literal %S" text
      in
      case "INT int" [ Value.Make.int i ] "pattern"
  | Some "BOOL.Bool" -> (
      match text with
      | "true" -> case "BOOL bool" [ Value.Make.bool true ] "pattern"
      | "false" -> case "BOOL bool" [ Value.Make.bool false ] "pattern"
      | _ -> error "bad boolean literal %S" text)
  | Some "STRING.String" ->
      case "STRING text" [ Value.Make.text text ] "pattern"
  | Some "FLOAT.Float" ->
      let text =
        try Floats.normalize text
        with Floats.Bad_float _ -> error "bad float literal %S" text
      in
      case "FLOAT text" [ Value.Make.text text ] "pattern"
  | Some "BYTES.Bytes" ->
      let bytes =
        List.init (String.length text) (fun i ->
            Value.Make.nat (Bigint.of_int (Char.code text.[i])))
      in
      case "BYTES nat*"
        [ Value.Make.list (Typ.Make.list Typ.Make.nat) bytes ]
        "pattern"
  | Some "MINT.MInt" -> (
      (* <value>p<width>, the value taken modulo 2^width *)
      match String.rindex_opt text 'p' with
      | Some k -> (
          try
            let v = Z.of_string (String.sub text 0 k)
            and w =
              int_of_string
                (String.sub text (k + 1) (String.length text - k - 1))
            in
            let v = Z.erem v (Z.shift_left Z.one w) in
            case "MINT nat nat"
              [
                Value.Make.nat (Bigint.of_int w);
                Value.Make.nat (Bigint.of_zarith_bigint v);
              ]
              "pattern"
          with _ -> error "bad machine integer literal %S" text)
      | None -> error "bad machine integer literal %S" text)
  | Some "KVAR.KVar" ->
      case "DV sort text" [ value_of_sort s; Value.Make.text text ] "pattern"
  | Some hook when hook <> "" ->
      error "domain values of %s are not supported" hook
  | _ -> case "DV sort text" [ value_of_sort s; Value.Make.text text ] "pattern"

(* \left-assoc{}(f(a, b, c)) = f(f(a, b), c), \right-assoc{}(f(a, b, c)) = f(a, f(b, c)) *)
let rec desugar_assoc (p : pattern) : pattern =
  match p with
  | App ("\\left-assoc", [], [ App (f, sorts, a :: rest) ]) ->
      List.fold_left
        (fun acc x -> App (f, sorts, [ acc; desugar_assoc x ]))
        (desugar_assoc a) rest
  | App ("\\right-assoc", [], [ App (f, sorts, args) ]) -> (
      match List.rev args with
      | [] -> App (f, sorts, [])
      | last :: rev_init ->
          List.fold_left
            (fun acc x -> App (f, sorts, [ desugar_assoc x; acc ]))
            (desugar_assoc last) rev_init)
  | App (f, sorts, args) -> App (f, sorts, List.map desugar_assoc args)
  | p -> p

let rec value_of_pattern (info : info) (p : pattern) : Value.t =
  match p with
  | Var (name, s) ->
      case "VAR varid sort" [ text_as "varid" name; value_of_sort s ] "pattern"
  | SetVar (name, _) -> error "set variable %s is not supported" name
  | Str s -> error "unexpected string literal %S in a pattern" s
  | App ("\\dv", [ s ], [ Str text ]) -> value_of_dv info s text
  | App ("\\and", [ s ], [ p1; p2 ]) ->
      case "AND sort pattern pattern"
        [ value_of_sort s; value_of_pattern info p1; value_of_pattern info p2 ]
        "pattern"
  | App ("\\or", [ s ], p1 :: (_ :: _ as ps)) ->
      (* \or is binary; a longer one is read from the right *)
      let rest = match ps with [ p2 ] -> p2 | _ -> App ("\\or", [ s ], ps) in
      case "OR sort pattern pattern"
        [
          value_of_sort s; value_of_pattern info p1; value_of_pattern info rest;
        ]
        "pattern"
  | App (("\\left-assoc" | "\\right-assoc"), _, _) ->
      value_of_pattern info (desugar_assoc p)
  | App (name, _, _) when name.[0] = '\\' ->
      error "connective %s is not supported in a pattern" name
  | App (name, sorts, args) ->
      if not (Hashtbl.mem info.symbols name) then
        error "undeclared symbol %s" name;
      case "APP symid sort* pattern*"
        [
          text_as "symid" name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_pattern (List.map (value_of_pattern info) args);
        ]
        "pattern"

(* Attributes that affect execution *)

let value_of_attrs (attrs : attr list) : Value.t =
  let flag name mixop =
    if has_attr name attrs then [ case mixop [] "attr" ] else []
  in
  let priority =
    match string_attr "priority" attrs with
    | Some n ->
        [ case "PRIORITY nat" [ Value.Make.nat (Bigint.of_string n) ] "attr" ]
    | None -> []
  in
  list typ_attr
    (flag "owise" "OWISE" @ priority @ flag "cool" "COOL"
    @ flag "cool-like" "COOLLIKE")

(* Splitting axioms *)

let is_true = function
  | App ("\\dv", [ Sort ("SortBool", []) ], [ Str "true" ]) -> true
  | _ -> false

let rec is_predicate = function
  | App (("\\top" | "\\not" | "\\in" | "\\ceil"), _, _) -> true
  | App ("\\equals", _, _) -> true
  | App ("\\and", _, ps) -> List.for_all is_predicate ps
  | _ -> false

(* The requires clause of a conjunction of predicates. \in conditions are
   returned separately (function arguments); \not is the owise condition. *)
let rec split_predicate (p : pattern) : pattern list * (pattern * pattern) list
    =
  match p with
  | App ("\\top", _, []) -> ([], [])
  | App ("\\not", _, _) -> ([], [])
  | App ("\\equals", _, [ req; t ]) when is_true t -> ([ req ], [])
  | App ("\\in", _, [ x; pat ]) -> ([], [ (x, pat) ])
  | App ("\\and", _, ps) ->
      List.fold_left
        (fun (reqs, ins) q ->
          let reqs', ins' = split_predicate q in
          (reqs @ reqs', ins @ ins'))
        ([], []) ps
  | _ -> error "unsupported condition %s" (string_of_pattern p)

let one_requires = function
  | [] -> None
  | [ req ] -> Some req
  | _ -> error "more than one requires clause"

(* \and(pattern, predicate) in either order *)
let split_side (side : pattern) : pattern * pattern list =
  match side with
  | App ("\\and", _, [ a; b ]) when is_predicate b && not (is_predicate a) ->
      (a, fst (split_predicate b))
  | App ("\\and", _, [ a; b ]) when is_predicate a && not (is_predicate b) ->
      (b, fst (split_predicate a))
  | App ("\\and", _, [ App ("\\not", _, _); App ("\\and", _, [ a; b ]) ])
    when is_predicate a ->
      (b, fst (split_predicate a))
  | _ -> (side, [])

let check_ensures (ens : pattern list) =
  if ens <> [] && not (List.for_all is_true ens) then
    error "ensures clauses are not supported"

let opt_pattern info p = opt typ_pattern (Option.map (value_of_pattern info) p)

let value_of_rule (info : info) (lhs_side : pattern) (rhs_side : pattern)
    (attrs : attr list) : Value.t =
  let lhs, reqs = split_side lhs_side in
  let rhs, ens = split_side rhs_side in
  check_ensures ens;
  case "RULE pattern '=>' pattern 'requires' pattern? attr*"
    [
      value_of_pattern info lhs;
      value_of_pattern info rhs;
      opt_pattern info (one_requires reqs);
      value_of_attrs attrs;
    ]
    "krule"

let value_of_equation (info : info) (cond : pattern) (f : string)
    (xs : pattern list) (rhs_side : pattern) (attrs : attr list) : Value.t =
  let reqs, ins = split_predicate cond in
  let rhs, ens = split_side rhs_side in
  check_ensures ens;
  let arg x =
    match List.find_opt (fun (y, _) -> y = x) ins with
    | Some (_, pat) -> pat
    | None -> x
  in
  case "EQN symid pattern* '=' pattern 'requires' pattern? attr*"
    [
      text_as "symid" f;
      list typ_pattern (List.map (fun x -> value_of_pattern info (arg x)) xs);
      value_of_pattern info rhs;
      opt_pattern info (one_requires reqs);
      value_of_attrs attrs;
    ]
    "equation"

(* Axioms that the LLVM backend does not execute *)
let logical_attrs =
  [
    "constructor";
    "functional";
    "total";
    "assoc";
    "comm";
    "unit";
    "idem";
    "non-executable";
    "simplification";
    "ceil";
  ]

type loaded = {
  info : info;
  definition : Value.t;
  rules : int;
  equations : int;
}

let load_definition (defn : definition) : loaded =
  let info = info_of_definition defn in
  let rules = ref []
  and equations = ref []
  and subsorts = ref []
  and overloads = ref [] in
  let is_hooked f =
    match Hashtbl.find_opt info.symbols f with
    | Some { hook = Some _; _ } -> true
    | _ -> false
  in
  List.iter
    (fun (m : module_) ->
      List.iter
        (function
          | Axiom { pattern; attrs; _ } -> (
              match find_attr "subsort" attrs with
              | Some (App (_, [ s1; s2 ], _)) ->
                  subsorts := (s1, s2) :: !subsorts
              | Some _ -> error "malformed subsort attribute"
              | None when List.exists (fun a -> has_attr a attrs) logical_attrs
                ->
                  ()
              | None when has_attr "symbol-overload" attrs -> (
                  (match find_attr "symbol-overload" attrs with
                  | Some (App (_, _, [ App (f, _, _); App (g, _, _) ])) ->
                      overloads := (f, g) :: !overloads
                  | _ -> error "malformed symbol-overload attribute");
                  (* the axiom itself is an equation of the anywhere symbol f *)
                  match pattern with
                  | App ("\\equals", _, [ App (f, _, xs); rhs ]) ->
                      equations :=
                        ( f,
                          value_of_equation info
                            (App ("\\top", [], []))
                            f xs rhs attrs )
                        :: !equations
                  | _ ->
                      error "unsupported overload axiom %s"
                        (string_of_pattern pattern))
              | None -> (
                  match pattern with
                  | App ("\\rewrites", _, [ lhs; rhs ]) ->
                      rules := value_of_rule info lhs rhs attrs :: !rules
                  | App
                      ( "\\implies",
                        _,
                        [ cond; App ("\\equals", _, [ App (f, _, xs); rhs ]) ]
                      ) ->
                      if not (is_hooked f) then
                        equations :=
                          (f, value_of_equation info cond f xs rhs attrs)
                          :: !equations
                  | App ("\\equals", _, [ App (f, _, xs); rhs ])
                    when Hashtbl.mem info.symbols f ->
                      if not (is_hooked f) then
                        equations :=
                          ( f,
                            value_of_equation info
                              (App ("\\top", [], []))
                              f xs rhs attrs )
                          :: !equations
                  | _ ->
                      error "unsupported axiom %s" (string_of_pattern pattern)))
          | _ -> ())
        m.sentences)
    defn.modules;
  let symdecls =
    Hashtbl.fold (fun name sym acc -> (name, sym) :: acc) info.symbols []
    |> List.sort compare
    |> List.map
         (fun
           (name, { args; result; hook; is_function; is_binder; is_anywhere; _ })
         ->
           let kind =
             match hook with
             | Some h -> case "HOOKED hookid" [ text_as "hookid" h ] "symkind"
             | None when is_function -> case "FUNCTION" [] "symkind"
             | None when is_anywhere -> case "ANYWHERE" [] "symkind"
             | None when is_binder -> case "BINDER" [] "symkind"
             | None -> case "CONSTRUCTOR" [] "symkind"
           in
           ( text_as "symid" name,
             case "sort* '->' sort symkind"
               [
                 list typ_sort (List.map value_of_sort args);
                 value_of_sort result;
                 kind;
               ]
               "symdecl" ))
  in
  (* map<K, V> is set<pair<K, V>> (spec-meta/common/0-stdlib.watsup) *)
  let map_value (typ_k : Typ.t) (typ_v : Typ.t)
      (entries : (Value.t * Value.t) list) =
    let targs = [ typ_k; typ_v ] in
    let typ_pair = Typ.Make.var ("pair" $ no_region) targs in
    let pairs =
      List.map
        (fun (k, v) ->
          Value.Make.(
            Value.Mixops.of_string "k ':' v" <|! [ k; v ] <<|! typ_pair))
        entries
    in
    Value.Make.(
      Value.Mixops.of_string "`{ k `}"
      <|! [ list typ_pair pairs ]
      <<|! Typ.Make.var ("map" $ no_region) targs)
  in
  let symbols = map_value (typ "symid") (typ "symdecl") symdecls in
  (* equations grouped by the symbol they define, in file order *)
  let equations_by_symbol =
    let typ_eqn = typ "equation" in
    let groups = Hashtbl.create 256 and order = ref [] in
    List.iter
      (fun (f, e) ->
        match Hashtbl.find_opt groups f with
        | Some es -> Hashtbl.replace groups f (e :: es)
        | None ->
            order := f :: !order;
            Hashtbl.replace groups f [ e ])
      (List.rev !equations);
    List.rev !order
    |> List.map (fun f ->
           (text_as "symid" f, list typ_eqn (List.rev (Hashtbl.find groups f))))
    |> map_value (typ "symid") (Typ.Make.list typ_eqn)
  in
  let subsorts =
    List.rev_map
      (fun (s1, s2) ->
        case "sort '<' sort" [ value_of_sort s1; value_of_sort s2 ] "subsort")
      !subsorts
  in
  let params =
    Hashtbl.fold
      (fun name sym acc ->
        if sym.params = [] then acc else (name, sym.params) :: acc)
      info.symbols []
    |> List.sort compare
    |> List.map (fun (name, ps) ->
           ( text_as "symid" name,
             list (typ "sortid") (List.map (text_as "sortid") ps) ))
    |> map_value (typ "symid") (Typ.Make.list (typ "sortid"))
  in
  let natsorts =
    Hashtbl.fold
      (fun name attrs acc ->
        match string_attr "nat" attrs with
        | Some n -> (name, n) :: acc
        | None -> acc)
      info.sort_attrs []
    |> List.sort compare
    |> List.map (fun (name, n) ->
           (text_as "sortid" name, Value.Make.nat (Bigint.of_string n)))
    |> map_value (typ "sortid") Typ.Make.nat
  in
  let field name v = (Atom.Keyword name $ no_region, v) in
  let definition =
    Value.Make.str (typ "definition")
      [
        field "SYMBOLS" symbols;
        field "SUBSORTS" (list (typ "subsort") subsorts);
        field "OVERLOADS"
          (list (typ "overload")
             (List.rev_map
                (fun (f, g) ->
                  case "symid '>' symid"
                    [ text_as "symid" f; text_as "symid" g ]
                    "overload")
                !overloads));
        field "RULES" (list (typ "krule") (List.rev !rules));
        field "EQUATIONS" equations_by_symbol;
        field "PARAMS" params;
        field "NATSORTS" natsorts;
      ]
  in
  {
    info;
    definition;
    rules = List.length !rules;
    equations = List.length !equations;
  }

let load_pattern (info : info) (p : pattern) : Value.t = value_of_pattern info p
