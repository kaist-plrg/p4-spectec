(* Loading KORE into values of the K spec (spec-k/1-syntax.watsup).

   Only the text is read here; which axioms are executed, and how, is for
   the spec to decide (spec-k/6-load.watsup). Every axiom is passed on with
   its attributes, after:
   - expanding aliases, as the LLVM backend's matching/Parser.scala does;
   - writing out \left-assoc and \right-assoc;
   - reading domain values of hooked sorts (INT.Int, BOOL.Bool, STRING.String,
     FLOAT.Float in canonical form, BYTES.Bytes, MINT.MInt). *)

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
let text_as (name : string) (s : string) = Value.Make.((text s) #@@ name)

let case (mixop : string) (args : Value.t list) (name : string) =
  Value.Make.(mixop <| args <<| name)

let list (t : Typ.t) (vs : Value.t list) = Value.Make.list (Typ.Make.list t) vs

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

(* KORE as kompile writes it (kore in spec-k/1-syntax.watsup): connectives
   named without their backslash, domain values read as above *)

let typ_kore = typ "kore"
let typ_kattr = typ "kattr"

let rec value_of_kore (info : info) (p : pattern) : Value.t =
  match p with
  | Var (name, s) ->
      case "KVAR varid sort" [ text_as "varid" name; value_of_sort s ] "kore"
  | SetVar (name, s) ->
      case "KSETVAR varid sort" [ text_as "varid" name; value_of_sort s ] "kore"
  | Str s -> case "KSTR text" [ Value.Make.text s ] "kore"
  | App ("\\dv", [ s ], [ Str text ]) ->
      case "KVAL pattern" [ value_of_dv info s text ] "kore"
  | App (("\\left-assoc" | "\\right-assoc"), _, _) ->
      value_of_kore info (desugar_assoc p)
  | App (name, sorts, args) when name.[0] = '\\' ->
      case "KML text sort* kore*"
        [
          Value.Make.text (String.sub name 1 (String.length name - 1));
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map (value_of_kore info) args);
        ]
        "kore"
  | App (name, sorts, args) ->
      if not (Hashtbl.mem info.symbols name) then
        error "undeclared symbol %s" name;
      case "KAPP symid sort* kore*"
        [
          text_as "symid" name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map (value_of_kore info) args);
        ]
        "kore"

(* Arguments of attributes may name symbols that are not declared *)
let rec value_of_attr_arg (p : pattern) : Value.t =
  match p with
  | Var (name, s) ->
      case "KVAR varid sort" [ text_as "varid" name; value_of_sort s ] "kore"
  | SetVar (name, s) ->
      case "KSETVAR varid sort" [ text_as "varid" name; value_of_sort s ] "kore"
  | Str s -> case "KSTR text" [ Value.Make.text s ] "kore"
  | App (name, sorts, args) when name.[0] = '\\' ->
      case "KML text sort* kore*"
        [
          Value.Make.text (String.sub name 1 (String.length name - 1));
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map value_of_attr_arg args);
        ]
        "kore"
  | App (name, sorts, args) ->
      case "KAPP symid sort* kore*"
        [
          text_as "symid" name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map value_of_attr_arg args);
        ]
        "kore"

let value_of_kattr (a : attr) : Value.t =
  match a with
  | App (name, sorts, args) ->
      case "KATTR text sort* kore*"
        [
          Value.Make.text name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map value_of_attr_arg args);
        ]
        "kattr"
  | _ -> error "malformed attribute %s" (string_of_pattern a)

let value_of_kattrs (attrs : attr list) : Value.t =
  list typ_kattr (List.map value_of_kattr attrs)

type loaded = { info : info; definition : Value.t; axioms : int }

(* Aliases (alias f(X1, ..., Xn) := body) are abbreviations: each use
   f(p1, ..., pn) in an axiom is replaced by body with Xi := pi, as the LLVM
   backend does before compiling the axioms (matching/.../Parser.scala,
   getAxioms). kompile of K 5 named the left-hand sides of rules this way. *)
let expand_aliases (defn : definition) : definition =
  let aliases = Hashtbl.create 16 in
  List.iter
    (fun (m : module_) ->
      List.iter
        (function
          | AliasDecl { name; lhs = App (_, _, params); body; _ } ->
              let names =
                List.map
                  (function
                    | Var (x, _) | SetVar (x, _) -> x
                    | _ -> error "alias %s: a parameter is not a variable" name)
                  params
              in
              Hashtbl.replace aliases name (names, body)
          | AliasDecl { name; _ } -> error "malformed alias %s" name
          | _ -> ())
        m.sentences)
    defn.modules;
  let rec subst env (p : pattern) : pattern =
    match p with
    | Var (x, _) | SetVar (x, _) -> (
        match List.assoc_opt x env with Some q -> q | None -> p)
    | App (f, sorts, args) -> App (f, sorts, List.map (subst env) args)
    | p -> p
  in
  let rec expand (p : pattern) : pattern =
    match p with
    | App (f, sorts, args) -> (
        let args = List.map expand args in
        match Hashtbl.find_opt aliases f with
        | Some (names, body) when List.length names = List.length args ->
            expand (subst (List.combine names args) body)
        | Some _ -> error "alias %s applied to the wrong number of arguments" f
        | None -> App (f, sorts, args))
    | p -> p
  in
  if Hashtbl.length aliases = 0 then defn
  else
    {
      defn with
      modules =
        List.map
          (fun (m : module_) ->
            {
              m with
              sentences =
                List.map
                  (function
                    | Axiom a -> Axiom { a with pattern = expand a.pattern }
                    | s -> s)
                  m.sentences;
            })
          defn.modules;
    }

let load_definition (defn : definition) : loaded =
  let defn = expand_aliases defn in
  let info = info_of_definition defn in
  (* every axiom, in file order *)
  let axioms =
    List.concat_map
      (fun (m : module_) ->
        List.filter_map
          (function
            | Axiom { pattern; attrs; _ } ->
                Some
                  (case "AXIOM kore kattr*"
                     [ value_of_kore info pattern; value_of_kattrs attrs ]
                     "kaxiom")
            | _ -> None)
          m.sentences)
      defn.modules
  in
  let symbol_attrs = Hashtbl.create 512 in
  List.iter
    (fun (m : module_) ->
      List.iter
        (function
          | SymbolDecl { name; attrs; _ } ->
              Hashtbl.replace symbol_attrs name attrs
          | _ -> ())
        m.sentences)
    defn.modules;
  let symdecls =
    Hashtbl.fold (fun name sym acc -> (name, sym) :: acc) info.symbols []
    |> List.sort compare
    |> List.map (fun (name, { args; result; _ }) ->
           ( text_as "symid" name,
             case "KSYMBOL sort* '->' sort kattr*"
               [
                 list typ_sort (List.map value_of_sort args);
                 value_of_sort result;
                 value_of_kattrs (Hashtbl.find symbol_attrs name);
               ]
               "ksymdecl" ))
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
  let symbols = map_value (typ "symid") (typ "ksymdecl") symdecls in
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
    Value.Make.str (typ "kdefinition")
      [
        field "KSYMBOLS" symbols;
        field "AXIOMS" (list (typ "kaxiom") axioms);
        field "KPARAMS" params;
        field "KNATSORTS" natsorts;
      ]
  in
  { info; definition; axioms = List.length axioms }

let load_pattern (info : info) (p : pattern) : Value.t = value_of_pattern info p
