(* Loading KORE into values of the K spec (kore in spec-k/1-syntax.watsup).

   The parsed text is passed on as it is: every axiom with its attributes,
   and the declarations of sorts, symbols, and aliases. Connectives are named
   without their backslash. Which axioms are executed and how they are read,
   down to domain values, is for the spec to decide (spec-k/6-load.watsup). *)

open Ast
module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Atom = Domain.Atom
open Util.Source

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* Information about a definition used while loading and unparsing *)

type symbol = { is_binder : bool }

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
          | SymbolDecl { name; attrs; _ } ->
              Hashtbl.replace info.symbols name
                { is_binder = has_attr "binder" attrs }
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
let typ_kore = typ "kore"
let typ_kattr = typ "kattr"
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

(* A pattern; an application must be of a symbol known to [known] *)
let rec value_of_kore (known : string -> bool) (p : pattern) : Value.t =
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
          list typ_kore (List.map (value_of_kore known) args);
        ]
        "kore"
  | App (name, sorts, args) ->
      if not (known name) then error "undeclared symbol %s" name;
      case "KAPP symid sort* kore*"
        [
          text_as "symid" name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map (value_of_kore known) args);
        ]
        "kore"

(* Arguments of attributes may name symbols that are not declared *)
let value_of_kattr (a : attr) : Value.t =
  match a with
  | App (name, sorts, args) ->
      case "KATTR text sort* kore*"
        [
          Value.Make.text name;
          list typ_sort (List.map value_of_sort sorts);
          list typ_kore (List.map (value_of_kore (fun _ -> true)) args);
        ]
        "kattr"
  | _ -> error "malformed attribute %s" (string_of_pattern a)

let value_of_kattrs (attrs : attr list) : Value.t =
  list typ_kattr (List.map value_of_kattr attrs)

(* map<K, V> is set<pair<K, V>> (spec-meta/common/0-stdlib.watsup) *)
let map_value (typ_k : Typ.t) (typ_v : Typ.t)
    (entries : (Value.t * Value.t) list) =
  let targs = [ typ_k; typ_v ] in
  let typ_pair = Typ.Make.var ("pair" $ no_region) targs in
  let pairs =
    List.map
      (fun (k, v) ->
        Value.Make.(Value.Mixops.of_string "k ':' v" <|! [ k; v ] <<|! typ_pair))
      entries
  in
  Value.Make.(
    Value.Mixops.of_string "`{ k `}"
    <|! [ list typ_pair pairs ]
    <<|! Typ.Make.var ("map" $ no_region) targs)

type loaded = { info : info; definition : Value.t; axioms : int }

let load_definition (defn : definition) : loaded =
  let info = info_of_definition defn in
  let sentences = List.concat_map (fun (m : module_) -> m.sentences) defn.modules in
  let aliases =
    List.filter_map
      (function
        | AliasDecl { name; lhs = App (_, _, params); body; _ } ->
            Some (name, params, body)
        | AliasDecl { name; _ } -> error "malformed alias %s" name
        | _ -> None)
      sentences
  in
  let known name =
    Hashtbl.mem info.symbols name
    || List.exists (fun (a, _, _) -> String.equal a name) aliases
  in
  let sorts =
    List.filter_map
      (function
        | SortDecl { name; attrs; _ } ->
            Some (text_as "sortid" name, value_of_kattrs attrs)
        | _ -> None)
      sentences
  in
  let symbols =
    List.filter_map
      (function
        | SymbolDecl { name; params; args; result; attrs; _ } ->
            Some
              ( text_as "symid" name,
                case "KSYMBOL sortid* sort* '->' sort kattr*"
                  [
                    list (typ "sortid") (List.map (text_as "sortid") params);
                    list typ_sort (List.map value_of_sort args);
                    value_of_sort result;
                    value_of_kattrs attrs;
                  ]
                  "ksymdecl" )
        | _ -> None)
      sentences
  in
  let aliases =
    List.map
      (fun (name, params, body) ->
        let names =
          List.map
            (function
              | Var (x, _) | SetVar (x, _) -> text_as "varid" x
              | _ -> error "alias %s: a parameter is not a variable" name)
            params
        in
        ( text_as "symid" name,
          case "ALIAS varid* ':=' kore"
            [ list (typ "varid") names; value_of_kore known body ]
            "kalias" ))
      aliases
  in
  let axioms =
    List.filter_map
      (function
        | Axiom { pattern; attrs; _ } ->
            Some
              (case "AXIOM kore kattr*"
                 [ value_of_kore known pattern; value_of_kattrs attrs ]
                 "kaxiom")
        | _ -> None)
      sentences
  in
  let field name v = (Atom.Keyword name $ no_region, v) in
  let definition =
    Value.Make.str (typ "kdefinition")
      [
        field "KSORTS" (map_value (typ "sortid") (Typ.Make.list typ_kattr) sorts);
        field "KSYMBOLS" (map_value (typ "symid") (typ "ksymdecl") symbols);
        field "KALIASES" (map_value (typ "symid") (typ "kalias") aliases);
        field "AXIOMS" (list (typ "kaxiom") axioms);
      ]
  in
  { info; definition; axioms = List.length axioms }

(* The initial term *)
let load_term (info : info) (p : pattern) : Value.t =
  value_of_kore (Hashtbl.mem info.symbols) p
