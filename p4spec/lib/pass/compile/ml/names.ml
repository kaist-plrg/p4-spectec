open Domain
open Lang
open Util.Source

(* Variables *)

let to_snake_case name =
  let len = String.length name in
  let buf = Buffer.create (len + 4) in
  String.iteri
    (fun idx char ->
      (if idx > 0 then
         let prev_char = name.[idx - 1] in
         let is_upper = char >= 'A' && char <= 'Z' in
         let prev_lower = prev_char >= 'a' && prev_char <= 'z' in
         let prev_upper = prev_char >= 'A' && prev_char <= 'Z' in
         let next_lower =
           idx + 1 < len && name.[idx + 1] >= 'a' && name.[idx + 1] <= 'z'
         in
         if is_upper && (prev_lower || (prev_upper && next_lower)) then
           Buffer.add_char buf '_');
      Buffer.add_char buf (Char.lowercase_ascii char))
    name;
  Buffer.contents buf

let escape_non_alphanumeric (name : string) =
  let buf = Buffer.create (String.length name) in
  String.iter
    (fun char ->
      if
        (char >= 'a' && char <= 'z')
        || (char >= 'A' && char <= 'Z')
        || (char >= '0' && char <= '9')
      then Buffer.add_char buf char
      else Buffer.add_char buf '_')
    name;
  Buffer.contents buf

[@@@ocamlformat "disable"]
let ocaml_keywords =
  [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
    "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
    "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
    "val"; "virtual"; "when"; "while"; "with";
  ]
[@@@ocamlformat "enable"]

let escape_keyword (name : string) =
  if List.mem name ocaml_keywords then name ^ "_" else name

let var (name : string) =
  name |> to_snake_case |> escape_non_alphanumeric |> escape_keyword

(* Spec-level function and relation names get a prefix to ensure they never
   collide with local variables that share the same spec identifier.
   Functions use "f__"; relations use "r__". *)

let func_name (name : string) = "f__" ^ var name
let rel_name (name : string) = "r__" ^ var name

(* Type variables *)

let tvar (name : string) = String.lowercase_ascii name

(* Constructors *)

let capitalize_first str =
  if str = "" then str
  else
    String.capitalize_ascii (String.sub str 0 1)
    ^ String.sub str 1 (String.length str - 1)

let rec ctor_of_typ (typ : Sl.typ) : string =
  match typ.it with
  | Il.BoolT -> "Bool"
  | Il.NumT _ -> "Int"
  | Il.TextT -> "Text"
  | Il.VarT (id, _) -> String.capitalize_ascii id.it
  | Il.TupleT typs -> typs |> List.map ctor_of_typ |> String.concat "_"
  | Il.IterT (typ, Il.List) -> ctor_of_typ typ ^ "_List"
  | Il.IterT (typ, Il.Opt) -> ctor_of_typ typ ^ "_Opt"
  | Il.FuncT _ -> "Func"

let ctor_of_nottyp ~fallback (nottyp : Sl.nottyp) : string =
  let typs_arg = Mixfix.args nottyp.it in
  let n_args = List.length typs_arg in
  let parts =
    nottyp.it |> Mixfix.atoms_matrix
    |> List.mapi (fun idx_slot atom_group ->
           let atom_parts =
             List.filter_map
               (fun atom ->
                 let part = Atom.ctor_of_atom atom.it in
                 if part = "" then None else Some (capitalize_first part))
               atom_group
           in
           let arg_parts =
             if idx_slot < n_args then
               let typ_arg = List.nth typs_arg idx_slot in
               let part = ctor_of_typ typ_arg in
               if part = "" then [] else [ capitalize_first part ]
             else []
           in
           atom_parts @ arg_parts)
    |> List.concat
  in
  match parts with [] -> fallback | _ -> String.concat "_" parts
