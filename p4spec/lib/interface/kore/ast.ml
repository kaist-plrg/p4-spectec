(* KORE abstract syntax, as produced by kompile (K 7.1.337).
   Only what definition.kore and krun's input terms use is represented. *)

type sort = Sort of string * sort list | SortVar of string

type pattern =
  | Var of string * sort (* element variable: VarX:SortInt{} *)
  | SetVar of string * sort (* set variable: @X:SortInt{} *)
  | App of string * sort list * pattern list (* symbol or \connective *)
  | Str of string (* string literal, unescaped *)

type attr = pattern

type sentence =
  | Import of string
  | SortDecl of { name : string; params : string list; hooked : bool; attrs : attr list }
  | SymbolDecl of {
      name : string;
      params : string list;
      args : sort list;
      result : sort;
      hooked : bool;
      attrs : attr list;
    }
  | AliasDecl of { name : string; attrs : attr list }
  | Axiom of { params : string list; pattern : pattern; attrs : attr list }
  | Claim of { params : string list; pattern : pattern; attrs : attr list }

type module_ = { name : string; sentences : sentence list; attrs : attr list }
type definition = { attrs : attr list; modules : module_ list }

(* Attributes *)

let attr_name (attr : attr) : string option =
  match attr with App (name, _, _) -> Some name | _ -> None

let find_attr (name : string) (attrs : attr list) : attr option =
  List.find_opt (fun attr -> attr_name attr = Some name) attrs

let has_attr (name : string) (attrs : attr list) : bool =
  Option.is_some (find_attr name attrs)

let string_attr (name : string) (attrs : attr list) : string option =
  match find_attr name attrs with
  | Some (App (_, _, [ Str s ])) -> Some s
  | _ -> None

(* Printing *)

let rec string_of_sort = function
  | Sort (name, sorts) -> name ^ "{" ^ string_of_sorts sorts ^ "}"
  | SortVar name -> name

and string_of_sorts sorts = String.concat ", " (List.map string_of_sort sorts)

let escape (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | '\012' -> Buffer.add_string b "\\f"
      | c when Char.code c < 32 || Char.code c >= 127 ->
          Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let rec string_of_pattern = function
  | Var (name, sort) -> name ^ ":" ^ string_of_sort sort
  | SetVar (name, sort) -> name ^ ":" ^ string_of_sort sort
  | App (name, sorts, args) ->
      name ^ "{" ^ string_of_sorts sorts ^ "}(" ^ string_of_patterns args ^ ")"
  | Str s -> "\"" ^ escape s ^ "\""

and string_of_patterns ps = String.concat ", " (List.map string_of_pattern ps)
