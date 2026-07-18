(* [Naming.name]/[Names.sanitize] re-emitted as fixed source, since the
   compiler itself isn't linked into the generated artifact

   Must stay in sync with [Interface.Naming.name] (gen/interface/naming.ml) *)

let converter_table : string =
  {|
let interface_keywords_ =
  [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
    "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
    "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or";
    "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
    "val"; "virtual"; "when"; "while"; "with" ]

let interface_snake_case_ (name : string) : string =
  let len = String.length name in
  let buf = Buffer.create (len + 4) in
  String.iteri
    (fun idx c ->
      (if idx > 0 then
         let prev = name.[idx - 1] in
         let is_upper = c >= 'A' && c <= 'Z' in
         let prev_lower = prev >= 'a' && prev <= 'z' in
         let prev_upper = prev >= 'A' && prev <= 'Z' in
         let next_lower =
           idx + 1 < len && name.[idx + 1] >= 'a' && name.[idx + 1] <= 'z'
         in
         if is_upper && (prev_lower || (prev_upper && next_lower)) then
           Buffer.add_char buf '_');
      Buffer.add_char buf (Char.lowercase_ascii c))
    name;
  Buffer.contents buf

let interface_escape_non_alnum_ (name : string) : string =
  let buf = Buffer.create (String.length name) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
      then Buffer.add_char buf c
      else Buffer.add_char buf '_')
    name;
  Buffer.contents buf

let interface_sanitize_ (name : string) : string =
  let name = interface_escape_non_alnum_ (interface_snake_case_ name) in
  if List.mem name interface_keywords_ then name ^ "_" else name

let rec interface_name_ (typ : Typ.t) : string =
  match typ.it with
  | Il.BoolT -> "bool"
  | Il.NumT `NatT -> "nat"
  | Il.NumT `IntT -> "int"
  | Il.TextT -> "text"
  | Il.VarT (id, []) -> interface_sanitize_ id.it
  | Il.VarT (id, targs) ->
      interface_sanitize_ id.it ^ "__"
      ^ String.concat "__" (List.map interface_name_ targs)
  | Il.TupleT typs -> String.concat "_" (List.map interface_name_ typs) ^ "_tup"
  | Il.IterT (t, Il.Opt) -> interface_name_ t ^ "__opt"
  | Il.IterT (t, Il.List) -> interface_name_ t ^ "__list"
  | Il.FuncT _ -> "func"

exception NoConverter of string

let find_converter_dynamic (typ : Typ.t) : Obj.t * Obj.t =
  let name = interface_name_ typ in
  match Hashtbl.find_opt interface_registry_ name with
  | Some entry -> entry
  | None -> raise (NoConverter name)
|}
