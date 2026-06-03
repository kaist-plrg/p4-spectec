(* Sanitizers *)

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
let keywords =
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
  if List.mem name keywords then name ^ "_" else name

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

let apply (name : string) =
  name |> to_snake_case |> escape_non_alphanumeric |> escape_keyword
