(* Headers and dune stanza for the split [spec_parts] library.

   The heavy generated code is emitted as many small modules so dune compiles
   them in parallel. Module names are globally unique, so every part [open]s all
   prior parts (plus [Ctx]) and cross-file calls stay unqualified. *)

let part_module_name (idx : int) : string = Printf.sprintf "Part_%03d" idx
let part_file_name (idx : int) : string = Printf.sprintf "part_%03d.ml" idx

(* [name] suffixes the generated library/module so multiple compiled specs
   (e.g. P4, spec-meta/il, spec-meta/sl) can coexist in one workspace. The
   empty name (P4's default) keeps the original unsuffixed identifiers. *)
let lib_name (name : string) : string =
  if name = "" then "spec_parts" else "spec_parts_" ^ name

let module_name (name : string) : string =
  if name = "" then "Spec_parts" else "Spec_parts_" ^ name

(* Common opens every generated unit needs: the runtime/domain/lang names the
   generated code uses unqualified, plus [Ctx] (Value/Typ/Run aliases, the
   prelude helpers, [Option]/[List], and the [cur__]/[with_ctx] glue). *)
let common_opens : string =
  "[@@@warning \"-8-11-26-27-30-32-33-39\"]\n\
   open Domain\n\
   open Lang\n\
   open Util.Source\n\
   open Ctx"

(* Header for [part_idx]: common opens + every earlier part (topo order, so all
   cross-part references resolve backwards). *)
let part_header (idx : int) : string =
  let prior =
    List.init idx (fun i -> "open " ^ part_module_name i) |> String.concat "\n"
  in
  if prior = "" then common_opens ^ "\n" else common_opens ^ "\n" ^ prior ^ "\n"

(* Header for [dispatch.ml]: opens every part so the dispatch matches can name
   any [f__]/[r__]. *)
let dispatch_header (n_parts : int) : string =
  let opens =
    List.init n_parts (fun i -> "open " ^ part_module_name i)
    |> String.concat "\n"
  in
  if opens = "" then common_opens ^ "\n" else common_opens ^ "\n" ^ opens ^ "\n"

(* [interface_name]/[Names.sanitize] re-emitted as fixed source, since the
   compiler itself isn't linked into the generated artifact. *)
let interface_name_fn : string =
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

exception No_marshaller_ of string

let interface_lookup_ (typ : Typ.t) : Obj.t * Obj.t =
  let name = interface_name_ typ in
  match Hashtbl.find_opt interface_registry_ name with
  | Some entry -> entry
  | None -> raise (No_marshaller_ name)
|}

(* Generated [compiled/dune]. [-opaque] keeps the [.cmx] jobs parallel despite
   the linear cmi chain. *)
let dune (name : string) : string =
  Printf.sprintf
    "(library\n\
    \ (name %s)\n\
    \ (public_name p4spectec.%s)\n\
    \ (libraries util domain frontend lang pass runtime)\n\
    \ (ocamlopt_flags (:standard -opaque)))\n"
    (lib_name name) (lib_name name)
