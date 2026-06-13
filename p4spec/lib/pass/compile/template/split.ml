(* Headers and dune stanza for the split [spec_parts] library.

   The heavy generated code is emitted as many small modules so dune compiles
   them in parallel. Module names are globally unique, so every part [open]s all
   prior parts (plus [Ctx]) and cross-file calls stay unqualified. *)

let part_module_name (idx : int) : string = Printf.sprintf "Part_%03d" idx
let part_file_name (idx : int) : string = Printf.sprintf "part_%03d.ml" idx

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

(* Generated [compiled/dune]. [-opaque] keeps the [.cmx] jobs parallel despite
   the linear cmi chain. *)
let dune : string =
  "(library\n\
  \ (name spec_parts)\n\
  \ (public_name p4spectec.spec_parts)\n\
  \ (libraries util domain frontend lang pass runtime)\n\
  \ (ocamlopt_flags (:standard -opaque)))\n"
