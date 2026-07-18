(* Utilities for splitting the compiled ML code *)

(* Bucketing for the split [spec_parts] library *)

let count_loc (toplevel_ml : Ml.toplevel) : int =
  let s = Ml.Print.print_toplevel toplevel_ml in
  if s = "" then 0
  else String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 1 s

let bucket (target : int) (toplevels_ml : Ml.toplevel list) :
    Ml.toplevel list list =
  let toplevels_ml_buckets, toplevel_ml_cur, _ =
    List.fold_left
      (fun (toplevels_ml_buckets, toplevel_ml_cur, loc_cur) toplevel_ml ->
        let loc = count_loc toplevel_ml in
        if toplevel_ml_cur <> [] && loc_cur + loc > target then
          ( toplevels_ml_buckets @ [ List.rev toplevel_ml_cur ],
            [ toplevel_ml ],
            loc )
        else
          (toplevels_ml_buckets, toplevel_ml :: toplevel_ml_cur, loc_cur + loc))
      ([], [], 0) toplevels_ml
  in
  toplevels_ml_buckets
  @ if toplevel_ml_cur = [] then [] else [ List.rev toplevel_ml_cur ]

(* Naming for the split [spec_parts] library *)

let name_lib (name : string) : string =
  if name = "" then "spec_parts" else "spec_parts_" ^ name

let name_module (name : string) : string =
  if name = "" then "Spec_parts" else "Spec_parts_" ^ name

let name_part_module (idx : int) : string = Printf.sprintf "Part_%03d" idx
let name_part_file (idx : int) : string = Printf.sprintf "part_%03d.ml" idx

(* Headers for the split [spec_parts] library *)

let prelude_open_common : string =
  "[@@@warning \"-8-11-26-27-30-32-33-39\"]\n\
   open Domain\n\
   open Lang\n\
   open Util.Source\n\
   open Ctx"

let prelude_part (idx : int) : string =
  let prelude_open_prior =
    List.init idx (fun i -> "open " ^ name_part_module i) |> String.concat "\n"
  in
  if prelude_open_prior = "" then prelude_open_common ^ "\n"
  else prelude_open_common ^ "\n" ^ prelude_open_prior ^ "\n"

(* Header for [dispatch.ml] *)

let prelude_dispatch (n_parts : int) : string =
  let prelude_open =
    List.init n_parts (fun i -> "open " ^ name_part_module i)
    |> String.concat "\n"
  in
  if prelude_open = "" then prelude_open_common ^ "\n"
  else prelude_open_common ^ "\n" ^ prelude_open ^ "\n"
