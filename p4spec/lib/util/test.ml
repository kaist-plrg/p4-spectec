(* stf helper *)

let p4_matches_stf basename_p4 basename_stf =
  String.equal basename_p4 basename_stf
  ||
  match String.split_on_char '_' basename_stf with
  | [ base; suffix ] ->
      String.equal basename_p4 base
      && String.for_all (function '0' .. '9' -> true | _ -> false) suffix
  | _ -> false

(* Collectors *)

let collect_exclude filename_exclude =
  let ic = open_in filename_exclude in
  let rec parse_lines excludes =
    try
      let exclude = input_line ic in
      if String.starts_with ~prefix:"#" exclude then parse_lines excludes
      else parse_lines (exclude :: excludes)
    with End_of_file -> excludes
  in
  let excludes = parse_lines [] in
  close_in ic;
  excludes

let collect_excludes (paths_exclude : string list) =
  let filenames_exclude =
    List.concat_map (Filesys.collect_files ~suffix:".exclude") paths_exclude
  in
  List.concat_map collect_exclude filenames_exclude

(* Patchers *)

let patch ~(suffix : string) (filenames : string list)
    (filenames_patch : string list) : string list =
  List.map
    (fun filename ->
      let filename_base = Filesys.base ~suffix filename in
      let filename_patch_opt =
        List.find_opt
          (fun filename_patch ->
            let filename_patch_base = Filesys.base ~suffix filename_patch in
            String.equal filename_base filename_patch_base)
          filenames_patch
      in
      match filename_patch_opt with
      | Some filename_patch -> filename_patch
      | None -> filename)
    filenames
