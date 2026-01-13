(* stf helper *)

let p4_matches_stf basename_p4 filepath_stf =
  match String.split_on_char '/' filepath_stf with
  | [ basedir; _ ] -> basedir = basename_p4
  | [ filename_stf ] ->
      let basename_stf = Filesys.base ~suffix:".stf" filename_stf in
      String.equal basename_p4 basename_stf
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

let patch_with_basedir ~(suffix : string) (filenames : (string * string) list)
    (filenames_patch : (string * string) list) : (string * string) list =
  List.map
    (fun (basedir, filename) ->
      let filename_base = Filesys.base ~suffix filename in
      let filename_patch_opt =
        List.find_opt
          (fun (basedir_patch, filename_patch) ->
            let filename_patch_base = Filesys.base ~suffix filename_patch in
            String.equal filename_base filename_patch_base)
          filenames_patch
      in
      match filename_patch_opt with
      | Some filename_patch -> filename_patch
      | None -> (basedir, filename))
    filenames
