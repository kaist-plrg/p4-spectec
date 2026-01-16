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
          (fun (_, filename_patch) ->
            let filename_patch_base = Filesys.base ~suffix filename_patch in
            String.equal filename_base filename_patch_base)
          filenames_patch
      in
      match filename_patch_opt with
      | Some filename_patch -> filename_patch
      | None -> (basedir, filename))
    filenames

(* Collectors for exclusion *)

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

(* Collector for P4-STF pairing *)

let p4_matches_stf basename_p4 filepath_stf =
  match String.split_on_char '/' filepath_stf with
  | [ basedir; _ ] -> basedir = basename_p4
  | [ filename_stf ] ->
      let basename_stf = Filesys.base ~suffix:".stf" filename_stf in
      String.equal basename_p4 basename_stf
  | _ -> false

let collect_test_pairs (arch : string) (testdirs_p4 : string list)
    (testdirs_stf : string list) (patchdir : string) : (string * string) list =
  let filenames_p4 =
    List.concat_map
      (Filesys.collect_files_with_basedir ~suffix:".p4")
      testdirs_p4
  in
  let filenames_p4 =
    List.filter
      (fun (dir, filename) ->
        let contents = Filesys.read_file (dir ^ "/" ^ filename) in
        match arch with
        | "v1model" ->
            Strings.contains_substring "#include <v1model.p4>" contents
            || Strings.contains_substring "#include \"v1model.p4\"" contents
        | _ -> false)
      filenames_p4
  in
  let filenames_p4_patch =
    Filesys.collect_files_with_basedir ~suffix:".p4" patchdir
  in
  let filenames_p4 =
    patch_with_basedir ~suffix:".p4" filenames_p4 filenames_p4_patch
  in
  let filenames_stf =
    List.concat_map
      (Filesys.collect_files_with_basedir ~suffix:".stf")
      testdirs_stf
  in
  let filenames_stf_patch =
    Filesys.collect_files_with_basedir ~suffix:".stf" patchdir
  in
  let filenames_stf =
    patch_with_basedir ~suffix:".stf" filenames_stf filenames_stf_patch
  in
  filenames_p4
  |> List.filter_map (fun (basedir_p4, filename_p4) ->
         let filename_p4_base = Filesys.base ~suffix:".p4" filename_p4 in
         let filenames_stf =
           List.filter_map
             (fun (basedir_stf, filename_stf) ->
               if p4_matches_stf filename_p4_base filename_stf then
                 Some (basedir_stf ^ "/" ^ filename_stf)
               else None)
             filenames_stf
         in
         match filenames_stf with
         | [] -> None
         | _ -> Some (basedir_p4 ^ "/" ^ filename_p4, filenames_stf))
  |> List.concat_map (fun (filename_p4, filenames_stf) ->
         List.map
           (fun filename_stf -> (filename_p4, filename_stf))
           filenames_stf)
