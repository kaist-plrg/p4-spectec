open Test_common
open Util.Error
module Sim = Runtime.Sim.Simulator
module Test = Util.Test
module Filesys = Util.Filesys

let collect_test_pairs (arch : string) (testdirs_p4 : string list)
    (testdirs_stf : string list) : (int * string * string) list =
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
            Util.Strings.contains_substring "#include <v1model.p4>" contents
            || Util.Strings.contains_substring "#include \"v1model.p4\""
                 contents
        | "ebpf" ->
            Util.Strings.contains_substring "#include <ebpf_model.p4>" contents
            || Util.Strings.contains_substring "#include \"ebpf_model.p4\""
                 contents
        | _ -> false)
      filenames_p4
  in
  let filenames_stf =
    List.concat_map
      (Filesys.collect_files_with_basedir ~suffix:".stf")
      testdirs_stf
  in
  filenames_p4
  |> List.filter_map (fun (basedir_p4, filename_p4) ->
      let filenames_stf =
        List.filter_map
          (fun (basedir_stf, filename_stf) ->
            if Util.Test.p4_matches_stf filename_p4 filename_stf then
              Some (basedir_stf ^ "/" ^ filename_stf)
            else None)
          filenames_stf
      in
      match filenames_stf with
      | [] -> None
      | _ -> Some (basedir_p4 ^ "/" ^ filename_p4, filenames_stf))
  |> List.concat_map (fun (filename_p4, filenames_stf) ->
      List.mapi
        (fun idx filename_stf -> (idx, filename_p4, filename_stf))
        filenames_stf)

let run_export_command arch testdirs_p4 testdirs_stf export_dir =
  let cp (filename_src : string) (filename_dst : string) : string =
    let ic = open_in filename_src in
    let oc = open_out filename_dst in
    try
      while true do
        output_string oc (input_line ic ^ "\n")
      done;
      raise End_of_file
    with End_of_file ->
      close_in ic;
      close_out oc;
      filename_dst
  in

  let double_underscore_suffix s =
    let re = Str.regexp "^\\(.*\\)_\\([0-9]+\\)$" in
    if Str.string_match re s 0 then
      Printf.sprintf "%s__%s" (Str.matched_group 1 s) (Str.matched_group 2 s)
    else s
  in

  if Sys.file_exists export_dir then
    Printf.printf "%s already exists!\n" export_dir
  else Filesys.mkdir export_dir;
  let abs = Unix.realpath export_dir in
  Printf.printf "Export directory created at: %s\n" abs;
  let filename_pairs = collect_test_pairs arch testdirs_p4 testdirs_stf in
  List.iter
    (fun (idx, p4, stf) ->
      let base = Filesys.base ~suffix:".stf" stf in
      let base = double_underscore_suffix base in

      let new_filename_p4 =
        Filename.concat export_dir (Format.asprintf "%s.p4" base)
      in
      let new_filename_stf =
        Filename.concat export_dir (Format.asprintf "%s.stf" base)
      in
      let _ = cp p4 new_filename_p4 in
      let _ = cp stf new_filename_stf in
      ())
    filename_pairs

let export_command =
  Core.Command.basic ~summary:"export test suite"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map testdirs_p4 =
       flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and testdirs_stf =
       flag "-stf-dir" (listed string) ~doc:"stf test directories"
     and export_dir =
       flag "-export-dir" (required string) ~doc:"export directory"
     and arch = flag "-arch" (required string) ~doc:"architecture name" in
     fun () -> run_export_command arch testdirs_p4 testdirs_stf export_dir)

let command =
  Core.Command.group ~summary:"p4spec-testdata-export"
    [ ("export-tests", export_command) ]

let () = Command_unix.run ~version command
