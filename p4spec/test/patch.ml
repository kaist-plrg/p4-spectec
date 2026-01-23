module Strings = Util.Strings
module Filesys = Util.Filesys
module Test = Util.Test

let version = "0.1"

let transform_stmt_name (name : Stf.Ast.name) =
  (match String.split_on_char '.' name with
    | [] -> failwith ("Invalid path to table " ^ name)
    | hd :: tl ->
        if
          Core.String.Caseless.is_substring hd ~substring:"ingress"
          || Core.String.Caseless.is_substring hd ~substring:"preqos"
        then [ "main"; "ig" ] @ tl
        else if
          Core.String.Caseless.is_substring hd ~substring:"egress"
          || Core.String.Caseless.is_substring hd ~substring:"postqos"
        then [ "main"; "eg" ] @ tl
        else hd :: tl)
  |> String.concat "."

let transform_stmt_action ((name, args) : Stf.Ast.action) =
  let action_split = String.split_on_char '.' name in
  let name =
    match action_split with
    | [] -> failwith ("Invalid action " ^ name)
    | _ -> action_split |> List.rev |> List.hd
  in
  (name, args)

let transform_qualified_paths (stf_stmts : Stf.Ast.stmt list) =
  let open Stf.Ast in
  let transform_stmt (stmt : stmt) =
    match stmt with
    | Add (name, priority_opt, mtches, action, id_opt) ->
        Add
          ( transform_stmt_name name,
            priority_opt,
            mtches,
            transform_stmt_action action,
            id_opt )
    | SetDefault (name, action) ->
        SetDefault (transform_stmt_name name, transform_stmt_action action)
    | _ -> stmt
  in
  List.map transform_stmt stf_stmts

let write_file path content =
  let oc = Out_channel.open_text path in
  Out_channel.output_string oc content;
  Out_channel.close oc

let patch_stf_file output_dir filename =
  print_endline filename;
  let stf_stmts = Stf.Parse.parse_file filename |> transform_qualified_paths in
  let content = Format.asprintf "%a@." Stf.Print.print_stmts stf_stmts in
  let filename = Filename.concat output_dir (Filename.basename filename) in
  write_file filename content

let patch_stf_dir stf_dir output_dir =
  let filenames_stf = Filesys.collect_files ~suffix:".stf" stf_dir in
  if not (Sys.file_exists output_dir) then Filesys.mkdir output_dir;
  print_endline output_dir;
  List.iter
    (fun filename_stf ->
      try patch_stf_file output_dir filename_stf
      with Util.Error.StfError _ -> (
        (* copy filename_stf to output_dir *)
        let ic = open_in filename_stf in
        let oc =
          open_out (output_dir ^ "/" ^ Filesys.base ~suffix:"" filename_stf)
        in
        try
          while true do
            output_string oc (input_line ic ^ "\n")
          done;
          raise End_of_file
        with End_of_file ->
          close_in ic;
          close_out oc))
    filenames_stf

let patch_stf_command =
  Core.Command.basic ~summary:"patch qualified names on STF files"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map stf_dir = flag "-stf-dir" (required string) ~doc:"stf directory"
     and output_dir =
       flag "-output-dir" (required string) ~doc:"output directory"
     in
     fun () -> patch_stf_dir stf_dir output_dir)

let command =
  Core.Command.group ~summary:"p4spec-test" [ ("patch-stf", patch_stf_command) ]

let () = Command_unix.run ~version command
