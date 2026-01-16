(* Filesystem helpers *)

(* File and directory operations *)

let rec collect_files ~(suffix : string) (dir : string) =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

let collect_files_with_basedir ~(suffix : string) (dir : string) =
  let rec collect_files_with_basedir ~reldir dir =
    let files = Sys_unix.readdir (dir ^ "/" ^ reldir) in
    Array.sort String.compare files;
    Array.fold_left
      (fun files file ->
        let abspath = dir ^ "/" ^ reldir ^ "/" ^ file in
        let relpath = if reldir = "" then file else reldir ^ "/" ^ file in
        if Sys_unix.is_directory_exn abspath && file <> "include" then
          files @ collect_files_with_basedir ~reldir:relpath dir
        else if String.ends_with ~suffix abspath then files @ [ (dir, relpath) ]
        else files)
      [] files
  in
  collect_files_with_basedir ~reldir:"" dir

let base ~(suffix : string) (filename : string) : string =
  let filename_base =
    String.split_on_char '/' filename |> List.rev |> List.hd
  in
  if String.ends_with ~suffix filename_base then
    String.sub filename_base 0
      (String.length filename_base - String.length suffix)
  else filename_base

let cp (filename_src : string) (dirname_dst : string) : string =
  let filename_dst =
    dirname_dst ^ "/" ^ base ~suffix:".p4" filename_src ^ ".p4"
  in
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

let rmdir (dirname : string) : unit =
  let files = collect_files ~suffix:".p4" dirname in
  List.iter Sys_unix.remove files;
  Unix.rmdir dirname

let mkdir (dirname : string) : unit = Unix.mkdir dirname 0o755

(* Readers *)

let read_file (filename : string) : string =
  let ic = open_in filename in
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line ic ^ "\n")
    done;
    raise End_of_file
  with End_of_file ->
    close_in ic;
    Buffer.contents buf
