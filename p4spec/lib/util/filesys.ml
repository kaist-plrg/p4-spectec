(* Filesystem helpers *)

(* File and directory operations *)

let rec collect_files ~(suffix : string) (dir : string) =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let path = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn path && file <> "include" then
        files @ collect_files ~suffix path
      else if String.ends_with ~suffix path then files @ [ path ]
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

let base ~(suffix : string) (path : string) : string =
  let path_base = String.split_on_char '/' path |> List.rev |> List.hd in
  if String.ends_with ~suffix path_base then
    String.sub path_base 0 (String.length path_base - String.length suffix)
  else path_base

let cp (path_src : string) (dirname_dst : string) : string =
  let path_dst = dirname_dst ^ "/" ^ base ~suffix:".p4" path_src ^ ".p4" in
  let ic = open_in path_src in
  let oc = open_out path_dst in
  try
    while true do
      output_string oc (input_line ic ^ "\n")
    done;
    raise End_of_file
  with End_of_file ->
    close_in ic;
    close_out oc;
    path_dst

let rmdir (dirname : string) : unit =
  let files = collect_files ~suffix:".p4" dirname in
  List.iter Sys_unix.remove files;
  Unix.rmdir dirname

let mkdir (dirname : string) : unit = Unix.mkdir dirname 0o755

(* Readers *)

let read_file (path : string) : string =
  let ic = open_in path in
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line ic ^ "\n")
    done;
    raise End_of_file
  with End_of_file ->
    close_in ic;
    Buffer.contents buf
