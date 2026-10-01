let canonical path = try Unix.realpath path with Unix.Unix_error _ -> path

let collect ?include_file root =
  let files = Filesys.collect_files ~suffix:".watsup" root in
  match include_file with
  | None -> files
  | Some path ->
      let canonical_path = canonical path in
      let same_file file = String.equal (canonical file) canonical_path in
      if List.exists same_file files then
        List.map (fun file -> if same_file file then path else file) files
      else
        let components = String.split_on_char '/' path in
        let rec insert = function
          | [] -> [ path ]
          | file :: rest as files ->
              if
                List.compare String.compare components
                  (String.split_on_char '/' file)
                < 0
              then path :: files
              else file :: insert rest
        in
        insert files

let root_of_file file =
  let holds_marker dir =
    match Sys.readdir dir with
    | exception Sys_error _ -> false
    | entries ->
        Array.exists
          (fun entry ->
            Filename.check_suffix entry ".spec"
            && not (Sys.is_directory (Filename.concat dir entry)))
          entries
  in
  let rec search dir =
    if holds_marker dir then Some dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else search parent
  in
  let file =
    if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file
    else file
  in
  search (Filename.dirname file)
