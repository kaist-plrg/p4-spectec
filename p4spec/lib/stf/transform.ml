module Name = struct
  let rewrite_substring ~(substrings : string list) ~(replacement : string)
      (name : Ast.name) : Ast.name =
    (match String.split_on_char '.' name with
    | [] -> failwith "Unreachable"
    | hd :: tl ->
        if
          List.exists
            (fun substring -> Core.String.Caseless.is_substring hd ~substring)
            substrings
        then replacement :: tl
        else hd :: tl)
    |> String.concat "."

  let replace_substring ~(substrings : string list) ~(replacement : string)
      (name : Ast.name) : Ast.name =
    let open Core.String.Caseless in
    List.fold_left
      (fun name substring ->
        if is_substring name ~substring then
          substr_replace_first name ~pattern:substring ~with_:replacement
        else name)
      name substrings
end

module Match = struct
  let rewrite_valid ((name, mtchkind) : Ast.mtch) : Ast.mtch =
    let name = Str.global_replace (Str.regexp "\\$valid\\$") "isValid()" name in
    (name, mtchkind)
end

module Action = struct
  let into_unqualified ((name, args) : Ast.action) : Ast.action =
    let name = String.split_on_char '.' name |> List.rev |> List.hd in
    (name, args)

  let replace_substring = Name.replace_substring
end
