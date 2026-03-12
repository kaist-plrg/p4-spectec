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
end
