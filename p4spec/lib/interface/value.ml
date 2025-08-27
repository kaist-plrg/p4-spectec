(* Extract text from TextV values *)

let get_text (value : Il.Ast.value) =
  match value.it with Il.Ast.TextV s -> s | _ -> failwith "get_text"
