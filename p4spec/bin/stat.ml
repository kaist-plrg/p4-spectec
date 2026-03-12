open Lang
open Pass
open Util.Error
open Util.Source

let frontend filenames_spec =
  filenames_spec |> List.concat_map Frontend.Parse.parse_file

let elab filenames_spec = filenames_spec |> frontend |> Elaborate.Elab.elab_spec

let count_loc filenames =
  List.fold_left
    (fun acc filename ->
      let ic = open_in filename in
      let n = ref 0 in
      (try
         while true do
           ignore (input_line ic);
           incr n
         done
       with End_of_file -> ());
      close_in ic;
      acc + !n)
    0 filenames

let count_stats spec =
  List.fold_left
    (fun (rels, rules, decs, syns) def ->
      match def.it with
      | Il.RelD (_, _, _, rulegroups, _, _) ->
          (rels + 1, rules + List.length rulegroups, decs, syns)
      | Il.ExternRelD _ -> (rels + 1, rules, decs, syns)
      | Il.FuncDecD _ | Il.ExternDecD _ | Il.BuiltinDecD _ | Il.TableDecD _ ->
          (rels, rules, decs + 1, syns)
      | Il.TypD _ | Il.ExternTypD _ -> (rels, rules, decs, syns + 1))
    (0, 0, 0, 0) spec

let () =
  let filenames = Array.to_list Sys.argv |> List.tl in
  if filenames = [] then (
    Printf.eprintf "Usage: stat <spec-file>...\n";
    exit 1);
  try
    let loc = count_loc filenames in
    let spec_il = elab filenames in
    let rels, rules, decs, syns = count_stats spec_il in
    Printf.printf "LoC:      %d\n" loc;
    Printf.printf "relation: %d\n" rels;
    Printf.printf "rule:     %d\n" rules;
    Printf.printf "dec:      %d\n" decs;
    Printf.printf "syn:      %d\n" syns
  with
  | ParseError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
  | ElabError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
