open Lang
open Pass
open Util.Error
open Util.Source

(* Meta-language layers *)

let frontend filenames_spec =
  filenames_spec |> List.concat_map Frontend.Parse.parse_file

(* EL statistics *)

let loc filenames =
  List.fold_left
    (fun loc filename ->
      let ic = open_in filename in
      let loc_file = ref 0 in
      (try
         while true do
           ignore (input_line ic);
           incr loc_file
         done
       with End_of_file -> ());
      close_in ic;
      loc + !loc_file)
    0 filenames

let constructs spec_el =
  List.fold_left
    (fun (num_rels, num_rules, num_decs, num_syns) def ->
      match def.it with
      | El.ExternSynD _ | El.TypD _ -> (num_rels, num_rules, num_decs, num_syns + 1)
      | El.ExternRelD _ | El.RelD _ -> (num_rels + 1, num_rules, num_decs, num_syns)
      | El.RuleGroupD (_, _, rules) -> (num_rels, num_rules + List.length rules, num_decs, num_syns)
      | El.ExternDecD _ | El.BuiltinDecD _ | El.TableDecD _ | El.FuncDecD _ ->
          (num_rels, num_rules, num_decs + 1, num_syns)
      | _ -> (num_rels, num_rules, num_decs, num_syns))
    (0, 0, 0, 0) spec_el

let () =
  let filenames = Array.to_list Sys.argv |> List.tl in
  if filenames = [] then (
    Printf.eprintf "Usage: stat <spec-file>...\n";
    exit 1);
  try
    let loc = loc filenames in
    let spec_el = filenames |> frontend in
    let num_rels, num_rules, num_decs, num_syns = constructs spec_el in
    Printf.printf "LoC:      %d\n" loc;
    Printf.printf "rel:      %d\n" num_rels;
    Printf.printf "rule:     %d\n" num_rules;
    Printf.printf "dec:      %d\n" num_decs;
    Printf.printf "syn:      %d\n" num_syns
  with
  | ParseError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
  | ElabError (at, msg) ->
      Printf.eprintf "%s\n" (string_of_error at msg);
      exit 1
