open Domain.Lib
open Lang
open Sl

(* Get filenames per target phantom *)

let parse_line (line : string) : iid * string list =
  let data = String.split_on_char ' ' line in
  match data with
  | iid :: filenames ->
      let iid = int_of_string iid in
      let filenames = filenames in
      (iid, filenames)
  | _ -> assert false

let rec parse_lines (targets : string list IIdMap.t) (ic : in_channel) :
    string list IIdMap.t =
  try
    let line = input_line ic in
    let iid, filenames = parse_line line in
    let targets =
      match IIdMap.find_opt iid targets with
      | Some filenames' -> IIdMap.add iid (filenames @ filenames') targets
      | None -> IIdMap.add iid filenames targets
    in
    parse_lines targets ic
  with End_of_file -> targets

let init (filename_target : string) : string list IIdMap.t =
  let ic = open_in filename_target in
  let targets = parse_lines IIdMap.empty ic in
  close_in ic;
  targets
