module Run = Runtime.Dynamic_Runner.Signature

(* Kind of interface, currently offering three: P4, SL, and IL *)

type interface = P4_interface | IL_interface | SL_interface

(* Layers *)

type layer = { specdir : string; rel : string }
type target = { includes : string list; path : string }

(* Tower is an alternation of a layer, an interface, and an execution mode.
   Each level picks its own [mode] independently — e.g. the boot level can
   run fully-compiled [ML_mode] while an intermediate or the target runs
   interpreted, or vice versa. *)

type level = { layer : layer; interface : interface; mode : Run.mode }

type tower = {
  level_boot : level;
  levels_interm : level list;
  level_target : level;
  target : target;
}

(* Load a tower from a JSON file.
   JSON schema:
     { "levels":
         [ { "specdir": "...", "rel": "...", "interface": "p4"|"il"|"sl",
             "mode": "il"|"sl"|"ml" }, ... ] }
   First level = boot, last = target, middle = intermediates.
   `target` is supplied separately (from CLI -p/-i flags). Each level's
   "mode" is independent: "ml" only works for a level whose "interface" has
   a compiled backend actually generated (spec-meta/il, spec-meta/sl, or
   spec — see the gen-ocaml* Makefile targets); "il"/"sl" run that level's
   interface interpreted regardless of what the levels above/below it do. *)

let tower_of_file path target =
  let json = Yojson.Basic.from_file path in
  let open Yojson.Basic.Util in
  let level_of_json json =
    let interface =
      match json |> member "interface" |> to_string with
      | "p4" -> P4_interface
      | "il" -> IL_interface
      | "sl" -> SL_interface
      | s -> failwith (Format.sprintf "tower: unknown interface %S" s)
    in
    let mode =
      match json |> member "mode" |> to_string with
      | "il" -> Run.IL_mode
      | "sl" -> Run.SL_mode
      | "ml" -> Run.ML_mode
      | s -> failwith (Format.sprintf "tower: unknown mode %S" s)
    in
    {
      layer =
        {
          specdir = json |> member "specdir" |> to_string;
          rel = json |> member "rel" |> to_string;
        };
      interface;
      mode;
    }
  in
  let levels = json |> member "levels" |> to_list |> List.map level_of_json in
  match levels with
  | [] | [ _ ] -> failwith "tower: at least two levels required (boot + target)"
  | _ ->
      let level_boot = List.hd levels in
      let levels_interm =
        levels |> List.tl |> List.rev |> List.tl |> List.rev
      in
      let level_target = levels |> List.rev |> List.hd in
      { level_boot; levels_interm; level_target; target }
