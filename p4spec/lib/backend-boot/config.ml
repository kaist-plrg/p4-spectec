module Run = Runtime.Dynamic_Runner.Signature

(* Kind of interface, currently offering three: P4, SL, and AL *)

type interface = P4_interface | AL_interface | SL_interface

(* Layers *)

type layer = { specdir : string; rel : string }
type target = { includes : string list; path : string }

(* Tower is an alternation of a layer and an interface *)

type level = { layer : layer; interface : interface }

type tower = {
  mode : Run.mode;
  level_boot : level;
  levels_interm : level list;
  level_target : level;
  target : target;
}

(* Load a tower from a JSON file.
   JSON schema:
     { "mode": "al"|"sl",
       "levels": [ { "specdir": "...", "rel": "...", "interface": "p4"|"al"|"sl" }, ... ] }
   First level = boot, last = target, middle = intermediates.
   `target` is supplied separately (from CLI -p/-i flags). *)

let tower_of_file path target =
  let json = Yojson.Basic.from_file path in
  let open Yojson.Basic.Util in
  let mode =
    match json |> member "mode" |> to_string with
    | "al" -> Run.AL_mode
    | "sl" -> Run.SL_mode
    | s -> failwith (Format.sprintf "tower: unknown mode %S" s)
  in
  let level_of_json json =
    let interface =
      match json |> member "interface" |> to_string with
      | "p4" -> P4_interface
      | "al" -> AL_interface
      | "sl" -> SL_interface
      | s -> failwith (Format.sprintf "tower: unknown interface %S" s)
    in
    {
      layer =
        {
          specdir = json |> member "specdir" |> to_string;
          rel = json |> member "rel" |> to_string;
        };
      interface;
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
      { mode; level_boot; levels_interm; level_target; target }
