open Domain.Lib
open Lang.Sl
open Util.Source

(* Instruction node *)

module Node = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status *)

  type status = Hit of string list | Miss

  (* Type *)

  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss }

  (* Equivalence *)

  let eq (node_a : t) (node_b : t) : bool =
    node_a.origin = node_b.origin && node_a.status = node_b.status

  (* Printer *)

  let to_string (node : t) : string =
    match node.status with Hit _ -> "H" | Miss -> "M"
end

(* Instruction node coverage map *)

module Cover = struct
  include MakeIIdEnv (Node)

  (* Constructor *)

  let is_ignored (hints : hint list) : bool =
    List.exists (fun (hint : hint) -> hint.hintid.it = "testgen_ignore") hints

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    let iid = instr.note.iid in
    let node = Node.init id in
    let cover = add iid node cover in
    match instr.it with
    | IfI (_, _, instrs_then, _) -> init_instrs cover id instrs_then
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (instrs_hold, instrs_nothold) ->
            let cover = init_instrs cover id instrs_hold in
            init_instrs cover id instrs_nothold
        | HoldH (instrs_hold, _) -> init_instrs cover id instrs_hold
        | NotHoldH (instrs_nothold, _) -> init_instrs cover id instrs_nothold)
    | CaseI (_, cases, _) ->
        let blocks = cases |> List.split |> snd in
        List.fold_left
          (fun cover instrs -> init_instrs cover id instrs)
          cover blocks
    | OtherwiseI instr -> init_instr cover id instr
    | GroupI (_, _, instrs_group) -> init_instrs cover id instrs_group
    | _ -> cover

  and init_instrs (cover : t) (id : id) (instrs : instr list) : t =
    List.fold_left (fun cover instr -> init_instr cover id instr) cover instrs

  let init_tablerow (cover : t) (id : id) (tablerow : tablerow) : t =
    let _, _, instrs = tablerow in
    init_instrs cover id instrs

  let init_tablerows (cover : t) (id : id) (tablerows : tablerow list) : t =
    List.fold_left
      (fun cover tablerow -> init_tablerow cover id tablerow)
      cover tablerows

  let init_def (cover : t) (def : def) : t =
    match def.it with
    | RelD (id, _, _, instrs, hints) when not (is_ignored hints) ->
        init_instrs cover id instrs
    | FuncDecD (id, _, _, _, instrs, hints) when not (is_ignored hints) ->
        init_instrs cover id instrs
    | TableDecD (id, _, _, tablerows, hints) when not (is_ignored hints) ->
        init_tablerows cover id tablerows
    | _ -> cover

  let init_spec (spec : spec) : t = List.fold_left init_def empty spec
end

(* Instruction node coverage *)

type t = Cover.t

(* Measuring coverage *)

let measure_coverage (cover : t) : int * int * float =
  let total = Cover.cardinal cover in
  let hits =
    Cover.fold
      (fun _ (node : Node.t) (hits : int) ->
        match node.status with Hit _ -> hits + 1 | Miss -> hits)
      cover 0
  in
  let coverage =
    if total = 0 then 0. else float_of_int hits /. float_of_int total *. 100.
  in
  (total, hits, coverage)

(* Extension from single coverage *)

let extend (cover : t) (filename_p4 : string) (cover_single : Single.t) : t =
  Cover.mapi
    (fun (iid : iid) (node : Node.t) ->
      let node_single = Single.Cover.find iid cover_single in
      match node.status with
      | Hit filenames_p4 -> (
          match node_single.status with
          | Hit ->
              let filenames_p4 = filename_p4 :: filenames_p4 in
              { node with status = Hit filenames_p4 }
          | _ -> node)
      | Miss -> (
          match node_single.status with
          | Hit ->
              let filenames_p4 = [ filename_p4 ] in
              { node with status = Hit filenames_p4 }
          | _ -> node))
    cover

(* Logging *)

let log ~(filename_cov_opt : string option) (cover : t) : unit =
  let output oc_opt =
    match oc_opt with Some oc -> output_string oc | None -> print_string
  in
  let oc_opt = Option.map open_out filename_cov_opt in
  (* Output overall coverage *)
  let total, hits, coverage = measure_coverage cover in
  Format.asprintf "# Overall Coverage: %d/%d (%.2f%%)\n" hits total coverage
  |> output oc_opt;
  (* Collect covers by origin *)
  let covers_origin =
    Cover.fold
      (fun (iid : iid) (node : Node.t) (covers_origin : t IdMap.t) ->
        let origin = node.origin in
        let cover_origin =
          match IdMap.find_opt origin covers_origin with
          | Some cover_origin -> Cover.add iid node cover_origin
          | None -> Cover.add iid node Cover.empty
        in
        IdMap.add origin cover_origin covers_origin)
      cover IdMap.empty
  in
  IdMap.iter
    (fun origin cover_origin ->
      let total, hits, coverage = measure_coverage cover_origin in
      Format.asprintf "# Coverage for %s: %d/%d (%.2f%%)\n" origin.it hits total
        coverage
      |> output oc_opt;
      Cover.iter
        (fun (iid : iid) (node : Node.t) ->
          let origin = node.origin in
          match node.status with
          | Hit filenames ->
              let filenames = String.concat " " filenames in
              Format.asprintf "%d Hit %s %s\n" iid origin.it filenames
              |> output oc_opt
          | Miss ->
              Format.asprintf "%d Miss %s\n" iid origin.it |> output oc_opt)
        cover_origin)
    covers_origin;
  Option.iter close_out oc_opt

(* Constructor *)

let init (spec : spec) : t = Cover.init_spec spec
