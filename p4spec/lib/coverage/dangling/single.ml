open Domain.Lib
open Lang
open Sl
open Util.Source

(* Dangling branch *)

module Branch = struct
  (* Enclosing relation or function id *)

  type origin = id

  (* Status of a branch:
     if missed, record the value ids for closest-AST derivation *)

  type status = Hit | Miss of vid list

  (* Type *)

  type t = { origin : origin; status : status }

  (* Constructor *)

  let init (id : id) : t = { origin = id; status = Miss [] }

  (* Equivalence *)

  let eq (branch_a : t) (branch_b : t) : bool =
    branch_a.origin.it = branch_b.origin.it && branch_a.status = branch_b.status

  (* Printer *)

  let to_string (branch : t) : string =
    match branch.status with
    | Hit -> "H" ^ branch.origin.it
    | Miss _ -> "M" ^ branch.origin.it
end

(* Dangling coverage map:

   Note that its domain must be set-up initially,
   and no new iid is added during the analysis *)

module Cover = struct
  include MakeVIdEnv (Branch)

  (* Constructor *)

  let is_ignored (hints : hint list) : bool =
    Hints.Flag.init hints "testgen_ignore"

  let rec init_instr (cover : t) (id : id) (instr : instr) : t =
    let iid = instr.note.iid in
    match instr.it with
    | IfI (_, _, block_then, dangle) ->
        let cover = init_block cover id block_then in
        if dangle then
          let branch = Branch.init id in
          add iid branch cover
        else cover
    | HoldI (_, _, _, holdcase) -> (
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            let cover = init_block cover id block_hold in
            init_block cover id block_nothold
        | HoldH (block_hold, dangle) ->
            let cover = init_block cover id block_hold in
            if dangle then
              let branch = Branch.init id in
              add iid branch cover
            else cover
        | NotHoldH (block_nothold, dangle) ->
            let cover = init_block cover id block_nothold in
            if dangle then
              let branch = Branch.init id in
              add iid branch cover
            else cover)
    | CaseI (_, cases, dangle) ->
        let blocks = cases |> List.split |> snd in
        let cover =
          List.fold_left
            (fun cover block -> init_block cover id block)
            cover blocks
        in
        if dangle then
          let branch = Branch.init id in
          add iid branch cover
        else cover
    | GroupI (_, _, _, block_group) -> init_block cover id block_group
    | LetI (_, _, _, block) -> init_block cover id block
    | RuleI (_, _, _, _, block) -> init_block cover id block
    | _ -> cover

  and init_block (cover : t) (id : id) (block : block) : t =
    List.fold_left (fun cover instr -> init_instr cover id instr) cover block

  let init_tablerow (cover : t) (id : id) (tablerow : tablerow) : t =
    let _, _, block = tablerow in
    init_block cover id block

  let init_tablerows (cover : t) (id : id) (tablerows : tablerow list) : t =
    List.fold_left
      (fun cover tablerow -> init_tablerow cover id tablerow)
      cover tablerows

  let init_def (cover : t) (def : def) : t =
    match def.it with
    | RelD (id, _, _, block, elseblock_opt, hints) when not (is_ignored hints)
      -> (
        let cover = init_block cover id block in
        match elseblock_opt with
        | Some elseblock -> init_block cover id elseblock
        | None -> cover)
    | FuncDecD (id, _, _, _, block, elseblock_opt, hints)
      when not (is_ignored hints) -> (
        let cover = init_block cover id block in
        match elseblock_opt with
        | Some elseblock -> init_block cover id elseblock
        | None -> cover)
    | TableDecD (id, _, _, tablerows, hints) when not (is_ignored hints) ->
        init_tablerows cover id tablerows
    | _ -> cover

  let init_spec (spec : spec) : t = List.fold_left init_def empty spec
end

(* Dangling coverage *)

type t = Cover.t

(* Querying coverage *)

let is_hit (cover : t) (iid : iid) : bool =
  let branch = Cover.find iid cover in
  match branch.status with Hit -> true | Miss _ -> false

let is_miss (cover : t) (iid : iid) : bool =
  let branch = Cover.find iid cover in
  match branch.status with Hit -> false | Miss _ -> true

let is_close_miss (cover : t) (iid : iid) : bool =
  let branch = Cover.find iid cover in
  match branch.status with Hit -> false | Miss vids -> List.length vids > 0

(* Hit and miss *)

let hit (cover : t) (iid : iid) : t =
  match Cover.find_opt iid cover with
  | Some branch ->
      let branch = { branch with status = Hit } in
      Cover.add iid branch cover
  | None -> cover

let miss (cover : t) (iid : iid) (vid : vid) : t =
  match Cover.find_opt iid cover with
  | Some branch -> (
      match branch.status with
      | Hit -> cover
      | Miss vids ->
          let branch = { branch with status = Miss (vid :: vids) } in
          Cover.add iid branch cover)
  | None -> cover

(* Extending coverage *)

let extend (cover : t) (cover_extend : t) : t =
  Cover.fold
    (fun (iid : iid) (branch_extend : Branch.t) (cover : t) ->
      match Cover.find_opt iid cover with
      | Some branch -> (
          match (branch.status, branch_extend.status) with
          | Hit, _ -> cover
          | Miss _, Hit ->
              let branch = { branch with status = Hit } in
              Cover.add iid branch cover
          | Miss vids, Miss vids_extend ->
              let vids = vids @ vids_extend in
              let branch = { branch with status = Miss vids } in
              Cover.add iid branch cover)
      | None -> cover)
    cover_extend cover

(* Collector *)

let collect_hit (cover : t) : iid list =
  Cover.fold
    (fun (iid : iid) (branch : Branch.t) (hits : iid list) ->
      match branch.status with Hit -> iid :: hits | Miss _ -> hits)
    cover []
  |> List.rev

let collect_miss (cover : t) : (iid * vid list) list =
  Cover.fold
    (fun (iid : iid) (branch : Branch.t) (misses : (iid * vid list) list) ->
      match branch.status with
      | Hit -> misses
      | Miss vids -> (iid, vids) :: misses)
    cover []
  |> List.rev

(* Constructor *)

let init (spec : spec) : t = Cover.init_spec spec
let empty : t = Cover.empty
