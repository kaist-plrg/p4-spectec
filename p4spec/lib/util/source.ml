(* Positions and regions *)

type pos = { file : string; line : int; column : int } [@@deriving yojson]
type region = { left : pos; right : pos } [@@deriving yojson]

let no_pos = { file = ""; line = 0; column = 0 }
let no_region = { left = no_pos; right = no_pos }
let pos_of_file file = { no_pos with file }
let region_of_file file = { left = pos_of_file file; right = pos_of_file file }
let before_region region = { left = region.left; right = region.left }
let after_region region = { left = region.right; right = region.right }

let string_of_pos pos =
  if pos.line = -1 then Printf.sprintf "0x%x" pos.column
  else string_of_int pos.line ^ "." ^ string_of_int (pos.column + 1)

let string_of_range left right =
  string_of_pos left ^ if left = right then "" else "-" ^ string_of_pos right

let string_of_region region =
  if region = region_of_file region.left.file then region.left.file
  else region.left.file ^ ":" ^ string_of_range region.left region.right

(* Phrases *)

type ('a, 'b, 'c) info = { it : 'a; note : 'b; at : 'c } [@@deriving yojson]
type ('a, 'b) note_phrase = ('a, 'b, region) info [@@deriving yojson]
type ('a, 'b) note = ('a, 'b, unit) info [@@deriving yojson]
type 'a phrase = ('a, unit, region) info [@@deriving yojson]

let ( $ ) it at = { it; at; note = () }
let ( $$ ) it (at, note) = { it; at; note }
let ( $$$ ) it note = { it; at = (); note }
let ( % ) at note = (at, note)
let it { it; _ } = it
let at { at; _ } = at
let note { note; _ } = note

let over_region = function
  | [] -> no_region
  | region :: regions ->
      List.fold_left
        (fun region_over region ->
          {
            left = min region_over.left region.left;
            right = max region_over.right region.right;
          })
        region regions

let exp_list_region = function
  | [] -> no_region
  | [ exp ] -> exp.at
  | exp :: exps ->
      (* take the leftmost and rightmost exps and construct a region that spans the whole list *)
      let left = exp.at.left in
      let right = (exps |> List.rev |> List.hd |> at).right in
      { left; right }
