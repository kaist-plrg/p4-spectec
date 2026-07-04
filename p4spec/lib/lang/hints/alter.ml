open El
open Util.Source

(* Alternation hints *)

type t =
  | TextH of text
  | AtomH of atom
  | SeqH of t list
  | BrackH of atom * t * atom
  | HoleH of [ `Next | `Num of int ]
  | FuseH of t * t
  | OtherH of exp

let rec to_string (hint : t) =
  Format.asprintf "hint(alter %s)" (to_string' hint)

and to_string' (hint : t) =
  match hint with
  | TextH str -> Print.string_of_text str
  | AtomH atom -> Print.string_of_atom atom
  | SeqH hintexps -> hintexps |> List.map to_string' |> String.concat " "
  | BrackH (atom_l, hintexp, atom_r) ->
      Format.asprintf "%s %s %s"
        (Print.string_of_atom atom_l)
        (to_string' hintexp)
        (Print.string_of_atom atom_r)
  | HoleH `Next -> "%"
  | HoleH (`Num idx) -> Format.asprintf "%%%d" idx
  | FuseH (hintexp_l, hintexp_r) ->
      Format.asprintf "%s#%s" (to_string' hintexp_l) (to_string' hintexp_r)
  | OtherH exp -> Print.string_of_exp exp

(* Creating hints *)

let rec init (hintexp : Hint.t) : t option =
  let ( let* ) = Option.bind in
  match hintexp.it with
  | TextE text -> Some (TextH text)
  | AtomE atom -> Some (AtomH atom)
  | SeqE hintexps ->
      let hintexps_opt = List.map init hintexps in
      if List.for_all Option.is_some hintexps_opt then
        Some (SeqH (List.map Option.get hintexps_opt))
      else None
  | BrackE (atom_l, hintexp, atom_r) ->
      let* hintexp = init hintexp in
      Some (BrackH (atom_l, hintexp, atom_r))
  | HoleE `Next -> Some (HoleH `Next)
  | HoleE (`Num idx) -> Some (HoleH (`Num idx))
  | FuseE (hintexp_l, hintexp_r) ->
      let* hintexp_l = init hintexp_l in
      let* hintexp_r = init hintexp_r in
      Some (FuseH (hintexp_l, hintexp_r))
  | _ -> Some (OtherH hintexp)

(* Validating hints *)

let rec validate (hint : t) (items : 'a list) : (unit, string) result =
  match validate' 0 hint items with Ok _ -> Ok () | Error msg -> Error msg

and validate' (cursor : int) (hint : t) (items : 'a list) : (int, string) result
    =
  let ( let* ) = Result.bind in
  match hint with
  | TextH _ -> Ok cursor
  | SeqH hints ->
      List.fold_left
        (fun cursor_result hint ->
          let* cursor = cursor_result in
          validate' cursor hint items)
        (Ok cursor) hints
  | BrackH (_, hint, _) -> validate' cursor hint items
  | HoleH `Next -> Ok (cursor + 1)
  | HoleH (`Num idx) when idx < List.length items -> Ok cursor
  | HoleH (`Num idx) -> Error (Format.asprintf "index %d out of bounds" idx)
  | FuseH (hint_l, hint_r) ->
      let* cursor_l = validate' cursor hint_l items in
      let* cursor_r = validate' cursor_l hint_r items in
      Ok cursor_r
  | _ -> Ok cursor

(* Re-alignment of alternation indices *)

let rec collect (hint : t) : int list = collect' [] hint

and collect' (idxs : int list) (hintexp : t) : int list =
  match hintexp with
  | TextH _ -> idxs
  | SeqH hints -> List.fold_left collect' idxs hints
  | BrackH (_, hint, _) -> collect' idxs hint
  | HoleH (`Num i) -> i :: idxs
  | HoleH `Next -> idxs
  | FuseH (hint_l, hint_r) ->
      let idxs = collect' idxs hint_l in
      collect' idxs hint_r
  | _ -> idxs

let rec realign (hint : t) (inputs : Input.t) : t =
  let outputs = collect hint in
  let all = inputs @ outputs |> List.sort compare in
  let realign =
    List.fold_left
      (fun outputs_realigned idx ->
        if List.mem idx outputs then
          let idx_realigned = List.length outputs_realigned in
          outputs_realigned @ [ (idx, idx_realigned) ]
        else outputs_realigned)
      [] all
  in
  realign' realign hint

and realign' (realign : (int * int) list) (hint : t) : t =
  match hint with
  | SeqH hints ->
      let hints = List.map (realign' realign) hints in
      SeqH hints
  | BrackH (atom_l, hint, atom_r) ->
      let hint = realign' realign hint in
      BrackH (atom_l, hint, atom_r)
  | HoleH (`Num idx) ->
      let idx_realigned = List.assoc idx realign in
      HoleH (`Num idx_realigned)
  | FuseH (hint_l, hint_r) ->
      let hint_l = realign' realign hint_l in
      let hint_r = realign' realign hint_r in
      FuseH (hint_l, hint_r)
  | _ -> hint

(* Alternation *)

module Ops = struct
  type 'd t = {
    empty : 'd;
    text : string -> 'd option;
    atom : atom -> 'd;
    join : 'd list -> 'd;
    fuse : 'd -> 'd -> 'd;
    other : exp -> 'd;
  }
end

let alternate (ops : 'd Ops.t) (hint : t) (render : 'a -> 'd) (items : 'a list)
    : 'd =
  let rec go (hint : t) (cursor : int) : int * 'd option =
    match hint with
    | TextH str -> (cursor, ops.text str)
    | AtomH atom -> (cursor, Some (ops.atom atom))
    | SeqH hints ->
        let cursor, ds =
          List.fold_left
            (fun (cursor, acc) hint ->
              let cursor, d = go hint cursor in
              (cursor, acc @ [ Option.value ~default:ops.empty d ]))
            (cursor, []) hints
        in
        (cursor, Some (ops.join ds))
    | BrackH (atom_l, hint, atom_r) -> (
        let cursor, d = go hint cursor in
        let ds =
          List.filter_map Fun.id
            [ Some (ops.atom atom_l); d; Some (ops.atom atom_r) ]
        in
        (cursor, match ds with [] -> None | _ -> Some (ops.join ds)))
    | HoleH `Next -> (cursor + 1, Some (render (List.nth items cursor)))
    | HoleH (`Num idx) -> (cursor, Some (render (List.nth items idx)))
    | FuseH (hint_l, hint_r) ->
        let cursor, d_l = go hint_l cursor in
        let cursor, d_r = go hint_r cursor in
        ( cursor,
          Some
            (ops.fuse
               (Option.value ~default:ops.empty d_l)
               (Option.value ~default:ops.empty d_r)) )
    | OtherH hintexp -> (cursor, Some (ops.other hintexp))
  in
  go hint 0 |> snd |> Option.value ~default:ops.empty
