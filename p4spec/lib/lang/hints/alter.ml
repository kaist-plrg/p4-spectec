open El
open Util.Source

(* Alternation hints *)

type t =
  | TextH of text
  | AtomH of atom
  | SeqH of t list
  | BrackH of atom * t * atom
  | HoleH of [ `Next | `Num of int ] phrase
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
  | HoleH { it = `Next; _ } -> "%"
  | HoleH { it = `Num idx; _ } -> Format.asprintf "%%%d" idx
  | FuseH (hintexp_l, hintexp_r) ->
      Format.asprintf "%s#%s" (to_string' hintexp_l) (to_string' hintexp_r)
  | OtherH exp -> Print.string_of_exp exp

(* Creating hints *)

let rec init (hintexp : Hint.t) : t =
  match hintexp.it with
  | TextE text -> TextH text
  | AtomE atom -> AtomH atom
  | SeqE hintexps -> SeqH (List.map init hintexps)
  | BrackE (atom_l, hintexp, atom_r) -> BrackH (atom_l, init hintexp, atom_r)
  | HoleE `Next -> HoleH (`Next $ hintexp.at)
  | HoleE (`Num idx) -> HoleH (`Num idx $ hintexp.at)
  | FuseE (hintexp_l, _, hintexp_r) -> FuseH (init hintexp_l, init hintexp_r)
  | _ -> OtherH hintexp

(* Validating hints *)

type invalid_oob = {
  at : region;
  placeholder : string;
  index : int;
  arity : int;
}

let rec validate (hint : t) (arity : int) : (unit, invalid_oob) result =
  match validate' 0 hint arity with Ok _ -> Ok () | Error err -> Error err

and validate' (cursor : int) (hint : t) (arity : int) :
    (int, invalid_oob) result =
  let ( let* ) = Result.bind in
  match hint with
  | TextH _ -> Ok cursor
  | SeqH hints ->
      List.fold_left
        (fun cursor_result hint ->
          let* cursor = cursor_result in
          validate' cursor hint arity)
        (Ok cursor) hints
  | BrackH (_, hint, _) -> validate' cursor hint arity
  | HoleH { it = `Next; _ } when cursor < arity -> Ok (cursor + 1)
  | HoleH { it = `Next; at; _ } ->
      Error { at; placeholder = "%"; index = cursor; arity }
  | HoleH { it = `Num idx; _ } when idx >= 0 && idx < arity -> Ok cursor
  | HoleH { it = `Num idx; at; _ } ->
      Error { at; placeholder = Format.asprintf "%%%d" idx; index = idx; arity }
  | FuseH (hint_l, hint_r) ->
      let* cursor_l = validate' cursor hint_l arity in
      let* cursor_r = validate' cursor_l hint_r arity in
      Ok cursor_r
  | _ -> Ok cursor

(* Re-alignment of alternation indices *)

let rec collect (hint : t) : int list = collect' [] hint

and collect' (idxs : int list) (hintexp : t) : int list =
  match hintexp with
  | TextH _ -> idxs
  | SeqH hints -> List.fold_left collect' idxs hints
  | BrackH (_, hint, _) -> collect' idxs hint
  | HoleH { it = `Num i; _ } -> i :: idxs
  | HoleH { it = `Next; _ } -> idxs
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
  | HoleH ({ it = `Num idx; _ } as hole) ->
      let idx_realigned = List.assoc idx realign in
      HoleH { hole with it = `Num idx_realigned }
  | FuseH (hint_l, hint_r) ->
      let hint_l = realign' realign hint_l in
      let hint_r = realign' realign hint_r in
      FuseH (hint_l, hint_r)
  | _ -> hint

(* Alternation *)

let alternate ~(empty : 'd) ~(text : string -> 'd option) ~(atom : atom -> 'd)
    ~(join : 'd list -> 'd) ~(fuse : 'd -> 'd -> 'd) ~(other : exp -> 'd)
    (hint : t) (render : 'a -> 'd) (items : 'a list) : 'd =
  let rec go (hint : t) (cursor : int) : int * 'd option =
    match hint with
    | TextH str -> (cursor, text str)
    | AtomH a -> (cursor, Some (atom a))
    | SeqH hints ->
        let cursor, ds =
          List.fold_left
            (fun (cursor, acc) hint ->
              let cursor, d = go hint cursor in
              (cursor, acc @ [ Option.value ~default:empty d ]))
            (cursor, []) hints
        in
        (cursor, Some (join ds))
    | BrackH (atom_l, hint, atom_r) -> (
        let cursor, d = go hint cursor in
        let ds =
          List.filter_map Fun.id [ Some (atom atom_l); d; Some (atom atom_r) ]
        in
        (cursor, match ds with [] -> None | _ -> Some (join ds)))
    | HoleH { it = `Next; _ } ->
        (cursor + 1, Some (render (List.nth items cursor)))
    | HoleH { it = `Num idx; _ } -> (cursor, Some (render (List.nth items idx)))
    | FuseH (hint_l, hint_r) ->
        let cursor, d_l = go hint_l cursor in
        let cursor, d_r = go hint_r cursor in
        ( cursor,
          Some
            (fuse
               (Option.value ~default:empty d_l)
               (Option.value ~default:empty d_r)) )
    | OtherH hintexp -> (cursor, Some (other hintexp))
  in
  go hint 0 |> snd |> Option.value ~default:empty
