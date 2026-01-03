open Util.Source

(* Alternation hints *)

type t = Hint.t

let to_string t = Format.asprintf "hint(alter %s)" (Hint.to_string t)

(* Creating hints *)

let rec validate (hint : t) (items : 'a list) : (unit, string) result =
  match validate' 0 hint items with Ok _ -> Ok () | Error msg -> Error msg

and validate' (cursor : int) (hintexp : El.exp) (items : 'a list) :
    (int, string) result =
  let ( let* ) = Result.bind in
  match hintexp.it with
  | El.TextE _ -> Ok cursor
  | El.SeqE hintexps ->
      List.fold_left
        (fun cursor_result hintexp ->
          let* cursor = cursor_result in
          validate' cursor hintexp items)
        (Ok cursor) hintexps
  | El.HoleE `Next -> Ok (cursor + 1)
  | El.HoleE (`Num idx) when idx < List.length items -> Ok cursor
  | El.HoleE (`Num idx) -> Error (Format.asprintf "index %d out of bounds" idx)
  | El.FuseE (hintexp_l, hintexp_r) ->
      let* cursor_l = validate' cursor hintexp_l items in
      let* cursor_r = validate' cursor_l hintexp_r items in
      Ok cursor_r
  | _ -> Ok cursor

(* Alternation *)

let rec alternate ?(base_text : string -> string = fun x -> x)
    ?(base_atom : El.atom -> string = El.Print.string_of_atom)
    ?(base_exp : El.exp -> string = El.Print.string_of_exp) (hint : t)
    (print : 'a -> string) (items : 'a list) : string =
  let _, result =
    alternate' ~base_text ~base_atom ~base_exp hint print items 0
  in
  result

and alternate' ?(base_text : string -> string = fun x -> x)
    ?(base_atom : El.atom -> string = El.Print.string_of_atom)
    ?(base_exp : El.exp -> string = El.Print.string_of_exp) (hintexp : El.exp)
    (print : 'a -> string) (items : 'a list) (cursor : int) : int * string =
  match hintexp.it with
  | El.TextE str -> (cursor, base_text str)
  | El.AtomE atom -> (cursor, base_atom atom)
  | El.SeqE hintexps ->
      let cursor, strs =
        List.fold_left
          (fun (cursor, strs) hintexp ->
            let cursor, str =
              alternate' ~base_text ~base_atom ~base_exp hintexp print items
                cursor
            in
            (cursor, strs @ [ str ]))
          (cursor, []) hintexps
      in
      (cursor, String.concat " " strs)
  | El.BrackE (atom_l, hintexp, atom_r) ->
      let cursor, str =
        alternate' ~base_text ~base_atom ~base_exp hintexp print items cursor
      in
      let strs =
        [
          Printf.sprintf "%s" (base_atom atom_l);
          str;
          Printf.sprintf "%s" (base_atom atom_r);
        ]
        |> List.filter (fun s -> String.length s > 0)
      in
      (cursor, String.concat " " strs)
  | El.HoleE `Next ->
      let item = List.nth items cursor in
      let str = print item in
      (cursor + 1, str)
  | El.HoleE (`Num idx) ->
      let item = List.nth items idx in
      let str = print item in
      (cursor, str)
  | El.FuseE (hintexp_l, hintexp_r) ->
      let cursor, str_l =
        alternate' ~base_text ~base_atom ~base_exp hintexp_l print items cursor
      in
      let cursor, str_r =
        alternate' ~base_text ~base_atom ~base_exp hintexp_r print items cursor
      in
      (cursor, str_l ^ str_r)
  | _ -> (cursor, El.Print.string_of_exp hintexp)

(* Re-alignment of alternation indices *)

let rec collect (hint : t) : int list = collect' [] hint

and collect' (idxs : int list) (hintexp : t) : int list =
  match hintexp.it with
  | El.TextE _ -> idxs
  | El.SeqE hintexps -> List.fold_left collect' idxs hintexps
  | El.HoleE (`Num i) -> i :: idxs
  | El.HoleE `Next -> idxs
  | El.FuseE (hintexp_l, hintexp_r) ->
      let idxs = collect' idxs hintexp_l in
      collect' idxs hintexp_r
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

and realign' (realign : (int * int) list) (hintexp : t) : t =
  match hintexp.it with
  | El.SeqE hintexps ->
      let hintexps = List.map (realign' realign) hintexps in
      El.SeqE hintexps $ hintexp.at
  | El.HoleE (`Num idx) ->
      let idx_realigned = List.assoc idx realign in
      El.HoleE (`Num idx_realigned) $ hintexp.at
  | El.FuseE (hintexp_l, hintexp_r) ->
      let hintexp_l = realign' realign hintexp_l in
      let hintexp_r = realign' realign hintexp_r in
      El.FuseE (hintexp_l, hintexp_r) $ hintexp.at
  | _ -> hintexp
