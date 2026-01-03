(* Validity checks *)

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

let groupby (eq : 'a -> 'a -> bool) (xs : 'a list) : 'a list list =
  let rec groupby' acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs ->
        let same, rest = List.partition (eq x) xs in
        groupby' ((x :: same) :: acc) rest
  in
  groupby' [] xs
