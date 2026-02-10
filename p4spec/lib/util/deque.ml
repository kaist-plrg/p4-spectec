(* Deque implementation with a single list *)

type 'a t = 'a list [@@deriving yojson]

let empty = []
let is_empty = List.is_empty
let push_back (x : 'a) (q : 'a t) : 'a t = q @ [ x ]
let push_front : 'a -> 'a t -> 'a t = List.cons

let pop_back_opt (q : 'a t) =
  match List.rev q with hd :: tl -> Some (hd, List.rev tl) | [] -> None

let pop_back (q : 'a t) = q |> pop_back_opt |> Option.get

let pop_front_opt (q : 'a t) =
  match q with hd :: tl -> Some (hd, tl) | [] -> None

let pop_front (q : 'a t) = q |> pop_front_opt |> Option.get
