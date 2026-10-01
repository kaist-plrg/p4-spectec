open Source

(* Backtracking *)

type failtrace = Failtrace of region * (unit -> string) * failtrace list
type 'a attempt = Ok of 'a | Fail of failtrace list

(* Failures *)

let rec depth_of (failtrace : failtrace) : int =
  let (Failtrace (_, _, subfailtraces)) = failtrace in
  let depth_sub = List.map depth_of subfailtraces |> List.fold_left max 0 in
  depth_sub + 1

let fail (at : region) (msg : string) : 'a attempt =
  Fail [ Failtrace (at, (fun () -> msg), []) ]

let fail_silent : 'a attempt = Fail []

(* Choosing between attempts *)

let rec choose_sequential = function
  | [] -> fail_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Fail failtraces_h -> (
          match choose_sequential fs with
          | Ok a -> Ok a
          | Fail failtraces_t -> Fail (failtraces_h @ failtraces_t)))

(* Nesting attempts *)

let nest at msg attempt =
  match attempt with
  | Ok a -> Ok a
  | Fail failtraces -> Fail [ Failtrace (at, (fun () -> msg), failtraces) ]
