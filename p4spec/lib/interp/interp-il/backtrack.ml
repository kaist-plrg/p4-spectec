open Error
open Util.Attempt
open Util.Source

(* Backtracking *)

type 'a backtrack =
  | Ok of 'a
  | Err of failtrace list
  | Unmatch of failtrace list

(* Backtracing *)

let back_err (at : region) (msg : string) : 'a backtrack =
  Err [ Failtrace (at, msg, []) ]

let back_unmatch_silent : 'a backtrack = Unmatch []

let back_unmatch (at : region) (msg : string) : 'a backtrack =
  Unmatch [ Failtrace (at, msg, []) ]

let back_nest (at : region) (msg : string) (backtrack : 'a backtrack) :
    'a backtrack =
  match backtrack with
  | Ok a -> Ok a
  | Err failtraces ->
      let failtrace = Failtrace (at, msg, failtraces) in
      Err [ failtrace ]
  | Unmatch failtraces ->
      let failtrace = Failtrace (at, msg, failtraces) in
      Unmatch [ failtrace ]

(* Check *)

let check_back_err (b : bool) (at : region) (msg : string) : unit backtrack =
  if b then Ok () else back_err at msg

(* Choose (sequential) *)

let rec choose_sequential = function
  | [] -> back_unmatch_silent
  | f :: fs -> (
      match f () with
      | Ok a -> Ok a
      | Err _ as backtrack -> backtrack
      | Unmatch failtraces -> (
          match choose_sequential fs with
          | Ok a -> Ok a
          | Err _ as backtrack -> backtrack
          | Unmatch failtraces_t -> Unmatch (failtraces @ failtraces_t)))

(* Monadic interface *)

let ( let* ) (backtrack : 'a backtrack) (f : 'a -> 'b) : 'b =
  match backtrack with
  | Ok a -> f a
  | Err _ as backtrack -> backtrack
  | Unmatch _ as backtrack -> backtrack

let ( let+ ) (backtrack : 'a backtrack) (f : 'a -> 'b) : 'b =
  match backtrack with
  | Ok a -> f a
  | Err failtraces | Unmatch failtraces ->
      error no_region (string_of_failtraces_short failtraces)
