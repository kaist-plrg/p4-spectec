module Run = Runtime.Dynamic_Runner.Signature
open Util.Attempt
open Util.Source

(* Backtracking *)

type 'a backtrack =
  | Ok of 'a
  | Err of failtrace list
  | Unmatch of failtrace list

(* Backtracing *)

let back_err (at : region) (msg : string) : 'a backtrack =
  Err [ Failtrace (at, (fun () -> msg), []) ]

let back_unmatch_silent : 'a backtrack = Unmatch []

let back_unmatch (at : region) (msg : string) : 'a backtrack =
  Unmatch [ Failtrace (at, (fun () -> msg), []) ]

let back_unmatch_of_failure (failure : Run.failure) : 'a backtrack =
  match failure with
  | Run.Abort _ -> raise (Run.ExternError failure)
  | Run.Unmatch failtraces -> Unmatch failtraces

let back_nest (at : region) (msg : unit -> string) (backtrack : 'a backtrack) :
    'a backtrack =
  match backtrack with
  | Ok a -> Ok a
  | Err failtraces -> Err [ Failtrace (at, msg, failtraces) ]
  | Unmatch failtraces -> Unmatch [ Failtrace (at, msg, failtraces) ]

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
