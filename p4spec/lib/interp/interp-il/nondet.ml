open Backtrack

(* Deterministic backtracking *)

type failtrace = Util.Attempt.failtrace

type ('a, 'b) backtrack_det =
  | Ok_det of 'a
  | Err_det of failtrace list
  | Unmatch_det of failtrace list
  | Nondet_det of 'b * 'b

(* Conversion *)

let as_det (backtrack : 'a backtrack) : ('a, 'b) backtrack_det =
  match backtrack with
  | Ok a -> Ok_det a
  | Err failtraces -> Err_det failtraces
  | Unmatch failtraces -> Unmatch_det failtraces

(* Backtracking *)

let back_unmatch_silent : ('a, 'b) backtrack_det = Unmatch_det []

(* Choice (deterministic) *)

let choose_deterministic (items : 'b list) fs =
  let choose_deterministic (items : 'b list) fs =
    List.fold_left2
      (fun backtrack_det item f ->
        match backtrack_det with
        | Ok_det (item_det, a_det) -> (
            match f () with
            | Ok _ -> Nondet_det (item_det, item)
            | Err failtraces -> Err_det failtraces
            | Unmatch _ -> Ok_det (item_det, a_det))
        | Err_det failtraces_det -> Err_det failtraces_det
        | Unmatch_det failtraces_det -> (
            match f () with
            | Ok a -> Ok_det (item, a)
            | Err failtraces -> Err_det failtraces
            | Unmatch failtraces -> Unmatch_det (failtraces_det @ failtraces))
        | Nondet_det _ -> backtrack_det)
      back_unmatch_silent items fs
  in
  match choose_deterministic items fs with
  | Ok_det (_, a_det) -> Ok_det a_det
  | Err_det failtraces -> Err_det failtraces
  | Unmatch_det failtraces -> Unmatch_det failtraces
  | Nondet_det (item_a, item_b) -> Nondet_det (item_a, item_b)
