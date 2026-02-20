include Util.Attempt
open Error
open Util.Source

(* Monadic interface *)

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with
  | Ok a -> f a
  | Fail failtraces -> error no_region (string_of_failtraces_short failtraces)
