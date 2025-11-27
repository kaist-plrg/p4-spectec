open Util.Backtrace

(* Shorthand for extracting type arguments and values *)

let zero at = function [] -> () | _ -> back at "arity mismatch"
let one at = function [ a ] -> a | _ -> back at "arity mismatch"
let two at = function [ a; b ] -> (a, b) | _ -> back at "arity mismatch"

let three at = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> back at "arity mismatch"
