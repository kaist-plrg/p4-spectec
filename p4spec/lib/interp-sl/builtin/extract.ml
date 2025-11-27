(* Shorthand for extracting type arguments and values *)

let zero at = function [] -> () | _ -> Backtrace.error at "arity mismatch"
let one at = function [ a ] -> a | _ -> Backtrace.error at "arity mismatch"
let two at = function [ a; b ] -> (a, b) | _ -> Backtrace.error at "arity mismatch"

let three at = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> Backtrace.error at "arity mismatch"
