open Lang
include Sl
open Util.Source

(* Linearized representation of NL instructions,
   for the prosification pass *)

type instr = (instr', inote) note_phrase

and instr' =
  | IfI of exp * iterexp list * block * phantom option
  | HoldI of id * notexp * iterexp list * holdcase
  | CaseI of exp * case list * phantom option
  | OtherwiseI of block
  | GroupI of id * rel_signature * exp list * block
  | LetI of exp * exp * iterinstr list
  | RuleI of id * notexp * Hints.Input.t * iterinstr list
  | ResultI of rel_signature * exp list
  | ReturnI of exp

and block = instr list

and holdcase =
  | BothH of block * block
  | HoldH of block * phantom option
  | NotHoldH of block * phantom option

and case = guard * block
