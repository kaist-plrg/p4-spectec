open Lang
include Sl
open Util.Source

(* Intermediate representation of SL instructions,
   for the optimization pass *)

type case = guard * block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp

and instr = instr' phrase

and instr' =
  | IfI of exp * iterexp list * block
  | HoldI of id * notexp * iterexp list * block * block
  | CaseI of exp * case list * bool
  | GroupI of id * rel_signature * exp list * block
  | LetI of exp * exp * iterinstr list * block
  | RuleI of id * notexp * Hints.Input.t * iterinstr list * block
  | ResultI of rel_signature * exp list
  | ReturnI of exp
  | DebugI of exp

and block = instr list
and elseblock = block
