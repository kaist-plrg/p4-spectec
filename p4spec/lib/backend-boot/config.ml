module Run = Runtime.Dynamic_Runner.Signature

(* Kind of interface, currently offering three: P4, SL, and IL *)

type interface = P4_interface | IL_interface | SL_interface

(* Layers *)

type layer = { specdir : string; rel : string }
type target = { includes : string list; path : string }

(* Tower is an alternation of a layer and an interface *)

type level = { layer : layer; interface : interface }

type tower = {
  mode : Run.mode;
  level_boot : level;
  levels_interm : level list;
  level_target : level;
  target : target;
}
