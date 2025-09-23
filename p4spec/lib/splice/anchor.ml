(* Splice anchors within skeleton document *)

type t = Syntax of { start : string; prefix : string; suffix : string } | Rule

(* Syntax anchor *)

let syntax =
  Syntax { start = "$"; prefix = "[source,bison]\n----\n"; suffix = "\n----\n" }
