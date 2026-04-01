open Lang
open Sl

(* Relation *)

type t =
  | Extern of rel_signature
  | Defined of rel_signature * exp list * block * elseblock option

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"

let get_signature = function
  | Extern rel_signature -> rel_signature
  | Defined (rel_signature, _, _, _) -> rel_signature
