open Lang
open Il
open Il.Print

(* Type definition *)

type t = Extern | Defined of tparam list * deftyp

let to_string = function
  | Extern -> "extern"
  | Defined (tparams, deftyp) ->
      string_of_tparams tparams ^ " " ^ string_of_deftyp deftyp
