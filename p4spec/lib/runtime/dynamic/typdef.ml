open Lang
open Il
open Il.Print

[@@@ocamlformat "disable"]

(* Type definition *)

type t =
  (* Type parameter *)
  | Param
  (* Extern type *)
  | Extern
  (* Defined type *)
  | Defined of tparam list * deftyp
[@@@ocamlformat "enable"]

let to_string = function
  | Param -> "Param"
  | Extern -> "Extern"
  | Defined (tparams, deftyp) ->
      "Defined " ^ string_of_tparams tparams ^ " " ^ string_of_deftyp deftyp
