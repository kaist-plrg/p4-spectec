open Lang
open Il
open Il.Print

[@@@ocamlformat "disable"]

(* Type definitions *)

type t =
  (* Type parameter *)
  | Param
  (* Extern type *)
  | Extern 
  (* Type being defined *)
  | Defining of tparam list
  (* Type that is completely defined *)
  | Defined of tparam list * deftyp
[@@@ocamlformat "enable"]

let to_string = function
  | Param -> "Param"
  | Extern -> "Extern"
  | Defining tparams -> "Defining" ^ string_of_tparams tparams
  | Defined (tparams, deftyp) ->
      "Defined " ^ string_of_tparams tparams ^ " " ^ string_of_deftyp deftyp

let get_tparams = function
  | Param | Extern -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
