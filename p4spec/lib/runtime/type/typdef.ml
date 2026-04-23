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
  | Defined of tparam list * [
      | `Plain of typ
      | `Struct of typfield list
      | `Variant of typcase list * Mixop.t list
    ]
[@@@ocamlformat "enable"]

let to_string = function
  | Param -> "Param"
  | Extern -> "Extern"
  | Defining tparams -> "Defining" ^ string_of_tparams tparams
  | Defined (tparams, typdef) ->
      let deftyp_str =
        match typdef with
        | `Plain typ -> string_of_typ typ
        | `Struct typfields ->
            "{ "
            ^ String.concat ", "
                (List.map
                   (fun (atom, typ) ->
                     string_of_atom atom ^ ": " ^ string_of_typ typ)
                   typfields)
            ^ " }"
        | `Variant (typcases, _) ->
            "| "
            ^ String.concat " | "
                (List.map
                   (fun (nottyp, _, _) -> string_of_nottyp nottyp)
                   typcases)
      in
      "Defined" ^ string_of_tparams tparams ^ " = " ^ deftyp_str

let of_deftyp (tparams : tparam list) (deftyp : deftyp) : t =
  match deftyp.it with
  | PlainT typ -> Defined (tparams, `Plain typ)
  | StructT typfields -> Defined (tparams, `Struct typfields)
  | VariantT typcases -> Defined (tparams, `Variant (typcases, []))

let get_tparams = function
  | Param | Extern -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
