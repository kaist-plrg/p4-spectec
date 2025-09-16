(* Boolens *)

type t = [ `BoolT ] [@@deriving yojson]
type typ = [ `BoolT ] [@@deriving yojson]

(* Operations *)

type unop = [ `NotOp ] [@@deriving yojson]
type binop = [ `AndOp | `OrOp | `ImplOp | `EquivOp ] [@@deriving yojson]
type cmpop = [ `EqOp | `NeOp ] [@@deriving yojson]

(* Stringifiers *)

let string_of_bool = string_of_bool
let string_of_unop = function `NotOp -> "~"

let string_of_binop = function
  | `AndOp -> "/\\"
  | `OrOp -> "\\/"
  | `ImplOp -> "=>"
  | `EquivOp -> "<=>"

let string_of_cmpop = function `EqOp -> "=" | `NeOp -> "=/="
