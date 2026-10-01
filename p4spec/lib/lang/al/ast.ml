open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.num [@@deriving yojson]

(* Texts *)

type text = Il.text [@@deriving yojson]

(* Identifiers *)

type id = Il.id [@@deriving yojson]
type id' = Il.id' [@@deriving yojson]

(* Atoms *)

type atom = Il.atom [@@deriving yojson]
type atom' = Il.atom' [@@deriving yojson]

(* Mixfix operators *)

type mixop = Il.mixop [@@deriving yojson]

(* Iterators *)

type iter = Il.iter [@@deriving yojson]

(* Variables *)

type var = Il.var [@@deriving yojson]

(* Types *)

type typ = Il.typ [@@deriving yojson]
type typ' = Il.typ' [@@deriving yojson]

type nottyp = Il.nottyp [@@deriving yojson]
type nottyp' = Il.nottyp' [@@deriving yojson]

type deftyp = Il.deftyp [@@deriving yojson]
type deftyp' = Il.deftyp' [@@deriving yojson]

type typfield = Il.typfield [@@deriving yojson]
type typcase = Il.typcase [@@deriving yojson]

(* Values *)

type vid = Il.vid
type vnote = Il.vnote [@@deriving yojson]

type value = Il.value [@@deriving yojson]
type value' = Il.value' [@@deriving yojson]

type valuefield = Il.valuefield [@@deriving yojson]
type valuecase = Il.valuecase [@@deriving yojson]

(* Operators *)

type numop = Il.numop [@@deriving yojson]
type unop = Il.unop [@@deriving yojson]
type binop = Il.binop [@@deriving yojson]
type cmpop = Il.cmpop [@@deriving yojson]
type optyp = Il.optyp [@@deriving yojson]

(* Subtype checks *)

type subcheck = Il.subcheck [@@deriving yojson]

(* Expressions *)

type exp = Il.exp [@@deriving yojson]
type exp' = Il.exp' [@@deriving yojson]

type notexp = Il.notexp [@@deriving yojson]
type iterexp = Il.iterexp [@@deriving yojson]

(* Patterns *)

type pattern = Il.pattern [@@deriving yojson]

(* Path *)

type path = Il.path [@@deriving yojson]
type path' = Il.path' [@@deriving yojson]

(* Parameters *)

type param = Il.param [@@deriving yojson]
type param' = Il.param' [@@deriving yojson]

(* Type parameters *)

type tparam = Il.tparam [@@deriving yojson]
type tparam' = Il.tparam' [@@deriving yojson]

(* Arguments *)

type arg = Il.arg [@@deriving yojson]
type arg' = Il.arg' [@@deriving yojson]

(* Type arguments *)

type targ = Il.targ [@@deriving yojson]
type targ' = Il.targ' [@@deriving yojson]

(* Premises *)

type prem = Il.prem [@@deriving yojson]
type prem' = Il.prem' [@@deriving yojson]

type iterprem = Il.iterprem [@@deriving yojson]

(* Rules *)

and rulematch = exp list * exp list * prem list
and rulepath = id * prem list * exp list

and rulegroup = rulegroup' phrase
and rulegroup' = id * rulematch * rulepath list

and elsegroup = elsegroup' phrase
and elsegroup' = id * rulematch * rulepath

(* Clauses *)

type clause = Il.clause
type clause' = Il.clause'

type elseclause = Il.elseclause
type elseclause' = Il.elseclause'

(* Table rows *)

and tablerow = tablerow' phrase
and tablerow' = exp list * arg list * exp * prem list

(* Hints *)

type hint = El.hint

(* Definitions *)

type def = def' phrase
and def' =
  (* `extern` `syntax` id hint* *)
  | ExternTypD of id * hint list
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp hint* *)
  | TypD of id * tparam list * deftyp * hint list
  (* `var` id `:` typ hint* *)
  | VarD of id * typ * hint list
  (* `extern` `relation` id `:` nottyp `hint(input` `%`int* `)` hint* *)
  | ExternRelD of id * nottyp * Hints.Input.t * hint list
  (* `relation` id `:` nottyp `hint(input` `%`int* `)` rulegroup* hint* *)
  | RelD of id * nottyp * Hints.Input.t * rulegroup list * elsegroup option * hint list
  (* `extern` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | ExternDecD of id * tparam list * param list * typ * hint list
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | BuiltinDecD of id * tparam list * param list * typ * hint list
  (* `table` `dec` id list(param, `,`) `:` typ hint* *)
  | TableDecD of id * param list * typ * tablerow list * hint list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* hint* *)
  | FuncDecD of id * tparam list * param list * typ * clause list * elseclause option * hint list

(* Spec *)

type spec = def list
