open Lang

(* Syntax *)

module SyntaxId = String

module Syntax = struct
  type source =
    | ExternS of El.hint list
    | DefinedS of El.tparam list * El.deftyp * El.hint list
end

(* Relation titles *)

module RelTitleId = String

module RelTitle = struct
  type source =
    | ExternS of El.nottyp * El.hint list
    | DefinedS of El.nottyp * El.hint list

  type prose = ExternP of Pl.rel_title | DefinedP of Pl.rel_title
end

(* Rule groups *)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

module RuleGroup = struct
  type source = El.rule list
  type prose = Pl.rulegroup
end

(* Function titles *)

module FuncTitleId = String

module FuncTitle = struct
  type source =
    | ExternS of El.tparam list * El.param list * El.plaintyp * El.hint list
    | BuiltinS of El.tparam list * El.param list * El.plaintyp * El.hint list
    | DefinedS of El.tparam list * El.param list * El.plaintyp * El.hint list

  type prose =
    | ExternP of Pl.func_title
    | BuiltinP of Pl.func_title
    | DefinedP of Pl.func_title
end

(* Functions *)

module FuncId = String

module Func = struct
  type source = (El.tparam list * El.arg list * El.exp * El.prem list) list
  type prose = Pl.func
end

(* Tables *)

module TableId = String

module Table = struct
  type source = El.tablerow list
  type prose = Pl.tablefunc
end
