open Lang

(* Syntax *)

module SyntaxId = String

module Syntax = struct
  type t =
    | ExternS of El.hint list
    | DefinedS of El.tparam list * El.deftyp * El.hint list
end

(* Relation titles *)

module RelTitleId = String

module RelTitleSource = struct
  type t =
    | ExternS of El.nottyp * El.hint list
    | DefinedS of El.nottyp * El.hint list
end

module RelTitleProse = struct
  type t = ExternP of Pl.rel_title | DefinedP of Pl.rel_title
end

(* Rule groups *)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

module RuleGroupSource = struct
  type t = El.rule list
end

module RuleGroupProse = struct
  type t = Pl.rulegroup
end

(* Function titles *)

module FuncTitleId = String

module FuncTitleSource = struct
  type t =
    | ExternS of El.tparam list * El.param list * El.plaintyp * El.hint list
    | BuiltinS of El.tparam list * El.param list * El.plaintyp * El.hint list
    | DefinedS of El.tparam list * El.param list * El.plaintyp * El.hint list
end

module FuncTitleProse = struct
  type t =
    | ExternP of Pl.func_title
    | BuiltinP of Pl.func_title
    | DefinedP of Pl.func_title
end

(* Functions *)

module FuncId = String

module FuncSource = struct
  type t = (El.tparam list * El.arg list * El.exp * El.prem list) list
end

module FuncProse = struct
  type t = Pl.func
end

(* Tables *)

module TableId = String

module TableSource = struct
  type t = El.tablerow list
end

module TableProse = struct
  type t = Pl.tablefunc
end
