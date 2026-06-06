open Lang
module Typ = Runtime.Type.Typ

(* Collect all monomorphic types used in the spec *)

let collect_types (_ctx : Ctx.t) (_spec : Sl.spec) : Typ.t list = assert false

(* Compiling marshal interface *)

module Marshal = struct
  let compile (_ctx : Ctx.t) (_typ : Typ.t) : Ml.funcdef = assert false
end

(* Compiling unmarshal interface *)

module Unmarshal = struct
  let compile (_ctx : Ctx.t) (_typ : Typ.t) : Ml.funcdef = assert false
end

(* Entry point *)

let compile (ctx : Ctx.t) (spec : Sl.spec) : Ml.funcdef list * Ml.funcdef list =
  let typs = collect_types ctx spec in
  let funcdefs_marshal_ml = typs |> List.map (Marshal.compile ctx) in
  let funcdefs_unmarshal_ml = typs |> List.map (Unmarshal.compile ctx) in
  (funcdefs_marshal_ml, funcdefs_unmarshal_ml)
