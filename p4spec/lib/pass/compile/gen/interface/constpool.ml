open Domain
module SMap = Map.Make (String)

(* Constant pool for hoisting marshal templates out of function bodies *)

type t = {
  ctr : int;
  consts : (string * Ml.expr) list;
  mixops : string SMap.t;
  typs : string SMap.t;
}

let empty : t = { ctr = 0; consts = []; mixops = SMap.empty; typs = SMap.empty }

let intern_mixop (pool : t) (mixop : Mixop.t) : t * string =
  let key = Dynamic_gen.mixop_lit mixop in
  match SMap.find_opt key pool.mixops with
  | Some n -> (pool, n)
  | None ->
      let n = Printf.sprintf "_mo_%d_" pool.ctr in
      let pool =
        {
          pool with
          ctr = pool.ctr + 1;
          consts = (n, Dynamic_gen.mixop_expr mixop) :: pool.consts;
          mixops = SMap.add key n pool.mixops;
        }
      in
      (pool, n)

let intern_typ (pool : t) (key : string) (expr : Ml.expr) : t * string =
  match SMap.find_opt key pool.typs with
  | Some n -> (pool, n)
  | None ->
      let n = "_ty_" ^ key ^ "_" in
      let pool =
        {
          pool with
          consts = (n, expr) :: pool.consts;
          typs = SMap.add key n pool.typs;
        }
      in
      (pool, n)
