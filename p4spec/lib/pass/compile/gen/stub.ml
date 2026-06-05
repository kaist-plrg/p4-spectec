open Lang
open Util.Source

(* Creating stub SpecTec variables *)

module SpecTec = struct
  let var (id : string) (typ : Sl.typ) : Sl.exp =
    let id = id $ no_region in
    Il.VarE id $$ (no_region, typ.it)
end

(* Creating stub OCaml variables *)

module OCaml = struct
  (* Variables *)

  let var (ctx : Ctx.t) (id : string) : Ctx.t * Ml.id =
    let id_ml = Ctx.fresh ctx id in
    let ctx = Ctx.add_binding ctx (id $ no_region, []) id_ml in
    (ctx, id_ml)

  let vars (ctx : Ctx.t) (id : string) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> id ^ string_of_int idx)
    |> List.fold_left
         (fun (ctx, ids_ml) id ->
           let ctx, id_ml = var ctx id in
           (ctx, ids_ml @ [ id_ml ]))
         (ctx, [])

  (* Iterators *)

  let iterator ~(prefix : string) (ctx : Ctx.t) (vars : Sl.var list) :
      Ctx.t * Ml.id list =
    let ids =
      List.init (List.length vars) (fun idx -> prefix ^ string_of_int idx)
    in
    List.fold_left2
      (fun (ctx, ids_ml) id (id_var, _, iters_var) ->
        let id_ml = Ctx.fresh ctx id in
        let ctx = Ctx.add_binding ctx (id_var, iters_var) id_ml in
        (ctx, ids_ml @ [ id_ml ]))
      (ctx, []) ids vars
end
