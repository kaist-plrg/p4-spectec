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
  let create (ctx : Ctx.t) (ids_ml : Ml.id list) : Ctx.t * Ml.id list =
    List.fold_left
      (fun (ctx, ids_ml) id_ml ->
        let ctx, id_ml = Ctx.fresh ctx id_ml in
        (ctx, ids_ml @ [ id_ml ]))
      (ctx, []) ids_ml

  let var (ctx : Ctx.t) (id : string) : Ctx.t * Ml.id = Ctx.fresh ctx id

  let slice (ctx : Ctx.t) : Ctx.t * Ml.id =
    let id_ml = "slice__" in
    Ctx.fresh ctx id_ml

  let tuple (ctx : Ctx.t) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> "tuple__" ^ string_of_int idx) |> create ctx

  let case (ctx : Ctx.t) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> "payload__" ^ string_of_int idx) |> create ctx

  let opt (ctx : Ctx.t) : Ctx.t * Ml.id =
    let id_ml = "opt__" in
    Ctx.fresh ctx id_ml

  let list (ctx : Ctx.t) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> "list__" ^ string_of_int idx) |> create ctx

  let cons (ctx : Ctx.t) : Ctx.t * Ml.id * Ml.id =
    let id_h_ml = "cons__h" in
    let ctx, id_h_ml = Ctx.fresh ctx id_h_ml in
    let id_t_ml = "cons__t" in
    let ctx, id_t_ml = Ctx.fresh ctx id_t_ml in
    (ctx, id_h_ml, id_t_ml)

  let iter_opt (ctx : Ctx.t) : Ctx.t * Ml.id =
    let id_ml = "iter__opt" in
    Ctx.fresh ctx id_ml

  let iter_opts (ctx : Ctx.t) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> "iter__opt" ^ string_of_int idx) |> create ctx

  let iter_list (ctx : Ctx.t) : Ctx.t * Ml.id =
    let id_ml = "iter__list" in
    Ctx.fresh ctx id_ml

  let iter_lists (ctx : Ctx.t) (n : int) : Ctx.t * Ml.id list =
    List.init n (fun idx -> "iter__list" ^ string_of_int idx) |> create ctx
end
