open Domain.Lib
open Lang
open Il
open Fold
open Util.Source

(* Identifier set *)

module Ids = struct
  type t = IdSet.t

  let empty = IdSet.empty
  let singleton = IdSet.singleton
  let ( + ) = IdSet.union

  (* Collect free identifiers *)

  let free_folder : (t, t, t) folder =
    {
      f_BoolE = (fun _ _ _ -> empty);
      f_NumE = (fun _ _ _ -> empty);
      f_TextE = (fun _ _ _ -> empty);
      f_VarE = (fun _ _ id -> singleton id);
      f_UnE = (fun _ _ _ _ set -> set);
      f_BinE = (fun _ _ _ _ set_l set_r -> set_l + set_r);
      f_CmpE = (fun _ _ _ _ set_l set_r -> set_l + set_r);
      f_UpCastE = (fun _ _ _ set -> set);
      f_DownCastE = (fun _ _ _ set -> set);
      f_SubE = (fun _ _ set _ -> set);
      f_MatchE = (fun _ _ set _ -> set);
      f_TupleE = (fun _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_CaseE = (fun _ _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_StrE =
        (fun _ _ set_fields ->
          set_fields |> List.map snd |> List.fold_left ( + ) empty);
      f_OptE =
        (fun _ _ set_opt ->
          match set_opt with Some set -> set | None -> empty);
      f_ListE = (fun _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_ConsE = (fun _ _ set_h set_t -> set_h + set_t);
      f_CatE = (fun _ _ set_l set_r -> set_l + set_r);
      f_MemE = (fun _ _ set_e set_s -> set_e + set_s);
      f_LenE = (fun _ _ set -> set);
      f_DotE = (fun _ _ set _ -> set);
      f_IdxE = (fun _ _ set_b set_i -> set_b + set_i);
      f_SliceE = (fun _ _ set_b set_l set_h -> set_b + set_l + set_h);
      f_UpdE = (fun _ _ set_b set_p set_f -> set_b + set_p + set_f);
      f_CallE =
        (fun _ _ _ _ sets_args -> sets_args |> List.fold_left ( + ) empty);
      f_IterE = (fun _ _ set _ -> set);
      f_RootP = (fun _ _ -> empty);
      f_IdxP = (fun _ _ set_p set_e -> set_p + set_e);
      f_SliceP = (fun _ _ set_p set_l set_h -> set_p + set_l + set_h);
      f_DotP = (fun _ _ set_p _ -> set_p);
      f_ExpA = (fun _ set -> set);
      f_DefA = (fun _ _ -> empty);
    }

  let free_exp (exp : exp) : t = fold_exp free_folder exp
  let free_arg (arg : arg) : t = fold_arg free_folder arg

  let free_args (args : arg list) : t =
    fold_args free_folder args |> List.fold_left ( + ) empty

  let free_path (path : path) : t = fold_path free_folder path
end

module Var = struct
  type t = var

  let to_string t = Il.Print.string_of_var t

  let compare (id_a, _, iters_a) (id_b, _, iters_b) =
    let compare_id (id_a : id) (id_b : id) : int = compare id_a.it id_b.it in
    let compare_iters (iters_a : iter list) (iters_b : iter list) : int =
      let compare_iter (iter_a : iter) (iter_b : iter) : int =
        match (iter_a, iter_b) with
        | Opt, Opt -> 0
        | Opt, List -> -1
        | List, Opt -> 1
        | List, List -> 0
      in
      List.compare compare_iter iters_a iters_b
    in
    match compare_id id_a id_b with
    | 0 -> compare_iters iters_a iters_b
    | n -> n

  let equal t1 t2 = compare t1 t2 = 0
end

module VarSet = struct
  include Set.Make (Var)
end

module Vars = struct
  type t = VarSet.t
  type key = Var.t

  let empty = VarSet.empty
  let singleton = VarSet.singleton
  let ( + ) = VarSet.union

  let free_folder : (t, t, t) folder =
    {
      f_BoolE = (fun _ _ _ -> empty);
      f_NumE = (fun _ _ _ -> empty);
      f_TextE = (fun _ _ _ -> empty);
      f_VarE = (fun note at id -> singleton (id, note $ at, []));
      f_UnE = (fun _ _ _ _ set -> set);
      f_BinE = (fun _ _ _ _ set_l set_r -> set_l + set_r);
      f_CmpE = (fun _ _ _ _ set_l set_r -> set_l + set_r);
      f_UpCastE = (fun _ _ _ set -> set);
      f_DownCastE = (fun _ _ _ set -> set);
      f_SubE = (fun _ _ set _ -> set);
      f_MatchE = (fun _ _ set _ -> set);
      f_TupleE = (fun _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_CaseE = (fun _ _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_StrE =
        (fun _ _ set_fields ->
          set_fields |> List.map snd |> List.fold_left ( + ) empty);
      f_OptE =
        (fun _ _ set_opt ->
          match set_opt with Some set -> set | None -> empty);
      f_ListE = (fun _ _ sets -> sets |> List.fold_left ( + ) empty);
      f_ConsE = (fun _ _ set_h set_t -> set_h + set_t);
      f_CatE = (fun _ _ set_l set_r -> set_l + set_r);
      f_MemE = (fun _ _ set_e set_s -> set_e + set_s);
      f_LenE = (fun _ _ set -> set);
      f_DotE = (fun _ _ set _ -> set);
      f_IdxE = (fun _ _ set_b set_i -> set_b + set_i);
      f_SliceE = (fun _ _ set_b set_l set_h -> set_b + set_l + set_h);
      f_UpdE = (fun _ _ set_b set_p set_f -> set_b + set_p + set_f);
      f_CallE =
        (fun _ _ _ _ sets_args -> sets_args |> List.fold_left ( + ) empty);
      f_IterE =
        (fun _ _ set_inner (iter, itervars) ->
          VarSet.map
            (fun itervar_inner ->
              if List.mem itervar_inner itervars then
                let var, typ, iters = itervar_inner in
                (var, typ, iters @ [ iter ])
              else itervar_inner)
            set_inner);
      f_RootP = (fun _ _ -> empty);
      f_IdxP = (fun _ _ set_p set_e -> set_p + set_e);
      f_SliceP = (fun _ _ set_p set_l set_h -> set_p + set_l + set_h);
      f_DotP = (fun _ _ set_p _ -> set_p);
      f_ExpA = (fun _ set -> set);
      f_DefA = (fun _ _ -> empty);
    }

  let free_exp (exp : exp) : t = fold_exp free_folder exp
  let free_arg (arg : arg) : t = fold_arg free_folder arg

  let free_args (args : arg list) : t =
    fold_args free_folder args |> List.fold_left ( + ) empty

  let free_path (path : path) : t = fold_path free_folder path
end
