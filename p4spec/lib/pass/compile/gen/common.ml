open Domain.Lib
open Lang
module Var = Runtime.Dynamic.Var
open Util.Source

(* Helpers *)

let rec is_iter_var_exp (exp : Sl.exp) : Var.t option =
  match exp.it with
  | VarE id_exp -> Some (id_exp, [])
  | IterE (exp_inner, iterexp) -> (
      match is_iter_var_exp exp_inner with
      | Some (id_var, iters_var) -> (
          match iterexp with
          | iter, [ var ] ->
              let id_iter, _, iters_iter = var in
              if Id.eq id_var id_iter && iters_var = iters_iter then
                Some (id_var, iters_var @ [ iter ])
              else None
          | _ -> None)
      | None -> None)
  | _ -> None

(* Raise an Unmatch exception *)

let raise_unmatch (msg : string) : Ml.expr =
  Ml.AppE (Ml.VarE "raise", [ Ml.AppE (Ml.VarE "Unmatch", [ Ml.StrE msg ]) ])

(* Cache helpers *)

let make_cache_key (ids_ml : Ml.id list) : Ml.expr =
  match ids_ml with
  | [] -> Ml.UnitE
  | [ id ] -> Ml.VarE id
  | ids -> Ml.TupleE (List.map (fun id -> Ml.VarE id) ids)

let make_cache_dispatcher (cache_id_ml : Ml.id) (key_ml : Ml.expr)
    (dispatch_ml : Ml.expr) : Ml.expr =
  Ml.LetE
    ( Ml.VarP "dispatch__",
      Ml.FunE ([ Ml.WildP ], dispatch_ml),
      Ml.IfE
        ( Ml.UnopE ("!", Ml.VarE "cache_enabled__"),
          Ml.LetE
            ( Ml.VarP "key__",
              key_ml,
              Ml.MatchE
                ( Ml.AppE
                    ( Ml.LitE "Hashtbl.find_opt",
                      [ Ml.VarE cache_id_ml; Ml.VarE "key__" ] ),
                  [
                    ( Ml.VariantP (`Mono ("Some", [ Ml.VarP "result__" ])),
                      Ml.VarE "result__" );
                    ( Ml.WildP,
                      Ml.LetE
                        ( Ml.VarP "cp_iface__",
                          Ml.AppE (Ml.LitE "Interface.checkpoint", [ Ml.UnitE ]),
                          Ml.LetE
                            ( Ml.VarP "cp_ext__",
                              Ml.AppE (Ml.LitE "Extern.checkpoint", [ Ml.UnitE ]),
                              Ml.LetE
                                ( Ml.VarP "result__",
                                  Ml.AppE (Ml.VarE "dispatch__", [ Ml.UnitE ]),
                                  Ml.SeqE
                                    [
                                      Ml.IfE
                                        ( Ml.BinopE
                                            ( "&&",
                                              Ml.UnopE
                                                ( "not",
                                                  Ml.AppE
                                                    ( Ml.LitE "Interface.seff",
                                                      [
                                                        Ml.VarE "cp_iface__";
                                                        Ml.AppE
                                                          ( Ml.LitE
                                                              "Interface.checkpoint",
                                                            [ Ml.UnitE ] );
                                                      ] ) ),
                                              Ml.UnopE
                                                ( "not",
                                                  Ml.AppE
                                                    ( Ml.LitE "Extern.seff",
                                                      [
                                                        Ml.VarE "cp_ext__";
                                                        Ml.AppE
                                                          ( Ml.LitE
                                                              "Extern.checkpoint",
                                                            [ Ml.UnitE ] );
                                                      ] ) ) ),
                                          Ml.AppE
                                            ( Ml.LitE "Hashtbl.replace",
                                              [
                                                Ml.VarE cache_id_ml;
                                                Ml.VarE "key__";
                                                Ml.VarE "result__";
                                              ] ),
                                          Some Ml.UnitE );
                                      Ml.VarE "result__";
                                    ] ) ) ) );
                  ] ) ),
          Some (Ml.AppE (Ml.VarE "dispatch__", [ Ml.UnitE ])) ) )
