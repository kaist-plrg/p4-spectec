open Domain
open Lib
open Lang
open Xl
open Ol.Ast
open Ol.Eq
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Util.Source

(* [6] Condition analysis and case analysis insertion *)

(* Syntactic analysis of conditions

   Note that this is best-effort analysis,
   since even semantic analysis cannot guarantee completeness of the analysis *)

(* Conversion between guard and conditional expression *)

let exp_as_guard (exp_target : exp) (exp_cond : exp) : guard option =
  match exp_cond.it with
  | UnE (`NotOp, _, exp) when eq_exp exp_target exp -> Some (BoolG false)
  | CmpE (`EqOp, optyp, exp_l, exp_r) when eq_exp exp_target exp_l ->
      Some (CmpG (`EqOp, optyp, exp_r))
  | CmpE (`EqOp, optyp, exp_l, exp_r) when eq_exp exp_target exp_r ->
      Some (CmpG (`EqOp, optyp, exp_l))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when eq_exp exp_target exp_l ->
      Some (CmpG (`NeOp, optyp, exp_r))
  | CmpE (`NeOp, optyp, exp_l, exp_r) when eq_exp exp_target exp_r ->
      Some (CmpG (`NeOp, optyp, exp_l))
  | SubE (exp, typ) when eq_exp exp_target exp -> Some (SubG typ)
  | MatchE (exp, pattern) when eq_exp exp_target exp -> Some (MatchG pattern)
  | MemE (exp_e, exp_s) when eq_exp exp_target exp_e -> Some (MemG exp_s)
  | _ -> None

let guard_as_exp (exp_target : exp) (guard : guard) : exp =
  match guard with
  | BoolG true -> exp_target
  | BoolG false ->
      Il.UnE (`NotOp, `BoolT, exp_target) $$ (exp_target.at, Il.BoolT)
  | CmpG (cmpop, optyp, exp) ->
      Il.CmpE (cmpop, optyp, exp_target, exp) $$ (exp_target.at, Il.BoolT)
  | SubG typ -> Il.SubE (exp_target, typ) $$ (exp_target.at, Il.BoolT)
  | MatchG pattern ->
      Il.MatchE (exp_target, pattern) $$ (exp_target.at, Il.BoolT)
  | MemG exp -> Il.MemE (exp_target, exp) $$ (exp_target.at, Il.BoolT)

(* Conversion from type to its variants *)

let typ_as_variant (tdenv : TDEnv.t) (typ : typ) : mixop list option =
  let typ_unrolled = TDEnv.unroll tdenv typ in
  match typ_unrolled.it with
  | VarT (tid, _) -> (
      let td = TDEnv.find tid tdenv in
      match td with
      | Param | Extern -> None
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | VariantT typcases ->
              let mixops =
                typcases |> List.map fst |> List.map it |> List.map fst
              in
              Some mixops
          | _ -> None))
  | _ -> None

(* Determine the overlapping guard of two conditions *)

type overlap =
  | Identical
  | Disjoint of exp * guard * guard
  | Partition of exp * guard * guard
  | Fuzzy

let partition_exp_literal (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a <> b_b
  | _ -> false

let rec disjoint_exp_literal (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.it, exp_b.it) with
  | BoolE b_a, BoolE b_b -> b_a <> b_b
  | NumE n_a, NumE n_b -> not (Num.eq n_a n_b)
  | TextE t_a, TextE t_b -> t_a <> t_b
  | UpCastE (typ_a, exp_a), UpCastE (typ_b, exp_b) when eq_typ typ_a typ_b ->
      disjoint_exp_literal exp_a exp_b
  | TupleE exps_a, TupleE exps_b ->
      assert (List.length exps_a = List.length exps_b);
      List.exists2 disjoint_exp_literal exps_a exps_b
  | CaseE (mixop_a, exps_a), CaseE (mixop_b, exps_b) ->
      if Mixop.eq mixop_a mixop_b then
        List.exists2 disjoint_exp_literal exps_a exps_b
      else true
  | ListE exps_a, ListE exps_b when List.length exps_a = List.length exps_b ->
      List.exists2 disjoint_exp_literal exps_a exps_b
  | ListE _, ListE _ -> true
  | _ -> false

let overlap_typ (tdenv : TDEnv.t) (exp : exp) (typ_a : typ) (typ_b : typ) :
    overlap =
  match (typ_as_variant tdenv typ_a, typ_as_variant tdenv typ_b) with
  | Some mixops_a, Some mixops_b ->
      let mixops_a = MixIdSet.of_list mixops_a in
      let mixops_b = MixIdSet.of_list mixops_b in
      if MixIdSet.eq mixops_a mixops_b then Identical
      else if MixIdSet.inter mixops_a mixops_b |> MixIdSet.is_empty then
        Disjoint (exp, SubG typ_a, SubG typ_b)
      else Fuzzy
  | _ -> Fuzzy

let rec overlap_pattern (exp : exp) (pattern_a : pattern) (pattern_b : pattern)
    : overlap =
  let guard_a = MatchG pattern_a in
  let guard_b = MatchG pattern_b in
  let overlap_pattern_unequal () : overlap =
    match (pattern_a, pattern_b) with
    | CaseP _, CaseP _ -> Disjoint (exp, guard_a, guard_b)
    | ListP `Cons, ListP (`Fixed n) | ListP (`Fixed n), ListP `Cons ->
        if n = 0 then Partition (exp, guard_a, guard_b)
        else Disjoint (exp, guard_a, guard_b)
    | ListP `Cons, ListP `Nil | ListP `Nil, ListP `Cons ->
        Partition (exp, guard_a, guard_b)
    | ListP (`Fixed _), ListP (`Fixed _) -> Disjoint (exp, guard_a, guard_b)
    | ListP (`Fixed n), ListP `Nil | ListP `Nil, ListP (`Fixed n) ->
        if n = 0 then Identical else Disjoint (exp, guard_a, guard_b)
    | OptP `Some, OptP `None | OptP `None, OptP `Some ->
        Partition (exp, guard_a, guard_b)
    | _ -> Fuzzy
  in
  if eq_pattern pattern_a pattern_b then Identical
  else overlap_pattern_unequal ()

and overlap_typ_and_pattern (tdenv : TDEnv.t) (exp : exp) (typ : typ)
    (pattern : pattern) : overlap =
  match pattern with
  | CaseP mixop -> (
      match typ_as_variant tdenv typ with
      | Some mixops ->
          let mixops = MixIdSet.of_list mixops in
          if MixIdSet.mem mixop mixops then Fuzzy
          else Disjoint (exp, SubG typ, MatchG pattern)
      | None -> Fuzzy)
  | _ -> Fuzzy

and overlap_pattern_and_typ (tdenv : TDEnv.t) (exp : exp) (pattern : pattern)
    (typ : typ) : overlap =
  match pattern with
  | CaseP mixop -> (
      match typ_as_variant tdenv typ with
      | Some mixops ->
          let mixops = MixIdSet.of_list mixops in
          if MixIdSet.mem mixop mixops then Fuzzy
          else Disjoint (exp, MatchG pattern, SubG typ)
      | None -> Fuzzy)
  | _ -> Fuzzy

and overlap_exp (tdenv : TDEnv.t) (exp_a : exp) (exp_b : exp) : overlap =
  let overlap_exp_unequal () : overlap =
    match (exp_a.it, exp_b.it) with
    (* Negation *)
    | UnE (`NotOp, _, exp_a), _ when eq_exp exp_a exp_b ->
        Partition (exp_a, BoolG false, BoolG true)
    | _, UnE (`NotOp, _, exp_b) when eq_exp exp_a exp_b ->
        Partition (exp_b, BoolG true, BoolG false)
    (* Equals literal *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_l
           && partition_exp_literal exp_a_r exp_b_r ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_r
           && partition_exp_literal exp_a_r exp_b_l ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_l) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_l
           && disjoint_exp_literal exp_a_r exp_b_r ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`EqOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_r
           && disjoint_exp_literal exp_a_r exp_b_l ->
        Disjoint
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`EqOp, optyp_b, exp_b_l) )
    (* Equals and not equals *)
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_l && eq_exp exp_a_r exp_b_r
      ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_r) )
    | ( CmpE (`EqOp, optyp_a, exp_a_l, exp_a_r),
        CmpE (`NeOp, optyp_b, exp_b_l, exp_b_r) )
      when optyp_a = optyp_b && eq_exp exp_a_l exp_b_r && eq_exp exp_a_r exp_b_l
      ->
        Partition
          ( exp_a_l,
            CmpG (`EqOp, optyp_a, exp_a_r),
            CmpG (`NeOp, optyp_b, exp_b_l) )
    (* Subtyping *)
    | SubE (exp_a, typ_a), SubE (exp_b, typ_b) when eq_exp exp_a exp_b ->
        overlap_typ tdenv exp_a typ_a typ_b
    (* Match on patterns *)
    | MatchE (exp_a, pattern_a), MatchE (exp_b, pattern_b)
      when eq_exp exp_a exp_b ->
        overlap_pattern exp_a pattern_a pattern_b
    (* Subtyping and match on patterns *)
    | SubE (exp_a, typ_a), MatchE (exp_b, pattern_b) when eq_exp exp_a exp_b ->
        overlap_typ_and_pattern tdenv exp_a typ_a pattern_b
    | MatchE (exp_a, pattern_a), SubE (exp_b, typ_b) when eq_exp exp_a exp_b ->
        overlap_pattern_and_typ tdenv exp_a pattern_a typ_b
    (* Membership on literals *)
    | ( MemE (exp_e_a, ({ it = ListE exps_s_a; _ } as exp_s_a)),
        MemE (exp_e_b, ({ it = ListE exps_s_b; _ } as exp_s_b)) )
      when eq_exp exp_e_a exp_e_b
           && List.for_all
                (fun exp_s_a ->
                  List.for_all (disjoint_exp_literal exp_s_a) exps_s_b)
                exps_s_a ->
        Disjoint (exp_e_a, MemG exp_s_a, MemG exp_s_b)
    | _ -> Fuzzy
  in
  if eq_exp exp_a exp_b then Identical else overlap_exp_unequal ()

let overlap_guard (tdenv : TDEnv.t) (exp : exp) (guard_a : guard)
    (guard_b : guard) : overlap =
  let exp_a = guard_as_exp exp guard_a in
  let exp_b = guard_as_exp exp guard_b in
  overlap_exp tdenv exp_a exp_b
