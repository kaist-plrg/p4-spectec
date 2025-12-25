open Lang
open Il

(* Reasons *)

type t =
  (*
     Cannot determine the root cause of failure.
  *)
  | Unknown
  (*
    Any leaf node that isn't a mismatch. A stronger candidate to be the real cause for failure
    Argument: index # of the clause/rule that caused the failure
  *)
  | Root of int
  (*
    This leaf node is a possible "mismatch", i.e. the failure is caused by one of any guard patterns
    in the beginning of a clause/rule.
    Argument: index # of the clause/rule that caused the failure
  *)
  | Mismatch of int
  (*
    One of its children is a root cause node.
    Argument 1: index # of the clause/rule that caused the failure
    Argument 2: execution depth (index # of clause/rule) of the child node referenced by Arg 1
  *)
  | RootParent of int * int
  (*
    All of its children failed because of a mismatch.
    Argument 1: index # of the clause/rule that likely caused the failure
    Argument 2: execution depth (index # of clause/rule) of the child node referenced by Arg 1
  *)
  | MismatchParent of int * int

(* Guessing reason from traces *)

type subexp = Let of exp' * exp' | If of exp'

let strip_subexp = function
  | Trace.Prem prem -> (
      match prem.it with
      | IfPr exp -> Some (If exp.it)
      | LetPr (exp_l, exp_r) -> Some (Let (exp_l.it, exp_r.it))
      | _ -> None)
  | _ -> None

let strip_subexps = function
  | Trace.Rel { subtraces; _ } -> subtraces |> List.filter_map strip_subexp
  | _ -> []

let guess_cursor_match (subexp : subexp) : bool =
  match subexp with
  | If (MatchE ({ it = VarE { it = "cursor"; _ }; _ }, _)) -> true
  | _ -> false

let guess (trace : Trace.t) : t =
  let subexps = strip_subexps trace in
  let premise_idx =
    match trace with
    | Rel { subtraces; _ } | Dec { subtraces; _ } | Iter { subtraces; _ } ->
        List.length subtraces
    | _ -> failwith "trace must be a relation, declaration or iteration"
  in
  match subexps with
  | [ If (SubE _) ] | [ If (MatchE _) ] -> Mismatch premise_idx
  | [ If (SubE (exp_a, _)); If (MatchE (exp_b, _)) ]
    when Il.Eq.eq_exp exp_a exp_b ->
      Mismatch premise_idx
  (* Expr_ok/binaryExpression-plusminusmult *)
  | [ If (SubE (exp_a, typ_a)); Let (_, DownCastE (typ_b, exp_b)); If (MemE _) ]
    when Il.Eq.eq_exp exp_a exp_b && Il.Eq.eq_typ typ_a typ_b ->
      Mismatch premise_idx
  (* Type_ok/boolean *)
  | [
   If (SubE (exp_a, typ_a)); Let (_, DownCastE (typ_b, exp_b)); If (MatchE _);
  ]
    when Il.Eq.eq_exp exp_a exp_b && Il.Eq.eq_typ typ_a typ_b ->
      Mismatch premise_idx
  (* ParserTransition_ok/name *)
  | [ If (MatchE (exp_a, _)); Let (CaseE _, exp_b); If (MatchE _) ]
    when Il.Eq.eq_exp' exp_a.it exp_b ->
      Mismatch premise_idx
  (* Decl_ok/instantiation-non-objectInitializer *)
  | [
   If (SubE (exp_a, typ_a));
   Let (exp_c, DownCastE (typ_b, exp_b));
   If (MatchE (exp_d, _));
   Let (CaseE _, exp_e);
   Let ((VarE _ as var_a), VarE _);
   If (SubE (var_c, _));
  ]
    when List.for_all Fun.id
           [
             Il.Eq.eq_exp exp_a exp_b;
             Il.Eq.eq_typ typ_a typ_b;
             Il.Eq.eq_exp' exp_c exp_d.it;
             Il.Eq.eq_exp' exp_d.it exp_e;
             Il.Eq.eq_exp' var_a var_c.it;
           ] ->
      Mismatch premise_idx
  | [ subexp; If (SubE _) ] when guess_cursor_match subexp ->
      Mismatch premise_idx
  | [
   subexp;
   If (SubE (exp_a, typ_a));
   Let (_, DownCastE (typ_b, exp_b));
   If (MatchE _);
  ]
    when guess_cursor_match subexp && Il.Eq.eq_exp exp_a exp_b
         && Il.Eq.eq_typ typ_a typ_b ->
      Mismatch premise_idx
  | _ -> Root premise_idx
