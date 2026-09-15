open Lang
open Il
module Mixfix = Domain.Mixfix
open Util.Source

(* Check that binding patterns are shallow

   Shallow binding allows only:
    - variables
    - upcasts of variables or case expressions
    - case expressions over variables *)

(* Expressions *)

type pattern = Variable of typ | Case of nottyp

let rec is_iterated_var exp =
  match exp.it with
  | VarE _ -> true
  | IterE (exp, _) -> is_iterated_var exp
  | _ -> false

let case_pattern (at : region) (notexp : notexp) : pattern =
  let mixop, exps = Mixfix.split notexp in
  let nottyp =
    Mixfix.fill mixop (List.map (fun exp -> exp.note $ exp.at) exps) $ at
  in
  Case nottyp

let classify_exp (exp : exp) : pattern option =
  match exp.it with
  | VarE _ -> Some (Variable (exp.note $ exp.at))
  | UpCastE (_, { it = VarE _; note; at }) -> Some (Variable (note $ at))
  | UpCastE (_, { it = CaseE notexp; _ }) -> Some (case_pattern exp.at notexp)
  | CaseE notexp when notexp |> Mixfix.args |> List.for_all is_iterated_var ->
      Some (case_pattern exp.at notexp)
  | _ -> None

let check_shallow_exp (exp : exp) : bool = classify_exp exp |> Option.is_some

let check_shallow_exps (exps : exp list) : bool =
  List.for_all check_shallow_exp exps

(* Arguments *)

let check_shallow_arg (arg : arg) : bool =
  match arg.it with ExpA exp -> check_shallow_exp exp | DefA _ -> false

let check_shallow_args (args : arg list) : bool =
  List.for_all check_shallow_arg args
