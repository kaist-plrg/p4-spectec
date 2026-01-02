open Lang
open Il
open Util.Source

(* Check that binding patterns are shallow

   Shallow binding allows only:
    - variables
    - upcasts of variables or case expressions
    - case expressions over variables *)

(* Expressions *)

let check_shallow_exp (exp : exp) : bool =
  let rec is_iterated_var exp =
    match exp.it with
    | VarE _ -> true
    | IterE (exp, _) -> is_iterated_var exp
    | _ -> false
  in
  match exp.it with
  | VarE _ -> true
  | UpCastE (_, { it = VarE _; _ }) | UpCastE (_, { it = CaseE _; _ }) -> true
  | CaseE notexp -> notexp |> snd |> List.for_all is_iterated_var
  | _ -> false

let check_shallow_exps (exps : exp list) : bool =
  List.for_all check_shallow_exp exps

(* Arguments *)

let check_shallow_arg (arg : arg) : bool =
  match arg.it with ExpA exp -> check_shallow_exp exp | DefA _ -> false

let check_shallow_args (args : arg list) : bool =
  List.for_all check_shallow_arg args
