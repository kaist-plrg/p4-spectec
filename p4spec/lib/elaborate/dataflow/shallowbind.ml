open Il.Ast
open Error
open Util.Source

(* Collect binding identifiers,
   while enforcing shallow binding *)

let error_deep (exp : exp) =
  error exp.at
    (Format.asprintf "%s not allowed in shallow binding"
       (Il.Print.string_of_exp exp))

let error_deep_msg (exp : exp) (msg : string) =
  error exp.at
    (Format.asprintf "%s not allowed in shallow binding: %s"
       (Il.Print.string_of_exp exp)
       msg)

(* Expressions *)

let check_shallow_exp (exp : exp) : bool =
  let rec is_iterated_var exp =
    match exp.it with
    | VarE _ -> true
    | IterE (e, _) -> is_iterated_var e
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
