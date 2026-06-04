open Lang
open Sl

(* Instructions *)

let rec compile_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Ml.expr =
  match instr.it with
  | IfI (exp_cond, iterexps, block, _) ->
      compile_if_instr ctx exp_cond iterexps block
  | HoldI (id, notexp, iterexps, holdcase) ->
      compile_hold_instr ctx id notexp iterexps holdcase
  | CaseI (exp, cases, _) -> compile_case_instr ctx exp cases
  | GroupI (_, _, _, block) -> compile_group_instr ctx block
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      compile_let_instr ctx exp_l exp_r iterinstrs block
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      compile_rule_instr ctx id notexp inputs iterinstrs block
  | ResultI (_, exps) -> compile_result_instr ctx exps
  | ReturnI exp -> compile_return_instr ctx exp
  | DebugI exp -> compile_debug_instr ctx exp

(* If instruction *)

and compile_if_instr (_ctx : Ctx.t) (_exp_cond : exp) (_iterexps : iterexp list)
    (_block : block) : Ctx.t * Ml.expr =
  failwith "compile_if_instr"

(* Hold instruction *)

and compile_hold_instr (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
    (_iterexps : iterexp list) (_holdcase : holdcase) : Ctx.t * Ml.expr =
  failwith "compile_hold_instr"

(* Case instruction *)

and compile_case_instr (_ctx : Ctx.t) (_exp : exp) (_cases : case list) :
    Ctx.t * Ml.expr =
  failwith "compile_case_instr"

(* Group instruction *)

and compile_group_instr (_ctx : Ctx.t) (_block : block) : Ctx.t * Ml.expr =
  failwith "compile_group_instr"

(* Let instruction *)

and compile_let_instr (_ctx : Ctx.t) (_exp_l : exp) (_exp_r : exp)
    (_iterinstrs : iterinstr list) (_block : block) : Ctx.t * Ml.expr =
  failwith "compile_let_instr"

(* Rule instruction *)

and compile_rule_instr (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
    (_inputs : Hints.Input.t) (_iterinstrs : iterinstr list) (_block : block) :
    Ctx.t * Ml.expr =
  failwith "compile_rule_instr"

(* Result instruction *)

and compile_result_instr (ctx : Ctx.t) (exps : exp list) : Ctx.t * Ml.expr =
  let ctx, exprs_ml = Exp.compile_exps ctx exps in
  let expr_ml =
    match exprs_ml with
    | [] -> Ml.UnitE
    | [ expr_ml ] -> expr_ml
    | _ -> Ml.TupleE exprs_ml
  in
  (ctx, expr_ml)

(* Return instruction *)

and compile_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = Exp.compile_exp ctx exp in
  (ctx, expr_ml)

(* Debug instruction *)

and compile_debug_instr (_ctx : Ctx.t) (_exp : exp) : Ctx.t * Ml.expr =
  failwith "compile_debug_instr"

(* Block *)

and compile_block (_ctx : Ctx.t) (_block : block) : Ctx.t * Ml.expr =
  failwith "compile_block"
