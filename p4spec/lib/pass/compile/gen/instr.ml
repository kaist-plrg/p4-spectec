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

(* Group instruction: [group { block }]

   [compile_block block] *)

and compile_group_instr (ctx : Ctx.t) (block : block) : Ctx.t * Ml.expr =
  compile_block ctx block

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

(* Debug instruction: [debug exp]

   [raise (Unmatch "debug")] *)

and compile_debug_instr (ctx : Ctx.t) (_exp : exp) : Ctx.t * Ml.expr =
  (ctx, Common.raise_unmatch "debug")

(* Block: [[instr_h; instrs_t..]]

   []                    ->  [raise (Unmatch "empty block")]
   [instr_h :: instrs_t] ->  [try compile_instr instr_h with Unmatch _ -> compile_block instrs_t] *)

and compile_block (ctx : Ctx.t) (block : block) : Ctx.t * Ml.expr =
  match block with
  | [] -> (ctx, Common.raise_unmatch "empty block")
  | instr_h :: instrs_t ->
      let ctx, expr_h_ml = compile_instr ctx instr_h in
      let ctx, expr_t_ml = compile_block ctx instrs_t in
      let arm_ml = (Ml.VariantP (`Mono ("Unmatch", [ Ml.WildP ])), expr_t_ml) in
      let expr_ml = Ml.TryE (expr_h_ml, [ arm_ml ]) in
      (ctx, expr_ml)
