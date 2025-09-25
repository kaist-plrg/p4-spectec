open Sl.Ast
open Sl.Print
open Util.Source
open Ctx

(* Case analysis *)

let rec render_case ctx case =
  let order = String.make (ctx.level + 1) '.' ^ " " in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (render_instrs (ctx |> increment_level) instrs)

and render_cases ctx cases =
  cases |> List.map (render_case ctx) |> String.concat "\n\n"

(* Instruction *)

and render_instr ctx instr =
  let order = String.make (ctx.level + 1) '.' ^ " " in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (render_instrs (ctx |> increment_level) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s"
            order (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs (ctx |> increment_level) instrs_hold)
            order
            (render_instrs (ctx |> increment_level) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s" order
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs (ctx |> increment_level) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          Format.asprintf "%sIf (%s: %s)%s does not hold, then\n\n%s" order
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs (ctx |> increment_level) instrs_nothold))
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (render_cases (ctx |> increment_level) cases)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (render_instr (ctx |> increment_level) instr)
  | GroupI _ -> assert false
  | LetI (exp_l, exp_r, iterexps) ->
      Format.asprintf "%s(Let %s be %s)%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterexps iterexps)
  | RuleI (id_rel, notexp, iterexps) ->
      Format.asprintf "%s(%s: %s)%s" order (string_of_relid id_rel)
        (string_of_notexp notexp)
        (string_of_iterexps iterexps)
  | ResultI [] -> Format.asprintf "%sThe relation holds" order
  | ResultI exps ->
      Format.asprintf "%sResult in %s" order (string_of_exps ", " exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" order (string_of_exp exp)
  | DebugI exp -> Format.asprintf "%sDebug: %s" order (string_of_exp exp)

and render_instrs ctx instrs =
  instrs |> List.map (render_instr ctx) |> String.concat "\n\n"

(* Rule prose *)

let render_ruleprose (ctx : Ctx.t) (mixop : mixop) (inputs : int list)
    (exps_input : exp list) (instrs : instr list) : string =
  "`"
  ^ string_of_relinput mixop inputs exps_input
  ^ "`\n\n" ^ render_instrs ctx instrs
