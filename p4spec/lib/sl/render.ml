open Ast
open Print
open Util.Source

(* Case analysis *)

let rec render_case ?(level = 0) case =
  let order = String.make (level + 1) '.' ^ " " in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (render_instrs ~level:(level + 1) instrs)

and render_cases ?(level = 0) cases =
  cases |> List.map (render_case ~level) |> String.concat "\n\n"

(* Instruction *)

and render_instr ?(level = 0) instr =
  let order = String.make (level + 1) '.' ^ " " in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (render_instrs ~level:(level + 1) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s"
            order (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_hold)
            order
            (render_instrs ~level:(level + 1) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s" order
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          Format.asprintf "%sIf (%s: %s)%s does not hold, then\n\n%s" order
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_nothold))
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (render_cases ~level:(level + 1) cases)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (render_instr ~level:(level + 1) instr)
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

and render_instrs ?(level = 0) instrs =
  instrs |> List.map (render_instr ~level) |> String.concat "\n\n"

(* Rule prose *)

let render_ruleprose (mixop : mixop) (inputs : int list) (exps_input : exp list)
    (instrs : instr list) : string =
  "`"
  ^ string_of_relinput mixop inputs exps_input
  ^ "`\n\n" ^ render_instrs instrs
