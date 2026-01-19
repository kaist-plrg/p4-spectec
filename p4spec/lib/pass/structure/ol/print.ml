open Lang
open Sl.Print
open Ast
open Util.Source

(* Expressions *)

let string_of_exp (exp : exp) : string = Sl.Print.string_of_exp exp

(* Case analysis *)

let rec string_of_case ?(level = 0) ?(index = 0) (case : case) : string =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_instrs ~level:(level + 1) instrs)

and string_of_cases ?(level = 0) (cases : case list) : string =
  cases
  |> List.mapi (fun idx case -> string_of_case ~level ~index:(idx + 1) case)
  |> String.concat "\n\n"

and string_of_guard (guard : guard) : string =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ string_of_cmpop cmpop ^ " " ^ string_of_exp exp ^ ")"
  | SubG typ -> "(% has type " ^ string_of_typ typ ^ ")"
  | MatchG patten -> "(% matches pattern " ^ string_of_pattern patten ^ ")"
  | MemG exp -> "(% in " ^ string_of_exp exp ^ ")"

(* Instructions *)

and string_of_instr ?(level = 0) ?(index = 0) (instr : instr) : string =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s" order
        (string_of_relid id) (string_of_notexp notexp)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_hold)
        order
        (string_of_instrs ~level:(level + 1) instrs_nothold)
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (string_of_cases ~level:(level + 1) cases)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (string_of_instr ~level:(level + 1) ~index:1 instr)
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" order (string_of_relid id_group)
        (string_of_relinput rel_signature exps_group)
        (string_of_instrs ~level:(level + 1) instrs_group)
  | LetI (exp_l, exp_r, iterinstrs) ->
      Format.asprintf "%s(Let %s be %s)%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterinstrs iterinstrs)
  | RuleI (id_rel, notexp, _inputs, iterinstrs) ->
      Format.asprintf "%s(%s: %s)%s" order (string_of_relid id_rel)
        (string_of_notexp notexp)
        (string_of_iterinstrs iterinstrs)
  | ResultI (_, []) -> Format.asprintf "%sThe relation holds" order
  | ResultI (rel_signature, exps) ->
      Format.asprintf "%sResult in %s" order
        (string_of_reloutput rel_signature exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" order (string_of_exp exp)
  | DebugI exp -> Format.asprintf "%sDebug: %s" order (string_of_exp exp)

and string_of_instrs ?(level = 0) (instrs : instr list) : string =
  instrs
  |> List.mapi (fun idx instr -> string_of_instr ~level ~index:(idx + 1) instr)
  |> String.concat "\n\n"
