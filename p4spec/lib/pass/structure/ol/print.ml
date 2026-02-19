open Lang
open Sl.Print
open Ast
open Util.Source

(* Expressions *)

let string_of_exp (exp : exp) : string = string_of_exp exp

(* Case analysis *)

let rec string_of_case ?(level = 0) ?(index = 0) (case : case) : string =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, block = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_block ~level:(level + 1) block)

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
  | IfI (exp_cond, iterexps, block_then) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (string_of_block ~level:(level + 1) block_then)
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s" order
        (string_of_relid id) (string_of_notexp notexp)
        (string_of_iterexps iterexps)
        (string_of_block ~level:(level + 1) block_hold)
        order
        (string_of_block ~level:(level + 1) block_nothold)
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (string_of_cases ~level:(level + 1) cases)
  | GroupI (id_group, rel_signature, exps_group, block) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" order (string_of_relid id_group)
        (string_of_relinput rel_signature exps_group)
        (string_of_block ~level:(level + 1) block)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      Format.asprintf "%s(Let %s be %s)%s\n\n%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterinstrs iterinstrs)
        (string_of_block ~level:(level + 1) block)
  | RuleI (id_rel, notexp, _inputs, iterinstrs, block) ->
      Format.asprintf "%s(%s: %s)%s\n\n%s" order (string_of_relid id_rel)
        (string_of_notexp notexp)
        (string_of_iterinstrs iterinstrs)
        (string_of_block ~level:(level + 1) block)
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

and string_of_block ?(level = 0) (block : block) : string =
  string_of_instrs ~level block
