open Lang
open Sl
open Print
open Util.Source

(* Case analysis *)

let rec log_case ?(level = 0) ?(index = 0) (cover : Multi.t) (case : case) :
    string =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let header = "  " ^ order in
  let guard, block = case in
  Format.asprintf "%sCase %s\n\n%s" header (string_of_guard guard)
    (log_block ~level:(level + 1) cover block)

and log_cases ?(level = 0) (cover : Multi.t) (cases : case list) : string =
  cases
  |> List.mapi (fun idx case -> log_case ~level ~index:(idx + 1) cover case)
  |> String.concat "\n\n"

(* Instructions *)

and log_instr ?(level = 0) ?(index = 0) (cover : Multi.t) (instr : instr) :
    string =
  let indent = String.make (level * 2) ' ' in
  let hit = Multi.is_hit cover instr.note.iid in
  let order = Format.asprintf "%s%d. " indent index in
  let header = (if hit then "+ " else "- ") ^ order in
  let header_trailing = "  " ^ order in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, _) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" header (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (log_block ~level:(level + 1) cover block_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      match holdcase with
      | BothH (block_hold, block_nothold) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s"
            header (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (log_block ~level:(level + 1) cover block_hold)
            header_trailing
            (log_block ~level:(level + 1) cover block_nothold)
      | HoldH (block_hold, _) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s" header
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (log_block ~level:(level + 1) cover block_hold)
      | NotHoldH (block_nothold, _) ->
          Format.asprintf "%sIf (%s: %s)%s does not hold, then\n\n%s" header
            (string_of_relid id) (string_of_notexp notexp)
            (string_of_iterexps iterexps)
            (log_block ~level:(level + 1) cover block_nothold))
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" header (string_of_exp exp)
        (log_cases ~level:(level + 1) cover cases)
  | GroupI (id_group, rel_signature, exps_group, block_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" header (string_of_relid id_group)
        (string_of_relinput rel_signature exps_group)
        (log_block ~level:(level + 1) cover block_group)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      Format.asprintf "%s(Let %s be %s)%s\n\n%s" header (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterinstrs iterinstrs)
        (log_block ~level:(level + 1) ~index cover block)
  | RuleI (id_rel, notexp, _inputs, iterinstrs, block) ->
      Format.asprintf "%s(%s: %s)%s\n\n%s" header (string_of_relid id_rel)
        (string_of_notexp notexp)
        (string_of_iterinstrs iterinstrs)
        (log_block ~level:(level + 1) ~index cover block)
  | ResultI (_, []) -> Format.asprintf "%sThe relation holds" header
  | ResultI (rel_signature, exps) ->
      Format.asprintf "%sResult in: %s" header
        (string_of_reloutput rel_signature exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" header (string_of_exp exp)
  | DebugI exp -> Format.asprintf "%sDebug: %s" header (string_of_exp exp)

and log_block ?(level = 0) ?(index = 0) (cover : Multi.t) (block : block) :
    string =
  block
  |> List.mapi (fun idx instr ->
         log_instr ~level ~index:(index + idx + 1) cover instr)
  |> String.concat "\n\n"

and log_elseblock ?(level = 0) ?(index = 0) (cover : Multi.t)
    (elseblock : elseblock) : string =
  Format.asprintf "%s%d. Otherwise,\n\n%s"
    (String.make (level * 2) ' ')
    (index + 1)
    (log_block ~level:(level + 1) cover elseblock)

and log_elseblock_opt ?(level = 0) ?(index = 0) (cover : Multi.t)
    (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | Some elseblock -> log_elseblock ~level ~index cover elseblock
  | None -> ""

(* Relations *)

let log_defined_rel (cover : Multi.t) (rel : rel) : string =
  let relid, rel_signature, exps_match, block, elseblock_opt, _hints = rel in
  string_of_relid relid ^ ": "
  ^ string_of_relinput rel_signature exps_match
  ^ "\n\n" ^ log_block cover block
  ^ log_elseblock_opt ~index:(List.length block) cover elseblock_opt

(* Functions *)

let log_tablerow (cover : Multi.t) (tablerow : tablerow) : string =
  let exps_match, exp_result, block = tablerow in
  "\n Row : "
  ^ string_of_exps ", " exps_match
  ^ " -> " ^ string_of_exp exp_result ^ "\n\n"
  ^ log_block ~level:2 cover block

let log_tablerows (cover : Multi.t) (tablerows : tablerow list) : string =
  tablerows |> List.map (log_tablerow cover) |> String.concat "\n"

let log_table_func (cover : Multi.t) (tablefunc : tablefunc) : string =
  let defid, params, _typ_ret, tablerows, _hints = tablefunc in
  string_of_defid defid ^ string_of_params params ^ "\n=\n"
  ^ log_tablerows cover tablerows

let log_defined_func (cover : Multi.t) (func : definedfunc) : string =
  let defid, tparams, params, _typ_ret, block, elseblock_opt, _hints = func in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params
  ^ "\n\n" ^ log_block cover block
  ^ log_elseblock_opt ~index:(List.length block) cover elseblock_opt

(* Definitions *)

let log_def (cover : Multi.t) (def : def) : string =
  (";; " ^ string_of_region def.at ^ "\n")
  ^
  match def.it with
  | ExternTypD (typid, _) -> "extern syntax " ^ string_of_typid typid
  | TypD (typid, tparams, deftyp, _) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | ExternRelD externrel -> "extern relation " ^ string_of_extern_rel externrel
  | RelD rel -> "rel " ^ log_defined_rel cover rel
  | ExternDecD externfunc -> "extern def " ^ string_of_extern_func externfunc
  | BuiltinDecD builtinfunc ->
      "builtin def " ^ string_of_builtin_func builtinfunc
  | TableDecD tablefunc -> "tbl def " ^ log_table_func cover tablefunc
  | FuncDecD func -> "def " ^ log_defined_func cover func

let log_defs (cover : Multi.t) (defs : def list) : string =
  String.concat "\n\n" (List.map (log_def cover) defs)

(* Spec *)

let log_spec ~(filename_cov_opt : string option) (cover : Multi.t) (spec : spec)
    : unit =
  let output oc_opt =
    match oc_opt with Some oc -> output_string oc | None -> print_string
  in
  let oc_opt = Option.map open_out filename_cov_opt in
  let output = output oc_opt in
  (* Output overall coverage *)
  let total, hits, coverage = Multi.measure_coverage cover in
  Format.asprintf ";; Instruction coverage: %d/%d (%.2f%%)\n" hits total
    coverage
  |> output;
  (* Output spec coverage *)
  log_defs cover spec |> output;
  (* Close output channel if any *)
  match oc_opt with Some oc -> close_out oc | None -> ()
