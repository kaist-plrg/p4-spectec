open Ast
open Util.Source

(* Numbers *)

let string_of_num num = Il.Print.string_of_num num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_typid typid = Il.Print.string_of_typid typid
let string_of_relid relid = Il.Print.string_of_relid relid
let string_of_relpathid relpathid = Il.Print.string_of_rulegroupid relpathid
let string_of_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let string_of_atom ?(lower = true) atom = Il.Print.string_of_atom ~lower atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

let string_of_iterexp iterexp =
  let iter, _ = iterexp in
  string_of_iter iter

let string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

let string_of_iterated string_of_item item iterexps =
  match iterexps with
  | [] -> string_of_item item
  | _ ->
      Format.asprintf "(%s)%s" (string_of_item item)
        (string_of_iterexps iterexps)

(* Variables *)

let string_of_var var = Il.Print.string_of_var var

(* Types *)

let string_of_typ typ = Il.Print.string_of_typ typ
let string_of_typs sep typs = Il.Print.string_of_typs sep typs
let string_of_nottyp nottyp = Il.Print.string_of_nottyp nottyp
let string_of_deftyp deftyp = Il.Print.string_of_deftyp deftyp
let string_of_typfield typfield = Il.Print.string_of_typfield typfield

let string_of_typfields sep typfields =
  Il.Print.string_of_typfields sep typfields

let string_of_typcase typcase = Il.Print.string_of_typcase typcase
let string_of_typcases sep typcases = Il.Print.string_of_typcases sep typcases

(* Values *)

let string_of_vid vid = "@" ^ string_of_int vid

let string_of_value ?(short = false) ?(level = 0) value =
  Il.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let rec string_of_exp exp =
  match exp.it with
  | Il.BoolE b -> string_of_bool b
  | Il.NumE n -> string_of_num n
  | Il.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.VarE varid -> string_of_varid varid
  | Il.UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | Il.BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.UpCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.DownCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.SubE (exp, typ) ->
      "(" ^ string_of_exp exp ^ " has type " ^ string_of_typ typ ^ ")"
  | Il.MatchE (exp, pattern) ->
      "(" ^ string_of_exp exp ^ " matches pattern " ^ string_of_pattern pattern
      ^ ")"
  | Il.TupleE exps -> "(" ^ string_of_exps ", " exps ^ ")"
  | Il.CaseE notexp -> "(" ^ string_of_notexp notexp ^ ")"
  | Il.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | Il.OptE (Some exp) -> "?(" ^ string_of_exp exp ^ ")"
  | Il.OptE None -> "?()"
  | Il.ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | Il.ConsE (exp_h, exp_t) ->
      string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | Il.CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | Il.MemE (exp_e, exp_s) ->
      string_of_exp exp_e ^ " is in " ^ string_of_exp exp_s
  | Il.LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | Il.DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | Il.IdxE (exp_b, exp_i) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | Il.SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | Il.CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | Il.IterE (exp, iterexp) -> string_of_iterated string_of_exp exp [ iterexp ]

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> string_of_exp)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

(* Patterns *)

and string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

and string_of_path path =
  match path.it with
  | Il.RootP -> ""
  | Il.IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | Il.SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.DotP ({ it = Il.RootP; _ }, atom) -> string_of_atom atom
  | Il.DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param =
  match param.it with
  | ExpP (_typ, exp) -> string_of_exp exp
  | DefP id -> string_of_defid id

and string_of_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map string_of_param params) ^ ")"

(* Type parameters *)

and string_of_tparam tparam = Il.Print.string_of_tparam tparam
and string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | Il.ExpA exp -> string_of_exp exp
  | Il.DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Il.Print.string_of_targ targ
and string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

and string_of_pid pid = Format.asprintf "Phantom#%d" pid
and string_of_phantom phantom = string_of_pid phantom

(* Case analysis *)

and string_of_case ?(level = 0) ?(index = 0) case =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, block = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_block ~level:(level + 1) block)

and string_of_cases ?(level = 0) cases =
  cases
  |> List.mapi (fun idx case -> string_of_case ~level ~index:(idx + 1) case)
  |> String.concat "\n\n"

and string_of_guard guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ string_of_cmpop cmpop ^ " " ^ string_of_exp exp ^ ")"
  | SubG typ -> "(% has type " ^ string_of_typ typ ^ ")"
  | MatchG patten -> "(% matches pattern " ^ string_of_pattern patten ^ ")"
  | MemG exp -> "(% is in " ^ string_of_exp exp ^ ")"

(* Instructions *)

and string_of_instr ?(short = false) ?(level = 0) ?(index = 0) instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, None) ->
      let s_short =
        Format.asprintf "If (%s)%s, then" (string_of_exp exp_cond)
          (string_of_iterexps iterexps)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) block_then)
  | IfI (exp_cond, iterexps, block, Some phantom) ->
      let s_short =
        Format.asprintf "If (%s)%s, then" (string_of_exp exp_cond)
          (string_of_iterexps iterexps)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s%s" order s_short
          (string_of_block ~level:(level + 1) block)
          ("\n\n" ^ order ^ "Else " ^ string_of_phantom phantom)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      match holdcase with
      | BothH (block_hold, block_nothold) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s holds, then" (string_of_relid id)
              (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s\n\n%sElse,\n\n%s" order s_short
              (string_of_block ~level:(level + 1) block_hold)
              order
              (string_of_block ~level:(level + 1) block_nothold)
      | HoldH (block_hold, None) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s holds, then" (string_of_relid id)
              (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s" order s_short
              (string_of_block ~level:(level + 1) block_hold)
      | HoldH (block_hold, Some phantom) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s holds, then" (string_of_relid id)
              (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s%s" order s_short
              (string_of_block ~level:(level + 1) block_hold)
              ("\n\n" ^ order ^ "Else " ^ string_of_phantom phantom)
      | NotHoldH (block_nothold, None) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s does not hold, then"
              (string_of_relid id) (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s" order s_short
              (string_of_block ~level:(level + 1) block_nothold)
      | NotHoldH (block_nothold, Some phantom) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s does not hold, then"
              (string_of_relid id) (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s%s" order s_short
              (string_of_block ~level:(level + 1) block_nothold)
              ("\n\n" ^ order ^ "Else " ^ string_of_phantom phantom))
  | CaseI (exp, cases, None) ->
      let s_short = Format.asprintf "Case analysis on %s" (string_of_exp exp) in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_cases ~level:(level + 1) cases)
  | CaseI (exp, cases, Some phantom) ->
      let s_short = Format.asprintf "Case analysis on %s" (string_of_exp exp) in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s%s" order s_short
          (string_of_cases ~level:(level + 1) cases)
          ("\n\n" ^ order ^ "Else " ^ string_of_phantom phantom)
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let s_short =
        Format.asprintf "Group %s: %s" (string_of_relid id_group)
          (string_of_relinput rel_signature exps_group)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) block)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let s_short =
        Format.asprintf "(Let %s be %s)%s" (string_of_exp exp_l)
          (string_of_exp exp_r)
          (string_of_iterinstrs iterinstrs)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) block)
  | RuleI (id_rel, notexp, _inputs, iterinstrs, block) ->
      let s_short =
        Format.asprintf "(%s: %s)%s" (string_of_relid id_rel)
          (string_of_notexp notexp)
          (string_of_iterinstrs iterinstrs)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) block)
  | ResultI (_, []) ->
      let s_short = "The relation holds" in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | ResultI (rel_signature, exps) ->
      let s_short =
        Format.asprintf "Result in: %s" (string_of_reloutput rel_signature exps)
      in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | ReturnI exp ->
      let s_short = Format.asprintf "Return %s" (string_of_exp exp) in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | DebugI exp ->
      let s_short = Format.asprintf "Debug: %s" (string_of_exp exp) in
      if short then s_short else Format.asprintf "%s%s" order s_short

and string_of_block ?(level = 0) ?(index = 0) block =
  block
  |> List.mapi (fun idx instr ->
         string_of_instr ~level ~index:(index + idx + 1) instr)
  |> String.concat "\n\n"

and string_of_elseblock ?(level = 0) ?(index = 0) elseblock =
  Format.asprintf "%s%d. Otherwise,\n\n%s"
    (String.make (level * 2) ' ')
    (index + 1)
    (string_of_block ~level:(level + 1) elseblock)

and string_of_elseblock_opt ?(level = 0) ?(index = 0) elseblock_opt =
  match elseblock_opt with
  | None -> ""
  | Some elseblock -> "\n\n" ^ string_of_elseblock ~level ~index elseblock

and string_of_iterinstr iterinstr =
  let iter, _, _ = iterinstr in
  string_of_iter iter

and string_of_iterinstrs iterinstrs =
  iterinstrs |> List.map string_of_iterinstr |> String.concat ""

(* Relations *)

and string_of_relinput rel_signature exps_input =
  let nottyp, inputs = rel_signature in
  let mixop, _ = nottyp.it in
  let exps_input = List.combine inputs exps_input in
  let exps =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.assoc_opt idx exps_input with
        | Some exp_input -> exp_input
        | None -> Il.VarE ("%" $ no_region) $$ (no_region, Il.TextT))
  in
  let notexp = (mixop, exps) in
  string_of_notexp notexp

and string_of_reloutput rel_signature exps_output =
  let nottyp, inputs = rel_signature in
  let mixop, _ = nottyp.it in
  let outputs =
    List.init
      (List.length mixop - 1)
      (fun idx -> if List.mem idx inputs then None else Some idx)
    |> List.filter_map Fun.id
  in
  let exps_output = List.combine outputs exps_output in
  let exps =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.assoc_opt idx exps_output with
        | Some exp_output -> exp_output
        | None -> Il.VarE ("%" $ no_region) $$ (no_region, Il.TextT))
  in
  let notexp = (mixop, exps) in
  string_of_notexp notexp

and string_of_extern_rel externrel =
  let relid, rel_signature, exps_match, _hints = externrel in
  string_of_relid relid ^ ": " ^ string_of_relinput rel_signature exps_match

and string_of_defined_rel rel =
  let relid, rel_signature, exps_match, block, elseblock_opt, _hints = rel in
  string_of_relid relid ^ ": "
  ^ string_of_relinput rel_signature exps_match
  ^ "\n\n" ^ string_of_block block
  ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt

(* Functions *)

let string_of_extern_func externfunc =
  let defid, tparams, params, _typ, _hints = externfunc in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params

let string_of_builtin_func builtinfunc =
  let defid, tparams, params, _typ, _hints = builtinfunc in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params

let string_of_tablerow (tablerow : tablerow) =
  let exps_match, exp_result, instrs = tablerow in
  Format.asprintf "\n  Row : %s -> %s:\n\n%s"
    (string_of_exps ", " exps_match)
    (string_of_exp exp_result)
    (string_of_block ~level:2 instrs)

let string_of_tablerows (tablerows : tablerow list) =
  String.concat "\n" (List.map string_of_tablerow tablerows)

let string_of_table_func (tablefunc : tablefunc) =
  let defid, params, _typ_ret, tablerows, _hints = tablefunc in
  string_of_defid defid ^ string_of_params params ^ "\n=\n"
  ^ string_of_tablerows tablerows

let string_of_defined_func (func : definedfunc) =
  let defid, tparams, params, _typ, block, elseblock_opt, _hints = func in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params
  ^ "\n\n" ^ string_of_block block
  ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt

(* Definitions *)

let rec string_of_def def =
  match def.it with
  | ExternTypD (id, _) -> "extern syntax " ^ string_of_typid id
  | TypD (id, tparams, deftyp, _) ->
      "syntax " ^ string_of_typid id ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | ExternRelD externrel -> "extern relation " ^ string_of_extern_rel externrel
  | RelD rel -> "relation " ^ string_of_defined_rel rel
  | ExternDecD externfunc -> "extern def " ^ string_of_extern_func externfunc
  | BuiltinDecD builtinfunc ->
      "builtin def " ^ string_of_builtin_func builtinfunc
  | TableDecD tablefunc -> "tbl def " ^ string_of_table_func tablefunc
  | FuncDecD func -> "def " ^ string_of_defined_func func

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
