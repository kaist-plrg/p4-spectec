open Domain
open Ast
open Annot
open Util.Source

(* Numbers *)

let string_of_num num = Sl.Print.string_of_num num

(* Texts *)

let string_of_text text = Sl.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Sl.Print.string_of_varid varid
let string_of_typid typid = Sl.Print.string_of_typid typid
let string_of_relid relid = Sl.Print.string_of_relid relid
let string_of_relpathid relpathid = Sl.Print.string_of_relpathid relpathid
let string_of_defid defid = Sl.Print.string_of_defid defid

(* Atoms *)

let string_of_atom atom = Sl.Print.string_of_atom atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Sl.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Sl.Print.string_of_iter iter
let string_of_iterexp iterexp = Sl.Print.string_of_iterexp iterexp
let string_of_iterexps iterexps = Sl.Print.string_of_iterexps iterexps

let string_of_iterated string_of_item item iterexps =
  match iterexps with
  | [] -> string_of_item item
  | _ ->
      Format.asprintf "(%s)%s" (string_of_item item)
        (string_of_iterexps iterexps)

(* Variables *)

let string_of_var var = Sl.Print.string_of_var var

(* Types *)

let string_of_typ typ = Sl.Print.string_of_typ typ
let string_of_typs sep typs = Sl.Print.string_of_typs sep typs
let string_of_nottyp nottyp = Sl.Print.string_of_nottyp nottyp
let string_of_deftyp deftyp = Sl.Print.string_of_deftyp deftyp
let string_of_typfield typfield = Sl.Print.string_of_typfield typfield

let string_of_typfields sep typfields =
  Sl.Print.string_of_typfields sep typfields

let string_of_typcase typcase = Sl.Print.string_of_typcase typcase
let string_of_typcases sep typcases = Sl.Print.string_of_typcases sep typcases

(* Values *)

let string_of_vid vid = Sl.Print.string_of_vid vid

let string_of_value ?(short = false) ?(level = 0) value =
  Sl.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Sl.Print.string_of_unop unop
let string_of_binop binop = Sl.Print.string_of_binop binop
let string_of_cmpop cmpop = Sl.Print.string_of_cmpop cmpop

(* Expressions *)

let rec string_of_exp exp =
  match exp.node.it with
  | BoolE b -> string_of_bool b
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE varid -> string_of_varid varid
  | UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | UpCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | DownCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | SubE (exp, typ, _) ->
      "(" ^ string_of_exp exp ^ " has type " ^ string_of_typ typ ^ ")"
  | MatchE (exp, pattern) ->
      "(" ^ string_of_exp exp ^ " matches pattern " ^ string_of_pattern pattern
      ^ ")"
  | TupleE exps -> "(" ^ string_of_exps ", " exps ^ ")"
  | CaseE notexp -> "(" ^ string_of_notexp notexp ^ ")"
  | StrE fields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             fields)
      ^ "}"
  | OptE None -> "?()"
  | OptE (Some exp) -> "?(" ^ string_of_exp exp ^ ")"
  | ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | ConsE (exp_h, exp_t) -> string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | MemE (exp_e, exp_s) -> string_of_exp exp_e ^ " is in " ^ string_of_exp exp_s
  | LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | IdxE (exp_b, exp_i) -> string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | IterE (exp, iterexp) -> string_of_iterated string_of_exp exp [ iterexp ]

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  Mixfix.render ~string_of_atom ~string_of_arg:string_of_exp notexp

(* Patterns *)

and string_of_pattern pattern = Sl.Print.string_of_pattern pattern

(* Paths *)

and string_of_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> string_of_atom atom
  | DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param =
  match param.it with
  | ExpP (_typ, exp) -> string_of_exp exp
  | DefP (defid, tparams, params, typ) ->
      string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ

and string_of_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map string_of_param params) ^ ")"

(* Type parameters *)

and string_of_tparam tparam = Sl.Print.string_of_tparam tparam
and string_of_tparams tparams = Sl.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | ExpA exp -> string_of_exp exp
  | DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Sl.Print.string_of_targ targ
and string_of_targs targs = Sl.Print.string_of_targs targs

(* Danglings *)

and string_of_dangle iid = Format.asprintf "Dangling#%d" iid

(* Case analysis *)

and string_of_case ?(level = 0) ?(index = 0) string_of_instr_tier case =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, block = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_block ~level:(level + 1) string_of_instr_tier block)

and string_of_cases ?(level = 0) string_of_instr_tier cases =
  cases
  |> List.mapi (fun idx case ->
         string_of_case ~level ~index:(idx + 1) string_of_instr_tier case)
  |> String.concat "\n\n"

and string_of_guard guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ string_of_cmpop cmpop ^ " " ^ string_of_exp exp ^ ")"
  | SubG (typ, _) -> "(% has type " ^ string_of_typ typ ^ ")"
  | MatchG pattern -> "(% matches pattern " ^ string_of_pattern pattern ^ ")"
  | MemG exp -> "(% is in " ^ string_of_exp exp ^ ")"
  | CheckLetSubG (typ, _, exp) ->
      "(let " ^ string_of_exp exp ^ " be %, % has type " ^ string_of_typ typ
      ^ ")"
  | CheckLetMatchG (pattern, exp) ->
      "(let " ^ string_of_exp exp ^ " be %, % matches pattern "
      ^ string_of_pattern pattern ^ ")"

(* Instructions: shared control-flow, parametric over the tier *)

and string_of_instr ?(short = false) ?(level = 0) ?(index = 0)
    (string_of_instr_tier :
      ?short:bool -> ?level:int -> ?index:int -> 'instr_tier -> string)
    (instr : 'instr_tier instr) =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.node.it with
  | IfI (exp_cond, iterexps, block, dangle) ->
      let s_short =
        Format.asprintf "If (%s)%s, then" (string_of_exp exp_cond)
          (string_of_iterexps iterexps)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s%s" order s_short
          (string_of_block ~level:(level + 1) string_of_instr_tier block)
          (if dangle then
             "\n\n" ^ order ^ "Else " ^ string_of_dangle instr.node.note.iid
           else "")
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
              (string_of_block ~level:(level + 1) string_of_instr_tier
                 block_hold)
              order
              (string_of_block ~level:(level + 1) string_of_instr_tier
                 block_nothold)
      | HoldH (block_hold, dangle) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s holds, then" (string_of_relid id)
              (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s%s" order s_short
              (string_of_block ~level:(level + 1) string_of_instr_tier
                 block_hold)
              (if dangle then
                 "\n\n" ^ order ^ "Else " ^ string_of_dangle instr.node.note.iid
               else "")
      | NotHoldH (block_nothold, dangle) ->
          let s_short =
            Format.asprintf "If (%s: %s)%s does not hold, then"
              (string_of_relid id) (string_of_notexp notexp)
              (string_of_iterexps iterexps)
          in
          if short then s_short
          else
            Format.asprintf "%s%s\n\n%s%s" order s_short
              (string_of_block ~level:(level + 1) string_of_instr_tier
                 block_nothold)
              (if dangle then
                 "\n\n" ^ order ^ "Else " ^ string_of_dangle instr.node.note.iid
               else ""))
  | CaseI (exp, cases, dangle) ->
      let s_short = Format.asprintf "Case analysis on %s" (string_of_exp exp) in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s%s" order s_short
          (string_of_cases ~level:(level + 1) string_of_instr_tier cases)
          (if dangle then
             "\n\n" ^ order ^ "Else " ^ string_of_dangle instr.node.note.iid
           else "")
  | LetI (exp_l, exp_r, iterinstrs) ->
      let s_short =
        Format.asprintf "(Let %s be %s)%s" (string_of_exp exp_l)
          (string_of_exp exp_r)
          (string_of_iterinstrs iterinstrs)
      in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | DebugI exp ->
      let s_short = Format.asprintf "Debug: %s" (string_of_exp exp) in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | DestructI (fields, exp_r) ->
      let targets =
        fields |> List.map snd |> List.map string_of_exp |> String.concat ", "
      in
      let s_short =
        Format.asprintf "(Destruct (%s) = %s)" targets (string_of_exp exp_r)
      in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | CheckLetSubI (typ, _, exp_l, exp_r, block) ->
      let s_short =
        Format.asprintf "(Let %s be %s, %s has type %s)" (string_of_exp exp_l)
          (string_of_exp exp_r) (string_of_exp exp_r) (string_of_typ typ)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) string_of_instr_tier block)
  | CheckLetMatchI (pattern, exp_l, exp_r, block) ->
      let s_short =
        Format.asprintf "(Let %s be %s, %s matches pattern %s)"
          (string_of_exp exp_l) (string_of_exp exp_r) (string_of_exp exp_r)
          (string_of_pattern pattern)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) string_of_instr_tier block)
  | OptionGetI (exp_l, exp_r, block) ->
      let s_short =
        Format.asprintf "(Let %s be ! %s)" (string_of_exp exp_l)
          (string_of_exp exp_r)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block ~level:(level + 1) string_of_instr_tier block)
  | TierI instr_tier -> string_of_instr_tier ~short ~level ~index instr_tier

and string_of_block ?(level = 0) ?(index = 0) string_of_instr_tier block =
  block
  |> List.mapi (fun idx instr ->
         string_of_instr ~level
           ~index:(index + idx + 1)
           string_of_instr_tier instr)
  |> String.concat "\n\n"

and string_of_elseblock ?(level = 0) ?(index = 0) string_of_instr_tier elseblock
    =
  Format.asprintf "%s%d. Otherwise,\n\n%s"
    (String.make (level * 2) ' ')
    (index + 1)
    (string_of_block ~level:(level + 1) string_of_instr_tier elseblock)

and string_of_elseblock_opt ?(level = 0) ?(index = 0) string_of_instr_tier
    elseblock_opt =
  match elseblock_opt with
  | None -> ""
  | Some elseblock ->
      "\n\n" ^ string_of_elseblock ~level ~index string_of_instr_tier elseblock

and string_of_iterinstr iterinstr =
  let iter, _, _ = iterinstr in
  string_of_iter iter

and string_of_iterinstrs iterinstrs =
  iterinstrs |> List.map string_of_iterinstr |> String.concat ""

(* Relations *)

and string_of_relinput rel_signature exps_input =
  let nottyp, inputs = rel_signature in
  let arity = Mixfix.arity nottyp.it in
  let exps_input = List.combine inputs exps_input in
  let sexps =
    List.init arity (fun idx ->
        match List.assoc_opt idx exps_input with
        | Some exp_input -> string_of_exp exp_input
        | None -> "%")
  in
  Mixop.assemble ~string_of_atom (Mixfix.to_mixop nottyp.it) sexps

and string_of_reloutput rel_signature exps_output =
  let nottyp, inputs = rel_signature in
  let arity = Mixfix.arity nottyp.it in
  let outputs =
    List.init arity (fun idx -> if List.mem idx inputs then None else Some idx)
    |> List.filter_map Fun.id
  in
  let exps_output = List.combine outputs exps_output in
  let sexps =
    List.init arity (fun idx ->
        match List.assoc_opt idx exps_output with
        | Some exp_output -> string_of_exp exp_output
        | None -> "%")
  in
  Mixop.assemble ~string_of_atom (Mixfix.to_mixop nottyp.it) sexps

and string_of_extern_rel (externrel : externrel) =
  let relid, rel_signature, exps_match = externrel in
  string_of_relid relid ^ ": " ^ string_of_relinput rel_signature exps_match

(* Group-body tier *)

let rec string_of_instr_group ?(short = false) ?(level = 0) ?(index = 0)
    instr_group =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr_group with
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
  | RuleI (id_rel, notexp, _inputs, iterinstrs) ->
      let s_short =
        Format.asprintf "(%s: %s)%s" (string_of_relid id_rel)
          (string_of_notexp notexp)
          (string_of_iterinstrs iterinstrs)
      in
      if short then s_short else Format.asprintf "%s%s" order s_short
  | BacktrackI arms ->
      let s_short = Format.asprintf "Block (%d arms)" (List.length arms) in
      if short then s_short
      else
        let s_arms =
          arms
          |> List.mapi (fun idx arm ->
                 Format.asprintf "%sArm %d:\n\n%s" indent (idx + 1)
                   (string_of_block_group ~level:(level + 1) arm))
          |> String.concat "\n\n"
        in
        Format.asprintf "%s%s\n\n%s" order s_short s_arms

and string_of_block_group ?(level = 0) ?(index = 0) block =
  string_of_block ~level ~index string_of_instr_group block

(* Dispatch tier *)

let rec string_of_instr_dispatch ?(short = false) ?(level = 0) ?(index = 0)
    instr_dispatch =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr_dispatch with
  | GroupI (id_group, _id_rel, rel_signature, exps_group, block) ->
      let s_short =
        Format.asprintf "Group %s: %s" (string_of_relid id_group)
          (string_of_relinput rel_signature exps_group)
      in
      if short then s_short
      else
        Format.asprintf "%s%s\n\n%s" order s_short
          (string_of_block_group ~level:(level + 1) block)
  | RouteI arms ->
      let s_short = Format.asprintf "Block (%d arms)" (List.length arms) in
      if short then s_short
      else
        let s_arms =
          arms
          |> List.mapi (fun idx arm ->
                 Format.asprintf "%sArm %d:\n\n%s" indent (idx + 1)
                   (string_of_block_dispatch ~level:(level + 1) arm))
          |> String.concat "\n\n"
        in
        Format.asprintf "%s%s\n\n%s" order s_short s_arms

and string_of_block_dispatch ?(level = 0) ?(index = 0) block =
  string_of_block ~level ~index string_of_instr_dispatch block

let string_of_defined_rel (rel : rel) =
  let relid, rel_signature, exps_match, block, elseblock_opt = rel in
  string_of_relid relid ^ ": "
  ^ string_of_relinput rel_signature exps_match
  ^ "\n\n"
  ^ string_of_block_dispatch block
  ^ string_of_elseblock_opt ~index:(List.length block) string_of_instr_dispatch
      elseblock_opt

(* Functions *)

let string_of_extern_func (externfunc : externfunc) =
  let defid, tparams, params, _typ = externfunc in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params

let string_of_builtin_func (builtinfunc : builtinfunc) =
  let defid, tparams, params, _typ = builtinfunc in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params

let string_of_tablerow (tablerow : tablerow) =
  let exps_match, exp_result, instrs = tablerow in
  Format.asprintf "\n  Row : %s -> %s:\n\n%s"
    (string_of_exps ", " exps_match)
    (string_of_exp exp_result)
    (string_of_block_group ~level:2 instrs)

let string_of_tablerows (tablerows : tablerow list) =
  String.concat "\n" (List.map string_of_tablerow tablerows)

let string_of_table_func (tablefunc : tablefunc) =
  let defid, params, _typ_ret, tablerows = tablefunc in
  string_of_defid defid ^ string_of_params params ^ "\n=\n"
  ^ string_of_tablerows tablerows

let string_of_defined_func (func : definedfunc) =
  let defid, tparams, params, _typ, block, elseblock_opt = func in
  string_of_defid defid ^ string_of_tparams tparams ^ string_of_params params
  ^ "\n\n"
  ^ string_of_block_group block
  ^ string_of_elseblock_opt ~index:(List.length block) string_of_instr_group
      elseblock_opt

(* Definitions *)

let rec string_of_def def =
  match def.node.it with
  | ExternTypD id -> "extern syntax " ^ string_of_typid id
  | TypD (id, tparams, deftyp) ->
      "syntax " ^ string_of_typid id ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | VarD (id, typ) -> "var " ^ string_of_varid id ^ " : " ^ string_of_typ typ
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
