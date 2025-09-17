open Sl.Ast
open Util.Source
module HEnv = Hintenv
module F = Format

(* Numbers *)

let string_of_num num = Il.Print.string_of_num num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_typid typid = Il.Print.string_of_typid typid
let string_of_relid relid = Il.Print.string_of_relid relid
let string_of_ruleid ruleid = Il.Print.string_of_ruleid ruleid
let string_of_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let string_of_atom atom = Il.Print.string_of_atom atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

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

let rec string_of_exp penv exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> string_of_varid varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp penv exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp penv exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp penv exp_r ^ ")"
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp penv exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp penv exp_r ^ ")"
  | Il.Ast.UpCastE (typ, exp) ->
      "(" ^ string_of_exp penv exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.Ast.DownCastE (typ, exp) ->
      "(" ^ string_of_exp penv exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.Ast.SubE (exp, typ) ->
      "(" ^ string_of_exp penv exp ^ " has type " ^ string_of_typ typ ^ ")"
  | Il.Ast.MatchE (exp, pattern) ->
      "(" ^ string_of_exp penv exp ^ " matches pattern " ^ string_of_pattern pattern
      ^ ")"
  | Il.Ast.TupleE es -> "(" ^ string_of_exps penv ", " es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ string_of_notexp penv notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp penv exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ string_of_exp penv exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE exps -> "[" ^ string_of_exps penv ", " exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) ->
      string_of_exp penv exp_h ^ " :: " ^ string_of_exp penv exp_t
  | Il.Ast.CatE (exp_l, exp_r) ->
      string_of_exp penv exp_l ^ " ++ " ^ string_of_exp penv exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      string_of_exp penv exp_e ^ " is in " ^ string_of_exp penv exp_s
  | Il.Ast.LenE exp -> "|" ^ string_of_exp penv exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) -> string_of_exp penv exp_b ^ "." ^ string_of_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      string_of_exp penv exp_b ^ "[" ^ string_of_exp penv exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp penv exp_b ^ "[" ^ string_of_exp penv exp_l ^ " : "
      ^ string_of_exp penv exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      string_of_exp penv exp_b ^ "[" ^ string_of_path penv path ^ " = "
      ^ string_of_exp penv exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) ->
      let prose_hint_opt = HEnv.get_func defid penv.Collect.prose in (
      match prose_hint_opt with
      | Some prose_hint ->
          let exps =
            args
            |> List.filter_map (fun arg ->
              match arg.it with
                 | Il.Ast.ExpA exp -> Some exp
                 | Il.Ast.DefA _ -> None)
          in
          F.asprintf "[%s](%s)"
            (string_of_prose_hint penv exps prose_hint)
            (string_of_defid defid)
      | None ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args penv args)
  | Il.Ast.IterE (exp, iterexp) -> string_of_exp penv exp ^ string_of_iterexp iterexp

and string_of_exps penv sep exps = String.concat sep (List.map (string_of_exp penv) exps)

and string_of_notexp penv notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> string_of_exp penv)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and string_of_iterexp (iter, _) = Il.Print.string_of_iter iter

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

and string_of_path penv path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      string_of_path penv path ^ "[" ^ string_of_exp penv exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      string_of_path penv path ^ "[" ^ string_of_exp penv exp_l ^ " : "
      ^ string_of_exp penv exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> string_of_atom atom
  | Il.Ast.DotP (path, atom) -> string_of_path penv path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param = Il.Print.string_of_param param
and string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

and string_of_tparam tparam = Il.Print.string_of_tparam tparam
and string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg penv arg =
  match arg.it with
  | Il.Ast.ExpA exp -> string_of_exp penv exp
  | Il.Ast.DefA defid -> string_of_defid defid

and string_of_args penv args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (string_of_arg penv) args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Il.Print.string_of_targ targ
and string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

and string_of_pid pid = F.asprintf "Phantom#%d" pid

and string_of_phantom phantom =
  let pid, _ = phantom in
  string_of_pid pid

and string_of_pathcond penv pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) ->
      Format.asprintf "(forall %s)%s"
        (string_of_pathcond penv pathcond)
        (string_of_iterexps iterexps)
  | ExistsC (pathcond, iterexps) ->
      Format.asprintf "(exists %s)%s"
        (string_of_pathcond penv pathcond)
        (string_of_iterexps iterexps)
  | PlainC exp -> "(" ^ string_of_exp penv exp ^ ")"
  | HoldC (relid, notexp) ->
      Format.asprintf "(%s: %s holds)" (string_of_relid relid)
        (string_of_notexp penv notexp)
  | NotHoldC (relid, notexp) ->
      Format.asprintf "(%s: %s does not hold)" (string_of_relid relid)
        (string_of_notexp penv notexp)

and string_of_pathconds penv pathconds =
  List.map (string_of_pathcond penv) pathconds |> String.concat " /\\ "

(* Holding conditions *)

and string_of_holdcase ?(level = 0) penv holdcase =
  let indent = String.make (level * 2) ' ' in
  match holdcase with
  | BothH (instrs_hold, instrs_nothold) ->
      Format.asprintf ", then:\n%s\n%selse:\n\n%s"
        (string_of_instrs ~level:(level + 1) penv instrs_hold)
        indent
        (string_of_instrs ~level:(level + 1) penv instrs_nothold)
  | HoldH (instrs_hold, _) ->
      Format.asprintf ", then:\n%s"
        (string_of_instrs ~level:(level + 1) penv instrs_hold)
  | NotHoldH (instrs_nothold, _) ->
      Format.asprintf "%sDoes not hold:\n\n%s" indent
        (string_of_instrs ~level:(level + 1) penv instrs_nothold)

(* Case analysis *)

and string_of_case ?(level = 0) ?(index = 0) penv exp case =
  let indent = String.make (level * 2) ' ' in
  let order = F.asprintf "%s%d. " indent index in
  let guard, instrs = case in
  F.asprintf "%sCase %s\n%s" order
    (string_of_guard penv exp guard)
    (string_of_instrs ~level:(level + 1) penv instrs)

and string_of_cases ?(level = 0) penv exp cases =
  cases
  |> List.mapi (fun idx case -> string_of_case ~level ~index:(idx + 1) penv exp case)
  |> String.concat "\n\n"

and string_of_guard penv exp_case guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      F.asprintf "%s %s %s" (string_of_exp penv exp_case) (string_of_cmpop cmpop)
        (string_of_exp penv exp)
  | SubG typ ->
      F.asprintf "%s has type %s" (string_of_exp penv exp_case) (string_of_typ typ)
  | MatchG pattern ->
      F.asprintf "%s matches pattern %s" (string_of_exp penv exp_case)
        (string_of_pattern pattern)
  | MemG exp ->
      F.asprintf "%s is in %s" (string_of_exp penv exp_case) (string_of_exp penv exp)

(* Instructions *)

and string_of_instr ?(level = 0) ?(index = 0) penv instr =
  let indent = String.make (level * 2) ' ' in
  let order = F.asprintf "%s%d. " indent index in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      F.asprintf "%sAssert that %s%s.\n%s" order (string_of_exp penv exp_cond)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) penv instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) ->
      let prose_hint_opt = Hintenv.get_rel id penv.Collect.prose in (
      match prose_hint_opt with
      | Some prose_hint ->
          let mixop, exps = notexp in
        F.asprintf "%s If [%s](%s)%s%s" order
            (string_of_prose_hint penv exps prose_hint) (string_of_relid id)
            (string_of_iterexps iterexps)
            (string_of_holdcase ~level:(level + 1) penv holdcase)
      | None ->
      Format.asprintf "%sIf (%s: %s)%s%s" order (string_of_relid id)
        (string_of_notexp penv notexp)
        (string_of_iterexps iterexps)
        (string_of_holdcase ~level:(level + 1) penv holdcase))
  | CaseI (exp, cases, _) ->
      F.asprintf "%sCase analysis on %s\n%s" order (string_of_exp penv exp)
        (string_of_cases ~level:(level + 1) penv exp cases)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise\n%s" order
        (string_of_instr ~level:(level + 1) ~index:1 penv instr)
  | LetI (exp_l, exp_r, iterexps) ->
      F.asprintf "%s(Let %s be %s)%s" order (string_of_exp penv exp_l)
        (string_of_exp penv exp_r)
        (string_of_iterexps iterexps)
  | RuleI (id_rel, notexp, iterexps) ->
      let prose_hint_opt = Hintenv.get_rel id_rel penv.Collect.prose in (
      match prose_hint_opt with 
      | Some prose_hint ->
          let mixop, exps = notexp in
        F.asprintf "%sLet [%s](%s)%s" order
            (string_of_prose_hint penv exps prose_hint) (string_of_relid id_rel)
            (string_of_iterexps iterexps)
      | None ->
      F.asprintf "%s(%s: %s)%s" order (string_of_relid id_rel)
        (string_of_notexp penv notexp)
        (string_of_iterexps iterexps))
  | ResultI [] -> F.asprintf "%sThe relation holds" order
  | ResultI exps -> F.asprintf "%sResult in %s" order (string_of_exps penv ", " exps)
  | ReturnI exp -> F.asprintf "%sReturn %s" order (string_of_exp penv exp)
  | DebugI exp -> F.asprintf "%sDebug: %s" order (string_of_exp penv exp)

and string_of_instrs ?(level = 0) penv instrs =
  instrs
  |> List.mapi (fun idx instr -> string_of_instr ~level ~index:(idx + 1) penv instr)
  |> String.concat "\n"

(* Rules *)

and string_of_prose_hint penv (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = string_of_prose_hint' penv exps hintexp 0 in
  str

and string_of_prose_hint' penv (exps : exp list) (hintexp : El.Ast.exp) (cursor : int) : int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text)
  | El.Ast.SeqE exps_hint ->
    let cursor, strs = List.fold_left (fun (cur, acc) exp -> 
      let cur, str = string_of_prose_hint' penv exps exp cur in
      (cur, acc @ [str])
    ) (cursor, []) exps_hint in
    (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
    (* holds cursor for HoleE.Next *)
      let exp = List.nth exps cursor in
      (cursor + 1, "`" ^ string_of_exp penv exp ^ "`")
  | El.Ast.HoleE (`Num i) ->
    (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, "`" ^ string_of_exp penv exp ^ "`")
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = string_of_prose_hint' penv exps exp_l cursor in
      let cursor_r, str_r = string_of_prose_hint' penv exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

(* Definitions *)

let rec string_of_def penv def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, (_mixop, _inputs), exps_input, instrs, _hints) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ string_of_exps penv ", " exps_input
      ^ "\n\n" ^ string_of_instrs penv instrs
  | DecD (defid, tparams, args_input, instrs, _hints) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args penv args_input ^ "\n\n" ^ string_of_instrs penv instrs

and string_of_defs penv defs = String.concat "\n\n" (List.map (string_of_def penv) defs)

(* Spec *)

let string_of_spec penv spec = string_of_defs penv spec
