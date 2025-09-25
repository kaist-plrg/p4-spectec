open Sl.Ast
open Util.Source
open Ctx
module HEnv = Hintenv
module F = Format
module IEnv = Runtime_static.Envs.IEnv
module InputHint = Runtime_static.Rel.InputHint

let reindent_lines ~(indent : string) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n " ^ indent) lines

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

let string_of_atom atom = Il.Print.string_of_atom atom
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

(* Variables *)

let string_of_var var = Il.Print.string_of_var var |> F.asprintf "%s"

(* Types *)

let string_of_typ typ = Il.Print.string_of_typ typ |> F.asprintf "`%s`"
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
let string_of_value ctx value = Il.Print.string_of_value ~level:ctx.level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let rec string_of_exp ctx exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> string_of_varid varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp ctx exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      string_of_exp ctx exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp ctx exp_r
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp ctx exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp ctx exp_r ^ ")"
  | Il.Ast.UpCastE (typ, exp) ->
      "`" ^ string_of_exp ctx exp ^ "` as " ^ string_of_typ typ
  | Il.Ast.DownCastE (typ, exp) ->
      "`" ^ string_of_exp ctx exp ^ "` as " ^ string_of_typ typ
  | Il.Ast.SubE (exp, typ) ->
      "`" ^ string_of_exp ctx exp ^ "` has type " ^ string_of_typ typ
  | Il.Ast.MatchE (exp, pattern) ->
      string_of_exp ctx exp ^ " matches pattern " ^ string_of_pattern pattern
  | Il.Ast.TupleE es -> "(" ^ string_of_exps ctx ", " es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ string_of_notexp ctx notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) ->
               string_of_atom atom ^ " " ^ string_of_exp ctx exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ string_of_exp ctx exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE exps -> "[" ^ string_of_exps ctx ", " exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) ->
      string_of_exp ctx exp_h ^ " :: " ^ string_of_exp ctx exp_t
  | Il.Ast.CatE (exp_l, exp_r) ->
      string_of_exp ctx exp_l ^ " ++ " ^ string_of_exp ctx exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      string_of_exp ctx exp_e ^ " is in " ^ string_of_exp ctx exp_s
  | Il.Ast.LenE exp -> "|" ^ string_of_exp ctx exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) ->
      string_of_exp ctx exp_b ^ "." ^ string_of_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      string_of_exp ctx exp_b ^ "[" ^ string_of_exp ctx exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp ctx exp_b ^ "[" ^ string_of_exp ctx exp_l ^ " : "
      ^ string_of_exp ctx exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      string_of_exp ctx exp_b ^ "[" ^ string_of_path ctx path ^ " = "
      ^ string_of_exp ctx exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) -> (
      let prose_hint_opt = HEnv.get_func defid ctx.penv.prose in
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
            (string_of_prose_hint ctx exps prose_hint)
            (string_of_defid defid)
      | None ->
          "`" ^ string_of_defid defid ^ string_of_targs targs
          ^ string_of_args ctx args ^ "`")
  | Il.Ast.IterE (exp, iterexp) ->
      string_of_exp ctx exp ^ string_of_iterexp iterexp

and string_of_exps ctx sep exps =
  String.concat sep (List.map (string_of_exp ctx) exps)

and string_of_notexp ctx notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> string_of_exp ctx)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and string_of_iterexp (iter, _) = string_of_iter iter

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

and prose_of_list items =
  List.fold_left
    (fun acc item ->
      if acc = "" then item
      else if String.contains acc ',' then acc ^ ", and " ^ item
      else acc ^ " and " ^ item)
    "" items

and prose_of_iteration_out ((iter, vars) : iterexp) =
  match iter with
  | List ->
      let iterated_var var =
        F.asprintf "`%s*` be the list of `%s`" (string_of_var var)
          (string_of_var var)
      in
      List.map iterated_var vars |> prose_of_list
  | Opt -> assert false

and prose_of_iteration ((iter, vars) : iterexp) =
  match iter with
  | Opt -> "?__"
  | List ->
      let iterated_var var =
        F.asprintf "`%s` in `%s*`" (string_of_var var) (string_of_var var)
      in
      List.map iterated_var vars |> prose_of_list

and string_of_iterations iterexps =
  iterexps |> List.map prose_of_iteration |> String.concat ""

and prose_of_iterations out_iterexps in_iterexps inner =
  if List.is_empty in_iterexps && List.is_empty out_iterexps then inner
  else
    F.asprintf "Let %s, obtained by repeating:\n %s\nfor each %s"
      (out_iterexps |> List.map prose_of_iteration |> String.concat "\nITER:")
      inner
      (in_iterexps |> List.map prose_of_iteration |> String.concat "\nITER:")

(* Patterns *)

and string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

and string_of_path ctx path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      string_of_path ctx path ^ "[" ^ string_of_exp ctx exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      string_of_path ctx path ^ "[" ^ string_of_exp ctx exp_l ^ " : "
      ^ string_of_exp ctx exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> string_of_atom atom
  | Il.Ast.DotP (path, atom) ->
      string_of_path ctx path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param = Il.Print.string_of_param param
and string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

and string_of_tparam tparam = Il.Print.string_of_tparam tparam
and string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg ctx arg =
  match arg.it with
  | Il.Ast.ExpA exp -> string_of_exp ctx exp
  | Il.Ast.DefA defid -> string_of_defid defid

and string_of_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (string_of_arg ctx) args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Il.Print.string_of_targ targ
and string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

and string_of_pid pid = F.asprintf "Phantom#%d" pid

and string_of_phantom phantom =
  let pid, _ = phantom in
  string_of_pid pid

and string_of_pathcond ctx pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) ->
      Format.asprintf "(forall %s)%s"
        (string_of_pathcond ctx pathcond)
        (string_of_iterexps iterexps)
  | ExistsC (pathcond, iterexps) ->
      Format.asprintf "(exists %s)%s"
        (string_of_pathcond ctx pathcond)
        (string_of_iterexps iterexps)
  | PlainC exp -> "(" ^ string_of_exp ctx exp ^ ")"
  | HoldC (relid, notexp) ->
      Format.asprintf "(%s: %s holds)" (string_of_relid relid)
        (string_of_notexp ctx notexp)
  | NotHoldC (relid, notexp) ->
      Format.asprintf "(%s: %s does not hold)" (string_of_relid relid)
        (string_of_notexp ctx notexp)

and string_of_pathconds ctx pathconds =
  List.map (string_of_pathcond ctx) pathconds |> String.concat " /\\ "

(* Holding conditions *)

and string_of_holdcase ctx holdcase =
  let indent = String.make (ctx.level * 2) ' ' in
  match holdcase with
  | BothH (instrs_hold, instrs_nothold) ->
      Format.asprintf ", then:\n%s\n%selse:\n\n%s"
        (string_of_instrs (ctx |> increment_level) instrs_hold)
        indent
        (string_of_instrs (ctx |> increment_level) instrs_nothold)
  | HoldH (instrs_hold, _) ->
      Format.asprintf ", then:\n%s"
        (string_of_instrs (ctx |> increment_level) instrs_hold)
  | NotHoldH (instrs_nothold, _) ->
      Format.asprintf "%sDoes not hold:\n\n%s" indent
        (string_of_instrs (ctx |> increment_level) instrs_nothold)

(* Case analysis *)

and string_of_case ctx exp case =
  let indent = String.make (ctx.level * 2) ' ' in
  let order = F.asprintf "%s%d. " indent ctx.index in
  let guard, instrs = case in
  F.asprintf "%sCase %s\n%s" order
    (string_of_guard ctx exp guard)
    (string_of_instrs (ctx |> increment_level) instrs)

and string_of_cases ctx exp cases =
  cases
  |> List.mapi (fun idx case ->
         string_of_case (with_index ctx (idx + 1)) exp case)
  |> String.concat "\n\n"

and string_of_guard ctx exp_case guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      F.asprintf "%s %s %s"
        (string_of_exp ctx exp_case)
        (string_of_cmpop cmpop) (string_of_exp ctx exp)
  | SubG typ ->
      F.asprintf "`%s` has type %s"
        (string_of_exp ctx exp_case)
        (string_of_typ typ)
  | MatchG pattern ->
      F.asprintf "`%s` matches pattern %s"
        (string_of_exp ctx exp_case)
        (string_of_pattern pattern)
  | MemG exp ->
      F.asprintf "`%s` is in `%s`"
        (string_of_exp ctx exp_case)
        (string_of_exp ctx exp)

(* Instructions *)

and string_of_instr ctx instr =
  let indent = String.make (ctx.level * 2) ' ' in
  let order = F.asprintf "%s%d. " indent ctx.index in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      F.asprintf "%sAssert that %s%s.\n%s" order
        (string_of_exp ctx exp_cond)
        (string_of_iterations iterexps)
        (string_of_instrs (with_start ctx (ctx.index + 1)) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      let prose_hint_opt = Hintenv.get_rel id ctx.penv.prose in
      match prose_hint_opt with
      | Some prose_hint ->
          let mixop, exps = notexp in
          F.asprintf "%s If [%s](%s)%s%s" order
            (string_of_prose_hint (ctx |> increment_level) exps prose_hint)
            (string_of_relid id)
            (string_of_iterations iterexps)
            (string_of_holdcase (ctx |> increment_level) holdcase)
      | None ->
          Format.asprintf "%sIf (%s: %s)%s%s" order (string_of_relid id)
            (string_of_notexp ctx notexp)
            (string_of_iterations iterexps)
            (string_of_holdcase (ctx |> increment_level) holdcase))
  | CaseI (exp, cases, _) ->
      F.asprintf "%sCase analysis on `%s`\n%s" order (string_of_exp ctx exp)
        (string_of_cases (ctx |> increment_level) exp cases)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise\n%s" order
        (string_of_instr (with_index (ctx |> increment_level) 1) instr)
  | GroupI (id_group, exps_group, instrs_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" order
        (string_of_relpathid id_group)
        (match ctx.signature with
        | Some (mixop, inputs) -> string_of_relinput ctx mixop inputs exps_group
        | None -> string_of_exps ctx ", " exps_group)
        (string_of_instrs (ctx |> increment_level) instrs_group)
  | LetI (exp_l, exp_r, iterexps) ->
      let free_l = Il.Free.free_exp exp_l in
      let out_iters, in_iters =
        let partition =
          List.map
            (fun (iter, vars) ->
              let out_vars, in_vars =
                List.partition
                  (fun (id, _, _) -> Domain.Lib.IdSet.mem id free_l)
                  vars
              in
              ((iter, out_vars), (iter, in_vars)))
            iterexps
        in
        List.split partition
      in
      F.asprintf "%sLet `%s` be %s" order (string_of_exp ctx exp_l)
        (string_of_exp ctx exp_r)
      |> prose_of_iterations out_iters in_iters
  | RuleI (id_rel, notexp, iterexps) -> (
      let prose_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose in
      (* let input_hint = IEnv.find id_rel ctx.ienv in *)
      (* let _, outputs = InputHint.split_exps_without_idx input_hint (snd notexp) in *)
      match prose_hint_opt with
      | Some prose_hint ->
          let mixop, exps = notexp in
          F.asprintf "%sLet [%s](%s)%s" order
            (string_of_prose_hint (ctx |> increment_level) exps prose_hint)
            (* (string_of_exps ctx ", " outputs) *)
            (string_of_relid id_rel)
            (string_of_iterations iterexps)
      | None ->
          F.asprintf "%s(%s: %s)%s" order (string_of_relid id_rel)
            (string_of_notexp ctx notexp)
            (string_of_iterations iterexps))
  | ResultI [] -> F.asprintf "%sThe relation holds" order
  | ResultI exps ->
      F.asprintf "%sResult in %s" order
        ("`" ^ string_of_exps ctx "`, `" exps ^ "`")
  | ReturnI exp -> F.asprintf "%sReturn %s" order (string_of_exp ctx exp)
  | DebugI exp -> F.asprintf "%sDebug: %s" order (string_of_exp ctx exp)

and string_of_instrs ctx instrs =
  instrs
  |> List.mapi (fun idx instr ->
         match ctx.start_index with
         | None -> string_of_instr (with_index ctx (idx + 1)) instr
         | Some start_index ->
             string_of_instr (with_index ctx (idx + start_index)) instr)
  |> String.concat "\n"

(* Rules *)

and string_of_prose_hint ctx (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = string_of_prose_hint' ctx exps hintexp 0 in
  str

and string_of_prose_hint' ctx (exps : exp list) (hintexp : El.Ast.exp)
    (cursor : int) : int * string =
  let indent = String.make (ctx.level * 2) ' ' in
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, reindent_lines ~indent text)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = string_of_prose_hint' ctx exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
      (* cursor holds position for HoleE.Next *)
      let exp = List.nth exps cursor in
      (* increment cursor *)
      (cursor + 1, "`" ^ string_of_exp ctx exp ^ "`")
  | El.Ast.HoleE (`Num i) ->
      (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, "`" ^ string_of_exp ctx exp ^ "`")
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = string_of_prose_hint' ctx exps exp_l cursor in
      let cursor_r, str_r = string_of_prose_hint' ctx exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

(* Relations *)

and string_of_relinput ctx mixop inputs exps_input =
  let exps_input = List.combine inputs exps_input in
  let exps =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.assoc_opt idx exps_input with
        | Some exp_input -> exp_input
        | None -> Il.Ast.VarE ("%" $ no_region) $$ (no_region, Il.Ast.TextT))
  in
  let notexp = (mixop, exps) in
  string_of_notexp ctx notexp

and string_of_reloutput ctx mixop inputs exps_output =
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
        | None -> Il.Ast.VarE ("%" $ no_region) $$ (no_region, Il.Ast.TextT))
  in
  let notexp = (mixop, exps) in
  string_of_notexp ctx notexp

and string_of_rel ctx rel =
  let relid, (mixop, inputs), exps_match, instrs, _hints = rel in
  string_of_relid relid ^ ": "
  ^ string_of_relinput ctx mixop inputs exps_match
  ^ "\n\n"
  ^ string_of_instrs (with_signature ctx (Some (mixop, inputs))) instrs

(* Definitions *)

let rec string_of_def ctx def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, (_mixop, _inputs), exps_input, instrs, _hints) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ string_of_exps ctx ", " exps_input
      ^ "\n\n"
      ^ string_of_instrs ctx instrs
  | DecD (defid, tparams, args_input, instrs, _hints) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args ctx args_input
      ^ "\n\n"
      ^ string_of_instrs ctx instrs

and string_of_defs ctx defs =
  String.concat "\n\n" (List.map (string_of_def ctx) defs)

(* Spec *)

let string_of_spec ctx spec = string_of_defs ctx spec
