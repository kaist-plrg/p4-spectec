open Sl.Ast
open Sl.Print
open Util.Source
open Ctx
module HEnv = Hintenv
module F = Format
module IEnv = Runtime_static.Envs.IEnv
module InputHint = Runtime_static.Rel.InputHint

(** Utility functions **)

(* Substitue linebreak with indented linebreak *)

let _reindent_lines ~(indent : string) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n " ^ indent) lines

(* Split iterators into input and output *)

let split_iterexps exps_out iterexps =
  let out_vars = Il.Free.free_exps exps_out in
  let partition =
    List.map
      (fun (iter, vars) ->
        let out_vars, in_vars =
          List.partition
            (fun (id, _, _) -> Domain.Lib.IdSet.mem id out_vars)
            vars
        in
        ((iter, out_vars), (iter, in_vars)))
      iterexps
  in
  List.split partition

(* Prose list: a and b / a, b, ..., y and z *)

let prose_list items =
  List.fold_left
    (fun acc item ->
      if acc = "" then item
      else if String.contains acc ',' then acc ^ ", and " ^ item
      else acc ^ " and " ^ item)
    "" items

(* Expressions *)

let rec prose_exp ctx exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> string_of_varid varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ prose_exp ctx exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      prose_exp ctx exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ prose_exp ctx exp_r
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ prose_exp ctx exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ prose_exp ctx exp_r ^ ")"
  | Il.Ast.UpCastE (typ, exp) ->
      "`" ^ prose_exp ctx exp ^ "` as " ^ string_of_typ typ
  | Il.Ast.DownCastE (typ, exp) ->
      "`" ^ prose_exp ctx exp ^ "` as " ^ string_of_typ typ
  | Il.Ast.SubE (exp, typ) ->
      "`" ^ prose_exp ctx exp ^ "` has type " ^ string_of_typ typ
  | Il.Ast.MatchE (exp, pattern) ->
      prose_exp ctx exp ^ " matches pattern " ^ string_of_pattern pattern
  | Il.Ast.TupleE es -> "(" ^ prose_exps ctx ", " es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ string_of_notexp ctx notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ prose_exp ctx exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ prose_exp ctx exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE exps -> "[" ^ prose_exps ctx ", " exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) ->
      prose_exp ctx exp_h ^ " :: " ^ prose_exp ctx exp_t
  | Il.Ast.CatE (exp_l, exp_r) ->
      prose_exp ctx exp_l ^ " ++ " ^ prose_exp ctx exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      prose_exp ctx exp_e ^ " is in " ^ prose_exp ctx exp_s
  | Il.Ast.LenE exp -> "|" ^ prose_exp ctx exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) -> prose_exp ctx exp_b ^ "." ^ string_of_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      prose_exp ctx exp_b ^ "[" ^ prose_exp ctx exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      prose_exp ctx exp_b ^ "[" ^ prose_exp ctx exp_l ^ " : "
      ^ prose_exp ctx exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      prose_exp ctx exp_b ^ "[" ^ prose_path ctx path ^ " = "
      ^ prose_exp ctx exp_f ^ "]"
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
            (prose_hintexp ctx exps prose_hint)
            (string_of_defid defid)
      | None ->
          "`" ^ string_of_defid defid ^ string_of_targs targs
          ^ prose_args ctx args ^ "`")
  | Il.Ast.IterE (exp, iterexp) -> prose_exp ctx exp ^ string_of_iterexp iterexp

and prose_exps ctx sep exps = String.concat sep (List.map (prose_exp ctx) exps)

and string_of_notexp ctx notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> prose_exp ctx)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and prose_hintexp ctx (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = prose_hintexp' ctx exps hintexp 0 in
  str

and prose_hintexp' ctx (exps : exp list) (hintexp : El.Ast.exp) (cursor : int) :
    int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = prose_hintexp' ctx exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
      (* cursor holds position for HoleE.Next *)
      let exp = List.nth exps cursor in
      (* increment cursor *)
      (cursor + 1, "`" ^ prose_exp ctx exp ^ "`")
  | El.Ast.HoleE (`Num i) ->
      (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, "`" ^ prose_exp ctx exp ^ "`")
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = prose_hintexp' ctx exps exp_l cursor in
      let cursor_r, str_r = prose_hintexp' ctx exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

(* Paths *)

and prose_path ctx path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      prose_path ctx path ^ "[" ^ prose_exp ctx exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      prose_path ctx path ^ "[" ^ prose_exp ctx exp_l ^ " : "
      ^ prose_exp ctx exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> string_of_atom atom
  | Il.Ast.DotP (path, atom) -> prose_path ctx path ^ "." ^ string_of_atom atom

(* Arguments *)

and prose_arg ctx arg =
  match arg.it with
  | Il.Ast.ExpA exp -> prose_exp ctx exp
  | Il.Ast.DefA defid -> string_of_defid defid

and prose_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (prose_arg ctx) args) ^ ")"

(* Case analysis *)

and prose_case ctx exp case =
  let guard, instrs = case in
  F.asprintf "%sCase %s\n%s" (bullet ctx)
    (prose_guard ctx exp guard)
    (render_instrs (ctx |> increment_level) instrs)

and prose_cases ctx exp cases =
  cases |> List.map (prose_case ctx exp) |> String.concat "\n\n"

and prose_guard ctx exp_case guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      F.asprintf "%s %s %s" (prose_exp ctx exp_case) (string_of_cmpop cmpop)
        (prose_exp ctx exp)
  | SubG typ ->
      F.asprintf "`%s` has type %s" (prose_exp ctx exp_case) (string_of_typ typ)
  | MatchG pattern ->
      F.asprintf "`%s` matches pattern %s" (prose_exp ctx exp_case)
        (string_of_pattern pattern)
  | MemG exp ->
      F.asprintf "`%s` is in `%s`" (prose_exp ctx exp_case) (prose_exp ctx exp)

(* Prose iterations *)

and prose_out_iterexp ((iter, vars) : iterexp) =
  match iter with
  | List ->
      let iterated_var var =
        F.asprintf "`%s*` be the list of `%s`" (string_of_var var)
          (string_of_var var)
      in
      List.map iterated_var vars |> prose_list
  | Opt -> assert false

and prose_in_iterexp ((iter, vars) : iterexp) =
  match iter with
  | Opt -> "?__"
  | List ->
      let iterated_var var =
        F.asprintf "`%s` in `%s*`" (string_of_var var) (string_of_var var)
      in
      List.map iterated_var vars |> prose_list

and prose_iterations ctx out_iterexps in_iterexps inner =
  if List.is_empty in_iterexps && List.is_empty out_iterexps then
    F.asprintf "%s%s" (bullet ctx) inner
  else if List.is_empty out_iterexps then
    F.asprintf "%sIf %s, for each %s" (bullet ctx)
      inner
      (in_iterexps |> List.map prose_in_iterexp |> String.concat "\nITER:")
  else
    F.asprintf "%sLet %s, obtained by repeating:\n%s%s\n%sfor each %s"
      (bullet ctx)
      (out_iterexps |> List.map prose_out_iterexp |> String.concat "\nITER:")
      (bullet (ctx |> increment_level))
      inner (bullet ctx)
      (in_iterexps |> List.map prose_in_iterexp |> String.concat "\nITER:")

(* Instruction *)

and render_instr ctx instr =
  let bullet = bullet ctx in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      F.asprintf "%sIf (%s)%s, then\n\n%s" bullet (prose_exp ctx exp_cond)
        (string_of_iterexps iterexps)
        (render_instrs (ctx |> increment_level) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      let prosed_relation = 
        let prose_hint_opt = Hintenv.get_rel id ctx.penv.prose in
        match prose_hint_opt with
        | Some prose_hint ->
            let mixop, exps = notexp in
            (F.asprintf "[%s](%s)"
              (prose_hintexp (ctx |> increment_level) exps prose_hint)
              (string_of_relid id))
            |> prose_iterations ctx [] iterexps
        | None ->
            (Format.asprintf "(%s: %s)"(string_of_relid id)
              (string_of_notexp ctx notexp))
            |> prose_iterations ctx [] iterexps
      in
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          F.asprintf "%s, then\n%s\n%sElse,\n\n%s" 
            prosed_relation
            (render_instrs (ctx |> increment_level) instrs_hold)
            bullet
            (render_instrs (ctx |> increment_level) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          F.asprintf "%s, then\n%s"
            prosed_relation
            (render_instrs (ctx |> increment_level) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          F.asprintf "%s does not hold, then\n%s"
            prosed_relation
            (render_instrs (ctx |> increment_level) instrs_nothold))
  | CaseI (exp, cases, _) ->
      F.asprintf "%sCase analysis on %s\n%s" bullet (prose_exp ctx exp)
        (prose_cases (ctx |> increment_level) exp cases)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise\n%s" bullet
        (render_instr (ctx |> increment_level) instr)
  | GroupI (id_group, exps_group, instrs_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" bullet
        (string_of_relpathid id_group)
        (match ctx.signature with
        | Some (mixop, inputs) -> string_of_relinput ctx mixop inputs exps_group
        | None -> prose_exps ctx ", " exps_group)
        (render_instrs (ctx |> increment_level) instrs_group)
  | LetI (exp_l, exp_r, iterexps) ->
      let out_iters, in_iters = split_iterexps [ exp_l ] iterexps in
      F.asprintf "Let `%s` be %s" (prose_exp ctx exp_l) (prose_exp ctx exp_r)
      |> prose_iterations ctx out_iters in_iters
  | RuleI (id_rel, notexp, iterexps) -> (
      let prose_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose in
      let input_hint = IEnv.find id_rel ctx.ienv in
      let _, outputs =
        InputHint.split_exps_without_idx input_hint (snd notexp)
      in
      let out_iters, in_iters = split_iterexps outputs iterexps in
      match prose_hint_opt with
      | Some prose_hint ->
          let mixop, exps = notexp in
          F.asprintf "Let `%s` be [%s](%s)"
            (prose_exps ctx ", " outputs)
            (prose_hintexp (ctx |> increment_level) exps prose_hint)
            (string_of_relid id_rel)
          |> prose_iterations ctx out_iters in_iters
      | None ->
          F.asprintf "(%s: %s)" (string_of_relid id_rel)
            (string_of_notexp ctx notexp)
          |> prose_iterations ctx [] iterexps)
  | ResultI [] -> F.asprintf "%sThe relation holds" bullet
  | ResultI exps ->
      F.asprintf "%sResult in %s" bullet (prose_exps ctx ", " exps)
  | ReturnI exp -> F.asprintf "%sReturn %s" bullet (prose_exp ctx exp)
  | DebugI exp -> F.asprintf "%sDebug: %s" bullet (prose_exp ctx exp)

and render_instrs ctx instrs =
  instrs |> List.map (render_instr ctx) |> String.concat "\n"

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

(* Rule prose *)

let render_ruleprose (ctx : Ctx.t) (mixop : mixop) (inputs : int list)
    (exps_input : exp list) (instrs : instr list) : string =
  "`"
  ^ string_of_relinput ctx mixop inputs exps_input
  ^ "`\n\n" ^ render_instrs ctx instrs

(* Definitions *)

let prose_def ctx def =
  match def.it with
  | TypD (typid, tparams, deftyp) -> ""
  | RelD (relid, (_mixop, _inputs), exps_input, instrs, _hints) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ prose_exps ctx ", " exps_input
      ^ "\n\n" ^ render_instrs ctx instrs
  | DecD (defid, tparams, args_input, instrs, _hints) -> ""

let prose_defs ctx defs = String.concat "" (List.map (prose_def ctx) defs)

(* Spec *)

let prose_spec ctx spec = prose_defs ctx spec
