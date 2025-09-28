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

(* Asciidoc monospace rendering *)

let render_mono ctx s =
  match ctx.mode with
  | Code -> s
  | Prose -> "`+" ^ s ^ "+`"

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

(** Printing as code **)

let code_of_iterexp (iter, _) = string_of_iter iter

let code_of_iterexps iterexps =
  iterexps |> List.map code_of_iterexp |> String.concat ""

let code_of_mixop mixop =
  let mixop = List.map (List.map it) mixop in
    String.concat "%"
      (List.map
         (fun atoms -> String.concat "" (List.map Xl.Atom.string_of_atom atoms))
         mixop)

let code_of_pattern pattern =
  match pattern with
  | Il.Ast.CaseP mixop -> code_of_mixop mixop
  | Il.Ast.ListP `Cons -> "_ :: _"
  | Il.Ast.ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | Il.Ast.ListP `Nil -> "[]"
  | Il.Ast.OptP `Some -> "(_)"
  | Il.Ast.OptP `None -> "()"

let code_of_typ ctx typ =
  string_of_typ typ |> render_mono ctx

(** Printing as prose **)

(* Prose list: a and b / a, b, ..., y and z *)

let prose_of_list items =
  List.fold_left
    (fun acc item ->
      if acc = "" then item
      else if String.contains acc ',' then acc ^ ", and " ^ item
      else acc ^ " and " ^ item)
    "" items

(* Expressions *)

let rec prose_of_exp ctx exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> string_of_varid varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ prose_of_exp ctx exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      prose_of_exp ctx exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ prose_of_exp ctx exp_r
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ prose_of_exp ctx exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ prose_of_exp ctx exp_r ^ ")"
  | Il.Ast.UpCastE (typ, exp) | Il.Ast.DownCastE (typ, exp) ->
    F.asprintf "%s as %s"
      (code_of_exp ctx exp)
      (code_of_typ ctx typ)
  | Il.Ast.SubE (exp, typ) ->
    F.asprintf "%s has type %s"
      (code_of_exp ctx exp)
      (code_of_typ ctx typ)
  | Il.Ast.MatchE (exp, pattern) ->
    F.asprintf "%s matches pattern %s"
      (code_of_exp ctx exp)
      (string_of_pattern pattern |> render_mono ctx)
  | Il.Ast.TupleE es -> "(" ^ prose_of_exps ctx es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ code_of_notexp ctx notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ prose_of_exp ctx exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ prose_of_exp ctx exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE exps -> "[" ^ prose_of_exps ctx exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) ->
      prose_of_exp ctx exp_h ^ " :: " ^ prose_of_exp ctx exp_t
  | Il.Ast.CatE (exp_l, exp_r) ->
      prose_of_exp ctx exp_l ^ " ++ " ^ prose_of_exp ctx exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      prose_of_exp ctx exp_e ^ " is in " ^ prose_of_exp ctx exp_s
  | Il.Ast.LenE exp -> "|" ^ prose_of_exp ctx exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) -> prose_of_exp ctx exp_b ^ "." ^ string_of_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_exp ctx exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_exp ctx exp_l ^ " : "
      ^ prose_of_exp ctx exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_path ctx path ^ " = "
      ^ prose_of_exp ctx exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) -> (
      let prose_of_hint_opt = HEnv.get_func defid ctx.penv.prose in
      match prose_of_hint_opt with
      | Some prose_of_hint ->
          let exps =
            args
            |> List.filter_map (fun arg ->
                   match arg.it with
                   | Il.Ast.ExpA exp -> Some exp
                   | Il.Ast.DefA _ -> None)
          in
          F.asprintf "[%s](%s)"
            (prose_of_hintexp ctx exps prose_of_hint)
            (string_of_defid defid)
      | None ->
          (F.asprintf "%s%s%s"
            (string_of_defid defid)
            (string_of_targs targs)
            (prose_of_args ctx args))
          |> render_mono ctx)
  | Il.Ast.IterE (exp, iterexp) -> prose_of_exp ctx exp ^ code_of_iterexp iterexp

and prose_of_exps ctx exps = prose_of_list (List.map (prose_of_exp ctx) exps)

and code_of_exp ctx exp =
  prose_of_exp (ctx |> in_code) exp |> render_mono ctx

and code_of_exps ctx exps =
  prose_of_exps (ctx |> in_code) exps |> render_mono ctx

and code_of_notexp ctx notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> string_of_atoms
      else idx / 2 |> List.nth exps |> prose_of_exp ctx)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and prose_of_hintexp ctx (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = prose_of_hintexp' ctx exps hintexp 0 in
  str

and prose_of_hintexp' ctx (exps : exp list) (hintexp : El.Ast.exp) (cursor : int) :
    int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = prose_of_hintexp' ctx exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
      (* cursor holds position for HoleE.Next *)
      let exp = List.nth exps cursor in
      (* increment cursor *)
      (cursor + 1, code_of_exp ctx exp)
  | El.Ast.HoleE (`Num i) ->
      (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, code_of_exp ctx exp)
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = prose_of_hintexp' ctx exps exp_l cursor in
      let cursor_r, str_r = prose_of_hintexp' ctx exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

(* Paths *)

and prose_of_path ctx path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      prose_of_path ctx path ^ "[" ^ prose_of_exp ctx exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      prose_of_path ctx path ^ "[" ^ prose_of_exp ctx exp_l ^ " : "
      ^ prose_of_exp ctx exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> string_of_atom atom
  | Il.Ast.DotP (path, atom) -> prose_of_path ctx path ^ "." ^ string_of_atom atom

(* Arguments *)

and prose_of_arg ctx arg =
  match arg.it with
  | Il.Ast.ExpA exp -> prose_of_exp ctx exp
  | Il.Ast.DefA defid -> string_of_defid defid

and prose_of_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (prose_of_arg ctx) args) ^ ")"

(* Case analysis *)

and prose_of_case ctx exp case =
  let guard, instrs = case in
  F.asprintf "%sCase %s\n%s" (bullet ctx)
    (prose_of_guard ctx exp guard)
    (render_instrs (ctx |> increment_level) instrs)

and prose_of_cases ctx exp cases =
  cases |> List.map (prose_of_case ctx exp) |> String.concat "\n\n"

and prose_of_guard ctx exp_case guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      F.asprintf "%s %s %s" (prose_of_exp ctx exp_case) (string_of_cmpop cmpop)
        (prose_of_exp ctx exp)
  | SubG typ ->
      F.asprintf "%s has type %s" (code_of_exp ctx exp_case) (code_of_typ ctx typ)
  | MatchG pattern ->
      F.asprintf "%s matches pattern %s" (code_of_exp ctx exp_case)
        (string_of_pattern pattern)
  | MemG exp ->
      F.asprintf "%s is in %s" (code_of_exp ctx exp_case) (code_of_exp ctx exp)

(* Prose iterations *)

and prose_of_out_iterexp ctx ((iter, vars) : iterexp) =
  match iter with
  | List ->
      let iterated_var var =
        F.asprintf "%s* be the list of %s" (string_of_var var |> render_mono ctx)
          (string_of_var var |> render_mono ctx)
      in
      List.map iterated_var vars |> prose_of_list
  | Opt -> assert false

and prose_of_in_iterexp ctx ((iter, vars) : iterexp) =
  match iter with
  (* Optional should not appear *)
  | Opt -> "?__"
  | List ->
      let iterated_var var =
        F.asprintf "%s in %s*" (string_of_var var |> render_mono ctx) (string_of_var var |> render_mono ctx)
      in
      List.map iterated_var vars |> prose_of_list

and prose_of_out_iterexps ctx iterexps =
  if List.is_empty iterexps then ""
  else if List.length iterexps > 1 then
    failwith "prosing nested iterations not supported"
  else
    F.asprintf "Let %s, obtained by repeating:"
      (iterexps |> List.hd |> prose_of_out_iterexp ctx)

and prose_of_in_iterexps ctx sep iterexps =
  if List.is_empty iterexps then ""
  else if List.length iterexps > 1 then
    failwith "prosing nested iterations not supported"
  else
    F.asprintf "%sfor each %s" sep (iterexps |> List.hd |> prose_of_in_iterexp ctx)

(* Instruction *)

and render_instr ctx instr =
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      F.asprintf "%sAssert that %s%s\n%s"
        (bullet ctx)
        (prose_of_exp ctx exp_cond)
        (prose_of_in_iterexps ctx ", " iterexps)
        (render_instrs ctx instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      let prosed_relation = 
        let prose_of_hint_opt = Hintenv.get_rel id ctx.penv.prose in
        match prose_of_hint_opt with
        | Some prose_of_hint ->
            let mixop, exps = notexp in
            F.asprintf "[%s](%s)%s"
              (prose_of_hintexp (ctx |> increment_level) exps prose_of_hint)
              (string_of_relid id)
              (prose_of_in_iterexps ctx ", " iterexps)
        | None ->
            F.asprintf "(%s: %s)%s" (string_of_relid id)
              (code_of_notexp ctx notexp) (prose_of_in_iterexps ctx ", " iterexps)
      in
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          F.asprintf "%sIf %s, then\n%s\n%sElse,\n\n%s" 
            (bullet ctx) prosed_relation
            (render_instrs (ctx |> increment_level) instrs_hold)
            (bullet ctx)
            (render_instrs (ctx |> increment_level) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          F.asprintf "%sIf %s, then\n%s"
            (bullet ctx) prosed_relation
            (render_instrs (ctx |> increment_level) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          F.asprintf "%sIf %s does not hold, then\n%s"
            (bullet ctx) prosed_relation
            (render_instrs (ctx |> increment_level) instrs_nothold))
  | CaseI (exp, cases, _) ->
      F.asprintf "%sCase analysis on %s\n%s" (bullet ctx) (code_of_exp ctx exp)
        (prose_of_cases (ctx |> increment_level) exp cases)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise\n%s" (bullet ctx)
        (render_instr (ctx |> increment_level) instr)
  | GroupI (id_group, exps_group, instrs_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" (bullet ctx)
        (string_of_relpathid id_group)
        (match ctx.signature with
        | Some (mixop, inputs) -> code_of_relinput ctx mixop inputs exps_group
        | None -> prose_of_exps ctx exps_group)
        (render_instrs (ctx |> increment_level) instrs_group)
  | LetI (exp_l, exp_r, iterexps) -> (
      let out_iters, in_iters = split_iterexps [ exp_l ] iterexps in 
      (* With no output iterators, print as a single line *)
      if List.is_empty out_iters then
        F.asprintf "%sLet %s be %s%s" (bullet ctx) (code_of_exp ctx exp_l)
          (prose_of_exp ctx exp_r)
          (prose_of_in_iterexps ctx ", " in_iters)
      (* With output iterators, print as a block with the loop contents indented *)
      else
        F.asprintf "%s%s\n%sLet %s be %s%s" (bullet ctx)
          (prose_of_out_iterexps ctx out_iters)
          (ctx |> increment_level |> bullet)
          (code_of_exp ctx exp_l)
          (prose_of_exp ctx exp_r)
          (prose_of_in_iterexps ctx ("\n" ^ bullet ctx) in_iters))
  | RuleI (id_rel, notexp, iterexps) -> (
      let prose_of_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose in
      let input_hint = IEnv.find id_rel ctx.ienv in
      let _, outputs =
        InputHint.split_exps_without_idx input_hint (snd notexp)
      in
      let out_iters, in_iters = split_iterexps outputs iterexps in
      match prose_of_hint_opt with
      | Some prose_of_hint ->
          let mixop, exps = notexp in
          if List.is_empty out_iters then
            F.asprintf "%sLet %s be [%s](%s)%s" (bullet ctx)
              (code_of_exps ctx outputs)
              (prose_of_hintexp (ctx |> increment_level) exps prose_of_hint)
              (string_of_relid id_rel)
          (prose_of_in_iterexps ctx ", " in_iters)
      else
            F.asprintf "%s%s\n%sLet %s be [%s](%s)%s" (bullet ctx)
              (prose_of_out_iterexps ctx out_iters)
              (ctx |> increment_level |> bullet)
              (code_of_exps ctx outputs)
              (prose_of_hintexp (ctx |> increment_level) exps prose_of_hint)
              (string_of_relid id_rel)
              (prose_of_in_iterexps ctx "\n" in_iters)
      | None ->
          F.asprintf "(%s: %s)%s" (string_of_relid id_rel)
            (code_of_notexp ctx notexp) (prose_of_in_iterexps ctx ", " iterexps))
  | ResultI [] -> F.asprintf "%sThe relation holds" (bullet ctx)
  | ResultI exps ->
      F.asprintf "%sResult in %s" (bullet ctx) (prose_of_exps ctx exps)
  | ReturnI exp -> F.asprintf "%sReturn %s" (bullet ctx) (prose_of_exp ctx exp)
  | DebugI exp -> F.asprintf "%sDebug: %s" (bullet ctx) (prose_of_exp ctx exp)

and render_instrs ctx instrs =
  instrs |> List.map (render_instr ctx) |> String.concat "\n"

(* Relations *)

and code_of_relinput ctx mixop inputs exps_input =
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
  code_of_notexp ctx notexp

(* Rule prose *)

let render_ruleprose (ctx : Ctx.t) (mixop : mixop) (inputs : int list)
    (exps_input : exp list) (instrs : instr list) : string =
  "`"
  ^ code_of_relinput ctx mixop inputs exps_input
  ^ "`\n\n" ^ render_instrs ctx instrs

(* Definitions *)

let prose_of_def ctx def =
  match def.it with
  | TypD (typid, tparams, deftyp) -> ""
  | RelD (relid, (_mixop, _inputs), exps_input, instrs, _hints) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ prose_of_exps ctx exps_input
      ^ "\n\n" ^ render_instrs ctx instrs
  | DecD (defid, tparams, args_input, instrs, _hints) -> ""

let prose_of_defs ctx defs = String.concat "" (List.map (prose_of_def ctx) defs)

(* Spec *)

let prose_of_spec ctx spec = prose_of_defs ctx spec
