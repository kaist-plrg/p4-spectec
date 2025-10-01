open Sl.Ast
open Xl
open Sl.Print
open Util.Source
open Ctx
module HEnv = Hintenv
module F = Format
module IEnv = Runtime_static.Envs.IEnv
module InputHint = Runtime_static.Rel.InputHint

(** Utility functions **)

(* Substitue linebreak with indented linebreak *)

let reindent_lines ctx (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ (ctx |> increment_level |> unordered_bullet)) lines

(* Asciidoc monospace rendering *)

let render_mono ctx s =
  match ctx.mode with Code -> s | Prose -> "``" ^ s ^ "``"

let render_subscript s = "~" ^ s ^ "~"
let render_superscript s = "^" ^ s ^ "^"
let render_bold s = "**" ^ s ^ "**"

(* Take only the outputs and construct a full list of expressions, for correct HolE `Next rendering *)
let output_to_signature (out_exps : exp list) (inputs : InputHint.t) :
    exp option list =
  let cursor = ref 0 in
  List.init
    (List.length inputs + List.length out_exps)
    (fun i ->
      let exp : exp option =
        if List.find_opt (fun a -> a = i) inputs |> Option.is_some then None
        else Some (List.nth out_exps !cursor)
      in
      cursor := if exp = None then !cursor else !cursor + 1;
      exp)

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
let as_code printer ctx a = printer (ctx |> in_code) a |> render_mono ctx

let prose_of_cond ctx =
  match ctx.cond_style with
  | Some If -> "If"
  | Some ElseIf -> "Else if"
  | Some Check -> "Check that"
  | None -> ""

let code_of_iter iter =
  match iter with
  | Il.Ast.List -> "*" |> render_superscript
  | Il.Ast.Opt -> "?" |> render_superscript

let code_of_iterexp (iter, _) = string_of_iter iter

let code_of_iterexps iterexps =
  iterexps |> List.map code_of_iterexp |> String.concat ""

let code_of_atom atom = string_of_atom ~lower:false atom
let code_of_atoms atoms = atoms |> List.map code_of_atom |> String.concat " "

let prose_of_cmpop cmpop =
  match cmpop with
  | `EqOp -> "is equal to"
  | `NeOp -> "is not equal to"
  | `LtOp -> "is less than"
  | `GtOp -> "is greater than"
  | `LeOp -> "is less than or equal to"
  | `GeOp -> "is greater than or equal to"

let code_of_mixop mixop =
  let mixop = List.map (List.map it) mixop in
  String.concat " % "
    (List.map
       (fun atoms -> String.concat " " (List.map Xl.Atom.string_of_atom atoms))
       mixop)
  |> String.trim

let code_of_pattern pattern =
  match pattern with
  | Il.Ast.CaseP mixop -> code_of_mixop mixop
  | Il.Ast.ListP `Cons -> "_ :: _"
  | Il.Ast.ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | Il.Ast.ListP `Nil -> "[]"
  | Il.Ast.OptP `Some -> "(_)"
  | Il.Ast.OptP `None -> "()"

let code_of_typ ctx typ = string_of_typ typ |> render_mono ctx

let code_of_varid ctx varid =
  let varid = varid.it in
  if String.starts_with ~prefix:"_" varid then "_" |> render_mono ctx
  else
    let var_slices = String.split_on_char '_' varid in
    match var_slices with
    | var_type :: [] -> var_type |> render_mono ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> render_subscript)
        |> render_mono ctx
    | _ -> assert false

(** Printing as prose **)

(* Prose list: a and b / a, b, ..., y and z *)

let prose_of_list items =
  match items with
  | [] -> ""
  | [ item ] -> item
  | [ item1; item2 ] -> item1 ^ " and " ^ item2
  | _ ->
      let items_rev = List.rev items in
      let items, items_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      String.concat ", " items ^ ", and " ^ items_last

(* Expressions *)

let rec prose_of_exp ctx exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> code_of_varid ctx varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ prose_of_exp ctx exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      (* always print as code *)
      prose_of_exp (ctx |> in_code) exp_l
      ^ " " ^ string_of_binop binop ^ " "
      ^ prose_of_exp (ctx |> in_code) exp_r
      |> render_mono ctx
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      if ctx.mode = Prose then
        prose_of_exp ctx exp_l ^ " " ^ prose_of_cmpop cmpop ^ " "
        ^ prose_of_exp ctx exp_r
      else
        prose_of_exp (ctx |> in_code) exp_l
        ^ " " ^ string_of_cmpop cmpop ^ " "
        ^ prose_of_exp (ctx |> in_code) exp_r
        |> render_mono ctx
  | Il.Ast.UpCastE (_typ, exp) | Il.Ast.DownCastE (_typ, exp) ->
      F.asprintf "%s" (code_of_exp ctx exp)
  | Il.Ast.SubE (exp, typ) ->
      F.asprintf "%s has type %s" (code_of_exp ctx exp) (code_of_typ ctx typ)
  | Il.Ast.MatchE (exp, pattern) ->
      F.asprintf "%s matches pattern %s" (code_of_exp ctx exp)
        (code_of_pattern pattern |> render_mono ctx)
  | Il.Ast.TupleE es -> "(" ^ prose_of_exps ctx es ^ ")"
  | Il.Ast.CaseE notexp -> code_of_notexp ctx notexp
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> code_of_atom atom ^ " " ^ prose_of_exp ctx exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ prose_of_exp ctx exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE [] -> "[ ]" |> render_mono ctx
  | Il.Ast.ListE exps ->
      "[" ^ prose_of_exps (ctx |> in_code) exps ^ "]" |> render_mono ctx
  | Il.Ast.ConsE (exp_h, exp_t) ->
      prose_of_exp (ctx |> in_code) exp_h
      ^ " :: "
      ^ prose_of_exp (ctx |> in_code) exp_t
      |> render_mono ctx
  | Il.Ast.CatE (exp_l, exp_r) ->
      prose_of_exp (ctx |> in_code) exp_l
      ^ " ++ "
      ^ prose_of_exp (ctx |> in_code) exp_r
      |> render_mono ctx
  | Il.Ast.MemE (exp_e, exp_s) ->
      prose_of_exp ctx exp_e ^ " is in " ^ prose_of_exp ctx exp_s
  | Il.Ast.LenE exp -> "the length of " ^ prose_of_exp ctx exp
  | Il.Ast.DotE (exp_b, atom) ->
      prose_of_exp (ctx |> in_code) exp_b ^ "." ^ code_of_atom atom
      |> render_mono ctx
  | Il.Ast.IdxE (exp_b, exp_i) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_exp ctx exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_exp ctx exp_l ^ " : "
      ^ prose_of_exp ctx exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      prose_of_exp ctx exp_b ^ "[" ^ prose_of_path ctx path ^ " = "
      ^ prose_of_exp ctx exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) -> (
      let hintexp_opt = HEnv.get_func defid ctx.penv.prose_in in
      match hintexp_opt with
      | Some hintexp ->
          let exps =
            args
            |> List.filter_map (fun arg ->
                   match arg.it with
                   | Il.Ast.ExpA exp -> Some exp
                   | Il.Ast.DefA _ -> None)
          in
          F.asprintf "<<%s, %s>>" defid.it
            (prose_of_hintexp ctx (exps |> List.map (fun a -> Some a)) hintexp)
      | None ->
          F.asprintf "%s%s%s" (string_of_defid defid) (string_of_targs targs)
            (prose_of_args (ctx |> in_code) args)
          |> render_mono ctx)
  | Il.Ast.IterE (exp, iterexp) ->
      prose_of_exp ctx exp ^ code_of_iterexp iterexp

and prose_of_exps ctx exps = prose_of_list (List.map (prose_of_exp ctx) exps)
and code_of_exp ctx exp = prose_of_exp (ctx |> in_code) exp |> render_mono ctx
and code_of_exps ctx exps = prose_of_list (List.map (code_of_exp ctx) exps)

and code_of_notexp ctx notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> prose_of_exp (ctx |> in_code))
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> render_mono ctx

and prose_of_hintexp ctx (exps : exp option list) (hintexp : El.Ast.exp) :
    string =
  let _, str = prose_of_hintexp' ctx exps hintexp 0 in
  str

and prose_of_hintexp' ctx (exps : exp option list) (hintexp : El.Ast.exp)
    (cursor : int) : int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text |> reindent_lines ctx)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = prose_of_hintexp' ctx exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next -> (
      (* cursor holds position for HoleE.Next *)
      match List.nth exps cursor with
      | Some exp ->
          (* access HoleE.Next with current cursor *)
          (cursor + 1, code_of_exp ctx exp)
      (* skip None *)
      | None -> prose_of_hintexp' ctx exps hintexp (cursor + 1))
  | El.Ast.HoleE (`Num i) -> (
      (* accesses HoleE.Num with index *)
      match List.nth exps i with
      | Some exp -> (cursor, code_of_exp ctx exp)
      (* print _ when `Num is out of bounds *)
      | None -> (cursor, "_" |> render_mono ctx))
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
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> code_of_atom atom
  | Il.Ast.DotP (path, atom) -> prose_of_path ctx path ^ "." ^ code_of_atom atom

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
  match ctx.cond_style with
  | Some Check ->
      F.asprintf "%sCheck that %s\n%s" (bullet ctx)
        (prose_of_guard ctx exp guard)
        (prose_of_instrs (ctx |> clear_cond) instrs)
  | Some _ -> (
      match (guard, instrs) with
      | MatchG _, instr_let :: instrs_rest -> (
          match instr_let.it with
          | LetI (exp_l, exp_r, iterexps) ->
              F.asprintf "%s%s let %s be %s:\n%s" (bullet ctx)
                (prose_of_cond ctx) (code_of_exp ctx exp_l)
                (prose_of_exp ctx exp_r)
                (prose_of_instrs (ctx |> increment_level) instrs_rest)
          | _ ->
              F.asprintf "%s%s %s.\n%s" (bullet ctx) (prose_of_cond ctx)
                (prose_of_guard ctx exp guard)
                (prose_of_instrs (ctx |> increment_level) instrs))
      | _ ->
          F.asprintf "%s%s %s.\n%s" (bullet ctx) (prose_of_cond ctx)
            (prose_of_guard ctx exp guard)
            (prose_of_instrs (ctx |> increment_level) instrs))
  | _ -> failwith "no condition style for case"

and prose_of_cases ctx exp cases =
  let l = List.length cases in
  if l = 1 then prose_of_case (ctx |> as_cond Check) exp (List.hd cases)
  else
    cases
    |> List.mapi (fun i case ->
           if i = 0 then prose_of_case (ctx |> as_cond If) exp case
           else prose_of_case (ctx |> as_cond ElseIf) exp case)
    |> String.concat "\n\n"

and prose_of_guard ctx exp_case guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      F.asprintf "%s %s %s"
        (prose_of_exp ctx exp_case)
        (string_of_cmpop cmpop) (prose_of_exp ctx exp)
  | SubG typ ->
      F.asprintf "%s has type %s" (code_of_exp ctx exp_case)
        (code_of_typ ctx typ)
  | MatchG pattern ->
      F.asprintf "%s matches pattern %s" (code_of_exp ctx exp_case)
        (code_of_pattern pattern |> render_mono ctx)
  | MemG exp ->
      F.asprintf "%s is in %s" (code_of_exp ctx exp_case) (code_of_exp ctx exp)

(* Prose iterations *)

and prose_of_out_iterexp ctx ((iter, vars) : iterexp) =
  match iter with
  | List ->
      let iterated_var var =
        F.asprintf "%s be the list of %s"
          (string_of_var var ^ "*" |> render_mono ctx)
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
        F.asprintf "%s in %s"
          (string_of_var var |> render_mono ctx)
          (string_of_var var ^ "*" |> render_mono ctx)
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
    F.asprintf "%sfor each %s" sep
      (iterexps |> List.hd |> prose_of_in_iterexp ctx)

(* Instruction *)

and prose_of_instr (ctx : Ctx.t) instr =
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      if ctx.cond_style = Some Check then
        F.asprintf "%sCheck that %s%s.\n%s" (bullet ctx)
          (prose_of_exp ctx exp_cond)
          (prose_of_in_iterexps ctx ", " iterexps)
          (prose_of_instrs (ctx |> clear_cond) instrs_then)
      else
        F.asprintf "%sIf %s%s:\n%s" (bullet ctx)
          (prose_of_exp ctx exp_cond)
          (prose_of_in_iterexps ctx ", " iterexps)
          (prose_of_instrs (ctx |> increment_level) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      let prosed_relation =
        let prose_of_hint_opt = Hintenv.get_rel id ctx.penv.prose_true in
        match prose_of_hint_opt with
        | Some prose_of_hint ->
            let mixop, exps = notexp in
            let exps = List.map (fun e -> Some e) exps in
            F.asprintf "[%s](%s)%s"
              (prose_of_hintexp (ctx |> increment_level) exps prose_of_hint)
              (string_of_relid id)
              (prose_of_in_iterexps ctx ", " iterexps)
        | None ->
            F.asprintf "(%s: %s)%s" (string_of_relid id)
              (code_of_notexp ctx notexp)
              (prose_of_in_iterexps ctx ", " iterexps)
      in
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          F.asprintf "%sIf %s, then\n%s\n%sOtherwise\n\n%s" (bullet ctx)
            prosed_relation
            (prose_of_instrs (ctx |> increment_level) instrs_hold)
            (bullet ctx)
            (prose_of_instrs (ctx |> increment_level) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          F.asprintf "%sIf %s, then\n%s" (bullet ctx) prosed_relation
            (prose_of_instrs (ctx |> increment_level) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          F.asprintf "%sIf %s does not hold, then\n%s" (bullet ctx)
            prosed_relation
            (prose_of_instrs (ctx |> increment_level) instrs_nothold))
  | CaseI (exp, cases, _) -> prose_of_cases ctx exp cases
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise\n%s" (bullet ctx)
        (prose_of_instr (ctx |> increment_level) instr)
  | GroupI (id_group, exps_group, instrs_group) ->
      Format.asprintf "%sGroup %s: %s\n\n%s" (bullet ctx)
        (string_of_relpathid id_group)
        (prose_of_exps ctx exps_group)
        (prose_of_instrs (ctx |> increment_level) instrs_group)
  | LetI (exp_l, exp_r, iterexps) ->
      let out_iters, in_iters = split_iterexps [ exp_l ] iterexps in
      (* With no output iterators, print as a single line *)
      if List.is_empty out_iters then
        F.asprintf "%sLet %s be %s%s." (bullet ctx) (code_of_exp ctx exp_l)
          (prose_of_exp ctx exp_r)
          (prose_of_in_iterexps ctx ", " in_iters)
        (* With output iterators, print as a block with the loop contents indented *)
      else
        F.asprintf "%s%s\n%sLet %s be %s%s." (bullet ctx)
          (prose_of_out_iterexps ctx out_iters)
          (ctx |> increment_level |> unordered_bullet)
          (code_of_exp ctx exp_l) (prose_of_exp ctx exp_r)
          (prose_of_in_iterexps ctx ("\n" ^ bullet ctx) in_iters)
  | RuleI (id_rel, notexp, iterexps) -> (
      let prose_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose_in in
      let input_hint = IEnv.find id_rel ctx.ienv in
      let _, outputs =
        InputHint.split_exps_without_idx input_hint (snd notexp)
      in
      let out_iters, in_iters = split_iterexps outputs iterexps in
      match prose_hint_opt with
      | Some prose_hint ->
          let mixop, exps = notexp in
          let exps_opt = List.map (fun e -> Some e) exps in
          if List.is_empty out_iters then
            F.asprintf "%sLet %s be the result of <<%s, %s>>%s." (bullet ctx)
              (code_of_exps ctx outputs) (string_of_relid id_rel)
              (prose_of_hintexp (ctx |> increment_level) exps_opt prose_hint)
              (prose_of_in_iterexps ctx ", " in_iters)
          else
            F.asprintf "%s%s\n%sLet %s be the result of <<%s, %s>>%s."
              (bullet ctx)
              (prose_of_out_iterexps ctx out_iters)
              (ctx |> increment_level |> unordered_bullet)
              (code_of_exps ctx outputs) (string_of_relid id_rel)
              (prose_of_hintexp (ctx |> increment_level) exps_opt prose_hint)
              (prose_of_in_iterexps ctx ("\n" ^ bullet ctx) in_iters)
      | None ->
          F.asprintf "%s(%s: %s)%s." (bullet ctx) (string_of_relid id_rel)
            (code_of_notexp ctx notexp)
            (prose_of_in_iterexps ctx ", " iterexps))
  | ResultI [] -> F.asprintf "%sThe relation holds." (bullet ctx)
  | ResultI exps -> (
      let result_opt =
        match ctx.def with
        | Relation rid ->
            Hintenv.get_rel rid ctx.penv.prose_out
            |> Option.map (fun h -> (h, rid))
        | None -> assert false
      in
      match result_opt with
      | Some (hintexp, rid) ->
          let exps_opt =
            output_to_signature exps
              (IEnv.find_opt rid ctx.ienv |> Option.value ~default:[])
          in
          F.asprintf "%sResult in %s." (bullet ctx)
            (prose_of_hintexp (ctx |> increment_level) exps_opt hintexp)
      | None -> F.asprintf "%sResult in %s." (bullet ctx) (code_of_exps ctx exps)
      )
  | ReturnI exp -> F.asprintf "%sReturn %s." (bullet ctx) (prose_of_exp ctx exp)
  | DebugI exp -> F.asprintf "%sDebug: %s" (bullet ctx) (prose_of_exp ctx exp)

and prose_of_instrs ctx instrs =
  let if_instrs =
    List.filter
      (fun instr ->
        match instr.it with IfI _ | OtherwiseI _ -> true | _ -> false)
      instrs
    |> List.length
  in
  (* When if is unique without else, render as assertion *)
  if if_instrs = 1 then
    instrs
    |> List.map (prose_of_instr (ctx |> as_cond Check))
    |> String.concat "\n"
  else
    instrs
    |> List.mapi (fun i instr ->
           if i = 0 then prose_of_instr (ctx |> as_cond If) instr
           else prose_of_instr (ctx |> as_cond ElseIf) instr)
    |> String.concat "\n"

(* Relations *)

and prose_of_relinput ctx id_rel mixop inputs exps_input =
  let prose_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose_in in
  match prose_hint_opt with
  | Some prose_hint ->
      let exps_opt = List.map Option.some exps_input in
      F.asprintf "%s:"
        (prose_of_hintexp (ctx |> increment_level) exps_opt prose_hint)
      |> String.capitalize_ascii
  | None -> code_of_relinput ctx mixop inputs exps_input

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
  code_of_notexp (ctx |> in_code) notexp |> render_mono ctx

(* Definitions *)

let prose_of_def ctx def =
  match def.it with
  | TypD (typid, tparams, deftyp) -> ""
  | RelD (relid, (_mixop, _inputs), exps_input, instrs, _hints) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ prose_of_exps ctx exps_input
      ^ "\n\n"
      ^ prose_of_instrs (ctx |> in_rel relid) instrs
  | DecD (defid, tparams, args_input, instrs, _hints) -> ""

let prose_of_defs ctx defs = String.concat "" (List.map (prose_of_def ctx) defs)

(* Spec *)

let prose_of_spec ctx spec = prose_of_defs ctx spec

(* Entry points for splicer *)

let code_of_ruleprose (ctx : Ctx.t) (id_rel : id) (mixop : mixop)
    (inputs : int list) (exps_input : exp list) (instrs : instr list) : string =
  F.asprintf "%s\n\n%s"
    (prose_of_relinput ctx id_rel mixop inputs exps_input)
    (prose_of_instrs (ctx |> in_rel id_rel) instrs)

let code_of_funcprose (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (args_input : arg list) (instrs : instr list) : string =
  let prose_of_funcinput =
    F.asprintf "%s%s%s" (string_of_defid id_def)
      (string_of_tparams tparams)
      (prose_of_args (ctx |> in_code) args_input)
    |> render_mono ctx
  in
  F.asprintf "%s\n\n%s" prose_of_funcinput (prose_of_instrs ctx instrs)
