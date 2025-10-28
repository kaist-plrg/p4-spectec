open Ast
open Util.Source
open Sl.Print
open Xl
module F = Format

(* Asciidoc rendering *)

type mode = Code | Prose
type context = { in_code : bool; in_link : bool }

let in_prose = { in_code = false; in_link = false }
let in_code = { in_code = true; in_link = false }
let code context = { context with in_code = true }
let link context = { context with in_link = true }
let adoc_mono ctx s = if ctx.in_code then s else "`" ^ s ^ "`"
let adoc_subscript s = "~" ^ s ^ "~"
let adoc_superscript s = "^" ^ s ^ "^"
let adoc_bold s = "**" ^ s ^ "**"
let adoc_indent level = String.make (level * 2) ' '
let adoc_attach_block level = "+\n"
let adoc_open_block level s = F.asprintf "--\n%s\n--" s

let adoc_ordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet level =
  Format.asprintf "%s%s " (String.make (level * 2) ' ') "*"

let adoc_link ~(link : string) ~(text : string) : string =
  "<<" ^ link ^ ", " ^ text ^ ">>"

(* AST utilities *)

let id_of_funcprose funcprose =
  match funcprose with
  | BoolProse (id, _, _) -> id
  | InputProse (id, _) -> id
  | Def id -> id

(* Printing as prose *)

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

(* Prose list: a and b / a, b, ..., y and z *)

let render_list items =
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

(* Prose and Code rendering *)

(* Identifiers *)

let string_of_relid = Sl.Print.string_of_relid
let string_of_relpathid = Sl.Print.string_of_relpathid
let string_of_defid = Sl.Print.string_of_defid

let code_of_varid ctx varid =
  let varid = varid.it in
  if String.starts_with ~prefix:"_" varid then "_" |> adoc_mono ctx
  else
    let var_slices = String.split_on_char '_' varid in
    match var_slices with
    | var_type :: [] -> var_type |> adoc_mono ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)
        |> adoc_mono ctx
    | _ -> assert false

(* Notation *)

let code_of_atom atom = string_of_atom ~lower:false atom
let code_of_atoms atoms = atoms |> List.map code_of_atom |> String.concat " "

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

let code_of_typ ctx typ = Sl.Print.string_of_typ typ |> adoc_mono ctx

(* Iterators *)

let code_of_iter iter =
  match iter with
  | Il.Ast.List -> "{asterisk}" |> adoc_superscript
  | Il.Ast.Opt -> "?" |> adoc_superscript

let code_of_iterexp (iter, _) = code_of_iter iter

(* Variables *)

let code_of_var ctx (id, _typ, iters) =
  code_of_varid ctx id ^ String.concat "" (List.map code_of_iter iters)

(* Iterated Variables *)

let render_in_itervars ctx vars : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (code_of_var in_code var |> adoc_mono ctx)
      (code_of_var in_code var ^ code_of_iter Il.Ast.List |> adoc_mono ctx)
  in
  List.map render_in_var vars |> render_list

let render_out_itervars ctx vars : string =
  let render_out_var var =
    let id, _, _ = var in
    if String.starts_with ~prefix:"_" id.it then None
    else
      Some
        (F.asprintf "%s be the list of %s"
           (code_of_var in_code var ^ code_of_iter Il.Ast.List |> adoc_mono ctx)
           (code_of_var in_code var |> adoc_mono ctx))
  in
  List.filter_map render_out_var vars |> render_list

let render_branchtype branchtype =
  match branchtype with If -> "If " | ElseIf -> "Else if " | Else -> "Else "

(* Operators *)

let render_cmpop cmpop =
  match cmpop with
  | `EqOp -> "is equal to"
  | `NeOp -> "is not equal to"
  | `LtOp -> "is less than"
  | `GtOp -> "is greater than"
  | `LeOp -> "is less than or equal to"
  | `GeOp -> "is greater than or equal to"

let code_of_cmpop = Sl.Print.string_of_cmpop

(* Expressions *)

let as_code ctx renderer =
  let string = renderer () in
  adoc_mono ctx string

let rec render_exp ctx exp : string =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> adoc_mono ctx
  | VarE varid -> code_of_varid ctx varid
  | UnE (#Bool.unop, _, { it = MatchE (exp, pattern); _ }) ->
      F.asprintf "%s does not match pattern %s" (code_of_exp ctx exp)
        (code_of_pattern pattern |> adoc_mono ctx)
  | UnE (#Bool.unop, _, { it = SubE (exp, typ); _ }) ->
      F.asprintf "%s does not have type %s" (code_of_exp ctx exp)
        (code_of_typ ctx typ)
  | UnE (unop, _, exp) ->
      string_of_unop unop ^ render_exp in_code exp |> adoc_mono ctx
  | BinE (binop, _, exp_l, exp_r) ->
      (* always print as code *)
      render_exp in_code exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ render_exp in_code exp_r
      |> adoc_mono ctx
  | CmpE (cmpop, _, exp_l, exp_r) ->
      if ctx.in_code then
        render_exp in_code exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
        ^ render_exp in_code exp_r
        |> adoc_mono ctx
      else
        render_exp ctx exp_l ^ " " ^ render_cmpop cmpop ^ " "
        ^ render_exp ctx exp_r
  | UpCastE (_typ, exp) | DownCastE (_typ, exp) ->
      F.asprintf "%s" (code_of_exp ctx exp)
  | SubE (exp, typ) ->
      F.asprintf "%s has type %s" (code_of_exp ctx exp) (code_of_typ ctx typ)
  | MatchE (exp, Il.Ast.ListP `Nil) ->
      F.asprintf "%s is an empty list" (code_of_exp ctx exp)
  | MatchE (exp, Il.Ast.ListP `Cons) ->
      F.asprintf "%s is a non-empty list" (code_of_exp ctx exp)
  | MatchE (exp, Il.Ast.ListP (`Fixed len)) ->
      F.asprintf "%s is a list of length %d" (code_of_exp ctx exp) len
  | MatchE (exp, Il.Ast.OptP `None) ->
      F.asprintf "%s is None" (code_of_exp ctx exp)
  | MatchE (exp, Il.Ast.OptP `Some) ->
      F.asprintf "%s is Some value" (code_of_exp ctx exp)
  | MatchE (exp, pattern) ->
      F.asprintf "%s matches pattern %s" (code_of_exp ctx exp)
        (code_of_pattern pattern |> adoc_mono ctx)
  | TupleE es -> "(" ^ render_exps ctx ~sep:(Some ", ") es ^ ")"
  | CaseE (id, renderer) -> render_renderer ctx renderer id
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> code_of_atom atom ^ " " ^ render_exp ctx exp)
             expfields)
      ^ "}"
  | OptE (Some exp) -> "" ^ render_exp ctx exp ^ ""
  | OptE None -> "None" |> adoc_mono ctx
  | ListE [] -> "[ ]" |> adoc_mono ctx
  | ListE exps ->
      "[" ^ render_exps in_code ~sep:(Some ", ") exps ^ "]" |> adoc_mono ctx
  | ConsE (exp_h, exp_t) ->
      render_exp in_code exp_h ^ " :: " ^ render_exp in_code exp_t
      |> adoc_mono ctx
  | CatE (exp_l, exp_r) ->
      render_exp in_code exp_l ^ " ++ " ^ render_exp in_code exp_r
      |> adoc_mono ctx
  | MemE (exp_e, exp_s) ->
      render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s
  | LenE exp -> "the length of " ^ render_exp ctx exp
  | DotE (exp_b, atom) ->
      render_exp in_code exp_b ^ "." ^ code_of_atom atom |> adoc_mono ctx
  | IdxE (exp_b, exp_i) ->
      render_exp ctx exp_b ^ "[" ^ render_exp ctx exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      render_exp ctx exp_b ^ "[" ^ render_exp ctx exp_l ^ " : "
      ^ render_exp ctx exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      render_exp ctx exp_b ^ "[" ^ render_path ctx path ^ " = "
      ^ render_exp ctx exp_f ^ "]"
  | CallE (funcprose, targs, args) -> (
      if ctx.in_code then
        let id = id_of_funcprose funcprose in
        adoc_link ~link:id.it
          ~text:
            (string_of_defid id ^ string_of_targs targs
           ^ render_args in_code args)
        |> adoc_mono ctx
      else
        match funcprose with
        | BoolProse (id, prose_true, _prose_false) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            adoc_link ~link:id.it ~text:(render_hintexp ctx exps prose_true)
        | InputProse (id, prose_in) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            adoc_link ~link:id.it ~text:(render_hintexp ctx exps prose_in)
        | Def id ->
            adoc_link ~link:id.it
              ~text:
                (string_of_defid id ^ string_of_targs targs
               ^ render_args in_code args)
            |> adoc_mono ctx)
  | IterE (exp, iterexp) ->
      if snd iterexp = [] then render_exp ctx exp
      else render_exp in_code exp ^ code_of_iterexp iterexp |> adoc_mono ctx

(* if sep is None, use natural language list *)

and render_exps ctx ?(sep : string option = None) exps =
  match sep with
  | None -> render_list (List.map (render_exp ctx) exps)
  | Some s -> String.concat s (List.map (render_exp ctx) exps)

and code_of_exp ctx (exp : exp) = render_exp in_code exp |> adoc_mono ctx

and code_of_exps ctx ?(sep : string option = None) (exps : exp list) =
  match sep with
  | None -> render_list (List.map (code_of_exp ctx) exps)
  | Some s -> String.concat s (List.map (code_of_exp ctx) exps)

and code_of_notexp ctx notexp =
  let mixop, exps = notexp in
  assert (List.length mixop - List.length exps = 1);
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> render_exp in_code)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> adoc_mono ctx

and code_of_relinput ctx notexp =
  let mixop, exps_input = notexp in
  let exps =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.nth_opt exps_input idx with
        | Some exp_input -> exp_input
        | None -> VarE ("%" $ no_region) $$ (no_region, Il.Ast.TextT))
  in
  let notexp = (mixop, exps) in
  code_of_notexp ctx notexp

and render_hintexp ctx (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = render_hintexp' ctx exps hintexp 0 in
  str

and render_hintexp' ctx (exps : exp list) (hintexp : El.Ast.exp) (cursor : int)
    : int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text |> reindent_lines ~level:0)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = render_hintexp' ctx exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
      (* cursor holds position for HoleE.Next *)
      let exp = List.nth exps cursor in
      (* access HoleE.Next with current cursor *)
      (cursor + 1, render_exp ctx exp)
  | El.Ast.HoleE (`Num i) ->
      (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, render_exp ctx exp)
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = render_hintexp' ctx exps exp_l cursor in
      let cursor_r, str_r = render_hintexp' ctx exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

and render_renderer ctx (renderer : relcall) id : string =
  match renderer with
  | Prose (hintexp, [], exps_in) -> render_hintexp ctx exps_in hintexp
  | Prose (hintexp, exps_out, exps_in) -> assert false
  | Mixop (mixop, exps) -> code_of_relinput in_prose (mixop, exps)

(* Paths *)

and render_path ctx path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> render_path ctx path ^ "[" ^ render_exp ctx exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      render_path ctx path ^ "[" ^ render_exp ctx exp_l ^ " : "
      ^ render_exp ctx exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) -> render_path ctx path ^ "." ^ code_of_atom atom

and render_arg ctx arg =
  match arg.it with
  | ExpA exp -> render_exp ctx exp
  | DefA defid -> string_of_defid defid

(* TODO: prose *)
and render_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (render_arg ctx) args) ^ ")"

let string_of_targs = Sl.Print.string_of_targs

let render_relcall ?(level = 0) (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      adoc_link ~link:(string_of_relid rid)
        ~text:(render_hintexp in_prose exps_in hintexp)
  | Prose (hintexp, exps_out, exps_in) ->
      F.asprintf "%s be the result of %s"
        (code_of_exps in_prose exps_out)
        (adoc_link ~link:(string_of_relid rid)
           ~text:(render_hintexp in_prose exps_in hintexp))
  | Mixop (mixop, exps) ->
      adoc_link ~link:(string_of_relid rid)
        ~text:(code_of_notexp in_prose (mixop, exps))

let render_reldef (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      adoc_link ~link:(string_of_relid rid)
        ~text:(render_hintexp in_prose exps_in hintexp)
      ^ " is defined as:"
  | Prose (hintexp, exps_out, exps_in) -> assert false
  | Mixop (mixop, exps) ->
      adoc_link ~link:(string_of_relid rid)
        ~text:(code_of_relinput in_prose (mixop, exps))

(* Conditions *)

let rec render_cond ctx (cond : cond) : string =
  match cond with
  | ExpCond exp -> render_exp ctx exp
  | RelCond (relcall, relid) -> render_relcall relcall relid
  | ForAllCond (cond, vars) ->
      F.asprintf "%s, for all %s" (render_cond ctx cond)
        (render_in_itervars ctx vars)
  | ForAnyCond (cond, vars) -> assert false
(* F.asprintf "%s, for any %s" *)
(*   (render_cond ctx cond) (render_in_itervars ctx vars) *)

let rec render_instr ?(level = 0) ?(unordered = false) (instr : instr) : string
    =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  match instr.it with
  | BranchI
      ( branchtype,
        ExpCond { it = MatchE (exp, _); _ },
        { it = LetI (exp_l, exp_r); _ } :: instrs_rest )
    when Eq.eq_exp exp_r exp ->
      F.asprintf "%s%slet %s be %s:%s" bullet
        (render_branchtype branchtype)
        (as_code in_prose (fun () -> render_exp in_code exp_l))
        (render_exp in_prose exp_r)
        (render_instrs ~level:(level + 1) instrs_rest)
  | BranchI
      ( branchtype,
        ExpCond { it = SubE (exp, typ); _ },
        { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
        :: instrs_rest )
    when Eq.eq_exp exp_r exp && Eq.eq_typ typ_r typ ->
      F.asprintf "%s%slet %s be %s:%s" bullet
        (render_branchtype branchtype)
        (as_code in_prose (fun () -> render_exp in_code exp_l))
        (render_exp in_prose exp_r)
        (render_instrs ~level:(level + 1) instrs_rest)
  | BranchI (branchtype, cond, instrs) ->
      F.asprintf "%s%s%s:%s" bullet
        (render_branchtype branchtype)
        (render_cond in_prose cond)
        (render_instrs ~level:(level + 1) instrs)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise:%s" bullet
        (render_instrs ~level:(level + 1) [ instr ])
  | CheckI cond ->
      F.asprintf "%sCheck that %s." bullet (render_cond in_prose cond)
  | LetI (exp_l, exp_r) ->
      F.asprintf "%sLet %s be %s." bullet
        (as_code in_prose (fun () -> render_exp in_code exp_l))
        (render_exp in_prose exp_r)
  | RelI (relcall, rid) ->
      F.asprintf "%sLet %s." bullet (render_relcall ~level relcall rid)
  | ReturnI exp -> F.asprintf "%sReturn %s." bullet (code_of_exp in_prose exp)
  | ResultI (Some hintexp, exps) ->
      F.asprintf "%sResult in %s." bullet (render_hintexp in_prose exps hintexp)
  | ResultI (None, []) -> bullet ^ "The relation holds."
  | ResultI (None, exps) ->
      F.asprintf "%sResult in %s." bullet (render_exps in_prose exps)
  | GroupI (id, _, instrs) ->
      F.asprintf "%sGroup %s:%s" bullet (string_of_relpathid id)
        (render_instrs ~level:(level + 1) instrs)
  | ForEachI ([], instr, vars_in) ->
      F.asprintf "%s%s, for each %s" bullet
        (render_instr ~level instr)
        (render_in_itervars in_prose vars_in)
  | ForEachI (vars_out, instr, vars_in) ->
      F.asprintf "%sLet %s, obtained by repeating:\n%s%s\n%sfor each %s" bullet
        (render_out_itervars in_prose vars_out)
        (adoc_attach_block level)
        (render_instr ~level:(level + 1) ~unordered:true instr
        |> adoc_open_block level)
        (adoc_attach_block level)
        (render_in_itervars in_prose vars_in)
  | CheckLetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~type~ %s be %s." bullet
        (code_of_exp in_prose exp_l)
        (render_exp in_prose exp_r)
  | OptionGetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~option~ %s be %s." bullet
        (code_of_exp in_prose exp_l)
        (render_exp in_prose exp_r)

and render_instrs ?(level = 0) instrs =
  let instrs = Shorthand.apply_all_shorthands instrs in
  match instrs with
  | [ { it = ReturnI ({ it = BoolE b; _ } as exp); _ } ] ->
      F.asprintf " return %s." (code_of_exp in_prose exp)
  | instrs ->
      "\n" ^ (List.map (render_instr ~level) instrs |> String.concat "\n")

let render_def (def : def) : string =
  match def.it with
  | RelD (relid, exps_input, instrs) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ render_exps in_prose exps_input
      ^ "\n\n" ^ render_instrs instrs
  | DecD _ -> ""

let render_defs defs = List.map render_def defs |> String.concat "\n"

(* entrypoint for binary *)

let render_spec (spec : spec) = render_defs spec

(* entrypoint for splicer *)

let render_rulegroup (relcall, id, instrs) : string =
  F.asprintf "%s\n\n%s" (render_reldef relcall id) (render_instrs instrs)

let render_funcdef (funcprose : funcprose) (tparams : tparam list)
    (args : arg list) : string =
  let exps_input =
    args
    |> List.filter_map (fun arg ->
           match arg.it with ExpA exp -> Some exp | DefA _ -> None)
  in
  match funcprose with
  | BoolProse (_id, prose_true, _prose_false) ->
      render_hintexp in_prose exps_input prose_true
  | InputProse (_id, prose_in) -> render_hintexp in_prose exps_input prose_in
  | Def id ->
      string_of_defid id ^ string_of_tparams tparams ^ render_args in_code args
      |> adoc_mono in_prose

let render_func (funcprose, tparams, args, instrs) : string =
  F.asprintf "%s\n\n%s"
    (render_funcdef funcprose tparams args)
    (render_instrs instrs)
