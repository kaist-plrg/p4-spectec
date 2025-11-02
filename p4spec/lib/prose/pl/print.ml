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
let in_link = { in_code = false; in_link = true }
let code context = { context with in_code = true }
let link context = { context with in_link = true }
let adoc_mono s = "`" ^ s ^ "`"
let adoc_subscript s = "~" ^ s ^ "~"
let adoc_superscript s = "^" ^ s ^ "^"
let adoc_bold s = "**" ^ s ^ "**"
let adoc_indent level = String.make (level * 2) ' '
let adoc_attach_block _level = "+\n"
let adoc_open_block _level s = F.asprintf "--\n%s\n--" s

let adoc_ordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet level =
  Format.asprintf "%s%s " (String.make (level * 2) ' ') "*"

let adoc_link ~(link : string) (text : string) : string =
  "<<" ^ link ^ ", " ^ text ^ ">>"

let as_code ctx string = if ctx.in_code then string else adoc_mono string
let as_link ctx ~link text = if ctx.in_link then text else adoc_link ~link text

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
let code_of_unop = Sl.Print.string_of_unop
let code_of_binop = Sl.Print.string_of_binop

let render_varid ctx varid =
  if String.starts_with ~prefix:"_" varid.it then "++_++" |> as_code ctx
  else
    let varid = varid.it in
    let var_slices = String.split_on_char '_' varid in
    match var_slices with
    | var_type :: [] -> var_type |> as_code ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)
        |> as_code ctx
    | _ -> assert false

(* Notation *)

let code_of_atom atom =
  match atom.it with
  | Atom.SilentAtom _ -> ""
  | Atom.Tick -> ""
  | _ -> Atom.string_of_atom atom.it

let code_of_atoms atoms = atoms |> List.map code_of_atom |> String.concat " "

let code_of_mixop mixop =
  let mixop = List.map (List.map it) mixop in
  String.concat " % "
    (List.map
       (fun atoms -> String.concat " " (List.map Xl.Atom.string_of_atom atoms))
       mixop)
  |> String.trim

let code_of_pattern (pattern : pattern) =
  match pattern with
  | CaseP mixop -> code_of_mixop mixop
  | ListP `Cons -> "_ :: _"
  | ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | ListP `Nil -> "[]"
  | OptP `Some -> "(_)"
  | OptP `None -> "()"

let code_of_typ ctx typ = Sl.Print.string_of_typ typ |> as_code ctx

(* Iterators *)

let code_of_iter (iter : iter) =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript
  | Opt -> "?" |> adoc_superscript

let code_of_iterexp (iter, _) = code_of_iter iter

(* Variables *)

let render_var ctx (id, _typ, iters) =
  if String.starts_with ~prefix:"_" id.it then "++_++" |> as_code ctx
  else render_varid ctx id ^ String.concat "" (List.map code_of_iter iters)

(* Iterated Variables *)

let render_in_itervars ctx vars : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (render_var in_code var |> as_code ctx)
      (render_var in_code var ^ code_of_iter List |> as_code ctx)
  in
  List.map render_in_var vars |> render_list

let render_out_itervars ctx vars : string =
  let render_out_var var =
    let id, _, _ = var in
    (* Do not render iterated underscored variables *)
    if String.starts_with ~prefix:"_" id.it then None
    else
      Some
        (F.asprintf "%s be the list of %s"
           (render_var in_code var ^ code_of_iter List |> as_code ctx)
           (render_var in_code var |> as_code ctx))
  in
  List.filter_map render_out_var vars |> render_list

let render_branchtype branchtype =
  match branchtype with If -> "If " | ElseIf -> "Else if " | Else -> "Else "

(* Operators *)

let render_cmpop ctx cmpop =
  if ctx.in_code then Sl.Print.string_of_cmpop cmpop
  else
    match cmpop with
    | `EqOp -> "is equal to"
    | `NeOp -> "is not equal to"
    | `LtOp -> "is less than"
    | `GtOp -> "is greater than"
    | `LeOp -> "is less than or equal to"
    | `GeOp -> "is greater than or equal to"

(* Expressions *)

let rec render_exp ctx exp : string =
  let in_code = code ctx in
  match exp.it with
  | BoolE b -> string_of_bool b |> as_code ctx
  | NumE n -> string_of_num n |> as_code ctx
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> as_code ctx
  | VarE varid -> render_varid in_code varid |> as_code ctx
  | UnE (#Bool.unop, _, { it = MatchE (exp, pattern); _ }) ->
      F.asprintf "%s does not match pattern %s"
        (render_exp_as_code ctx exp)
        (code_of_pattern pattern |> as_code ctx)
  | UnE (#Bool.unop, _, { it = SubE (exp, typ); _ }) ->
      F.asprintf "%s does not have type %s"
        (render_exp_as_code ctx exp)
        (code_of_typ ctx typ)
  | UnE (unop, _, exp) ->
      (* always print as code *)
      code_of_unop unop ^ render_exp in_code exp |> as_code ctx
  | BinE (binop, _, exp_l, exp_r) ->
      (* always print as code *)
      render_exp in_code exp_l ^ " " ^ code_of_binop binop ^ " "
      ^ render_exp in_code exp_r
      |> as_code ctx
  | CmpE (cmpop, _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx exp_r
  | UpCastE (_typ, exp) | DownCastE (_typ, exp) -> render_exp_as_code ctx exp
  | SubE (exp, typ) ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp)
        (code_of_typ ctx typ)
  | MatchE (exp, Il.Ast.ListP `Nil) ->
      F.asprintf "%s is an empty list" (render_exp_as_code ctx exp)
  | MatchE (exp, Il.Ast.ListP `Cons) ->
      F.asprintf "%s is a non-empty list" (render_exp_as_code ctx exp)
  | MatchE (exp, Il.Ast.ListP (`Fixed len)) ->
      F.asprintf "%s is a list of length %d" (render_exp_as_code ctx exp) len
  | MatchE (exp, Il.Ast.OptP `None) ->
      F.asprintf "%s is None" (render_exp_as_code ctx exp)
  | MatchE (exp, Il.Ast.OptP `Some) ->
      F.asprintf "%s is Some value" (render_exp_as_code ctx exp)
  | MatchE (exp, pattern) ->
      F.asprintf "%s matches pattern %s"
        (render_exp_as_code ctx exp)
        (code_of_pattern pattern |> as_code ctx)
  | TupleE es -> "(" ^ render_exps ctx ~sep:", " es ^ ")"
  | CaseE (id, mixop, exps, prose_hint) -> (
      if ctx.in_code then code_of_notexp ctx (mixop, exps)
      else
        match prose_hint with
        | Some hintexp ->
            render_hintexp (ctx |> link) exps hintexp |> as_link ctx ~link:id.it
        | None -> code_of_notexp ctx (mixop, exps))
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> code_of_atom atom ^ " " ^ render_exp ctx exp)
             expfields)
      ^ "}"
  | OptE (Some exp) -> "" ^ render_exp ctx exp ^ ""
  | OptE None -> "None" |> as_code ctx
  | ListE [] -> "[ ]" |> as_code ctx
  | ListE exps -> "[" ^ render_exps in_code ~sep:", " exps ^ "]" |> as_code ctx
  | ConsE (exp_h, exp_t) ->
      (* always print as code *)
      render_exp in_code exp_h ^ " :: " ^ render_exp in_code exp_t
      |> as_code ctx
  | CatE (exp_l, exp_r) ->
      (* always print as code *)
      render_exp in_code exp_l ^ " ++ " ^ render_exp in_code exp_r
      |> as_code ctx
  | MemE (exp_e, exp_s) ->
      render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s
  | LenE exp -> "the length of " ^ render_exp ctx exp
  | DotE (exp_b, atom) ->
      render_exp in_code exp_b ^ "." ^ code_of_atom atom |> as_code ctx
  | IdxE (exp_b, exp_i) ->
      render_exp in_code exp_b ^ "[" ^ render_exp in_code exp_i ^ "]"
      |> as_code ctx
  | SliceE (exp_b, exp_l, exp_h) ->
      (* always print as code *)
      render_exp in_code exp_b ^ "[" ^ render_exp in_code exp_l ^ " : "
      ^ render_exp in_code exp_h ^ "]"
      |> as_code ctx
  | UpdE (exp_b, path, exp_f) ->
      (* always print as code *)
      render_exp in_code exp_b ^ "[" ^ render_path in_code path ^ " = "
      ^ render_exp in_code exp_f ^ "]"
      |> as_code ctx
  | CallE (funcprose, targs, args) -> (
      if ctx.in_code then
        let id = id_of_funcprose funcprose in
        string_of_defid id ^ string_of_targs targs
        ^ render_args (ctx |> link |> code) args
        |> as_link ctx ~link:id.it |> as_code ctx
      else
        match funcprose with
        | BoolProse (id, prose_true, _prose_false) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            render_hintexp (link ctx) exps prose_true |> as_link ctx ~link:id.it
        | InputProse (id, prose_in) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            render_hintexp (link ctx) exps prose_in |> as_link ctx ~link:id.it
        | Def id ->
            string_of_defid id ^ string_of_targs targs
            ^ render_args (ctx |> link |> code) args
            |> as_link ctx ~link:id.it |> as_code ctx)
  | IterE (exp, (_, [])) -> render_exp ctx exp
  | IterE (({ it = VarE varid; _ } as exp), iterexp) ->
      render_exp in_code exp ^ code_of_iterexp iterexp |> as_code ctx
  | IterE (exp, iterexp) ->
      "(" ^ render_exp in_code exp ^ ")" ^ code_of_iterexp iterexp
      |> as_code ctx

(* if sep is None, use natural language list *)

and render_exps ctx ?sep:sep_opt exps =
  match (ctx.in_code, sep_opt) with
  | _, Some s -> String.concat s (List.map (render_exp ctx) exps)
  | true, None -> String.concat ", " (List.map (render_exp ctx) exps)
  | false, None -> render_list (List.map (render_exp ctx) exps)

and render_exp_as_code ctx (exp : exp) =
  render_exp (code ctx) exp |> as_code ctx

and code_of_notexp ctx notexp =
  let mixop, exps = notexp in
  assert (List.length mixop - List.length exps = 1);
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> render_exp in_code)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> as_code ctx

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

and render_renderer ctx (renderer : relcall) _id : string =
  match renderer with
  | Prose (hintexp, [], exps_in) -> render_hintexp ctx exps_in hintexp
  | Prose _ -> assert false
  | Mixop (mixop, exps) -> code_of_relinput ctx (mixop, exps)

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

let render_relcall (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      render_hintexp in_link exps_in hintexp
      |> as_link in_prose ~link:(string_of_relid rid)
  | Prose (hintexp, exps_out, exps_in) ->
      F.asprintf "%s be the result of %s"
        (render_exps in_prose exps_out)
        (render_hintexp in_link exps_in hintexp
        |> as_link in_prose ~link:(string_of_relid rid))
  | Mixop (mixop, exps) ->
      code_of_notexp in_link (mixop, exps)
      |> as_link in_prose ~link:(string_of_relid rid)

let render_reldef (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      (render_hintexp in_link exps_in hintexp
      |> as_link in_prose ~link:(string_of_relid rid))
      ^ " is defined as:"
  | Prose _ -> assert false
  | Mixop (mixop, exps) ->
      code_of_relinput in_link (mixop, exps)
      |> as_link in_prose ~link:(string_of_relid rid)

(* Conditions *)

let rec render_cond ctx (cond : cond) : string =
  match cond with
  | ExpCond exp -> render_exp ctx exp
  | RelCond (relcall, relid) -> render_relcall relcall relid
  | ForAllCond (cond, vars) ->
      F.asprintf "%s, for all %s" (render_cond ctx cond)
        (render_in_itervars ctx vars)
  | ForAnyCond _ -> assert false
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
        (render_exp in_code exp_l |> as_code in_prose)
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
        (render_exp_as_code in_prose exp_l)
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
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
  | RelI (relcall, rid) ->
      F.asprintf "%sLet %s." bullet (render_relcall relcall rid)
  | ReturnI exp ->
      F.asprintf "%sReturn %s." bullet
        (render_exp in_code exp |> as_code in_prose)
  | ResultI (Some hintexp, exps) ->
      F.asprintf "%sResult in %s." bullet (render_hintexp in_prose exps hintexp)
  | ResultI (None, []) -> bullet ^ "The relation holds."
  | ResultI (None, exps) ->
      F.asprintf "%sResult in %s." bullet (render_exps in_prose exps)
  | GroupI (id, _, instrs) ->
      F.asprintf "%sGroup %s:%s" bullet (string_of_relpathid id)
        (render_instrs ~level:(level + 1) instrs)
  | DestructI (partial_binds, exp_r) ->
      let exps, fieldnames = List.split partial_binds in
      F.asprintf "%sLet %s be %s of %s." bullet
        (render_exps in_prose exps)
        (render_list (List.map (fun s -> "the " ^ s) fieldnames))
        (render_exp in_prose exp_r)
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
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
  | OptionGetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~option~ %s be %s." bullet
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)

and render_instrs ?(level = 0) instrs =
  let instrs = Shorthand.apply_all_shorthands instrs in
  match instrs with
  | [ { it = ReturnI ({ it = BoolE _; _ } as exp); _ } ] ->
      F.asprintf " return %s." (render_exp_as_code in_prose exp)
  | instrs ->
      "\n" ^ (List.map (render_instr ~level) instrs |> String.concat "\n")

let render_def (def : def) : string =
  match def.it with
  | ExternRelD (relid, exps_input) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ render_exps in_prose exps_input
  | RelD (relid, exps_input, instrs) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ render_exps in_prose exps_input
      ^ "\n\n" ^ render_instrs instrs
  | BuiltinDecD _ | DecD _ -> ""

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
      |> as_code in_prose

let render_func (funcprose, tparams, args, instrs) : string =
  F.asprintf "%s\n\n%s"
    (render_funcdef funcprose tparams args)
    (render_instrs instrs)
