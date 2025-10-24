open Ast
open Util.Source
open Sl.Print
open Xl
module F = Format

(* Asciidoc rendering *)

type mode = Code | Prose

let render_mono ~mode s = match mode with Code -> s | Prose -> "`" ^ s ^ "`"
let render_subscript s = "~" ^ s ^ "~"
let render_superscript s = "^" ^ s ^ "^"
let render_bold s = "**" ^ s ^ "**"
let render_indent level = String.make (level * 2) ' '
let render_attach_block level = "+\n"
let render_open_block level s = F.asprintf "--\n%s\n--" s

let render_ordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let render_unordered_bullet level =
  Format.asprintf "%s%s " (String.make (level * 2) ' ') "*"

let render_link ~(link : string) ~(text : string) : string =
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
  String.concat ("\n" ^ render_unordered_bullet level) lines

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

(* Prose and Code rendering *)

(* Identifiers *)

let string_of_relid = Sl.Print.string_of_relid
let string_of_relpathid = Sl.Print.string_of_relpathid
let string_of_defid = Sl.Print.string_of_defid

let code_of_varid ?(mode = Prose) varid =
  let varid = varid.it in
  if String.starts_with ~prefix:"_" varid then "_" |> render_mono ~mode
  else
    let var_slices = String.split_on_char '_' varid in
    match var_slices with
    | var_type :: [] -> var_type |> render_mono ~mode
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> render_subscript)
        |> render_mono ~mode
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

let code_of_typ ~mode typ = Sl.Print.string_of_typ typ |> render_mono ~mode

(* Iterators *)

let code_of_iter iter =
  match iter with
  | Il.Ast.List -> "{asterisk}" |> render_superscript
  | Il.Ast.Opt -> "?" |> render_superscript

let code_of_iterexp (iter, _) = code_of_iter iter

(* Variables *)

let code_of_var ~mode (id, _typ, iters) =
  code_of_varid ~mode id ^ String.concat "" (List.map code_of_iter iters)

(* Iterated Variables *)

let prose_of_in_itervars ?(mode = Prose) vars : string =
  let prose_of_in_var var =
    F.asprintf "%s in %s"
      (code_of_var ~mode:Code var |> render_mono ~mode)
      (code_of_var ~mode:Code var ^ code_of_iter Il.Ast.List
      |> render_mono ~mode)
  in
  List.map prose_of_in_var vars |> prose_of_list

let prose_of_out_itervars ?(mode = Prose) vars : string =
  let prose_of_out_var var =
    F.asprintf "%s be the list of %s"
      (code_of_var ~mode:Code var ^ code_of_iter Il.Ast.List
      |> render_mono ~mode)
      (code_of_var ~mode:Code var |> render_mono ~mode)
  in
  List.map prose_of_out_var vars |> prose_of_list

let prose_of_branchtype branchtype =
  match branchtype with If -> "If " | ElseIf -> "Else if " | Else -> "Else "

(* Operators *)

let prose_of_cmpop cmpop =
  match cmpop with
  | `EqOp -> "is equal to"
  | `NeOp -> "is not equal to"
  | `LtOp -> "is less than"
  | `GtOp -> "is greater than"
  | `LeOp -> "is less than or equal to"
  | `GeOp -> "is greater than or equal to"

let code_of_cmpop = Sl.Print.string_of_cmpop

(* Expressions *)

let rec prose_of_exp ?(mode = Prose) exp : string =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> render_mono ~mode
  | VarE varid -> code_of_varid ~mode varid
  | UnE (#Bool.unop, _, { it = MatchE (exp, pattern); _ }) ->
      F.asprintf "%s does not match pattern %s" (code_of_exp ~mode exp)
        (code_of_pattern pattern |> render_mono ~mode)
  | UnE (#Bool.unop, _, { it = SubE (exp, typ); _ }) ->
      F.asprintf "%s does not have type %s" (code_of_exp ~mode exp)
        (code_of_typ ~mode typ)
  | UnE (unop, _, exp) ->
      string_of_unop unop ^ prose_of_exp ~mode:Code exp |> render_mono ~mode
  | BinE (binop, _, exp_l, exp_r) ->
      (* always print as code *)
      prose_of_exp ~mode:Code exp_l
      ^ " " ^ string_of_binop binop ^ " "
      ^ prose_of_exp ~mode:Code exp_r
      |> render_mono ~mode
  | CmpE (cmpop, _, exp_l, exp_r) ->
      if mode = Prose then
        prose_of_exp ~mode exp_l ^ " " ^ prose_of_cmpop cmpop ^ " "
        ^ prose_of_exp ~mode exp_r
      else
        prose_of_exp ~mode:Code exp_l
        ^ " " ^ string_of_cmpop cmpop ^ " "
        ^ prose_of_exp ~mode:Code exp_r
        |> render_mono ~mode
  | UpCastE (_typ, exp) | DownCastE (_typ, exp) ->
      F.asprintf "%s" (code_of_exp ~mode exp)
  | SubE (exp, typ) ->
      F.asprintf "%s has type %s" (code_of_exp ~mode exp)
        (code_of_typ ~mode typ)
  | MatchE (exp, Il.Ast.ListP `Nil) ->
      F.asprintf "%s is an empty list" (code_of_exp ~mode exp)
  | MatchE (exp, Il.Ast.ListP `Cons) ->
      F.asprintf "%s is a non-empty list" (code_of_exp ~mode exp)
  | MatchE (exp, Il.Ast.ListP (`Fixed len)) ->
      F.asprintf "%s is a list of length %d" (code_of_exp ~mode exp) len
  | MatchE (exp, Il.Ast.OptP `None) ->
      F.asprintf "%s is None" (code_of_exp ~mode exp)
  | MatchE (exp, Il.Ast.OptP `Some) ->
      F.asprintf "%s is Some value" (code_of_exp ~mode exp)
  | MatchE (exp, pattern) ->
      F.asprintf "%s matches pattern %s" (code_of_exp ~mode exp)
        (code_of_pattern pattern |> render_mono ~mode)
  | TupleE es -> "(" ^ prose_of_exps ~mode ~sep:(Some ", ") es ^ ")"
  | CaseE notexp -> code_of_notexp ~mode notexp
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) ->
               code_of_atom atom ^ " " ^ prose_of_exp ~mode exp)
             expfields)
      ^ "}"
  | OptE (Some exp) -> "" ^ prose_of_exp ~mode exp ^ ""
  | OptE None -> "None" |> render_mono ~mode
  | ListE [] -> "[ ]" |> render_mono ~mode
  | ListE exps ->
      "[" ^ prose_of_exps ~mode:Code ~sep:(Some ", ") exps ^ "]"
      |> render_mono ~mode
  | ConsE (exp_h, exp_t) ->
      prose_of_exp ~mode:Code exp_h ^ " :: " ^ prose_of_exp ~mode:Code exp_t
      |> render_mono ~mode
  | CatE (exp_l, exp_r) ->
      prose_of_exp ~mode:Code exp_l ^ " ++ " ^ prose_of_exp ~mode:Code exp_r
      |> render_mono ~mode
  | MemE (exp_e, exp_s) ->
      prose_of_exp ~mode exp_e ^ " is in " ^ prose_of_exp ~mode exp_s
  | LenE exp -> "the length of " ^ prose_of_exp ~mode exp
  | DotE (exp_b, atom) ->
      prose_of_exp ~mode:Code exp_b ^ "." ^ code_of_atom atom
      |> render_mono ~mode
  | IdxE (exp_b, exp_i) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_exp ~mode exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_exp ~mode exp_l ^ " : "
      ^ prose_of_exp ~mode exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_path ~mode path ^ " = "
      ^ prose_of_exp ~mode exp_f ^ "]"
  | CallE (funcprose, targs, args) -> (
      if mode = Code then
        let id = id_of_funcprose funcprose in
        render_link ~link:id.it
          ~text:
            (string_of_defid id ^ string_of_targs targs
            ^ prose_of_args ~mode:Code args)
        |> render_mono ~mode
      else
        match funcprose with
        | BoolProse (id, prose_true, _prose_false) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            render_link ~link:id.it ~text:(prose_of_hintexp exps prose_true)
        | InputProse (id, prose_in) ->
            let exps =
              args
              |> List.filter_map (fun arg ->
                     match arg.it with ExpA exp -> Some exp | DefA _ -> None)
            in
            render_link ~link:id.it ~text:(prose_of_hintexp exps prose_in)
        | Def id ->
            render_link ~link:id.it
              ~text:
                (string_of_defid id ^ string_of_targs targs
                ^ prose_of_args ~mode:Code args)
            |> render_mono ~mode)
  | IterE (exp, iterexp) ->
      if snd iterexp = [] then prose_of_exp ~mode exp
      else
        prose_of_exp ~mode:Code exp ^ code_of_iterexp iterexp
        |> render_mono ~mode

(* if sep is None, use natural language list *)

and prose_of_exps ?(mode = Prose) ?(sep : string option = None) exps =
  match sep with
  | None -> prose_of_list (List.map (prose_of_exp ~mode) exps)
  | Some s -> String.concat s (List.map (prose_of_exp ~mode) exps)

and code_of_exp ?(mode = Prose) (exp : exp) =
  prose_of_exp ~mode:Code exp |> render_mono ~mode

and code_of_exps ?(mode = Prose) ?(sep : string option = None) (exps : exp list)
    =
  match sep with
  | None -> prose_of_list (List.map (code_of_exp ~mode) exps)
  | Some s -> String.concat s (List.map (code_of_exp ~mode) exps)

and code_of_notexp ~mode notexp =
  let mixop, exps = notexp in
  assert (List.length mixop - List.length exps = 1);
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> prose_of_exp ~mode:Code)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> render_mono ~mode

and code_of_relinput ~mode notexp =
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
  code_of_notexp ~mode notexp

and prose_of_hintexp (exps : exp list) (hintexp : El.Ast.exp) : string =
  let _, str = prose_of_hintexp' exps hintexp 0 in
  str

and prose_of_hintexp' (exps : exp list) (hintexp : El.Ast.exp) (cursor : int) :
    int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text |> reindent_lines ~level:0)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = prose_of_hintexp' exps exp cur in
            (cur, acc @ [ str ]))
          (cursor, []) exps_hint
      in
      (cursor, String.concat " " strs)
  | El.Ast.HoleE `Next ->
      (* cursor holds position for HoleE.Next *)
      let exp = List.nth exps cursor in
      (* access HoleE.Next with current cursor *)
      (cursor + 1, code_of_exp ~mode:Prose exp)
  | El.Ast.HoleE (`Num i) ->
      (* accesses HoleE.Num with index *)
      let exp = List.nth exps i in
      (cursor, code_of_exp ~mode:Prose exp)
  | El.Ast.FuseE (exp_l, exp_r) ->
      let cursor_l, str_l = prose_of_hintexp' exps exp_l cursor in
      let cursor_r, str_r = prose_of_hintexp' exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"

(* Paths *)

and prose_of_path ~mode path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) ->
      prose_of_path ~mode path ^ "[" ^ prose_of_exp ~mode exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      prose_of_path ~mode path ^ "[" ^ prose_of_exp ~mode exp_l ^ " : "
      ^ prose_of_exp ~mode exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) -> prose_of_path ~mode path ^ "." ^ code_of_atom atom

and prose_of_arg ~mode arg =
  match arg.it with
  | ExpA exp -> prose_of_exp ~mode exp
  | DefA defid -> string_of_defid defid

(* TODO: prose *)
and prose_of_args ~mode args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (prose_of_arg ~mode) args) ^ ")"

let string_of_targs = Sl.Print.string_of_targs

let prose_of_relcall ?(level = 0) (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      render_link ~link:(string_of_relid rid)
        ~text:(prose_of_hintexp exps_in hintexp)
  | Prose (hintexp, exps_out, exps_in) ->
      F.asprintf "%s be the result of %s"
        (code_of_exps ~mode:Prose exps_out)
        (render_link ~link:(string_of_relid rid)
           ~text:(prose_of_hintexp exps_in hintexp))
  | Mixop (mixop, exps) ->
      render_link ~link:(string_of_relid rid)
        ~text:(code_of_notexp ~mode:Prose (mixop, exps))

let prose_of_reldef (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
      render_link ~link:(string_of_relid rid)
        ~text:(prose_of_hintexp exps_in hintexp)
      ^ " is defined as:"
  | Prose (hintexp, exps_out, exps_in) -> assert false
  | Mixop (mixop, exps) ->
      render_link ~link:(string_of_relid rid)
        ~text:(code_of_relinput ~mode:Prose (mixop, exps))

(* Conditions *)

let rec prose_of_cond ?(mode = Prose) (cond : cond) : string =
  match cond with
  | ExpCond exp -> prose_of_exp ~mode exp
  | RelCond (relcall, relid) -> prose_of_relcall relcall relid
  | ForAllCond (cond, vars) ->
      F.asprintf "%s, for all %s" (prose_of_cond ~mode cond)
        (prose_of_in_itervars ~mode vars)
  | ForAnyCond (cond, vars) -> assert false
(* F.asprintf "%s, for any %s" *)
(*   (prose_of_cond ~mode cond) (prose_of_in_itervars ~mode vars) *)

let rec prose_of_instr ?(level = 0) ?(unordered = false) (instr : instr) :
    string =
  let bullet =
    if unordered then render_unordered_bullet level
    else render_ordered_bullet level
  in
  match instr.it with
  | BranchI
      ( branchtype,
        ExpCond { it = MatchE (exp, _); _ },
        { it = LetI (exp_l, exp_r); _ } :: instrs_rest )
    when Eq.eq_exp exp_r exp ->
      F.asprintf "%s%slet %s be %s:%s" bullet
        (prose_of_branchtype branchtype)
        (code_of_exp exp_l) (prose_of_exp exp_r)
        (prose_of_instrs ~level:(level + 1) instrs_rest)
  | BranchI
      ( branchtype,
        ExpCond { it = SubE (exp, typ); _ },
        { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
        :: instrs_rest )
    when Eq.eq_exp exp_r exp && Eq.eq_typ typ_r typ ->
      F.asprintf "%s%slet %s be %s:%s" bullet
        (prose_of_branchtype branchtype)
        (code_of_exp exp_l) (prose_of_exp exp_r)
        (prose_of_instrs ~level:(level + 1) instrs_rest)
  | BranchI (branchtype, cond, instrs) ->
      F.asprintf "%s%s%s:%s" bullet
        (prose_of_branchtype branchtype)
        (prose_of_cond cond)
        (prose_of_instrs ~level:(level + 1) instrs)
  | OtherwiseI instr ->
      F.asprintf "%sOtherwise:%s" bullet
        (prose_of_instrs ~level:(level + 1) [ instr ])
  | CheckI cond -> F.asprintf "%sCheck that %s." bullet (prose_of_cond cond)
  | LetI (exp_l, exp_r) ->
      F.asprintf "%sLet %s be %s." bullet (code_of_exp exp_l)
        (prose_of_exp exp_r)
  | RelI (relcall, rid) ->
      F.asprintf "%sLet %s." bullet (prose_of_relcall ~level relcall rid)
  | ReturnI exp -> F.asprintf "%sReturn %s." bullet (code_of_exp exp)
  | ResultI (Some hintexp, exps) ->
      F.asprintf "%sResult in %s." bullet (prose_of_hintexp exps hintexp)
  | ResultI (None, []) -> bullet ^ "The relation holds."
  | ResultI (None, exps) ->
      F.asprintf "%sResult in %s." bullet (prose_of_exps exps)
  | GroupI (id, _, instrs) ->
      F.asprintf "%sGroup %s:%s" bullet (string_of_relpathid id)
        (prose_of_instrs ~level:(level + 1) instrs)
  | ForEachI ([], instr, vars_in) ->
      F.asprintf "%s%s, for each %s" bullet
        (prose_of_instr ~level instr)
        (prose_of_in_itervars vars_in)
  | ForEachI (vars_out, instr, vars_in) ->
      F.asprintf "%sLet %s, obtained by repeating:\n%s%s\n%sfor each %s" bullet
        (prose_of_out_itervars vars_out)
        (render_attach_block level)
        (prose_of_instr ~level:(level + 1) ~unordered:true instr
        |> render_open_block level)
        (render_attach_block level)
        (prose_of_in_itervars vars_in)
  | CheckLetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~type~ %s be %s." bullet (code_of_exp exp_l)
        (prose_of_exp exp_r)
  | OptionGetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~option~ %s be %s." bullet (code_of_exp exp_l)
        (prose_of_exp exp_r)

and prose_of_instrs ?(level = 0) instrs =
  let instrs = Shorthand.apply_all_shorthands instrs in
  match instrs with
  | [ { it = ReturnI ({ it = BoolE b; _ } as exp); _ } ] ->
      F.asprintf " return %s." (code_of_exp exp)
  | instrs ->
      "\n" ^ (List.map (prose_of_instr ~level) instrs |> String.concat "\n")

let prose_of_def (def : def) : string =
  match def.it with
  | RelD (relid, exps_input, instrs) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": " ^ prose_of_exps exps_input
      ^ "\n\n" ^ prose_of_instrs instrs
  | DecD _ -> ""

let prose_of_defs defs = List.map prose_of_def defs |> String.concat "\n"
let prose_of_spec (spec : spec) = prose_of_defs spec

(* entrypoint for splicer *)

let prose_of_rulegroup (relcall, id, instrs) : string =
  F.asprintf "%s\n\n%s" (prose_of_reldef relcall id) (prose_of_instrs instrs)

let prose_of_funcdef (funcprose : funcprose) (tparams : tparam list)
    (args : arg list) : string =
  let exps_input =
    args
    |> List.filter_map (fun arg ->
           match arg.it with ExpA exp -> Some exp | DefA _ -> None)
  in
  match funcprose with
  | BoolProse (_id, prose_true, _prose_false) ->
      prose_of_hintexp exps_input prose_true
  | InputProse (_id, prose_in) -> prose_of_hintexp exps_input prose_in
  | Def id ->
      string_of_defid id ^ string_of_tparams tparams
      ^ prose_of_args ~mode:Code args
      |> render_mono ~mode:Prose

let prose_of_func (funcprose, tparams, args, instrs) : string =
  F.asprintf "%s\n\n%s"
    (prose_of_funcdef funcprose tparams args)
    (prose_of_instrs instrs)
