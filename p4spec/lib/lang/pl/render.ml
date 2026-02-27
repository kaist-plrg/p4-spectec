open Domain
open Lib
open Xl
open Ast
open Util.Source
module F = Format

(* Asciidoc rendering context *)

type context = { in_code : bool; in_link : bool }

let in_prose = { in_code = false; in_link = false }
let in_code = { in_code = true; in_link = false }
let in_link = { in_code = false; in_link = true }
let code context = { context with in_code = true }
let link context = { context with in_link = true }

(* Asciidoc utils *)

(* Escaping *)

let rec adoc_escape c text =
  match String.index_opt text c with
  | None -> text
  | Some idx ->
      let text_before = String.sub text 0 idx in
      let text_after =
        String.sub text (idx + 1) (String.length text - idx - 1)
      in
      text_before ^ "+" ^ String.make 1 c ^ "+" ^ adoc_escape c text_after

(* Widths *)

let adoc_width_short = 30
let adoc_fits_in_width_short (s : string) = String.length s <= adoc_width_short

(* Subscript, superscript, and ligature *)

let adoc_subscript (s : string) = "~" ^ s ^ "~"
let adoc_superscript (s : string) = "^" ^ s ^ "^"
let adoc_bold (s : string) = "*" ^ s ^ "*"

(* Monospaced text *)

let adoc_mono (s : string) = "``" ^ s ^ "``"

let adoc_mono_chopped (s : string) =
  s |> String.split_on_char ' ' |> List.map adoc_mono |> String.concat " "

let adoc_as_code (ctx : context) (s : string) : string =
  if ctx.in_code then s else adoc_mono_chopped s

(* Lists *)

let adoc_ordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet level =
  Format.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '*')

(* Links *)

let adoc_link ~(link : string) (text : string) : string =
  let brackets = String.contains text '[' || String.contains text ']' in
  let angles = String.contains text '<' || String.contains text '>' in
  match (brackets, angles) with
  | false, false | false, true -> "xref:" ^ link ^ "[" ^ text ^ "]"
  | true, false -> "<<" ^ link ^ "," ^ text ^ ">>"
  | true, true ->
      Format.eprintf
        "Warning: Asciidoc link text contains both brackets and angle \
         brackets. Link may not render correctly.\n\
         \t%s\n"
        text;
      text

let adoc_as_link (ctx : context) ~link (s : string) : string =
  if ctx.in_link then s else adoc_link ~link s

(* Blocks *)

let adoc_attach_block = "+\n"
let adoc_open_block s = F.asprintf "--\n%s\n--" s

(* Rendering utils *)

let capitalize_first s =
  if String.length s = 0 then s
  else
    let first_char = String.get s 0 |> Char.uppercase_ascii in
    let rest = String.sub s 1 (String.length s - 1) in
    String.make 1 first_char ^ rest

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""

let render_list (items : string list) : string =
  match items with
  | [] -> ""
  | [ item ] -> item
  | [ item_a; item_b ] -> item_a ^ " and " ^ item_b
  | _ ->
      let items_rev = List.rev items in
      let items, item_last =
        (items_rev |> List.tl |> List.rev, items_rev |> List.hd)
      in
      String.concat ", " items ^ ", and " ^ item_last

(* Numbers *)

let string_of_num num = Il.Print.string_of_num num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_relid relid = Il.Print.string_of_relid relid

let string_of_defid ?(link = false) defid =
  if link then Il.Print.string_of_varid defid
  else Il.Print.string_of_defid defid

let render_varid (ctx : context) (id_var : Sl.id) =
  if Id.is_underscored id_var then "++_++" |> adoc_as_code ctx
  else
    let var_slices = String.split_on_char '_' id_var.it in
    match var_slices with
    | [] -> assert false
    | [ var_type ] -> var_type |> adoc_as_code ctx
    | var_type :: var_subscripts ->
        var_type ^ (var_subscripts |> String.concat "_" |> adoc_subscript)
        |> adoc_as_code ctx

(* Notation *)

let code_of_atom atom =
  match atom.it with
  | Atom.Tick -> ""
  | _ -> "+" ^ (atom.it |> Atom.string_of_atom) ^ "+"

let code_of_atoms atoms = atoms |> List.map code_of_atom |> String.concat " "

(* Mixfix operators *)

let code_of_mixop mixop =
  let mixop = List.map (List.map it) mixop in
  String.concat " % "
    (List.map
       (fun atoms -> String.concat " " (List.map Atom.string_of_atom atoms))
       mixop)
  |> String.trim

(* Iterators *)

let code_of_iter (iter : iter) =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript
  | Opt -> "?" |> adoc_superscript

let code_of_iterexp (iter, _) = code_of_iter iter

(* Variables *)

let render_var ctx (id, _typ, iters) =
  if Id.is_underscored id then "++_++" |> adoc_as_code ctx
  else render_varid ctx id ^ String.concat "" (List.map code_of_iter iters)

let render_in_itervars ctx vars : string =
  let render_in_var var =
    F.asprintf "%s in %s"
      (render_var in_code var |> adoc_as_code ctx)
      (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx)
  in
  List.map render_in_var vars |> render_list

let render_out_itervars ctx vars : string =
  let render_out_var var =
    let id, _, _ = var in
    (* Do not render iterated underscored variables *)
    if String.starts_with ~prefix:"_" id.it then None
    else
      Some
        (F.asprintf "%s be the list"
           (render_var in_code var ^ code_of_iter List |> adoc_as_code ctx))
  in
  List.filter_map render_out_var vars |> render_list

(* Types *)

let code_of_typ (ctx : context) (typ : typ) : string =
  Sl.Print.string_of_typ typ |> adoc_as_code ctx

(* Operators *)

let render_unop = Sl.Print.string_of_unop

let render_binop ctx binop =
  if ctx.in_code then Sl.Print.string_of_binop binop
  else
    match binop with
    | `AndOp -> "and"
    | `OrOp -> "or"
    | `ImplOp -> "implies"
    | `EquivOp -> "is equivalent to"
    | _ -> Sl.Print.string_of_binop binop

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

(* Hints *)

let render_alter_hint ?(caps = false) (ctx : context) (hint : Hints.Alter.t)
    (render_base : string -> string) (render : context -> 'a -> string)
    (items : 'a list) : string =
  let render_atom (atom : atom) : string =
    "+" ^ (atom.it |> Atom.string_of_atom) ^ "+" |> adoc_as_code ctx
  in
  items
  |> Hints.Alter.alternate ~base_text:render_base ~base_atom:render_atom hint
       (fun a -> render ctx a)
  |> fun s -> if caps then capitalize_first s else s

(* Call prose *)

let rec render_rel_call (ctx : context) (rel_call : rel_call) : string =
  match rel_call with
  | ProseRelCall (`Hold (id_rel, hint, exps_input)) ->
      render_alter_hint in_link hint (reindent_lines ~level:0) render_exp
        exps_input
      |> adoc_as_link ctx ~link:(string_of_relid id_rel)
  | ProseRelCall
      (`Yield (id_rel, hint_input, exps_input, hint_output, exps_output)) ->
      let prose_out =
        render_alter_hint in_link hint_output unindent_lines render_exp
          exps_output
      in
      let prose_in =
        render_alter_hint in_link hint_input unindent_lines render_exp
          exps_input
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
      in
      if adoc_fits_in_width_short prose_in then
        F.asprintf "%s be the result of %s" prose_out prose_in
      else
        F.asprintf "%s be\n%sthe result of %s" prose_out
          (adoc_unordered_bullet 0) prose_in
  | MathRelCall (id_rel, mixop, exps) ->
      code_of_notexp (in_link |> code) (mixop, exps)
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

(* Expressions *)

and render_exp ctx exp : string =
  let in_code = code ctx in
  match exp.it with
  | BoolE b -> string_of_bool b |> adoc_as_code ctx
  | NumE n -> string_of_num n |> adoc_as_code ctx
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> adoc_as_code ctx
  | VarE id_var -> render_varid in_code id_var |> adoc_as_code ctx
  | UnE (#Bool.unop, _, { it = MatchE (exp, pattern); _ }) ->
      F.asprintf "%s does not match pattern %s" (render_exp ctx exp)
        (code_of_pattern pattern |> adoc_as_code ctx)
  | UnE (#Bool.unop, _, { it = SubE (exp, typ); _ }) ->
      F.asprintf "%s does not have type %s"
        (render_exp_as_code ctx exp)
        (code_of_typ ctx typ)
  | UnE (#Bool.unop, _, { it = MemE (exp_e, exp_s); _ }) ->
      F.asprintf "%s is not in %s"
        (render_exp_as_code ctx exp_e)
        (render_exp_as_code ctx exp_s)
  | UnE
      ( #Bool.unop,
        _,
        { it = CallE (ProseFuncCall (`Check (id, _, hint_false, _, args))); _ }
      )
    when not ctx.in_code ->
      render_alter_hint (link ctx) hint_false (reindent_lines ~level:0)
        render_arg args
      |> adoc_as_link ctx ~link:id.it
  | UnE (unop, _, exp) ->
      render_unop unop ^ render_exp in_code exp |> adoc_as_code ctx
  | BinE (`ImplOp, _, exp_l, exp_r) when not ctx.in_code ->
      "if " ^ render_exp ctx exp_l ^ ", then " ^ render_exp ctx exp_r
  | BinE ((#Bool.binop as binop), _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_binop ctx binop ^ " "
      ^ render_exp ctx exp_r
  | BinE ((#Num.binop as binop), _, exp_l, exp_r) ->
      render_exp in_code exp_l ^ " " ^ render_binop in_code binop ^ " "
      ^ render_exp in_code exp_r
      |> adoc_as_code ctx
  | CmpE (cmpop, _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx exp_r
  | UpCastE (_typ, exp) | DownCastE (_typ, exp) -> render_exp_as_code ctx exp
  | SubE (exp, typ) ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp)
        (code_of_typ ctx typ)
  | MatchE (exp, Il.CaseP mixop) when List.length mixop = 1 ->
      F.asprintf "%s is %s" (render_exp ctx exp)
        (code_of_pattern (Il.CaseP mixop) |> adoc_as_code ctx)
  | MatchE (exp, Il.ListP `Nil) ->
      F.asprintf "%s is an empty list" (render_exp ctx exp)
  | MatchE (exp, Il.ListP `Cons) ->
      F.asprintf "%s is a non-empty list" (render_exp ctx exp)
  | MatchE (exp, Il.ListP (`Fixed len)) ->
      F.asprintf "%s is a list of length %d" (render_exp ctx exp) len
  | MatchE (exp, Il.OptP `None) -> F.asprintf "%s is none" (render_exp ctx exp)
  | MatchE (exp, Il.OptP `Some) ->
      F.asprintf "%s is defined" (render_exp ctx exp)
  | MatchE (exp, pattern) ->
      F.asprintf "%s matches pattern %s" (render_exp ctx exp)
        (code_of_pattern pattern |> adoc_as_code ctx)
  | TupleE es -> "( " ^ render_exps ctx ~sep:", " es ^ " )"
  | CaseE (id, mixop, exps, hint) -> (
      if ctx.in_code then code_of_notexp ctx (mixop, exps)
      else
        match hint with
        | Some hint ->
            render_alter_hint (ctx |> link) hint (reindent_lines ~level:0)
              render_exp exps
            |> adoc_as_link ctx ~link:id.it
        | None -> code_of_notexp ctx (mixop, exps))
  | StrE expfields ->
      "+{+"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> code_of_atom atom ^ " " ^ render_exp ctx exp)
             expfields)
      ^ "+}+"
  | OptE (Some exp) -> "" ^ render_exp ctx exp ^ ""
  | OptE None -> "·" |> adoc_as_code ctx
  | ListE [] -> "·" |> adoc_as_code ctx
  | ListE [ exp ] -> render_exp in_code exp |> adoc_as_code ctx
  | ListE exps ->
      "+[+ " ^ render_exps in_code ~sep:", " exps ^ " +]+" |> adoc_as_code ctx
  | ConsE (exp_h, exp_t) ->
      (* always print as code *)
      render_exp in_code exp_h ^ " {two-colons} " ^ render_exp in_code exp_t
      |> adoc_as_code ctx
  | CatE (exp_l, exp_r) ->
      if ctx.in_code then render_exp ctx exp_l ^ " {pp} " ^ render_exp ctx exp_r
      else render_exp ctx exp_l ^ " concatenated with " ^ render_exp ctx exp_r
  | MemE (exp_e, exp_s) ->
      render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s
  | LenE exp -> "the length of " ^ render_exp ctx exp
  | DotE (exp_b, atom) ->
      render_exp in_code exp_b ^ "." ^ code_of_atom atom |> adoc_as_code ctx
  | IdxE (exp_b, exp_i) ->
      render_exp in_code exp_b ^ "[" ^ render_exp in_code exp_i ^ "]"
      |> adoc_as_code ctx
  | SliceE (exp_b, exp_l, exp_h) ->
      (* always print as code *)
      render_exp in_code exp_b ^ "[" ^ render_exp in_code exp_l ^ " : "
      ^ render_exp in_code exp_h ^ "]"
      |> adoc_as_code ctx
  | UpdE (exp_b, path, exp_f) ->
      if ctx.in_code then
        render_exp in_code exp_b ^ "[" ^ render_path in_code path ^ " = "
        ^ render_exp in_code exp_f ^ "]"
        |> adoc_as_code ctx
      else
        (render_exp in_code exp_b |> adoc_as_code ctx)
        ^ " with "
        ^ (render_path in_code path |> adoc_as_code ctx)
        ^ " set to "
        ^ (render_exp in_code exp_f |> adoc_as_code ctx)
  | CallE func_call when ctx.in_code ->
      let id, targs, args =
        match func_call with
        | ProseFuncCall (`Check (id, _, _, targs, args)) -> (id, targs, args)
        | ProseFuncCall (`Yield (id, _, targs, args)) -> (id, targs, args)
        | MathFuncCall (id, targs, args) -> (id, targs, args)
      in
      string_of_defid id ^ string_of_targs targs
      ^ render_args (ctx |> link |> code) args
      |> adoc_as_link ctx ~link:id.it
      |> adoc_as_code ctx
  | CallE (ProseFuncCall (`Check (id, hint_true, _, _, args))) ->
      render_alter_hint (link ctx) hint_true (reindent_lines ~level:0)
        render_arg args
      |> adoc_as_link ctx ~link:id.it
  | CallE (ProseFuncCall (`Yield (id, hint_in, _targs, args))) ->
      render_alter_hint (link ctx) hint_in (reindent_lines ~level:0) render_arg
        args
      |> adoc_as_link ctx ~link:id.it
  | CallE (MathFuncCall (id, targs, args)) ->
      string_of_defid id ^ string_of_targs targs
      ^ render_args (ctx |> link |> code) args
      |> adoc_as_link ctx ~link:id.it
      |> adoc_as_code ctx
  | IterE (exp, (_, [])) -> render_exp ctx exp
  | IterE (({ it = VarE _; _ } as exp), iterexp)
  | IterE (({ it = TupleE _; _ } as exp), iterexp) ->
      render_exp in_code exp ^ code_of_iterexp iterexp |> adoc_as_code ctx
  | IterE (exp, iterexp) ->
      let sexp = render_exp in_code exp in
      if String.contains sexp ' ' then
        "( " ^ sexp ^ " )" ^ code_of_iterexp iterexp |> adoc_as_code ctx
      else sexp ^ code_of_iterexp iterexp |> adoc_as_code ctx

and render_exp_as_code ctx (exp : exp) =
  render_exp (code ctx) exp |> adoc_as_code ctx

and render_exps ctx ?sep:sep_opt exps =
  match (ctx.in_code, sep_opt) with
  | _, Some s -> String.concat s (List.map (render_exp ctx) exps)
  | true, None -> String.concat ", " (List.map (render_exp ctx) exps)
  | false, None -> render_list (List.map (render_exp ctx) exps)

and code_of_notexp ctx notexp =
  let mixop, exps = notexp in
  assert (List.length mixop - List.length exps = 1);
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> render_exp in_code)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> adoc_as_code ctx

(* Patterns *)

and code_of_pattern (pattern : pattern) =
  match pattern with
  | CaseP mixop -> code_of_mixop mixop
  | ListP `Cons -> "_ :: _"
  | ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | ListP `Nil -> "[]"
  | OptP `Some -> "(_)"
  | OptP `None -> "()"

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

(* Parameters *)

and render_param ctx param =
  match param.it with
  | ExpP (_typ, exp) -> render_exp ctx exp
  | DefP defid -> string_of_defid defid |> adoc_as_code ctx

and render_params ctx params =
  match params with
  | [] -> ""
  | params ->
      "(" ^ String.concat ", " (List.map (render_param ctx) params) ^ ")"

(* Arguments *)

and render_arg ctx arg =
  match arg.it with
  | ExpA exp -> render_exp ctx exp
  | DefA defid -> string_of_defid defid |> adoc_as_code ctx

and render_args ctx args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (render_arg ctx) args) ^ ")"

(* Type arguments *)

and string_of_targs = Sl.Print.string_of_targs

(* Instructions *)

let render_branch (branch : branch) : string =
  match branch with If -> "If " | ElseIf -> "Else if " | Else -> "Else "

let rec render_cond (ctx : context) (cond : cond) : string =
  match cond with
  | ExpCond exp -> render_exp ctx exp
  | RelCond rel_call -> render_rel_call ctx rel_call
  | ForAllCond (cond, vars) ->
      F.asprintf "%s, for all %s" (render_cond ctx cond)
        (render_in_itervars ctx vars)
  | ForAnyCond (cond, vars) ->
      F.asprintf "%s, for any %s" (render_cond ctx cond)
        (render_in_itervars ctx vars)

let rec render_instr ?(level = 0) ?(unordered = false) (instr : instr) : string
    =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  match instr.it with
  | ForEachI ([], instr, vars_in) ->
      F.asprintf "%s%s, for each %s" bullet
        (render_instr ~level instr)
        (render_in_itervars in_prose vars_in)
  | ForEachI (vars_out, instr, vars_in) ->
      F.asprintf "%sLet %s obtained by repeating:\n%s%s\n%sfor each %s" bullet
        (render_out_itervars in_prose vars_out)
        adoc_attach_block
        (render_instr ~level:(level + 1) ~unordered:true instr
        |> adoc_open_block)
        adoc_attach_block
        (render_in_itervars in_prose vars_in)
  | BranchI (Else, _, instrs) ->
      F.asprintf "%sElse:%s" bullet (render_instrs ~level:(level + 1) instrs)
  | BranchI
      ( branch,
        ExpCond { it = MatchE (exp, _); _ },
        { it = LetI (exp_l, exp_r); _ } :: instrs_rest )
    when Eq.eq_exp exp_r exp ->
      F.asprintf "%s%slet %s be %s:%s" bullet (render_branch branch)
        (render_exp in_code exp_l |> adoc_as_code in_prose)
        (render_exp in_prose exp_r)
        (render_instrs ~level:(level + 1) instrs_rest)
  | BranchI
      ( branch,
        ExpCond { it = SubE (exp, typ); _ },
        { it = LetI (exp_l, { it = DownCastE (typ_r, exp_r); _ }); _ }
        :: instrs_rest )
    when Eq.eq_exp exp_r exp && Eq.eq_typ typ_r typ ->
      F.asprintf "%s%slet %s be %s:%s" bullet (render_branch branch)
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
        (render_instrs ~level:(level + 1) instrs_rest)
  | BranchI (branch, cond, instrs) ->
      F.asprintf "%s%s%s:%s" bullet (render_branch branch)
        (render_cond in_prose cond)
        (render_instrs ~level:(level + 1) instrs)
  | OtherwiseI instrs ->
      F.asprintf "%sOtherwise:%s" bullet
        (render_instrs ~level:(level + 1) instrs)
  | CheckI cond ->
      F.asprintf "%sCheck that %s." bullet (render_cond in_prose cond)
  | LetI (exp_l, exp_r) ->
      F.asprintf "%sLet %s be %s." bullet
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
  | RuleI rel_call ->
      F.asprintf "%sLet %s." bullet (render_rel_call in_prose rel_call)
  | ReturnI exp -> F.asprintf "%sReturn %s." bullet (render_exp in_prose exp)
  | ResultI (ProseResult `Hold) -> bullet ^ "Then, the relation holds."
  | ResultI (ProseResult (`Yield (hint, exps))) ->
      F.asprintf "%sResult in %s." bullet
        (render_alter_hint in_prose hint (reindent_lines ~level:0) render_exp
           exps)
  | ResultI (MathResult []) -> bullet ^ "The relation holds."
  | ResultI (MathResult exps) ->
      F.asprintf "%sResult in %s." bullet (render_exps in_prose exps)
  | DestructI (binds, exp_r) ->
      let exps_l, fields = List.split binds in
      F.asprintf "%sLet %s be %s of %s." bullet
        (render_exps in_prose exps_l)
        (render_list (List.map (fun s -> "the " ^ s) fields))
        (render_exp in_prose exp_r)
  | CheckLetI (exp_l, exp_r) ->
      F.asprintf "%sLet!~type~ %s be %s." bullet
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
  | OptionGetI (exp_l, exp_r) ->
      F.asprintf "%sLet %s be %s %s." bullet
        (render_exp_as_code in_prose exp_l)
        (adoc_link ~link:"option_get" "*!*")
        (render_exp in_prose exp_r)

and render_instrs ?(level = 0) instrs =
  match instrs with
  | [ { it = ReturnI ({ it = BoolE _; _ } as exp); _ } ] ->
      F.asprintf " return %s." (render_exp_as_code in_prose exp)
  | instrs ->
      "\n" ^ (List.map (render_instr ~level) instrs |> String.concat "\n")

(* Definitions *)

let rec render_def (def : def) : string =
  match def.it with
  | ExternRelD externrel -> render_extern_rel_def externrel
  | RelD rel -> render_defined_rel_def rel
  | ExternDecD externfunc -> render_extern_func_def externfunc
  | BuiltinDecD builtinfunc -> render_builtin_func_def builtinfunc
  | TableDecD tablefunc -> render_table_func_def tablefunc
  | FuncDecD func -> render_defined_func_def func

and render_defs defs = defs |> List.map render_def |> String.concat "\n\n"

(* Relation definitions *)

and render_rel_title (rel_title : rel_title) : string =
  match rel_title with
  | ProseRelTitle (`Hold (id_rel, hint_hold, exps_input)) ->
      F.asprintf "%s:\n\n%s%s"
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_hold
           (reindent_lines ~level:0) render_exp exps_input)
  | ProseRelTitle
      (`Yield (id_rel, hint_input, exps_input, hint_output, exps_output)) ->
      F.asprintf "%s:\n\n%s%s:\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_input
           (reindent_lines ~level:1) render_exp exps_input)
        (adoc_unordered_bullet 0)
        ("Result in "
        ^ render_alter_hint ~caps:false in_prose hint_output
            (reindent_lines ~level:1) render_exp exps_output)
  | MathRelTitle (id_rel, mixop, exps) ->
      F.asprintf "%s: %s"
        (Sl.Print.string_of_relid id_rel)
        (code_of_notexp in_prose (mixop, exps))
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

(* Extern relation definitions *)

and render_extern_rel_def (externrel : externrel) : string =
  render_rel_title externrel

(* Defined relation definitions *)

and render_rulegroup_title (id_rel : id) (rulegroup_title : rulegroup_title) :
    string =
  match rulegroup_title with
  | ProseRuleTitle (`Hold (_id_rulegroup, hintexp, exps_input)) ->
      render_alter_hint ~caps:true in_link hintexp (reindent_lines ~level:0)
        render_exp exps_input
      ^ " when"
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
  | ProseRuleTitle (`Yield (_id_rulegroup, hintexp, exps_input)) ->
      render_alter_hint ~caps:true in_link hintexp (reindent_lines ~level:0)
        render_exp exps_input
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
  | MathRuleTitle (_id_rulegroup, mixop, exps) ->
      code_of_notexp in_prose (mixop, exps)
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

and render_rulegroup (id_rel : id) (rulegroup : rulegroup) : string =
  let rulegroup_title, instrs = rulegroup in
  render_rulegroup_title id_rel rulegroup_title ^ ":\n" ^ render_instrs instrs

and render_rulegroups (id_rel : id) (rulegroups : rulegroup list) : string =
  rulegroups |> List.map (render_rulegroup id_rel) |> String.concat "\n\n"

and render_defined_rel_def (rel : rel) : string =
  let rel_title, rulegroups = rel in
  let id_rel =
    match rel_title with
    | ProseRelTitle (`Hold (id_rel, _, _))
    | ProseRelTitle (`Yield (id_rel, _, _, _, _))
    | MathRelTitle (id_rel, _, _) ->
        id_rel
  in
  render_rel_title rel_title ^ "\n\n" ^ render_rulegroups id_rel rulegroups

(* Function definitions *)

and render_func_title (func_title : func_title) : string =
  match func_title with
  | ProseFuncTitle (`Check (id_func, hint_true, params)) ->
      F.asprintf "%s:\n\n%s%s"
        (string_of_defid id_func
        |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_true
           (reindent_lines ~level:0) render_param params)
  | ProseFuncTitle (`Yield (id_func, hint_input, params)) ->
      F.asprintf "%s:\n\n%s%s"
        (string_of_defid id_func
        |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose hint_input
           (reindent_lines ~level:0) render_param params)
  | MathFuncTitle (id_func, tparams, params) ->
      (string_of_defid id_func
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func))
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params

and render_func_header (func_title : func_title) : string =
  match func_title with
  | ProseFuncTitle (`Check (id_func, hint_true, params)) ->
      render_alter_hint ~caps:true in_prose hint_true (reindent_lines ~level:0)
        render_param params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)
  | ProseFuncTitle (`Yield (id_func, hint_input, params)) ->
      render_alter_hint ~caps:true in_prose hint_input (reindent_lines ~level:0)
        render_param params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)
  | MathFuncTitle (id_func, tparams, params) ->
      string_of_defid id_func
      ^ Sl.Print.string_of_tparams tparams
      ^ render_params (in_link |> code) params
      |> adoc_as_link in_prose ~link:(string_of_defid ~link:true id_func)

(* Extern function definitions *)

and render_extern_func_def (externfunc : externfunc) : string =
  render_func_header externfunc

(* Builtin function definitions *)

and render_builtin_func_def (builtinfunc : builtinfunc) : string =
  render_func_header builtinfunc

(* Table function definitions *)

and render_table_func_def (tablefunc : tablefunc) : string =
  let func_title, tablerows = tablefunc in
  let params =
    match func_title with
    | ProseFuncTitle (`Check (_, _, params))
    | ProseFuncTitle (`Yield (_, _, params))
    | MathFuncTitle (_, _, params) ->
        params
  in
  let table_meta =
    "[cols=\""
    ^ string_of_int (List.length params + 1)
    ^ "\", options=\"header\"]\n"
  in
  let table_header =
    "|===" ^ "\n" ^ "| " ^ render_params in_prose params ^ " | " ^ "Result \n\n"
  in
  let table_rows =
    tablerows
    |> List.map (fun tablerow ->
           let exps_sig, exp_res, _instrs = tablerow in
           let row_output = render_exp in_code exp_res in
           let row_input = render_exps in_code exps_sig in
           "| " ^ row_input ^ " | " ^ row_output)
    |> String.concat "\n"
  in
  let table_footer = "\n\n|===" in
  render_func_header func_title
  ^ ":\n" ^ table_meta ^ table_header ^ table_rows ^ table_footer

(* Defined function definitions *)

and render_defined_func_def (func : func) : string =
  let func_title, instrs = func in
  render_func_header func_title ^ "\n\n" ^ render_instrs instrs

(* Entrypoint for binary *)

let render_spec (spec : spec) = render_defs spec
