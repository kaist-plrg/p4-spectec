open Ast
open Util.Source
open Sl.Print
open Xl
module F = Format

(* Asciidoc rendering *)

type mode = Code | Prose

let render_mono ~mode s =
  match mode with Code -> s | Prose -> "`" ^ s ^ "`"

let render_subscript s = "~" ^ s ^ "~"
let render_superscript s = "^" ^ s ^ "^"
let render_bold s = "**" ^ s ^ "**"
let render_attach_block = "+\n"
let render_open_block s = "--\n" ^ s ^ "\n--"
let render_ordered_bullet level =
  Format.asprintf "%s%s "
    (String.make level ' ')
    (String.make (level + 1) '.')

let render_unordered_bullet level =
  Format.asprintf "%s%s "
    (String.make (level * 2) ' ')
    ("*")

let render_link ~(link : string) ~(text : string) : string
  = "<<" ^ link ^ ", " ^ text ^ ">>"


let reindent_lines ~level (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ (render_unordered_bullet (level + 1))) lines
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


let prose_of_branchtype branchtype =
  match branchtype with
  | If -> "If "
  | ElseIf -> "Else if "
  | Else -> "Else "

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
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> ("\"" ^ String.escaped text ^ "\"") |> render_mono ~mode
  | Il.Ast.VarE varid -> code_of_varid ~mode varid
  | Il.Ast.UnE (unop, _, exp) -> (
    match unop with
    (* | #Bool.unop ->  TODO *)
    | #Bool.unop
    | #Num.unop -> (string_of_unop unop ^ prose_of_exp ~mode:Code exp) |> render_mono ~mode
  )
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      (* always print as code *)
      prose_of_exp ~mode:Code exp_l
      ^ " " ^ string_of_binop binop ^ " "
      ^ prose_of_exp ~mode:Code exp_r
      |> render_mono ~mode
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      if mode = Prose then
        prose_of_exp ~mode exp_l ^ " " ^ prose_of_cmpop cmpop ^ " "
        ^ prose_of_exp ~mode exp_r
      else
        prose_of_exp ~mode:Code exp_l
        ^ " " ^ string_of_cmpop cmpop ^ " "
        ^ prose_of_exp ~mode:Code exp_r
        |> render_mono ~mode
  | Il.Ast.UpCastE (_typ, exp) | Il.Ast.DownCastE (_typ, exp) ->
      F.asprintf "%s" (code_of_exp ~mode exp)
  | Il.Ast.SubE (exp, typ) ->
    let verb = "has type" in
      F.asprintf "%s %s %s" (code_of_exp ~mode exp) verb (code_of_typ ~mode typ)
  | Il.Ast.MatchE (exp, pattern) ->
    let verb = "matches pattern" in
      F.asprintf "%s %s %s" (code_of_exp ~mode exp) verb
        (code_of_pattern pattern |> render_mono ~mode)
  | Il.Ast.TupleE es -> "(" ^ prose_of_exps ~mode ~sep:(Some ", ") es ^ ")"
  | Il.Ast.CaseE notexp -> code_of_notexp ~mode notexp
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> code_of_atom atom ^ " " ^ prose_of_exp ~mode exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?" ^ prose_of_exp ~mode exp ^ ""
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE [] -> "[ ]" |> render_mono ~mode
  | Il.Ast.ListE exps ->
      "[" ^ prose_of_exps ~mode:Code ~sep:(Some ", ") exps ^ "]" |> render_mono ~mode
  | Il.Ast.ConsE (exp_h, exp_t) ->
      prose_of_exp ~mode:Code exp_h
      ^ " :: "
      ^ prose_of_exp ~mode:Code exp_t
      |> render_mono ~mode
  | Il.Ast.CatE (exp_l, exp_r) ->
      prose_of_exp ~mode:Code exp_l
      ^ " ++ "
      ^ prose_of_exp ~mode:Code exp_r
      |> render_mono ~mode
  | Il.Ast.MemE (exp_e, exp_s) ->
      prose_of_exp ~mode exp_e ^ " is in " ^ prose_of_exp ~mode exp_s
  | Il.Ast.LenE exp -> "the length of " ^ prose_of_exp ~mode exp
  | Il.Ast.DotE (exp_b, atom) ->
      prose_of_exp ~mode:Code exp_b ^ "." ^ code_of_atom atom
      |> render_mono ~mode
  | Il.Ast.IdxE (exp_b, exp_i) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_exp ~mode exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_exp ~mode exp_l ^ " : "
      ^ prose_of_exp ~mode exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      prose_of_exp ~mode exp_b ^ "[" ^ prose_of_path ~mode path ^ " = "
      ^ prose_of_exp ~mode exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) ->
      (* TODO : re-add hints *)
      (* let hintexp_opt = *)
      (*   match exp.note with *)
      (*   | Il.Ast.BoolT when ctx.neg -> HEnv.get_func defid ctx.penv.prose_false *)
      (*   | Il.Ast.BoolT -> HEnv.get_func defid ctx.penv.prose_true *)
      (*   | _ -> HEnv.get_func defid ctx.penv.prose_in *)
      (* in *)
      (* match hintexp_opt with *)
      (* | Some hintexp -> *)
      (*     let exps = *)
      (*       args *)
      (*       |> List.filter_map (fun arg -> *)
      (*              match arg.it with *)
      (*              | Il.Ast.ExpA exp -> Some exp *)
      (*              | Il.Ast.DefA _ -> None) *)
      (*     in *)
      (*     F.asprintf "<<%s, %s>>" defid.it *)
      (*       (prose_of_hintexp ctx (exps |> List.map (fun a -> Some a)) hintexp) *)
      (* | None -> *)
      F.asprintf "%s%s%s" (string_of_defid defid) (string_of_targs targs)
        (prose_of_args ~mode:Code args)
      |> render_mono ~mode
  | Il.Ast.IterE (exp, iterexp) ->
    if snd(iterexp) = [] then prose_of_exp ~mode exp
    else
      (prose_of_exp ~mode:Code exp ^ code_of_iterexp iterexp) |> render_mono ~mode

(* if sep is None, use natural language list *)
and prose_of_exps ~mode ?(sep : string option = None) exps =
  match sep with
  | None -> prose_of_list (List.map (prose_of_exp ~mode) exps)
  | Some s -> String.concat s (List.map (prose_of_exp ~mode) exps)

and code_of_exp ~mode exp = prose_of_exp ~mode:Code exp |> render_mono ~mode

and code_of_exps ~mode ?(sep : string option = None) exps = 
  match sep with
  | None -> prose_of_list (List.map (code_of_exp ~mode) exps)
  | Some s -> String.concat s (List.map (code_of_exp ~mode) exps)

and code_of_notexp ~mode notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> code_of_atoms
      else idx / 2 |> List.nth exps |> prose_of_exp ~mode:Code)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " " |> render_mono ~mode

and prose_of_hintexp ~level (exps : exp list) (hintexp : El.Ast.exp) :
    string =
  let _, str = prose_of_hintexp' ~level exps hintexp 0 in
  str

and prose_of_hintexp' ~level (exps : exp list) (hintexp : El.Ast.exp)
    (cursor : int) : int * string =
  match hintexp.it with
  | El.Ast.TextE text -> (cursor, text |> reindent_lines ~level)
  | El.Ast.SeqE exps_hint ->
      let cursor, strs =
        List.fold_left
          (fun (cur, acc) exp ->
            let cur, str = prose_of_hintexp' ~level exps exp cur in
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
      let cursor_l, str_l = prose_of_hintexp' ~level exps exp_l cursor in
      let cursor_r, str_r = prose_of_hintexp' ~level exps exp_r cursor_l in
      (cursor_r, str_l ^ str_r)
  | _ -> failwith "unsupported prose hint"
(* Paths *)

and prose_of_path ~mode path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      prose_of_path ~mode path ^ "[" ^ prose_of_exp ~mode exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      prose_of_path ~mode path ^ "[" ^ prose_of_exp ~mode exp_l ^ " : "
      ^ prose_of_exp ~mode exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> code_of_atom atom
  | Il.Ast.DotP (path, atom) -> prose_of_path ~mode path ^ "." ^ code_of_atom atom

and prose_of_arg ~mode arg =
  match arg.it with
  | Il.Ast.ExpA exp -> prose_of_exp ~mode exp
  | Il.Ast.DefA defid -> string_of_defid defid

(* TODO: prose *)
and prose_of_args ~mode args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map (prose_of_arg ~mode) args) ^ ")"

let string_of_targs = Sl.Print.string_of_targs

let rec prose_of_cond ?(mode = Prose) (cond : cond) : string =
  match cond with
  | ExpCond exp -> prose_of_exp exp
  | RelCond (relcall, relid) -> "relcond!"
  | ForAllCond (cond, vars) -> "forall!"
  | ForAnyCond (cond, vars) -> "forany!"

let prose_of_relcall ~level (relcall : relcall) rid : string =
  match relcall with
  | Prose (hintexp, [], exps_in) ->
    render_link ~link:(string_of_relid rid)
      ~text:(prose_of_hintexp ~level exps_in hintexp)
  | Prose (hintexp, exps_out, exps_in) ->
    F.asprintf "%s be the result of %s"
      (code_of_exps ~mode:Prose exps_out)
      (render_link ~link:(string_of_relid rid)
      ~text:(prose_of_hintexp ~level exps_in hintexp))
  | Mixop (mixop, exps) ->
    render_link
      ~link:(string_of_relid rid)
      ~text:(code_of_notexp ~mode:Code (mixop, exps))

let rec prose_of_instr ?(level = 0) ?(mode = Prose) (instr : instr) : string =
  (* let bullet = render_ordered_bullet level in *)
  match instr.it with
  | Branch (branchtype, cond, instrs) ->
    F.asprintf "%s%s%s:\n%s"
      (render_ordered_bullet level) (prose_of_branchtype branchtype)
      (prose_of_cond ~mode cond)
      (prose_of_instrs ~level:(level + 1) instrs)
  | Bind (branchtype, exp_l, exp_r, instrs) ->
    F.asprintf "%s%slet %s be %s:\n%s"
      (render_ordered_bullet level) (prose_of_branchtype branchtype)
      (prose_of_exp ~mode:Code exp_l) (prose_of_exp ~mode:Code exp_r)
      (prose_of_instrs ~level:(level + 1) instrs)
  | Otherwise instr ->
    F.asprintf "%sOtherwise:\n%s" (render_ordered_bullet level)
      (prose_of_instr ~level:(level + 1) instr)
  | Check cond ->
    F.asprintf "%sCheck that %s." (render_ordered_bullet level)
      (prose_of_cond ~mode cond)
  | Let (exp_l, exp_r) ->
    F.asprintf "%sLet %s be %s."
      (render_ordered_bullet level)
      (code_of_exp ~mode:Prose exp_l)
      (prose_of_exp ~mode:Prose exp_r)
  | Rel (relcall, rid) ->
    F.asprintf "%sLet %s."
      (render_ordered_bullet level)
      (prose_of_relcall ~level relcall rid)
  | Return exp ->
    F.asprintf "%sReturn %s."
      (render_ordered_bullet level)
      (prose_of_exp ~mode:Prose exp)
  | Result (Some hintexp, exps) -> 
    F.asprintf "%sResult in %s."
      (render_ordered_bullet level)
      (prose_of_hintexp ~level:(level + 1) exps hintexp)
  | Result (None, exps) ->
    F.asprintf "%sResult in %s."
      (render_ordered_bullet level)
      (prose_of_exps ~mode:Prose exps)
  | Group (id, _, instrs) ->
    F.asprintf "%sGroup %s:\n%s"
      (render_ordered_bullet level) (string_of_relpathid id)
      (prose_of_instrs ~level:(level + 1) instrs)
  | ForEach _ -> F.asprintf "%sForEach." (render_ordered_bullet level)

and prose_of_instrs ?(level = 0) ?(mode = Prose) instrs =
  List.map (prose_of_instr ~level ~mode) instrs |> String.concat "\n"

let prose_of_def ?(mode = Prose) (def : def) : string =
  match def.it with
  | RelD (relid, exps_input, instrs) ->
      "\n\nrelation " ^ string_of_relid relid ^ ": "
      ^ prose_of_exps ~mode exps_input ^ "\n\n"
      ^ prose_of_instrs ~mode instrs
  | DecD _ -> ""

let prose_of_defs ?(mode = Prose) defs =
  List.map (prose_of_def ~mode) defs |> String.concat "\n"

let prose_of_spec (spec : spec) = prose_of_defs spec

(* let code_of_relinput mixop inputs exps_input = *)
(*   let exps_input = List.combine inputs exps_input in *)
(*   let exps = *)
(*     List.init *)
(*       (List.length mixop - 1) *)
(*       (fun idx -> *)
(*         match List.assoc_opt idx exps_input with *)
(*         | Some exp_input -> exp_input *)
(*         | None -> Il.Ast.VarE ("%" $ no_region) $$ (no_region, Il.Ast.TextT)) *)
(*   in *)
(*   let notexp = (mixop, exps) in *)
(*   code_of_notexp ~mode:Code notexp |> render_mono ~mode:Code *)
(**)
(* let prose_of_relinput id_rel mixop inputs exps_input = *)
(*   let prose_hint_opt = Hintenv.get_rel id_rel ctx.penv.prose_in in *)
(*   match prose_hint_opt with *)
(*   | Some prose_hint -> *)
(*       let exps_opt = List.map Option.some exps_input in *)
(*       F.asprintf "%s:" *)
(*         (prose_of_hintexp (ctx |> increment_level) exps_opt prose_hint) *)
(*       |> String.capitalize_ascii *)
(*   | None -> code_of_relinput ctx mixop inputs exps_input *)

(* (* entrypoint for splicer *) *)
(**)
(* let prose_of_rulegroup (id_rel : rid) (mixop : mixop) (inputs : int list) (exps_input : exp list) *)
(*   (instrs : instr list) : string = *)
(*   F.asprintf "%s\n\n%s" *)
(*     (prose_of_rel_signature hintexp  *)
(*     (prose_of_instrs instrs) *)
(**)
(* let prose_of_func (id_def : fid) (tparams : tparam list) *)
(*     (args_input : arg list) (instrs : instr list) : string = *)
(*   let prose_of_funcinput = *)
(*     F.asprintf "%s%s%s" (string_of_defid id_def) *)
(*       (string_of_tparams tparams) *)
(*       (prose_of_args ~mode:Code args_input) *)
(*     |> render_mono ~mode:Code *)
(*   in *)
(*   F.asprintf "%s\n\n%s" prose_of_funcinput (prose_of_instrs instrs) *)
