open Ast
open Print
open Util.Source

(* Numbers *)

let render_num num = Il.Print.string_of_num num

(* Texts *)

let render_text text = Il.Print.string_of_text text

(* Identifiers *)

let render_varid varid =
  if String.starts_with ~prefix:"_" varid.it then "_"
  else Il.Print.string_of_varid varid

let render_typid typid = Il.Print.string_of_typid typid
let render_relid relid = Il.Print.string_of_relid relid
let render_relpathid relpathid = Il.Print.string_of_rulegroupid relpathid
let render_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let render_atom atom = Xl.Atom.render_atom atom.it
let render_atoms atoms = String.concat "" (List.map render_atom atoms)

(* Mixfix operators *)

let render_mixop mixop = Xl.Mixop.render_mixop mixop

(* Iterators *)

let render_iter iter = Il.Print.string_of_iter iter

(* Variables *)

let render_var (id, _, iters) =
  render_varid id ^ String.concat "" (List.map render_iter iters)

(* Types *)

let render_typ typ = Il.Print.string_of_typ typ
let render_typs sep typs = Il.Print.string_of_typs sep typs
let render_nottyp nottyp = Il.Print.string_of_nottyp nottyp
let render_deftyp deftyp = Il.Print.string_of_deftyp deftyp
let render_typfield typfield = Il.Print.string_of_typfield typfield
let render_typfields sep typfields = Il.Print.string_of_typfields sep typfields
let render_typcase typcase = Il.Print.string_of_typcase typcase
let render_typcases sep typcases = Il.Print.string_of_typcases sep typcases

(* Operators *)

let render_unop unop = Il.Print.string_of_unop unop
let render_binop binop = Il.Print.string_of_binop binop
let render_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let rec render_exp exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> render_varid varid
  | Il.Ast.UnE (unop, _, exp) -> render_unop unop ^ render_exp exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      "(" ^ render_exp exp_l ^ " " ^ render_binop binop ^ " " ^ render_exp exp_r
      ^ ")"
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ render_exp exp_l ^ " " ^ render_cmpop cmpop ^ " " ^ render_exp exp_r
      ^ ")"
  | Il.Ast.UpCastE (_, exp) | Il.Ast.DownCastE (_, exp) -> render_exp exp
  | Il.Ast.SubE (exp, typ) ->
      "(" ^ render_exp exp ^ " has type " ^ render_typ typ ^ ")"
  | Il.Ast.MatchE (exp, pattern) ->
      "(" ^ render_exp exp ^ " matches pattern " ^ render_pattern pattern ^ ")"
  | Il.Ast.TupleE es -> "(" ^ render_exps ", " es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ render_notexp notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> render_atom atom ^ " " ^ render_exp exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> render_exp exp
  | Il.Ast.OptE None -> "none"
  | Il.Ast.ListE exps -> "[" ^ render_exps ", " exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) -> render_exp exp_h ^ " :: " ^ render_exp exp_t
  | Il.Ast.CatE (exp_l, exp_r) -> render_exp exp_l ^ " ++ " ^ render_exp exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      render_exp exp_e ^ " is in " ^ render_exp exp_s
  | Il.Ast.LenE exp -> "|" ^ render_exp exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) -> render_exp exp_b ^ "." ^ render_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      render_exp exp_b ^ "[" ^ render_exp exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      render_exp exp_b ^ "[" ^ render_exp exp_l ^ " : " ^ render_exp exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      render_exp exp_b ^ "[" ^ render_path path ^ " = " ^ render_exp exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) ->
      render_defid defid ^ render_targs targs ^ render_args args
  | Il.Ast.IterE (exp, iterexp) -> render_exp exp ^ render_iterexp iterexp

and render_exps sep exps = String.concat sep (List.map render_exp exps)

and render_notexp notexp =
  let mixop, exps = notexp in
  let len = List.length mixop + List.length exps in
  List.init len (fun idx ->
      if idx mod 2 = 0 then idx / 2 |> List.nth mixop |> render_atoms
      else idx / 2 |> List.nth exps |> render_exp)
  |> List.filter_map (fun str -> if str = "" then None else Some str)
  |> String.concat " "

and render_iterexp (iter, _) = render_iter iter

and render_iterexps iterexps =
  String.concat "" (List.map render_iterexp iterexps)

(* Patterns *)

and render_pattern pattern =
  match pattern with
  | Il.Ast.CaseP mixop -> render_mixop mixop
  | Il.Ast.ListP `Cons -> "_ :: _"
  | Il.Ast.ListP (`Fixed i) -> Format.asprintf "[ _/%d ]" i
  | Il.Ast.ListP `Nil -> "[]"
  | Il.Ast.OptP `Some -> "some _"
  | Il.Ast.OptP `None -> "none"

(* Paths *)

and render_path path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) -> render_path path ^ "[" ^ render_exp exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      render_path path ^ "[" ^ render_exp exp_l ^ " : " ^ render_exp exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> render_atom atom
  | Il.Ast.DotP (path, atom) -> render_path path ^ "." ^ render_atom atom

(* Parameters *)

and render_param param =
  match param.it with
  | Il.Ast.ExpP typ -> render_typ typ
  | Il.Ast.DefP (id, tparams, params, typ) ->
      render_defid id ^ render_tparams tparams ^ render_params params ^ " : "
      ^ render_typ typ

and render_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map render_param params) ^ ")"

(* Type parameters *)

and render_tparam tparam = Il.Print.string_of_tparam tparam
and render_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and render_arg arg =
  match arg.it with
  | Il.Ast.ExpA exp -> render_exp exp
  | Il.Ast.DefA defid -> render_defid defid

and render_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map render_arg args) ^ ")"

(* Type arguments *)

and render_targ targ = render_typ targ

and render_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map render_targ targs) ^ ">"

(* Case analysis *)

let rec render_case ?(level = 0) case =
  let order = String.make (level + 1) '.' ^ " " in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (render_guard guard)
    (render_instrs ~level:(level + 1) instrs)

and render_cases ?(level = 0) cases =
  cases |> List.map (render_case ~level) |> String.concat "\n\n"

and render_guard guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ render_cmpop cmpop ^ " " ^ render_exp exp ^ ")"
  | SubG typ -> "(% has type " ^ render_typ typ ^ ")"
  | MatchG patten -> "(% matches pattern " ^ render_pattern patten ^ ")"
  | MemG exp -> "(% is in " ^ render_exp exp ^ ")"

(* Instruction *)

and render_instr ?(level = 0) instr =
  let order = String.make (level + 1) '.' ^ " " in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, _) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (render_exp exp_cond)
        (render_iterexps iterexps)
        (render_instrs ~level:(level + 1) instrs_then)
  | HoldI (id, notexp, iterexps, holdcase) -> (
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s\n\n%sElse,\n\n%s"
            order (render_relid id) (render_notexp notexp)
            (render_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_hold)
            order
            (render_instrs ~level:(level + 1) instrs_nothold)
      | HoldH (instrs_hold, _) ->
          Format.asprintf "%sIf (%s: %s)%s holds, then\n\n%s" order
            (render_relid id) (render_notexp notexp) (render_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_hold)
      | NotHoldH (instrs_nothold, _) ->
          Format.asprintf "%sIf (%s: %s)%s does not hold, then\n\n%s" order
            (render_relid id) (render_notexp notexp) (render_iterexps iterexps)
            (render_instrs ~level:(level + 1) instrs_nothold))
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (render_exp exp)
        (render_cases ~level:(level + 1) cases)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (render_instr ~level:(level + 1) instr)
  | GroupI _ -> assert false
  | LetI (exp_l, exp_r, iterexps) ->
      Format.asprintf "%s(Let %s be %s)%s" order (render_exp exp_l)
        (render_exp exp_r) (render_iterexps iterexps)
  | RuleI (id_rel, notexp, iterexps) ->
      Format.asprintf "%s(%s: %s)%s" order (render_relid id_rel)
        (render_notexp notexp) (render_iterexps iterexps)
  | ResultI [] -> Format.asprintf "%sThe relation holds" order
  | ResultI exps ->
      Format.asprintf "%sResult in %s" order (render_exps ", " exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" order (render_exp exp)
  | DebugI exp -> Format.asprintf "%sDebug: %s" order (render_exp exp)

and render_instrs ?(level = 0) instrs =
  instrs |> List.map (render_instr ~level) |> String.concat "\n\n"

(* Relations *)

and render_relinput mixop inputs exps_input =
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
  render_notexp notexp

(* Rule prose *)

let render_ruleprose (mixop : mixop) (inputs : int list) (exps_input : exp list)
    (instrs : instr list) : string =
  "`" ^ render_relinput mixop inputs exps_input ^ "`\n\n" ^ render_instrs instrs
