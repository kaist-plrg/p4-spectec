open Domain
open Lib
open Lang
open El
open Runtime.Type
open Util.Source
open Attempt
open Error
open Util.Checks
module Mixfix = Domain.Mixfix
module F = Format

(* Checks *)

(* Identifiers *)

let valid_tid (id : id) = id.it = (Xl.Var.strip_var_suffix id).it

(* Iteration elaboration *)

let elab_iter (iter : iter) : Il.iter =
  match iter with Opt -> Il.Opt | List -> Il.List

(* Types *)

(* Type destructuring *)

let as_text_typ (ctx : Ctx.t) (typ_il : Il.typ) : unit attempt =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | TextT -> Ok ()
  | _ -> fail typ_il.at "cannot destruct type as text"

let as_iter_typ (ctx : Ctx.t) (typ_il : Il.typ) : (Il.typ * Il.iter) attempt =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | IterT (typ_il, iter) -> Ok (typ_il, iter)
  | _ -> fail typ_il.at "cannot destruct type as an iteration"

let as_tuple_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typ list attempt =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | TupleT typs_il -> Ok typs_il
  | _ -> fail typ_il.at "cannot destruct type as a tuple"

let as_list_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typ attempt =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | IterT (typ_il, List) -> Ok typ_il
  | _ -> fail typ_il.at "cannot destruct type as a list"

let as_struct_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typfield list attempt =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, deftyp)) -> (
          match deftyp.it with
          | StructT typfields_il -> Ok typfields_il
          | _ -> fail typ_il.at "cannot destruct type as a struct")
      | _ -> fail typ_il.at "cannot destruct type as a struct")
  | _ -> fail typ_il.at "cannot destruct type as a struct"

(* Elaboration of plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.typ' =
  match plaintyp with
  | BoolT -> Il.BoolT
  | NumT numtyp -> Il.NumT numtyp
  | TextT -> Il.TextT
  | VarT (tid, targs) ->
      let td = Ctx.find_typdef ctx tid in
      let tparams = Typdef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      let targs_il = List.map (elab_plaintyp ctx) targs in
      Il.VarT (tid, targs_il)
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupleT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.TupleT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.IterT (typ_il, iter_il)

(* Elaboration of notation types *)

let rec elab_nottyp (ctx : Ctx.t) (typ : typ) : Il.nottyp =
  match typ with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Mixfix.Arg typ_il $ plaintyp.at
  | NotationT nottyp -> (
      match nottyp.it with
      | AtomT atom -> Mixfix.Atom atom $ nottyp.at
      | SeqT [] -> Mixfix.Seq [] $ nottyp.at
      | SeqT (typ_h :: typs_t) ->
          let mixfix_h = elab_nottyp ctx typ_h |> it in
          let mixfix_t =
            elab_nottyp ctx (NotationT (SeqT typs_t $ nottyp.at)) |> it
          in
          let mixfix =
            match mixfix_t with
            | Mixfix.Seq mixfixes_t -> Mixfix.Seq (mixfix_h :: mixfixes_t)
            | _ -> assert false
          in
          mixfix $ nottyp.at
      | InfixT (typ_l, atom, typ_r) ->
          let mixfix_l = elab_nottyp ctx typ_l |> it in
          let mixfix_r = elab_nottyp ctx typ_r |> it in
          Mixfix.Infix (mixfix_l, atom, mixfix_r) $ nottyp.at
      | BrackT (atom_l, typ, atom_r) ->
          let mixfix = elab_nottyp ctx typ |> it in
          Mixfix.Brack (atom_l, mixfix, atom_r) $ nottyp.at)

(* Elaboration of definition types *)

and elab_deftyp (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Typdef.t * Il.deftyp =
  match deftyp.it with
  | PlainTD plaintyp -> elab_deftyp_plain ctx tparams plaintyp
  | StructTD typfields -> elab_deftyp_struct ctx deftyp.at tparams typfields
  | VariantTD typcases -> elab_deftyp_variant ctx deftyp.at id tparams typcases

(* Elaboration of plain type definitions *)

and elab_deftyp_plain (ctx : Ctx.t) (tparams : tparam list)
    (plaintyp : plaintyp) : Typdef.t * Il.deftyp =
  let typ_il = elab_plaintyp ctx plaintyp in
  let deftyp_il = Il.PlainT typ_il $ plaintyp.at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Elaboration of struct type definitions *)

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.typfield =
  let atom, plaintyp, _hints = typfield in
  let typ_il = elab_plaintyp ctx plaintyp in
  (atom, typ_il)

and elab_deftyp_struct (ctx : Ctx.t) (at : region) (tparams : tparam list)
    (typfields : typfield list) : Typdef.t * Il.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.StructT typfields_il $ at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Elaboration of variant type definitions *)

and elab_typcase_plain (ctx : Ctx.t) (typ_il : Il.typ) : Il.typcase list =
  let typ_il = Expand.expand_typ (Ctx.find_typdef_opt ctx) typ_il in
  match typ_il.it with
  | VarT (tid, targs_il) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defining _ -> error typ_il.at "cannot extend an incomplete type"
      | Defined (tparams, deftyp) -> (
          match deftyp.it with
          | VariantT typcases_il ->
              let theta = TIdMap.of_lists tparams targs_il in
              List.map (Subst.subst_typcase theta) typcases_il
          | _ -> error typ_il.at "cannot extend a non-variant type")
      | _ -> error typ_il.at "cannot extend a non-variant type")
  | _ -> error typ_il.at "cannot extend a non-variant type"

and elab_typcase (ctx : Ctx.t) (typorigin_il : Il.typorigin) (typcase : typcase)
    : Il.typcase list =
  let typ, hints = typcase in
  match typ with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      elab_typcase_plain ctx typ_il
  | NotationT _ ->
      let nottyp_il = elab_nottyp ctx typ in
      let typcase_il = (nottyp_il, typorigin_il, hints) in
      [ typcase_il ]

and elab_deftyp_variant (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (typcases : typcase list) : Typdef.t * Il.deftyp =
  let typorigin_il =
    let targs_il =
      List.map (fun tparam -> Il.VarT (tparam, []) $ tparam.at) tparams
    in
    (id, targs_il) $ id.at
  in
  let typcases_il = List.concat_map (elab_typcase ctx typorigin_il) typcases in
  let mixops =
    typcases_il
    |> List.map (fun (nottyp_il, _, _) -> Mixfix.to_mixop nottyp_il.it)
  in
  let mixop_groups = groupby Mixop.eq mixops in
  let mixop_duplicates =
    List.filter (fun mixop_group -> List.length mixop_group > 1) mixop_groups
  in
  check
    (List.length mixop_duplicates = 0)
    at
    ("variant cases are ambiguous: "
    ^ String.concat ", "
        (List.map
           (fun mixop_group -> Mixop.string_of_mixop (List.hd mixop_group))
           mixop_duplicates));
  let deftyp_il = Il.VariantT typcases_il $ at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Expressions *)

(* Inference of expression type *)

let fail_infer (at : region) (construct : string) =
  fail at ("cannot infer type of " ^ construct)

let rec infer_exp (ctx : Ctx.t) (exp : exp) : (Ctx.t * Il.exp * Il.typ) attempt
    =
  let* ctx, exp_il, typ_il = infer_exp' ctx exp.at exp.it in
  let exp_il = exp_il $$ (exp.at, typ_il) in
  let typ_il = typ_il $ exp.at in
  Ok (ctx, exp_il, typ_il)

and infer_exp' (ctx : Ctx.t) (at : region) (exp : exp') :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  match exp with
  | BoolE b -> infer_bool_exp ctx b
  | NumE (_, num) -> infer_num_exp ctx num
  | TextE text -> infer_text_exp ctx text
  | VarE id -> infer_var_exp ctx id
  | UnE (unop, exp) -> infer_unop_exp ctx at unop exp
  | BinE (exp_l, binop, exp_r) -> infer_binop_exp ctx at binop exp_l exp_r
  | CmpE (exp_l, cmpop, exp_r) -> infer_cmpop_exp ctx at cmpop exp_l exp_r
  | ArithE exp -> infer_arith_exp ctx exp
  | EpsE -> fail_infer at "empty sequence"
  | ListE exps -> infer_list_exp ctx at exps
  | ConsE (exp_h, exp_t) -> infer_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> infer_cat_exp ctx exp_l exp_r
  | IdxE (exp_b, exp_i) -> infer_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_i, exp_n) -> infer_slice_exp ctx exp_b exp_i exp_n
  | LenE exp -> infer_len_exp ctx exp
  | MemE (exp_e, exp_s) -> infer_mem_exp ctx exp_e exp_s
  | StrE _ -> fail_infer at "struct expression"
  | DotE (exp, atom) -> infer_dot_exp ctx exp atom
  | UpdE (exp_b, path, exp_f) -> infer_upd_exp ctx exp_b path exp_f
  | ParenE exp -> infer_paren_exp ctx exp
  | TupleE exps -> infer_tuple_exp ctx exps
  | CallE (id, targs, args) -> infer_call_exp ctx at id targs args
  | IterE (exp, iter) -> infer_iter_exp ctx exp iter
  | SubE (exp, plaintyp) -> infer_sub_exp ctx exp plaintyp
  | AtomE _ -> fail_infer at "atom"
  | SeqE _ -> fail_infer at "sequence expression"
  | InfixE _ -> fail_infer at "infix expression"
  | BrackE _ -> fail_infer at "bracket expression"
  | HoleE _ -> error at "misplaced hole"
  | FuseE _ -> error at "misplaced token concatenation"
  | UnparenE _ -> error at "misplaced unparenthesize"
  | LatexE _ -> error at "misplaced LaTeX literal"

and infer_exps (ctx : Ctx.t) (exps : exp list) :
    (Ctx.t * Il.exp list * Il.typ list) attempt =
  match exps with
  | [] -> Ok (ctx, [], [])
  | exp :: exps ->
      let* ctx, exp_il, typ_il = infer_exp ctx exp in
      let* ctx, exps_il, typs_il = infer_exps ctx exps in
      Ok (ctx, exp_il :: exps_il, typ_il :: typs_il)

(* Inference of boolean expressions *)

and infer_bool_exp (ctx : Ctx.t) (b : bool) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.BoolE b in
  let typ_il = Il.BoolT in
  Ok (ctx, exp_il, typ_il)

(* Inference of number expressions *)

and infer_num_exp (ctx : Ctx.t) (num : Xl.Num.t) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.NumE num in
  let typ_il = Il.NumT (Xl.Num.to_typ num) in
  Ok (ctx, exp_il, typ_il)

(* Inference of text expressions *)

and infer_text_exp (ctx : Ctx.t) (text : string) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.TextE text in
  let typ_il = Il.TextT in
  Ok (ctx, exp_il, typ_il)

(* Inference of variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) : (Ctx.t * Il.exp' * Il.typ') attempt
    =
  let tid = Xl.Var.strip_var_suffix id in
  let meta_opt = Ctx.find_metavar_opt ctx tid in
  match meta_opt with
  | Some typ_il ->
      let exp_il = Il.VarE id in
      Ok (ctx, exp_il, typ_il.it)
  | None -> fail_infer id.at "variable"

(* Inference of unary expressions *)

and infer_unop (ctx : Ctx.t) (at : region) (unop : unop) (typ_il : Il.typ)
    (exp_il : Il.exp) : (Il.optyp * Il.exp * Il.typ') attempt =
  let unop_candidates =
    match unop with
    | #Xl.Bool.unop -> [ (`BoolT, Il.BoolT, Il.BoolT) ]
    | #Xl.Num.unop ->
        [
          (`NatT, Il.NumT `NatT, Il.NumT `NatT);
          (`IntT, Il.NumT `IntT, Il.NumT `IntT);
        ]
  in
  let fail =
    fail at
      (F.asprintf "unary operator `%s` is not defined for operand type %s"
         (Il.Print.string_of_unop unop)
         (Il.Print.string_of_typ typ_il))
  in
  List.fold_left
    (fun unop_infer (optyp_il, typ_il_expect, typ_il_res_expect) ->
      match unop_infer with
      | Ok _ -> unop_infer
      | _ -> (
          let exp_il_attempt =
            cast_exp ctx (typ_il_expect $ typ_il.at) typ_il exp_il
          in
          match exp_il_attempt with
          | Ok exp_il -> Ok (optyp_il, exp_il, typ_il_res_expect)
          | _ -> fail))
    fail unop_candidates

and infer_unop_exp (ctx : Ctx.t) (at : region) (unop : unop) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let* optyp_il, exp_il, typ_il_expect = infer_unop ctx at unop typ_il exp_il in
  let exp_il = Il.UnE (unop, optyp_il, exp_il) in
  Ok (ctx, exp_il, typ_il_expect)

(* Inference of binary expressions *)

and infer_binop (ctx : Ctx.t) (at : region) (binop : binop) (typ_il_l : Il.typ)
    (exp_il_l : Il.exp) (typ_il_r : Il.typ) (exp_il_r : Il.exp) :
    (Il.optyp * Il.exp * Il.exp * Il.typ') attempt =
  let binop_candidates =
    match binop with
    | #Xl.Bool.binop -> [ (`BoolT, Il.BoolT, Il.BoolT, Il.BoolT) ]
    | `SubOp ->
        [
          (`IntT, Il.NumT `NatT, Il.NumT `NatT, Il.NumT `IntT);
          (`IntT, Il.NumT `IntT, Il.NumT `IntT, Il.NumT `IntT);
        ]
    | #Xl.Num.binop ->
        [
          (`NatT, Il.NumT `NatT, Il.NumT `NatT, Il.NumT `NatT);
          (`IntT, Il.NumT `IntT, Il.NumT `IntT, Il.NumT `IntT);
        ]
  in
  let fail =
    fail at
      (F.asprintf
         "binary operator `%s` is not defined for operand types %s and %s"
         (Il.Print.string_of_binop binop)
         (Il.Print.string_of_typ typ_il_l)
         (Il.Print.string_of_typ typ_il_r))
  in
  List.fold_left
    (fun binop_infer
         (optyp_il, typ_il_l_expect, typ_il_r_expect, typ_il_res_expect) ->
      match binop_infer with
      | Ok _ -> binop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (typ_il_l_expect $ typ_il_l.at) typ_il_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (typ_il_r_expect $ typ_il_r.at) typ_il_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r ->
              Ok (optyp_il, exp_il_l, exp_il_r, typ_il_res_expect)
          | _ -> fail))
    fail binop_candidates

and infer_binop_exp (ctx : Ctx.t) (at : region) (binop : binop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_l, typ_il_l_infer = infer_exp ctx exp_l in
  let* ctx, exp_il_r, typ_il_r_infer = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r, typ_il_expect =
    infer_binop ctx at binop typ_il_l_infer exp_il_l typ_il_r_infer exp_il_r
  in
  let exp_il = Il.BinE (binop, optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, typ_il_expect)

(* Inference of comparison expressions *)

and infer_cmpop_exp_bool (ctx : Ctx.t) (cmpop : Xl.Bool.cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_r, typ_il_r = infer_exp ctx exp_r in
        let* ctx, exp_il_l = elab_exp ctx typ_il_r exp_l in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, Il.BoolT));
      (fun () ->
        let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
        let* ctx, exp_il_r = elab_exp ctx typ_il_l exp_r in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, Il.BoolT));
    ]

and infer_cmpop_num (ctx : Ctx.t) (at : region) (cmpop : Xl.Num.cmpop)
    (typ_il_l : Il.typ) (exp_il_l : Il.exp) (typ_il_r : Il.typ)
    (exp_il_r : Il.exp) : (Il.optyp * Il.exp * Il.exp) attempt =
  let cmpop_candidates =
    [
      (`NatT, Il.NumT `NatT, Il.NumT `NatT);
      (`IntT, Il.NumT `IntT, Il.NumT `IntT);
    ]
  in
  let fail =
    fail at
      (F.asprintf
         "comparison operator `%s` is not defined for operand types %s and %s"
         (Il.Print.string_of_cmpop (cmpop :> Il.cmpop))
         (Il.Print.string_of_typ typ_il_l)
         (Il.Print.string_of_typ typ_il_r))
  in
  List.fold_left
    (fun cmpop_infer (optyp_il, typ_il_l_expect, typ_il_r_expect) ->
      match cmpop_infer with
      | Ok _ -> cmpop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (typ_il_l_expect $ typ_il_l.at) typ_il_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (typ_il_r_expect $ typ_il_r.at) typ_il_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r -> Ok (optyp_il, exp_il_l, exp_il_r)
          | _ -> fail))
    fail cmpop_candidates

and infer_cmpop_exp_num (ctx : Ctx.t) (at : region) (cmpop : Xl.Num.cmpop)
    (exp_l : exp) (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_l, typ_il_l_infer = infer_exp ctx exp_l in
  let* ctx, exp_il_r, typ_il_r_infer = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r =
    infer_cmpop_num ctx at cmpop typ_il_l_infer exp_il_l typ_il_r_infer exp_il_r
  in
  let exp_il = Il.CmpE ((cmpop :> Il.cmpop), optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, Il.BoolT)

and infer_cmpop_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  match cmpop with
  | #Xl.Bool.cmpop as cmpop -> infer_cmpop_exp_bool ctx cmpop exp_l exp_r
  | #Xl.Num.cmpop as cmpop -> infer_cmpop_exp_num ctx at cmpop exp_l exp_r

(* Inference of arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of list expressions *)

and infer_list_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  match exps with
  | [] -> fail_infer at "empty list"
  | exp :: exps ->
      let* ctx, exp_il, typ_il = infer_exp ctx exp in
      let* ctx, exps_il, typs_il = infer_exps ctx exps in
      if List.for_all (Equiv.equiv_typ (Ctx.find_typdef_opt ctx) typ_il) typs_il
      then
        let exp_il = Il.ListE (exp_il :: exps_il) in
        let typ_il = Il.IterT (typ_il, List) in
        Ok (ctx, exp_il, typ_il)
      else fail_infer at "list with heterogeneous elements"

(* Inference of cons expressions *)

and infer_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_h, typ_il_h = infer_exp ctx exp_h in
  let typ_il = Il.IterT (typ_il_h, List) in
  let* ctx, exp_il_t = elab_exp ctx (typ_il $ typ_il_h.at) exp_t in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il, typ_il)

(* Inference of concatenation expressions *)

and infer_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
        let* typ_il = as_list_typ ctx typ_il_l in
        let typ_il = Il.IterT (typ_il, List) $ typ_il.at in
        let* ctx, exp_il_r = elab_exp ctx typ_il exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, typ_il.it));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (Il.TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (Il.TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
        let* typ_il = as_list_typ ctx typ_il_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, typ_il.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (Il.TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
        let* _ = as_list_typ ctx typ_il_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let* ctx, exp_il_n = elab_exp ctx (Il.NumT `NatT $ exp_n.at) exp_n in
        let exp_il = Il.SliceE (exp_il_b, exp_il_i, exp_il_n) in
        Ok (ctx, exp_il, typ_il_b.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (Il.TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let* ctx, exp_il_n = elab_exp ctx (Il.NumT `NatT $ exp_n.at) exp_n in
        let exp_il = Il.SliceE (exp_il_b, exp_il_i, exp_il_n) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of member expressions *)

and infer_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_e, typ_il_e = infer_exp ctx exp_e in
        let* ctx, exp_il_s =
          elab_exp ctx (Il.IterT (typ_il_e, List) $ typ_il_e.at) exp_s
        in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        let typ_il = Il.BoolT in
        Ok (ctx, exp_il, typ_il));
      (fun () ->
        let* ctx, exp_il_s, typ_il_s = infer_exp ctx exp_s in
        let* typ_il_s = as_list_typ ctx typ_il_s in
        let* ctx, exp_il_e = elab_exp ctx typ_il_s exp_e in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        let typ_il = Il.BoolT in
        Ok (ctx, exp_il, typ_il));
    ]

(* Inference of dot expressions *)

and infer_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let* typfields_il = as_struct_typ ctx typ_il in
  let* typ_il =
    List.find_opt (fun (atom_t, _) -> atom.it = atom_t.it) typfields_il
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, typ_il) -> Ok typ_il
    | None -> fail atom.at "cannot infer type of field"
  in
  let exp_il = Il.DotE (exp_il, atom) in
  Ok (ctx, exp_il, typ_il.it)

(* Inference of update expressions *)

and infer_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
  let* ctx, path_il, typ_il_f = elab_path ctx typ_il_b path in
  let* ctx, exp_il_f = elab_exp ctx typ_il_f exp_f in
  let exp_il = Il.UpdE (exp_il_b, path_il, exp_il_f) in
  Ok (ctx, exp_il, typ_il_b.it)

(* Inference of length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il, typ_il = infer_exp ctx exp in
        let* _ = as_list_typ ctx typ_il in
        let exp_il = Il.LenE exp_il in
        let typ_il = Il.NumT `NatT in
        Ok (ctx, exp_il, typ_il));
      (fun () ->
        let* ctx, exp_il = elab_exp ctx (Il.TextT $ exp.at) exp in
        let exp_il = Il.LenE exp_il in
        let typ_il = Il.NumT `NatT in
        Ok (ctx, exp_il, typ_il));
    ]

(* Inference of parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exps_il, typs_il = infer_exps ctx exps in
  let exp_il = Il.TupleE exps_il in
  let typ_il = Il.TupleT typs_il in
  Ok (ctx, exp_il, typ_il)

(* Inference of call expressions *)

and infer_call_exp (ctx : Ctx.t) (at : region) (id : id) (targs : targ list)
    (args : arg list) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let tparams_il, params_il, typ_il = Ctx.find_func_signature ctx id in
  check
    (List.length targs = List.length tparams_il)
    id.at "type arguments do not match";
  let targs_il = List.map (elab_plaintyp ctx) targs in
  let theta = TIdMap.of_lists tparams_il targs_il in
  let params_il = Subst.subst_params theta params_il in
  let typ_il = Subst.subst_typ theta typ_il in
  let ctx, args_il = elab_args at ctx params_il args in
  let exp_il = Il.CallE (id, targs_il, args_il) in
  Ok (ctx, exp_il, typ_il.it)

(* Inference of iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.IterE (exp_il, (iter_il, [])) in
  let typ_il = Il.IterT (typ_il, iter_il) in
  Ok (ctx, exp_il, typ_il)

(* Inference of subtype expressions *)

and infer_sub_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il_exp = infer_exp ctx exp in
  let typ_il = elab_plaintyp ctx plaintyp in
  if
    Sub.sub_typ (Ctx.find_typdef_opt ctx) typ_il_exp typ_il
    || Sub.sub_typ (Ctx.find_typdef_opt ctx) typ_il typ_il_exp
  then
    let subcheck_il =
      Sub.optimize (Ctx.find_typdef_opt ctx) ~typ_source:typ_il_exp
        ~typ_target:typ_il
    in
    let exp_il = Il.SubE (exp_il, typ_il, subcheck_il) in
    let typ_il = Il.BoolT in
    Ok (ctx, exp_il, typ_il)
  else
    fail exp.at
      (F.asprintf "incomparable types %s and %s"
         (Il.Print.string_of_typ typ_il_exp)
         (Il.Print.string_of_typ typ_il))

(* Elaboration of expression:

   - If an iterated type is expected,
      - first try elaborating the expression as a singleton iteration,
        but except wildcard, epsilon, and empty list expressions
      - then try usual elaboration
   - Otherwise, directly try usual elaboration *)

and elab_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  elab_exp' ctx typ_il_expect exp
  |> nest exp.at
       (F.asprintf "elaboration of expression %s as type %s failed"
          (El.Print.string_of_exp exp)
          (Il.Print.string_of_typ typ_il_expect))

and elab_exp' (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  match as_iter_typ ctx typ_il_expect with
  | Ok (typ_il_expect_base, iter_expect) ->
      choose_sequential
        [
          (fun () ->
            match exp.it with
            | VarE id when id.it = "_" -> fail_silent
            | EpsE | ListE [] -> fail_silent
            | _ ->
                elab_exp_iter ctx typ_il_expect typ_il_expect_base iter_expect
                  exp);
          (fun () -> elab_exp_normal ctx typ_il_expect exp);
        ]
  | _ -> elab_exp_normal ctx typ_il_expect exp

and elab_exps (ctx : Ctx.t) (typs_il_expect : Il.typ list) (exps : exp list) :
    (Ctx.t * Il.exp list) attempt =
  match (typs_il_expect, exps) with
  | [], [] -> Ok (ctx, [])
  | [], _ -> fail no_region "more expressions than expected"
  | _, [] -> fail no_region "more expected types than expressions"
  | typ_il_expect :: typs_il_expect, exp :: exps ->
      let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
      let* ctx, exps_il = elab_exps ctx typs_il_expect exps in
      Ok (ctx, exp_il :: exps_il)

(* Elaboration of expression as a singleton iteration *)

and elab_exp_iter (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typ_il_expect_base : Il.typ) (iter_il_expect : Il.iter) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp ctx typ_il_expect_base exp in
  match iter_il_expect with
  | Opt ->
      let exp_il = Il.OptE (Some exp_il) $$ (exp.at, typ_il_expect.it) in
      Ok (ctx, exp_il)
  | List ->
      let exp_il = Il.ListE [ exp_il ] $$ (exp.at, typ_il_expect.it) in
      Ok (ctx, exp_il)

(* Normal elaboration of expressions: a two-phase process,

   - if a type can be inferred without any contextual information,
     match the inferred type with the expected type
      - this may fail for some expressions that require contextual information,
        e.g., notation expressions or expression sequences
   - for such cases, try to elaborate the expression using the expected type *)

and fail_cast (at : region) (typ_il_a : Il.typ) (typ_il_b : Il.typ) =
  let msg =
    F.asprintf "cannot cast %s to %s"
      (Il.Print.string_of_typ typ_il_a)
      (Il.Print.string_of_typ typ_il_b)
  in
  fail at msg

and cast_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (typ_il_infer : Il.typ)
    (exp_il : Il.exp) : Il.exp attempt =
  if Equiv.equiv_typ (Ctx.find_typdef_opt ctx) typ_il_expect typ_il_infer then
    Ok exp_il
  else if Sub.sub_typ (Ctx.find_typdef_opt ctx) typ_il_infer typ_il_expect then
    let exp_il =
      Il.UpCastE (typ_il_expect, exp_il) $$ (exp_il.at, typ_il_expect.it)
    in
    Ok exp_il
  else fail_cast exp_il.at typ_il_infer typ_il_expect

and elab_exp_normal (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (ctx, exp_il, typ_il_infer) ->
      let* exp_il = cast_exp ctx typ_il_expect typ_il_infer exp_il in
      Ok (ctx, exp_il)
  | Fail _ -> (
      match exp.it with
      | VarE id when id.it = "_" -> elab_exp_wildcard ctx exp.at typ_il_expect
      | _ -> (
          match typ_il_expect.it with
          | VarT (tid, targs_il) -> (
              let td = Ctx.find_typdef ctx tid in
              match td with
              | Param | Extern | Defining _ ->
                  elab_exp_plain ctx typ_il_expect exp
              | Defined (tparams, deftyp_il) -> (
                  let theta = TIdMap.of_lists tparams targs_il in
                  match deftyp_il.it with
                  | PlainT typ_il ->
                      let typ_il = Subst.subst_typ theta typ_il in
                      elab_exp_normal ctx typ_il exp
                  | StructT typfields_il ->
                      let typfields_il =
                        List.map
                          (fun (atom, typ_il) ->
                            let typ_il = Subst.subst_typ theta typ_il in
                            (atom, typ_il))
                          typfields_il
                      in
                      elab_exp_struct ctx typ_il_expect typfields_il exp
                  | VariantT typcases_il ->
                      let typcases_il =
                        List.map (Subst.subst_typcase theta) typcases_il
                      in
                      elab_exp_variant ctx typ_il_expect typcases_il exp))
          | _ -> elab_exp_plain ctx typ_il_expect exp))

(* Elaboration of wildcard variable expressions *)

and elab_exp_wildcard (ctx : Ctx.t) (at : region) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.exp) attempt =
  let id_fresh, typ_fresh, iters_fresh =
    Il.Fresh.var_from_exp ~wildcard:true ctx.menv ctx.frees
      (Il.VarE ("_" $ at) $$ (at, typ_il_expect.it))
  in
  let ctx = Ctx.add_free ctx id_fresh in
  let exp_il = Il.Var.as_exp ~dim:false (id_fresh, typ_fresh, iters_fresh) in
  Ok (ctx, exp_il)

(* Elaboration of plain expressions *)

and fail_elab_plain (at : region) (msg : string) =
  fail at ("cannot elaborate expression because " ^ msg)

and elab_exp_plain (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp_plain' ctx exp.at typ_il_expect exp.it in
  let exp_il = exp_il $$ (exp.at, typ_il_expect.it) in
  Ok (ctx, exp_il)

and elab_exp_plain' (ctx : Ctx.t) (at : region) (typ_il_expect : Il.typ)
    (exp : exp') : (Ctx.t * Il.exp') attempt =
  match exp with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain at
        (F.asprintf "the type of %s should have been inferred"
           (El.Print.string_of_exp (exp $ at)))
  | EpsE -> elab_eps_exp ctx typ_il_expect
  | ListE exps -> elab_list_exp ctx typ_il_expect exps
  | ConsE (exp_h, exp_t) -> elab_cons_exp ctx typ_il_expect exp_h exp_t
  | CatE (exp_l, exp_r) -> elab_cat_exp ctx typ_il_expect exp_l exp_r
  | ParenE exp -> elab_paren_exp ctx typ_il_expect exp
  | TupleE exps -> elab_tuple_exp ctx typ_il_expect exps
  | IterE (exp, iter) -> elab_iter_exp ctx typ_il_expect exp iter
  | _ ->
      fail at
        (F.asprintf "cannot elaborate expression %s as type %s"
           (El.Print.string_of_exp (exp $ at))
           (Il.Print.string_of_typ typ_il_expect))

(* Elaboration of episilon expressions *)

and elab_eps_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.exp') attempt =
  let* _typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  let exp_il =
    match iter_expect with Opt -> Il.OptE None | List -> Il.ListE []
  in
  Ok (ctx, exp_il)

(* Elaboration of list expressions *)

and elab_list_exp_elementwise (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (exps : exp list) : (Ctx.t * Il.exp list) attempt =
  match exps with
  | [] -> Ok (ctx, [])
  | exp :: exps ->
      let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
      let* ctx, exps_il = elab_list_exp_elementwise ctx typ_il_expect exps in
      Ok (ctx, exp_il :: exps_il)

and elab_list_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exps : exp list) :
    (Ctx.t * Il.exp') attempt =
  let* typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  match iter_expect with
  | Opt -> fail_elab_plain no_region "list expression with optional iteration"
  | List ->
      let* ctx, exps_il = elab_list_exp_elementwise ctx typ_il_expect exps in
      let exp_il = Il.ListE exps_il in
      Ok (ctx, exp_il)

(* Elaboration of cons expressions *)

and elab_cons_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp_h : exp)
    (exp_t : exp) : (Ctx.t * Il.exp') attempt =
  let* typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  let* ctx, exp_il_h = elab_exp ctx typ_il_expect exp_h in
  let* ctx, exp_il_t =
    elab_exp ctx
      (Il.IterT (typ_il_expect, iter_expect) $ typ_il_expect.at)
      exp_t
  in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il)

(* Elaboration of concatenation expressions *)

and elab_cat_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp') attempt =
  choose_sequential
    [
      (fun () ->
        let* typ_il_expect, iter_il_expect = as_iter_typ ctx typ_il_expect in
        let typ_il_expect =
          Il.IterT (typ_il_expect, iter_il_expect) $ typ_il_expect.at
        in
        let* ctx, exp_il_l = elab_exp ctx typ_il_expect exp_l in
        let* ctx, exp_il_r = elab_exp ctx typ_il_expect exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (Il.TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (Il.TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
    ]

(* Elaboration of tuple expressions *)

and elab_tuple_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exps : exp list) :
    (Ctx.t * Il.exp') attempt =
  let* typs_il_expect = as_tuple_typ ctx typ_il_expect in
  let* ctx, exps_il = elab_exps ctx typs_il_expect exps in
  let exp_il = Il.TupleE exps_il in
  Ok (ctx, exp_il)

(* Elaboration of parenthesized expressions *)

and elab_paren_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp') attempt =
  let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
  Ok (ctx, exp_il.it)

(* Elaboration of iterated expressions *)

and elab_iter_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp)
    (iter : iter) : (Ctx.t * Il.exp') attempt =
  let iter_il = elab_iter iter in
  let* typ_il_expect, iter_il_expect = as_iter_typ ctx typ_il_expect in
  if iter_il <> iter_il_expect then fail_elab_plain exp.at "iteration mismatch"
  else
    let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
    let exp_il = Il.IterE (exp_il, (iter_il_expect, [])) in
    Ok (ctx, exp_il)

(* Elaboration of notation expressions *)

and fail_elab_not (at : region) (msg : string) : (Ctx.t * Il.notexp) attempt =
  fail at ("cannot elaborate notation expression because " ^ msg)

and elab_exp_not (ctx : Ctx.t) (nottyp_il : Il.nottyp) (exp : exp) :
    (Ctx.t * Il.notexp) attempt =
  let open Mixfix in
  match (nottyp_il.it, exp.it) with
  | _, ParenE exp -> elab_exp_not ctx nottyp_il exp
  | Arg typ_il, _ ->
      let* ctx, exp_il = elab_exp ctx typ_il exp in
      Ok (ctx, Arg exp_il)
  | Atom atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at
        (F.asprintf "atom %s does not match the expected atom %s"
           (Il.Print.string_of_atom atom_e)
           (Il.Print.string_of_atom atom_t))
  | Atom atom_t, AtomE _ -> Ok (ctx, Atom atom_t)
  | Seq [], SeqE [] -> Ok (ctx, Seq [])
  | Seq (nottyp_il_h :: nottyps_il_t), SeqE (exp_h :: exps_t) ->
      let* ctx, notexp_il_h =
        elab_exp_not ctx (nottyp_il_h $ nottyp_il.at) exp_h
      in
      let* ctx, notexp_il_t =
        elab_exp_not ctx (Seq nottyps_il_t $ nottyp_il.at) (SeqE exps_t $ exp.at)
      in
      let mixfix_t =
        match notexp_il_t with
        | Seq mixfixes_t -> mixfixes_t
        | _ -> assert false
      in
      Ok (ctx, Seq (notexp_il_h :: mixfix_t))
  | Seq (_ :: _), SeqE [] -> fail_elab_not exp.at "omitted sequence tail"
  | Seq [], SeqE (_ :: _) -> fail_elab_not exp.at "expression is not empty"
  | Infix (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at
        (F.asprintf "infix operator %s does not match the expected operator %s"
           (Il.Print.string_of_atom atom_e)
           (Il.Print.string_of_atom atom_t))
  | Infix (nottyp_il_l, atom, nottyp_il_r), InfixE (exp_l, _, exp_r) ->
      let* ctx, notexp_il_l =
        elab_exp_not ctx (nottyp_il_l $ nottyp_il.at) exp_l
      in
      let* ctx, notexp_il_r =
        elab_exp_not ctx (nottyp_il_r $ nottyp_il.at) exp_r
      in
      Ok (ctx, Infix (notexp_il_l, atom, notexp_il_r))
  | Brack (atom_t_l, _, atom_t_r), BrackE (atom_e_l, exp, atom_e_r)
    when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
      fail_elab_not exp.at
        (F.asprintf
           "bracketing operator %s %s does not match the expected operator %s \
            %s"
           (Il.Print.string_of_atom atom_e_l)
           (Il.Print.string_of_atom atom_e_r)
           (Il.Print.string_of_atom atom_t_l)
           (Il.Print.string_of_atom atom_t_r))
  | Brack (atom_l, nottyp_inner, atom_r), BrackE (_, exp, _) ->
      let* ctx, notexp_il =
        elab_exp_not ctx (nottyp_inner $ nottyp_il.at) exp
      in
      Ok (ctx, Brack (atom_l, notexp_il, atom_r))
  | _ ->
      fail_elab_not exp.at
        (F.asprintf "expression %s does not match notation %s"
           (El.Print.string_of_exp exp)
           (Mixfix.to_string nottyp_il.it))

(* Elaboration of struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  fail at ("cannot elaborate struct expression because " ^ msg)

and elab_expfields (ctx : Ctx.t) (at : region)
    (typfields : (atom * Il.typ) list) (expfields : (atom * exp) list) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  match (typfields, expfields) with
  | [], [] -> Ok (ctx, [])
  | [], (atom_e, _) :: _ ->
      fail_elab_struct atom_e.at "expression has extra fields"
  | _ :: _, [] -> fail_elab_struct at "expression omitted struct fields"
  | (atom_t, _) :: _, (atom_e, _) :: _ when atom_t.it <> atom_e.it ->
      fail_elab_struct atom_e.at "atom does not match"
  | (atom_t, typ_il) :: typfields, (_, exp) :: expfields ->
      let* ctx, exp_il = elab_exp ctx typ_il exp in
      let* ctx, expfields_il = elab_expfields ctx at typfields expfields in
      Ok (ctx, (atom_t, exp_il) :: expfields_il)

and elab_exp_struct (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typfields_il : Il.typfield list) (exp : exp) : (Ctx.t * Il.exp) attempt =
  let* ctx, expfields_il = elab_exp_struct' ctx typfields_il exp in
  let exp_il = Il.StrE expfields_il $$ (exp.at, typ_il_expect.it) in
  Ok (ctx, exp_il)

and elab_exp_struct' (ctx : Ctx.t) (typfields_il : Il.typfield list) (exp : exp)
    : (Ctx.t * (Il.atom * Il.exp) list) attempt =
  match exp.it with
  | StrE expfields -> elab_expfields ctx exp.at typfields_il expfields
  | _ -> fail_elab_struct exp.at "expression is not a struct"

(* Elaboration of variant expressions

   This finds a single case that matches the expression,
   where it has the smallest possible type, according to the variant type subtyping rule

   Finding the smallest type is important because the interpreter needs to
   propagate the type information while evaluating expressions,
   and it has to perform runtime type checks of whether a value is a subtype of some particular type *)

and fail_elab_variant (at : region) (msg : string) : (Ctx.t * Il.exp) attempt =
  fail at ("cannot elaborate variant case because " ^ msg)

and elab_exp_variant (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typcases_il : Il.typcase list) (exp : exp) : (Ctx.t * Il.exp) attempt =
  let ctx, exps_il =
    List.fold_left
      (fun (ctx, exps_il) typcase_il ->
        let nottyp_il, typorigin_il, _ = typcase_il in
        match elab_exp_not ctx nottyp_il exp with
        | Ok (ctx, notexp_il) ->
            let typ_il =
              let id, targs_il = typorigin_il.it in
              Il.VarT (id, targs_il) $ typorigin_il.at
            in
            let exp_il = Il.CaseE notexp_il $$ (exp.at, typ_il.it) in
            let+ exp_il = cast_exp ctx typ_il_expect typ_il exp_il in
            (ctx, exps_il @ [ exp_il ])
        | Fail _ -> (ctx, exps_il))
      (ctx, []) typcases_il
  in
  match exps_il with
  | [ exp_il ] -> Ok (ctx, exp_il)
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Elaboration of paths *)

and elab_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path) :
    (Ctx.t * Il.path * Il.typ) attempt =
  let* ctx, path_il, typ_il = elab_path' ctx typ_il_expect path.it in
  let path_il = path_il $$ (path.at, typ_il) in
  let typ_il = typ_il $ path.at in
  Ok (ctx, path_il, typ_il)

and elab_path' (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path') :
    (Ctx.t * Il.path' * Il.typ') attempt =
  match path with
  | RootP -> elab_root_path ctx typ_il_expect
  | IdxP (path, exp) -> elab_idx_path ctx typ_il_expect path exp
  | SliceP (path, exp_l, exp_h) ->
      elab_slice_path ctx typ_il_expect path exp_l exp_h
  | DotP (path, atom) -> elab_dot_path ctx typ_il_expect path atom

(* Elaboration of root paths *)

and elab_root_path (ctx : Ctx.t) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.path' * Il.typ') attempt =
  Ok (ctx, Il.RootP, typ_il_expect.it)

(* Elaboration of index paths *)

and elab_idx_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (exp : exp) : (Ctx.t * Il.path' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il = elab_exp ctx (Il.NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* typ_il = as_list_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il = elab_exp ctx (Il.NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* _ = as_text_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
    ]

(* Elaboration of slice paths *)

and elab_slice_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (exp_l : exp) (exp_h : exp) : (Ctx.t * Il.path' * Il.typ') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_list_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_text_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
    ]

(* Elaboration of dot paths *)

and elab_dot_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (atom : atom) : (Ctx.t * Il.path' * Il.typ') attempt =
  let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
  let* typfields_il = as_struct_typ ctx typ_il in
  let* typ_il =
    List.find_opt (fun (atom_t, _) -> atom.it = atom_t.it) typfields_il
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, typ_il) -> Ok typ_il
    | None -> fail atom.at "cannot infer type of field"
  in
  let path_il = Il.DotP (path_il, atom) in
  Ok (ctx, path_il, typ_il.it)

(* Elaboration of parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      let ctx_local = ctx in
      let ctx_local = Ctx.add_tparams ctx_local tparams in
      let params_il = List.map (elab_param ctx_local) params in
      let typ_il = elab_plaintyp ctx_local plaintyp in
      Il.DefP (id, tparams, params_il, typ_il) $ param.at

(* Elaboration of arguments: either as definition, or part of a call expression

   Handling of function parameters differs based on whether it is intended to be a definition

    - If it is a definition, the function argument must matched the name of the function parameter,
      and it adds the function definition to the context
    - Otherwise, the function argument must match the type of the function parameter *)

and elab_arg ?(as_def = false) (ctx : Ctx.t) (param_il : Il.param) (arg : arg) :
    Ctx.t * Il.arg =
  match (param_il.it, arg.it) with
  | ExpP typ_il, ExpA exp ->
      let+ ctx, exp_il = elab_exp ctx typ_il exp in
      let arg_il = Il.ExpA exp_il $ arg.at in
      (ctx, arg_il)
  | DefP (id_p, tparams_il_p, params_il_p, typ_il_p), DefA id_a when as_def ->
      check (id_p.it = id_a.it) arg.at
        (F.asprintf
           "function argument does not match the declared function parameter %s"
           (Id.to_string id_p));
      let ctx =
        Ctx.add_defined_func_dec ctx id_p tparams_il_p params_il_p typ_il_p
      in
      let arg_il = Il.DefA id_a $ arg.at in
      (ctx, arg_il)
  | DefP (id_p, tparams_il_p, params_il_p, typ_il_p), DefA id_a ->
      let tparams_il_a, params_il_a, typ_il_a =
        Ctx.find_func_signature ctx id_a
      in
      let typs_params_il_p = Typ.Make.of_params_il params_il_p in
      let typs_params_il_a = Typ.Make.of_params_il params_il_a in
      check
        (Equiv.equiv_functyp (Ctx.find_typdef_opt ctx) arg.at tparams_il_p
           typs_params_il_p typ_il_p tparams_il_a typs_params_il_a typ_il_a)
        arg.at
        (F.asprintf
           "function argument does not match the declared function parameter %s"
           (Id.to_string id_p));
      let arg_il = Il.DefA id_a $ arg.at in
      (ctx, arg_il)
  | ExpP _, DefA _ ->
      error arg.at
        "expected an expression argument, but got a function argument"
  | DefP _, ExpA _ ->
      error arg.at
        "expected a function argument, but got an expression argument"

and elab_args ?(as_def = false) (at : region) (ctx : Ctx.t)
    (params_il : Il.param list) (args : arg list) : Ctx.t * Il.arg list =
  check (List.length args = List.length params_il) at "arguments do not match";
  List.fold_left2
    (fun (ctx, args_il) param_il arg ->
      let ctx, arg_il = elab_arg ~as_def ctx param_il arg in
      (ctx, args_il @ [ arg_il ]))
    (ctx, []) params_il args

(* Elaboration of premises *)

type prem_internal = prem_internal' phrase
and prem_internal' = SomePr of Il.prem' | VarPr | ElsePr

let internalize_prem (prem_il : Il.prem) : prem_internal =
  SomePr prem_il.it $ prem_il.at

let externalize_prem (prem_internal : prem_internal) : Il.prem option =
  match prem_internal.it with
  | SomePr prem_il -> Some (prem_il $ prem_internal.at)
  | VarPr | ElsePr -> None

let is_else_prem_internal (prem_internal : prem_internal) : bool =
  match prem_internal.it with ElsePr -> true | _ -> false

let check_prems_internal (at : region) (prems_internal : prem_internal list) :
    unit =
  let prems_else_internal = List.filter is_else_prem_internal prems_internal in
  if List.length prems_else_internal <= 1 then ()
  else error at "cannot use multiple otherwise premises"

let rec elab_prem (ctx : Ctx.t) (prem : prem) : Ctx.t * prem_internal =
  let ctx, prem_internal = elab_prem' ctx prem.it in
  let prem_internal = prem_internal $ prem.at in
  (ctx, prem_internal)

and elab_prem' (ctx : Ctx.t) (prem : prem') : Ctx.t * prem_internal' =
  let wrap_some (ctx, prem) = (ctx, SomePr prem) in
  let wrap_var ctx = (ctx, VarPr) in
  let wrap_else ctx = (ctx, ElsePr) in
  match prem with
  | VarPr (id, plaintyp) -> elab_var_prem ctx id plaintyp |> wrap_var
  | RulePr (id, exp) -> elab_rule_prem ctx id exp |> wrap_some
  | RuleNotPr (id, exp) -> elab_rule_not_prem ctx id exp |> wrap_some
  | IfPr exp -> elab_if_prem ctx exp |> wrap_some
  | ElsePr -> ctx |> wrap_else
  | IterPr (prem, iter) -> elab_iter_prem ctx prem iter |> wrap_some
  | DebugPr exp -> elab_debug_prem ctx exp |> wrap_some

and elab_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t * prem_internal list =
  List.fold_left_map (fun ctx prem -> elab_prem ctx prem) ctx prems

(* Elaboration of variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id typ_il

(* Elaboration of rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let nottyp_il, inputs = Ctx.find_rel_signature ctx id in
  let+ ctx, notexp_il = elab_exp_not ctx nottyp_il exp in
  let exps_il = Mixfix.args notexp_il in
  if Hints.Input.is_conditional inputs exps_il then
    let prem_il = Il.IfHoldPr (id, notexp_il) in
    (ctx, prem_il)
  else
    let prem_il = Il.RulePr (id, notexp_il, inputs) in
    (ctx, prem_il)

(* Elaboration of negated rule premises *)

and elab_rule_not_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let nottyp_il, inputs = Ctx.find_rel_signature ctx id in
  let+ ctx, notexp_il = elab_exp_not ctx nottyp_il exp in
  let exps_il = Mixfix.args notexp_il in
  check
    (Hints.Input.is_conditional inputs exps_il)
    exp.at "negated rule premises do not take inputs";
  let prem_il = Il.IfNotHoldPr (id, notexp_il) in
  (ctx, prem_il)

(* Elaboration of if premises *)

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.prem' =
  let+ ctx, exp_il = elab_exp ctx (Il.BoolT $ exp.at) exp in
  let prem_il = Il.IfPr exp_il in
  (ctx, prem_il)

(* Elaboration of iterated premises *)

and elab_iter_prem (ctx : Ctx.t) (prem : prem) (iter : iter) : Ctx.t * Il.prem'
    =
  let iter_il = elab_iter iter in
  let ctx, prem_internal = elab_prem ctx prem in
  let prem_il =
    match prem_internal.it with
    | SomePr prem_il -> prem_il $ prem_internal.at
    | VarPr -> error prem.at "cannot iterate a var premise"
    | ElsePr -> error prem.at "cannot iterate an otherwise premise"
  in
  let prem_il = Il.IterPr (prem_il, (iter_il, [], [])) in
  (ctx, prem_il)

(* Elaboration of debug premises *)

and elab_debug_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.prem' =
  let+ ctx, exp_il, _ = infer_exp ctx exp in
  let prem_il = Il.DebugPr exp_il in
  (ctx, prem_il)

(* Elaboration of rules *)

type rule_internal = SomeRule of Il.rule | ElseRule of Il.rule
type rulegroup_internal = Group of Il.rulegroup | ElseGroup of Il.elsegroup

let is_else_rule_internal (rule_internal : rule_internal) : bool =
  match rule_internal with ElseRule _ -> true | SomeRule _ -> false

let elab_rule (ctx : Ctx.t) (at : region) (id_rule : id) (nottyp_il : Il.nottyp)
    (exp : exp) (prems : prem list) : rule_internal =
  let+ ctx, notexp_il = elab_exp_not ctx nottyp_il exp in
  let _ctx, prems_internal = elab_prems ctx prems in
  check_prems_internal id_rule.at prems_internal;
  let is_else_path = List.exists is_else_prem_internal prems_internal in
  let prems_il = List.filter_map externalize_prem prems_internal in
  let rule_il = (id_rule, notexp_il, prems_il) $ at in
  if is_else_path then ElseRule rule_il else SomeRule rule_il

let elab_rulegroup (ctx : Ctx.t) (at : region) (id_rel : id) (id_rulegroup : id)
    (rules : rule list) : rulegroup_internal =
  let nottyp_il, _, _, _ = Ctx.find_defined_rel ctx id_rel in
  let ctxs_local =
    List.map
      (fun rule ->
        let ctx_local = { ctx with frees = IdSet.empty } in
        El.Free.free_rule rule |> Ctx.add_frees ctx_local)
      rules
  in
  let rules_internal =
    List.map2
      (fun ctx_local rule ->
        let id_rel_rule, id_rule, exp, prems = rule.it in
        check (Id.eq id_rel id_rel_rule) id_rule.at
          "rule group identifier does not match relation identifier";
        elab_rule ctx_local rule.at id_rule nottyp_il exp prems)
      ctxs_local rules
  in
  let rules_else_internal = List.filter is_else_rule_internal rules_internal in
  match rules_else_internal with
  | [] ->
      let rules_il =
        List.map
          (function SomeRule rule_il -> rule_il | _ -> assert false)
          rules_internal
      in
      Group ((id_rulegroup, rules_il) $ at)
  | [ ElseRule rule_il_else ] ->
      check
        (List.length rules_internal = 1)
        at "cannot have other rules alongside an otherwise rule";
      ElseGroup ((id_rulegroup, rule_il_else) $ at)
  | _ -> error at "cannot use multiple otherwise rules in a rule group"

(* Elaboration of clauses *)

type clause_internal = Clause of Il.clause | ElseClause of Il.clause

let elab_clause (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : clause_internal =
  let tparams_il_expected, params_il, typ_il, _, _ =
    Ctx.find_defined_func ctx id
  in
  check
    (List.length tparams = List.length tparams_il_expected
    && List.for_all2 ( = ) (List.map it tparams)
         (List.map it tparams_il_expected))
    id.at "type arguments do not match";
  check (List.length params_il = List.length args) at "arguments do not match";
  let ctx_local = { ctx with frees = IdSet.empty } in
  let ctx_local =
    let def = FuncDefD (id, tparams, args, exp, prems) $ at in
    El.Free.free_id_def def |> Ctx.add_frees ctx_local
  in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let ctx_local, args_il = elab_args ~as_def:true at ctx_local params_il args in
  let ctx_local, prems_internal = elab_prems ctx_local prems in
  check_prems_internal at prems_internal;
  let is_else_clause = List.exists is_else_prem_internal prems_internal in
  let prems_il = List.filter_map externalize_prem prems_internal in
  let+ _ctx_local, exp_il = elab_exp ctx_local typ_il exp in
  let clause_il = (args_il, exp_il, prems_il) $ at in
  if is_else_clause then ElseClause clause_il else Clause clause_il

(* Elaboration of definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  let at = def.at in
  match def.it with
  | ExternSynD (id, hints) -> elab_extern_syn_def ctx at id hints |> wrap_some
  | SynD syns -> elab_syn_def ctx syns |> wrap_none
  | TypD (id, tparams, deftyp, hints) ->
      elab_typ_def ctx id tparams deftyp hints |> wrap_some
  | VarD (id, plaintyp, hints) ->
      elab_var_def ctx id plaintyp hints |> wrap_some
  | ExternRelD (id, nottyp, hints) ->
      elab_extern_rel_def ctx at id nottyp hints |> wrap_some
  | RelD (id, nottyp, hints) -> elab_rel_def ctx at id nottyp hints |> wrap_some
  | RuleGroupD (id_rel, id_rulegroup, rules) ->
      elab_rulegroup_def ctx at id_rel id_rulegroup rules |> wrap_none
  | ExternDecD (id, tparams, params, plaintyp, hints) ->
      elab_extern_dec_def ctx at id tparams params plaintyp hints |> wrap_some
  | BuiltinDecD (id, tparams, params, plaintyp, hints) ->
      elab_builtin_dec_def ctx at id tparams params plaintyp hints |> wrap_some
  | TableDecD (id, params, list_typ, hints) ->
      elab_table_dec_def ctx at id params list_typ hints |> wrap_some
  | FuncDecD (id, tparams, params, plaintyp, hints) ->
      elab_func_dec_def ctx at id tparams params plaintyp hints |> wrap_some
  | TableDefD (id, tablerows) ->
      elab_table_def_def ctx at id tablerows |> wrap_none
  | FuncDefD (id, tparams, args, exp, prems) ->
      elab_func_def ctx at id tparams args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Elaboration of type declarations *)

and elab_extern_syn_def (ctx : Ctx.t) (at : region) (id : id)
    (hints : hint list) : Ctx.t * Il.def =
  check (valid_tid id) id.at "invalid type identifier";
  let td = Typdef.Extern in
  let ctx = Ctx.add_typdef ctx id td in
  let typ_il = Il.VarT (id, []) $ id.at in
  let ctx = Ctx.add_metavar ctx id typ_il in
  let def_il = Il.ExternTypD (id, hints) $ at in
  (ctx, def_il)

and elab_syn_def (ctx : Ctx.t) (syns : (id * tparam list) list) : Ctx.t =
  List.fold_left
    (fun ctx (id, tparams) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      check (valid_tid id) id.at "invalid type identifier";
      let td = Typdef.Defining tparams in
      let ctx = Ctx.add_typdef ctx id td in
      if tparams = [] then
        let typ_il = Il.VarT (id, []) $ id.at in
        Ctx.add_metavar ctx id typ_il
      else ctx)
    ctx syns

(* Elaboration of type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) (hints : hint list) : Ctx.t * Il.def =
  let td_opt = Ctx.find_typdef_opt ctx id in
  let ctx =
    match td_opt with
    | Some (Typdef.Defining tparams_defining) ->
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match";
        ctx
    | None ->
        check (valid_tid id) id.at "invalid type identifier";
        let td = Typdef.Defining tparams in
        let ctx = Ctx.add_typdef ctx id td in
        if tparams = [] then
          let typ_il = Il.VarT (id, []) $ id.at in
          Ctx.add_metavar ctx id typ_il
        else ctx
    | _ -> error id.at "type was already defined"
  in
  check (List.for_all valid_tid tparams) id.at "invalid type parameter";
  let ctx_local = Ctx.add_tparams ctx tparams in
  let td, deftyp_il = elab_deftyp ctx_local id tparams deftyp in
  let def_il = Il.TypD (id, tparams, deftyp_il, hints) $ deftyp.at in
  let ctx = Ctx.update_typdef ctx id td in
  (ctx, def_il)

(* Elaboration of variables *)

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp)
    (hints : hint list) : Ctx.t * Il.def =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let typ_il = elab_plaintyp ctx plaintyp in
  let ctx = Ctx.add_metavar ctx id typ_il in
  let def_il = Il.VarD (id, typ_il, hints) $ id.at in
  (ctx, def_il)

(* Elaboration of relations *)

and fetch_rel_input_hint (at : region) (nottyp_il : Il.nottyp)
    (hints : hint list) : int list =
  let len = Mixfix.arity nottyp_il.it in
  let hint_input_default = List.init len Fun.id in
  let hintexp_input_opt =
    List.find_map
      (fun hint -> if hint.hintid.it = "input" then Some hint.hintexp else None)
      hints
  in
  match hintexp_input_opt with
  | Some hintexp -> (
      let inputs_opt = Hints.Input.init hintexp in
      match inputs_opt with
      | Some inputs -> (
          match Hints.Input.validate inputs len with
          | Ok () -> inputs
          | Error msg -> error at (F.asprintf "invalid input hint: %s" msg))
      | None ->
          error at
            (F.asprintf
               "malformed input hint: should be a sequence of indexed holes \
                %%N (N < %d)"
               len))
  (* If no hint is provided, assume all fields are inputs *)
  | None ->
      warn at "no input hint provided";
      hint_input_default

and elab_extern_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.def =
  let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
  let inputs = fetch_rel_input_hint at nottyp_il hints in
  let ctx = Ctx.add_extern_rel ctx id nottyp_il inputs in
  let def_il = Il.ExternRelD (id, nottyp_il, inputs, hints) $ at in
  (ctx, def_il)

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.def =
  let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
  let inputs = fetch_rel_input_hint at nottyp_il hints in
  let ctx = Ctx.add_defined_rel ctx id nottyp_il inputs in
  let def_il = Il.RelD (id, nottyp_il, inputs, [], None, hints) $ at in
  (ctx, def_il)

(* Elaboration of rule groups *)

and elab_rulegroup_def (ctx : Ctx.t) (at : region) (id_rel : id)
    (id_rulegroup : id) (rules : rule list) : Ctx.t =
  let rulegroup_internal = elab_rulegroup ctx at id_rel id_rulegroup rules in
  match rulegroup_internal with
  | Group rulegroup_il -> Ctx.add_defined_rulegroup ctx id_rel rulegroup_il
  | ElseGroup elsegroup_il -> Ctx.add_defined_elsegroup ctx id_rel elsegroup_il

(* Elaboration of function declarations *)

and elab_extern_dec_def (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (params : param list) (plaintyp : plaintyp)
    (hints : hint list) : Ctx.t * Il.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let ctx = Ctx.add_extern_func_dec ctx id tparams params_il typ_il in
  let def_il = Il.ExternDecD (id, tparams, params_il, typ_il, hints) $ at in
  (ctx, def_il)

and elab_builtin_dec_def (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (params : param list) (plaintyp : plaintyp)
    (hints : hint list) : Ctx.t * Il.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let ctx = Ctx.add_builtin_func_dec ctx id tparams params_il typ_il in
  let def_il = Il.BuiltinDecD (id, tparams, params_il, typ_il, hints) $ at in
  (ctx, def_il)

and elab_table_dec_def (ctx : Ctx.t) (at : region) (id : id)
    (params : param list) (plaintyp : plaintyp) (hints : hint list) :
    Ctx.t * Il.def =
  let params_il = List.map (elab_param ctx) params in
  check
    (List.for_all
       (fun (param_il : Il.param) ->
         match param_il.it with ExpP _ -> true | DefP _ -> false)
       params_il)
    at "table cannot have function parameters";
  let typ_il = elab_plaintyp ctx plaintyp in
  check (typ_il.it = Il.BoolT) typ_il.at "table must return a boolean type";
  let ctx = Ctx.add_table_func_dec ctx id params_il typ_il in
  let def_il = Il.TableDecD (id, params_il, typ_il, [], hints) $ at in
  (ctx, def_il)

and elab_func_dec_def (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (params : param list) (plaintyp : plaintyp)
    (hints : hint list) : Ctx.t * Il.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let def_il =
    Il.FuncDecD (id, tparams, params_il, typ_il, [], None, hints) $ at
  in
  let ctx = Ctx.add_defined_func_dec ctx id tparams params_il typ_il in
  (ctx, def_il)

(* Elaboration of table function definitions *)

and elab_tablerow (ctx : Ctx.t) (at : region) (id : id)
    (params_il : Il.param list) (typ_il : Il.typ) (tablerow : tablerow) :
    Il.tablerow =
  let exp_pattern, exp_body = tablerow.it in
  let exps =
    match exp_pattern.it with TupleE exps -> exps | _ -> [ exp_pattern ]
  in
  let args = List.map (fun exp -> ExpA exp $ exp.at) exps in
  let ctx_local = { ctx with frees = IdSet.empty } in
  let ctx_local =
    let def = TableDefD (id, [ tablerow ]) $ at in
    El.Free.free_id_def def |> Ctx.add_frees ctx_local
  in
  let ctx_local, args_il = elab_args ~as_def:true at ctx_local params_il args in
  let+ _ctx_local, exp_il = elab_exp ctx_local typ_il exp_body in
  let tablerow_il = (args_il, exp_il) $ tablerow.at in
  tablerow_il

and elab_tablerows (ctx : Ctx.t) (at : region) (id : id)
    (params_il : Il.param list) (typ_il : Il.typ) (tablerows : tablerow list) :
    Il.tablerow list =
  List.map (elab_tablerow ctx at id params_il typ_il) tablerows

and elab_table_def_def (ctx : Ctx.t) (at : region) (id : id)
    (tablerows : tablerow list) : Ctx.t =
  let params_il, typ_il, _ = Ctx.find_table_func ctx id in
  let tablerows_il = elab_tablerows ctx at id params_il typ_il tablerows in
  Ctx.add_table_func_tablerows ctx id tablerows_il

(* Elaboration of function definitions *)

and elab_func_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let clause_internal = elab_clause ctx at id tparams args exp prems in
  match clause_internal with
  | Clause clause_il -> Ctx.add_defined_func_clause ctx id clause_il
  | ElseClause clause_il -> Ctx.add_defined_func_elseclause ctx id clause_il

(* Elaboration of spec *)

(* Populate type definitions *)

let populate_typs (ctx : Ctx.t) : unit =
  Envs.TDEnv.iter
    (fun tid td ->
      match td with
      | Typdef.Defining tparams ->
          warn tid.at
            (F.asprintf "type %s%s was declared but not defined"
               (Il.Print.string_of_typid tid)
               (Il.Print.string_of_tparams tparams))
      | _ -> ())
    ctx.tdenv

(* Populate rules to their respective relations *)

let populate_rule (ctx : Ctx.t) (def_il : Il.def) : Il.def =
  match def_il.it with
  | Il.RelD (id, nottyp_il, inputs, [], None, hints) ->
      let _, _, rulegroups_il, elsegroup_il_opt = Ctx.find_defined_rel ctx id in
      Il.RelD (id, nottyp_il, inputs, rulegroups_il, elsegroup_il_opt, hints)
      $ def_il.at
  | Il.RelD _ -> error def_il.at "relation was already populated"
  | _ -> def_il

let populate_rules (ctx : Ctx.t) (spec_il : Il.spec) : Il.spec =
  let spec_il = List.map (populate_rule ctx) spec_il in
  List.iter
    (fun def_il ->
      match def_il.it with
      | Il.RelD (id, _, _, [], None, _) ->
          warn def_il.at
            (F.asprintf "relation %s has no rule groups defined"
               (Id.to_string id))
      | _ -> ())
    spec_il;
  spec_il

(* Populate clauses to their respective function declarations *)

let populate_clause (ctx : Ctx.t) (def_il : Il.def) : Il.def =
  match def_il.it with
  | Il.TableDecD (id, params_il, typ_il, [], hints) ->
      let _, _, tablerows_il = Ctx.find_table_func ctx id in
      Il.TableDecD (id, params_il, typ_il, tablerows_il, hints) $ def_il.at
  | Il.FuncDecD (id, tparams_il, params_il, typ_il, [], None, hints) ->
      let _, _, _, clauses_il, elseclause_il_opt =
        Ctx.find_defined_func ctx id
      in
      Il.FuncDecD
        (id, tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt, hints)
      $ def_il.at
  | Il.TableDecD _ -> error def_il.at "table was already populated"
  | Il.FuncDecD _ -> error def_il.at "function was already populated"
  | _ -> def_il

let populate_clauses (ctx : Ctx.t) (spec_il : Il.spec) : Il.spec =
  let spec_il = List.map (populate_clause ctx) spec_il in
  List.iter
    (fun def_il ->
      match def_il.it with
      | Il.TableDecD (id, _, _, [], _) ->
          warn def_il.at
            (F.asprintf "table %s has no rows defined" (Id.to_string id))
      | Il.FuncDecD (id, _, _, _, [], None, _) ->
          warn def_il.at
            (F.asprintf "function %s has no clauses defined" (Id.to_string id))
      | _ -> ())
    spec_il;
  spec_il

(* Entry point *)

let elab_spec (spec : spec) : Il.spec =
  let ctx = Ctx.init () in
  let ctx, spec_il = elab_defs ctx spec in
  populate_typs ctx;
  spec_il |> populate_rules ctx |> populate_clauses ctx
  |> Dimension.analyze_spec
