open Domain
open Lib
open Lang
open El
open Runtime.Static
open Attempt
open Error
open Util.Checks
open Util.Source
module Xl = Lang.Xl
module F = Format

(* Checks *)

(* Identifiers *)

let valid_tid (id : id) = id.it = (Xl.Var.strip_var_suffix id).it

(* Iteration elaboration *)

let elab_iter (iter : iter) : Il.iter =
  match iter with Opt -> Il.Opt | List -> Il.List

(* Types *)

(* Type destructuring *)

let as_text_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : unit attempt =
  let plaintyp = Plaintyp.expand_plaintyp ctx.tdenv plaintyp in
  match plaintyp.it with
  | TextT -> Ok ()
  | _ -> fail plaintyp.at "cannot destruct type as text"

let as_iter_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) :
    (plaintyp * iter) attempt =
  let plaintyp = Plaintyp.expand_plaintyp ctx.tdenv plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, iter) -> Ok (plaintyp, iter)
  | _ -> fail plaintyp.at "cannot destruct type as an iteration"

let as_tuple_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) :
    plaintyp list attempt =
  let plaintyp = Plaintyp.expand_plaintyp ctx.tdenv plaintyp in
  match plaintyp.it with
  | TupleT plaintyps -> Ok plaintyps
  | _ -> fail plaintyp.at "cannot destruct type as a tuple"

let as_list_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = Plaintyp.expand_plaintyp ctx.tdenv plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, List) -> Ok plaintyp
  | _ -> fail plaintyp.at "cannot destruct type as a list"

let as_struct_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) :
    typfield list attempt =
  let plaintyp = Plaintyp.expand_plaintyp ctx.tdenv plaintyp in
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, `Struct typfields)) -> Ok typfields
      | _ -> fail plaintyp.at "cannot destruct type as a struct")
  | _ -> fail plaintyp.at "cannot destruct type as a struct"

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

and elab_nottyp (ctx : Ctx.t) (typ : typ) : Il.nottyp =
  match typ with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let typ_il = elab_plaintyp ctx plaintyp in
      (mixop, [ typ_il ]) $ plaintyp.at
  | NotationT nottyp -> (
      match nottyp.it with
      | AtomT atom ->
          let mixop = [ [ atom ] ] in
          let typs_il = [] in
          (mixop, typs_il) $ nottyp.at
      | SeqT [] ->
          let mixop = [ [] ] in
          let typs_il = [] in
          (mixop, typs_il) $ nottyp.at
      | SeqT (typ :: typs) ->
          let mixop_h, typs_il_h = elab_nottyp ctx typ |> it in
          let mixop_t, typs_il_t =
            elab_nottyp ctx (NotationT (SeqT typs $ nottyp.at)) |> it
          in
          let mixop = Mixop.merge mixop_h mixop_t in
          let typs_il = typs_il_h @ typs_il_t in
          (mixop, typs_il) $ nottyp.at
      | InfixT (typ_l, atom, typ_r) ->
          let mixop_l, typs_il_l = elab_nottyp ctx typ_l |> it in
          let mixop_r, typs_il_r = elab_nottyp ctx typ_r |> it in
          let mixop_l = Mixop.merge mixop_l [ [ atom ] ] in
          let mixop = Mixop.merge mixop_l mixop_r in
          let typs_il = typs_il_l @ typs_il_r in
          (mixop, typs_il) $ nottyp.at
      | BrackT (atom_l, typ, atom_r) ->
          let mixop, typs_il = elab_nottyp ctx typ |> it in
          let mixop_l = Mixop.merge [ [ atom_l ] ] mixop in
          let mixop = Mixop.merge mixop_l [ [ atom_r ] ] in
          (mixop, typs_il) $ nottyp.at)

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
  let td = Typdef.Defined (tparams, `Plain plaintyp) in
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
  let td = Typdef.Defined (tparams, `Struct typfields) in
  (td, deftyp_il)

(* Elaboration of variant type definitions *)

and expand_typcase (ctx : Ctx.t) (plaintyp : plaintyp) (typcase : typcase) :
    ((nottyp * hint list) * plaintyp) list =
  let typ, hints = typcase in
  match typ with
  | PlainT plaintyp -> (
      let kind = Plaintyp.kind_plaintyp ctx.tdenv plaintyp in
      match kind with
      | Opaque -> error plaintyp.at "cannot extend an incomplete type"
      | Variant typcases -> typcases
      | _ -> error plaintyp.at "cannot extend a non-variant type")
  | NotationT nottyp -> [ ((nottyp, hints), plaintyp) ]

and elab_typcase (ctx : Ctx.t) (typcase : nottyp * hint list) : Il.typcase =
  let nottyp, hints = typcase in
  (elab_nottyp ctx (NotationT nottyp), hints)

and elab_deftyp_variant (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (typcases : typcase list) : Typdef.t * Il.deftyp =
  let plaintyp =
    let targs =
      List.map (fun tparam -> VarT (tparam, []) $ no_region) tparams
    in
    VarT (id, targs) $ no_region
  in
  let typcases = List.concat_map (expand_typcase ctx plaintyp) typcases in
  let typcases_il = typcases |> List.map fst |> List.map (elab_typcase ctx) in
  let mixops = typcases_il |> List.map fst |> List.map it |> List.map fst in
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
  let td = Typdef.Defined (tparams, `Variant typcases) in
  (td, deftyp_il)

(* Expressions *)

(* Inference of expression type *)

and fail_infer (at : region) (construct : string) =
  fail at ("cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : (Ctx.t * Il.exp * plaintyp) attempt =
  let* ctx, exp_il, plaintyp = infer_exp' ctx exp.at exp.it in
  let typ_il = elab_plaintyp ctx (plaintyp $ exp.at) in
  let exp_il = exp_il $$ (exp.at, typ_il.it) in
  let plaintyp = plaintyp $ exp.at in
  Ok (ctx, exp_il, plaintyp)

and infer_exp' (ctx : Ctx.t) (at : region) (exp : exp') :
    (Ctx.t * Il.exp' * plaintyp') attempt =
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
    (Ctx.t * Il.exp list * plaintyp list) attempt =
  match exps with
  | [] -> Ok (ctx, [], [])
  | exp :: exps ->
      let* ctx, exp_il, plaintyp = infer_exp ctx exp in
      let* ctx, exps_il, plaintyps = infer_exps ctx exps in
      Ok (ctx, exp_il :: exps_il, plaintyp :: plaintyps)

(* Inference of boolean expressions *)

and infer_bool_exp (ctx : Ctx.t) (b : bool) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let exp_il = Il.BoolE b in
  let plaintyp = BoolT in
  Ok (ctx, exp_il, plaintyp)

(* Inference of number expressions *)

and infer_num_exp (ctx : Ctx.t) (num : Xl.Num.t) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let exp_il = Il.NumE num in
  let plaintyp = NumT (Xl.Num.to_typ num) in
  Ok (ctx, exp_il, plaintyp)

(* Inference of text expressions *)

and infer_text_exp (ctx : Ctx.t) (text : string) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let exp_il = Il.TextE text in
  let plaintyp = TextT in
  Ok (ctx, exp_il, plaintyp)

(* Inference of variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let tid = Xl.Var.strip_var_suffix id in
  let meta_opt = Ctx.find_metavar_opt ctx tid in
  match meta_opt with
  | Some plaintyp ->
      let exp_il = Il.VarE id in
      Ok (ctx, exp_il, plaintyp.it)
  | None -> fail_infer id.at "variable"

(* Inference of unary expressions *)

and infer_unop (ctx : Ctx.t) (at : region) (unop : unop) (plaintyp : plaintyp)
    (exp_il : Il.exp) : (Il.optyp * Il.exp * plaintyp') attempt =
  let unop_candidates =
    match unop with
    | #Xl.Bool.unop -> [ (`BoolT, BoolT, BoolT) ]
    | #Xl.Num.unop ->
        [ (`NatT, NumT `NatT, NumT `NatT); (`IntT, NumT `IntT, NumT `IntT) ]
  in
  let fail =
    fail at
      (F.asprintf "unary operator `%s` is not defined for operand type %s"
         (El.Print.string_of_unop unop)
         (El.Print.string_of_plaintyp plaintyp))
  in
  List.fold_left
    (fun unop_infer (optyp_il, plaintyp_expect, plaintyp_res_expect) ->
      match unop_infer with
      | Ok _ -> unop_infer
      | _ -> (
          let exp_il_attempt =
            cast_exp ctx (plaintyp_expect $ plaintyp.at) plaintyp exp_il
          in
          match exp_il_attempt with
          | Ok exp_il -> Ok (optyp_il, exp_il, plaintyp_res_expect)
          | _ -> fail))
    fail unop_candidates

and infer_unop_exp (ctx : Ctx.t) (at : region) (unop : unop) (exp : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il, plaintyp = infer_exp ctx exp in
  let* optyp_il, exp_il, plaintyp_expect =
    infer_unop ctx at unop plaintyp exp_il
  in
  let exp_il = Il.UnE (unop, optyp_il, exp_il) in
  Ok (ctx, exp_il, plaintyp_expect)

(* Inference of binary expressions *)

and infer_binop (ctx : Ctx.t) (at : region) (binop : binop)
    (plaintyp_l : plaintyp) (exp_il_l : Il.exp) (plaintyp_r : plaintyp)
    (exp_il_r : Il.exp) : (Il.optyp * Il.exp * Il.exp * plaintyp') attempt =
  let binop_candidates =
    match binop with
    | #Xl.Bool.binop -> [ (`BoolT, BoolT, BoolT, BoolT) ]
    | `SubOp ->
        [
          (`IntT, NumT `NatT, NumT `NatT, NumT `IntT);
          (`IntT, NumT `IntT, NumT `IntT, NumT `IntT);
        ]
    | #Xl.Num.binop ->
        [
          (`NatT, NumT `NatT, NumT `NatT, NumT `NatT);
          (`IntT, NumT `IntT, NumT `IntT, NumT `IntT);
        ]
  in
  let fail =
    fail at
      (F.asprintf
         "binary operator `%s` is not defined for operand types %s and %s"
         (El.Print.string_of_binop binop)
         (El.Print.string_of_plaintyp plaintyp_l)
         (El.Print.string_of_plaintyp plaintyp_r))
  in
  List.fold_left
    (fun binop_infer
         (optyp_il, plaintyp_l_expect, plaintyp_r_expect, plaintyp_res_expect) ->
      match binop_infer with
      | Ok _ -> binop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (plaintyp_l_expect $ plaintyp_l.at) plaintyp_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (plaintyp_r_expect $ plaintyp_r.at) plaintyp_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r ->
              Ok (optyp_il, exp_il_l, exp_il_r, plaintyp_res_expect)
          | _ -> fail))
    fail binop_candidates

and infer_binop_exp (ctx : Ctx.t) (at : region) (binop : binop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il_l, plaintyp_l_infer = infer_exp ctx exp_l in
  let* ctx, exp_il_r, plaintyp_r_infer = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r, plaintyp_expect =
    infer_binop ctx at binop plaintyp_l_infer exp_il_l plaintyp_r_infer exp_il_r
  in
  let exp_il = Il.BinE (binop, optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, plaintyp_expect)

(* Inference of comparison expressions *)

and infer_cmpop_exp_bool (ctx : Ctx.t) (cmpop : Xl.Bool.cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_r, plaintyp_r = infer_exp ctx exp_r in
        let* ctx, exp_il_l = elab_exp ctx plaintyp_r exp_l in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, BoolT));
      (fun () ->
        let* ctx, exp_il_l, plaintyp_l = infer_exp ctx exp_l in
        let* ctx, exp_il_r = elab_exp ctx plaintyp_l exp_r in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, BoolT));
    ]

and infer_cmpop_num (ctx : Ctx.t) (at : region) (cmpop : Xl.Num.cmpop)
    (plaintyp_l : plaintyp) (exp_il_l : Il.exp) (plaintyp_r : plaintyp)
    (exp_il_r : Il.exp) : (Il.optyp * Il.exp * Il.exp) attempt =
  let cmpop_candidates =
    [ (`NatT, NumT `NatT, NumT `NatT); (`IntT, NumT `IntT, NumT `IntT) ]
  in
  let fail =
    fail at
      (F.asprintf
         "comparison operator `%s` is not defined for operand types %s and %s"
         (El.Print.string_of_cmpop (cmpop :> Il.cmpop))
         (El.Print.string_of_plaintyp plaintyp_l)
         (El.Print.string_of_plaintyp plaintyp_r))
  in
  List.fold_left
    (fun cmpop_infer (optyp_il, plaintyp_l_expect, plaintyp_r_expect) ->
      match cmpop_infer with
      | Ok _ -> cmpop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (plaintyp_l_expect $ plaintyp_l.at) plaintyp_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (plaintyp_r_expect $ plaintyp_r.at) plaintyp_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r -> Ok (optyp_il, exp_il_l, exp_il_r)
          | _ -> fail))
    fail cmpop_candidates

and infer_cmpop_exp_num (ctx : Ctx.t) (at : region) (cmpop : Xl.Num.cmpop)
    (exp_l : exp) (exp_r : exp) : (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il_l, plaintyp_l_infer = infer_exp ctx exp_l in
  let* ctx, exp_il_r, plaintyp_r_infer = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r =
    infer_cmpop_num ctx at cmpop plaintyp_l_infer exp_il_l plaintyp_r_infer
      exp_il_r
  in
  let exp_il = Il.CmpE ((cmpop :> Il.cmpop), optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, BoolT)

and infer_cmpop_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * plaintyp') attempt =
  match cmpop with
  | #Xl.Bool.cmpop as cmpop -> infer_cmpop_exp_bool ctx cmpop exp_l exp_r
  | #Xl.Num.cmpop as cmpop -> infer_cmpop_exp_num ctx at cmpop exp_l exp_r

(* Inference of arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of list expressions *)

and infer_list_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  match exps with
  | [] -> fail_infer at "empty list"
  | exp :: exps ->
      let* ctx, exp_il, plaintyp = infer_exp ctx exp in
      let* ctx, exps_il, plaintyps = infer_exps ctx exps in
      if List.for_all (Types.Equiv.equiv_plaintyp ctx.tdenv plaintyp) plaintyps
      then
        let exp_il = Il.ListE (exp_il :: exps_il) in
        let plaintyp = IterT (plaintyp, List) in
        Ok (ctx, exp_il, plaintyp)
      else fail_infer at "list with heterogeneous elements"

(* Inference of cons expressions *)

and infer_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il_h, plaintyp_h = infer_exp ctx exp_h in
  let plaintyp = IterT (plaintyp_h, List) in
  let* ctx, exp_il_t = elab_exp ctx (plaintyp $ plaintyp_h.at) exp_t in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il, plaintyp)

(* Inference of concatenation expressions *)

and infer_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_l, plaintyp_l = infer_exp ctx exp_l in
        let* plaintyp = as_list_plaintyp ctx plaintyp_l in
        let plaintyp = IterT (plaintyp, List) $ plaintyp.at in
        let* ctx, exp_il_r = elab_exp ctx plaintyp exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, plaintyp.it));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, TextT));
    ]

(* Inference of index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_b, plaintyp_b = infer_exp ctx exp_b in
        let* plaintyp = as_list_plaintyp ctx plaintyp_b in
        let* ctx, exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, plaintyp.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, TextT));
    ]

(* Inference of slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_b, plaintyp_b = infer_exp ctx exp_b in
        let* _ = as_list_plaintyp ctx plaintyp_b in
        let* ctx, exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
        let* ctx, exp_il_n = elab_exp ctx (NumT `NatT $ exp_n.at) exp_n in
        let exp_il = Il.SliceE (exp_il_b, exp_il_i, exp_il_n) in
        Ok (ctx, exp_il, plaintyp_b.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
        let* ctx, exp_il_n = elab_exp ctx (NumT `NatT $ exp_n.at) exp_n in
        let exp_il = Il.SliceE (exp_il_b, exp_il_i, exp_il_n) in
        Ok (ctx, exp_il, TextT));
    ]

(* Inference of member expressions *)

and infer_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il_e, plaintyp_e = infer_exp ctx exp_e in
        let* ctx, exp_il_s =
          elab_exp ctx (IterT (plaintyp_e, List) $ plaintyp_e.at) exp_s
        in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        let plaintyp = BoolT in
        Ok (ctx, exp_il, plaintyp));
      (fun () ->
        let* ctx, exp_il_s, plaintyp_s = infer_exp ctx exp_s in
        let* plaintyp_s = as_list_plaintyp ctx plaintyp_s in
        let* ctx, exp_il_e = elab_exp ctx plaintyp_s exp_e in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        let plaintyp = BoolT in
        Ok (ctx, exp_il, plaintyp));
    ]

(* Inference of dot expressions *)

and infer_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il, plaintyp = infer_exp ctx exp in
  let* typfields = as_struct_plaintyp ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail exp.at "cannot infer type of field"
  in
  let exp_il = Il.DotE (exp_il, atom) in
  Ok (ctx, exp_il, plaintyp.it)

(* Inference of update expressions *)

and infer_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* ctx, path_il, plaintyp_f = elab_path ctx plaintyp_b path in
  let* ctx, exp_il_f = elab_exp ctx plaintyp_f exp_f in
  let exp_il = Il.UpdE (exp_il_b, path_il, exp_il_f) in
  Ok (ctx, exp_il, plaintyp_b.it)

(* Inference of length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, exp_il, plaintyp = infer_exp ctx exp in
        let* _ = as_list_plaintyp ctx plaintyp in
        let exp_il = Il.LenE exp_il in
        let plaintyp = NumT `NatT in
        Ok (ctx, exp_il, plaintyp));
      (fun () ->
        let* ctx, exp_il = elab_exp ctx (TextT $ exp.at) exp in
        let exp_il = Il.LenE exp_il in
        let plaintyp = NumT `NatT in
        Ok (ctx, exp_il, plaintyp));
    ]

(* Inference of parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exps_il, plaintyps = infer_exps ctx exps in
  let exp_il = Il.TupleE exps_il in
  let plaintyp = TupleT plaintyps in
  Ok (ctx, exp_il, plaintyp)

(* Inference of call expressions *)

and infer_call_exp (ctx : Ctx.t) (at : region) (id : id) (targs : targ list)
    (args : arg list) : (Ctx.t * Il.exp' * plaintyp') attempt =
  let tparams, params, plaintyp = Ctx.find_func_signature ctx id in
  check
    (List.length targs = List.length tparams)
    id.at "type arguments do not match";
  let theta = List.combine tparams targs |> TIdMap.of_list in
  let params = Plaintyp.subst_params theta params in
  let plaintyp = Plaintyp.subst_plaintyp theta plaintyp in
  let targs_il = List.map (elab_plaintyp ctx) targs in
  let ctx, args_il = elab_args at ctx params args in
  let exp_il = Il.CallE (id, targs_il, args_il) in
  Ok (ctx, exp_il, plaintyp.it)

(* Inference of iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il, plaintyp = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.IterE (exp_il, (iter_il, [])) in
  let plaintyp = IterT (plaintyp, iter) in
  Ok (ctx, exp_il, plaintyp)

(* Inference of subtype expressions *)

and infer_sub_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Ctx.t * Il.exp' * plaintyp') attempt =
  let* ctx, exp_il, plaintyp_exp = infer_exp ctx exp in
  let typ_il = elab_plaintyp ctx plaintyp in
  if
    Types.Sub.sub_plaintyp ctx.tdenv plaintyp_exp plaintyp
    || Types.Sub.sub_plaintyp ctx.tdenv plaintyp plaintyp_exp
  then
    let exp_il = Il.SubE (exp_il, typ_il) in
    let plaintyp = BoolT in
    Ok (ctx, exp_il, plaintyp)
  else
    fail exp.at
      (F.asprintf "incomparable types %s and %s"
         (El.Print.string_of_plaintyp plaintyp_exp)
         (El.Print.string_of_plaintyp plaintyp))

(* Elaboration of expression:

   - If an iterated type is expected,
      - first try elaborating the expression as a singleton iteration,
        but except wildcard, epsilon, and empty list expressions
      - then try usual elaboration
   - Otherwise, directly try usual elaboration *)

and is_pure_exp (exp_il : Il.exp) : bool =
  match exp_il.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> true
  | UnE (_, _, exp_il) -> is_pure_exp exp_il
  | BinE (_, _, exp_l_il, exp_r_il) | CmpE (_, _, exp_l_il, exp_r_il) ->
      is_pure_exp exp_l_il && is_pure_exp exp_r_il
  | UpCastE (_, exp_il)
  | DownCastE (_, exp_il)
  | SubE (exp_il, _)
  | MatchE (exp_il, _) ->
      is_pure_exp exp_il
  | TupleE exps_il | CaseE (_, exps_il) -> List.for_all is_pure_exp exps_il
  | StrE expfields_il ->
      let exps_il = List.map snd expfields_il in
      List.for_all is_pure_exp exps_il
  | OptE (Some exp_il) -> is_pure_exp exp_il
  | OptE None -> true
  | ListE exps_il -> List.for_all is_pure_exp exps_il
  | ConsE (exp_h_il, exp_t_il) -> is_pure_exp exp_h_il && is_pure_exp exp_t_il
  | CatE (exp_l_il, exp_r_il) -> is_pure_exp exp_l_il && is_pure_exp exp_r_il
  | MemE (exp_e_il, exp_s_il) -> is_pure_exp exp_e_il && is_pure_exp exp_s_il
  | LenE exp_il | DotE (exp_il, _) -> is_pure_exp exp_il
  | IdxE (exp_b_il, exp_i_il) -> is_pure_exp exp_b_il && is_pure_exp exp_i_il
  | SliceE (exp_b_il, exp_i_il, exp_n_il) ->
      is_pure_exp exp_b_il && is_pure_exp exp_i_il && is_pure_exp exp_n_il
  | UpdE (exp_b_il, path_il, exp_f_il) ->
      is_pure_exp exp_b_il && is_pure_path path_il && is_pure_exp exp_f_il
  | CallE _ -> false
  | IterE (exp_il, _) -> is_pure_exp exp_il

and elab_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  elab_exp' ctx plaintyp_expect exp
  |> nest exp.at
       (F.asprintf "elaboration of expression %s as type %s failed"
          (El.Print.string_of_exp exp)
          (El.Print.string_of_plaintyp plaintyp_expect))

and elab_exp' (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  match as_iter_plaintyp ctx plaintyp_expect with
  | Ok (plaintyp_expect_base, iter_expect) ->
      choose_sequential
        [
          (fun () ->
            match exp.it with
            | VarE id when id.it = "_" -> fail_silent
            | EpsE | ListE [] -> fail_silent
            | _ ->
                elab_exp_iter ctx plaintyp_expect plaintyp_expect_base
                  iter_expect exp);
          (fun () -> elab_exp_normal ctx plaintyp_expect exp);
        ]
  | _ -> elab_exp_normal ctx plaintyp_expect exp

and elab_exps (ctx : Ctx.t) (plaintyps_expect : plaintyp list) (exps : exp list)
    : (Ctx.t * Il.exp list) attempt =
  match (plaintyps_expect, exps) with
  | [], [] -> Ok (ctx, [])
  | [], _ -> fail no_region "more expressions than expected"
  | _, [] -> fail no_region "more expected types than expressions"
  | plaintyp_expect :: plaintyps_expect, exp :: exps ->
      let* ctx, exp_il = elab_exp ctx plaintyp_expect exp in
      let* ctx, exps_il = elab_exps ctx plaintyps_expect exps in
      Ok (ctx, exp_il :: exps_il)

(* Elaboration of expression as a singleton iteration *)

and elab_exp_iter (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (plaintyp_expect_base : plaintyp) (iter_expect : iter) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp ctx plaintyp_expect_base exp in
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  match iter_expect with
  | Opt ->
      let exp_il = Il.OptE (Some exp_il) $$ (exp.at, typ_il.it) in
      Ok (ctx, exp_il)
  | List ->
      let exp_il = Il.ListE [ exp_il ] $$ (exp.at, typ_il.it) in
      Ok (ctx, exp_il)

(* Normal elaboration of expressions: a two-phase process,

   - if a type can be inferred without any contextual information,
     match the inferred type with the expected type
      - this may fail for some expressions that require contextual information,
        e.g., notation expressions or expression sequences
   - for such cases, try to elaborate the expression using the expected type *)

and fail_cast (at : region) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp) =
  let msg =
    F.asprintf "cannot cast %s to %s"
      (El.Print.string_of_plaintyp plaintyp_a)
      (El.Print.string_of_plaintyp plaintyp_b)
  in
  fail at msg

and cast_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (plaintyp_infer : plaintyp) (exp_il : Il.exp) : Il.exp attempt =
  if Types.Equiv.equiv_plaintyp ctx.tdenv plaintyp_expect plaintyp_infer then
    Ok exp_il
  else if Types.Sub.sub_plaintyp ctx.tdenv plaintyp_infer plaintyp_expect then
    let typ_il_expect = elab_plaintyp ctx plaintyp_expect in
    let exp_il =
      Il.UpCastE (typ_il_expect, exp_il) $$ (exp_il.at, typ_il_expect.it)
    in
    Ok exp_il
  else fail_cast exp_il.at plaintyp_infer plaintyp_expect

and elab_exp_normal (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (ctx, exp_il, plaintyp_infer) ->
      let* exp_il = cast_exp ctx plaintyp_expect plaintyp_infer exp_il in
      Ok (ctx, exp_il)
  | Fail _ -> (
      match exp.it with
      | VarE id when id.it = "_" -> elab_exp_wildcard ctx exp.at plaintyp_expect
      | _ -> (
          let kind = Plaintyp.kind_plaintyp ctx.tdenv plaintyp_expect in
          match kind with
          | Opaque -> elab_exp_plain ctx plaintyp_expect exp
          | Plain plaintyp -> elab_exp_plain ctx plaintyp exp
          | Struct typfields ->
              elab_exp_struct ctx plaintyp_expect typfields exp
          | Variant typcases ->
              elab_exp_variant ctx plaintyp_expect typcases exp))

(* Elaboration of wildcard variable expressions *)

and elab_exp_wildcard (ctx : Ctx.t) (at : region) (plaintyp_expect : plaintyp) :
    (Ctx.t * Il.exp) attempt =
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  let id_fresh, typ_fresh, iters_fresh =
    Il.Fresh.fresh_var_from_exp ~wildcard:true ctx.frees
      (Il.VarE ("_" $ at) $$ (at, typ_il.it))
  in
  let ctx = Ctx.add_free ctx id_fresh in
  let exp_il = Var.as_exp (id_fresh, typ_fresh, iters_fresh) in
  Ok (ctx, exp_il)

(* Elaboration of plain expressions *)

and fail_elab_plain (at : region) (msg : string) =
  fail at ("cannot elaborate expression because " ^ msg)

and elab_exp_plain (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp_plain' ctx exp.at plaintyp_expect exp.it in
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  let exp_il = exp_il $$ (exp.at, typ_il.it) in
  Ok (ctx, exp_il)

and elab_exp_plain' (ctx : Ctx.t) (at : region) (plaintyp_expect : plaintyp)
    (exp : exp') : (Ctx.t * Il.exp') attempt =
  match exp with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain at
        (F.asprintf "the type of %s should have been inferred"
           (El.Print.string_of_exp (exp $ at)))
  | EpsE -> elab_eps_exp ctx plaintyp_expect
  | ListE exps -> elab_list_exp ctx plaintyp_expect exps
  | ConsE (exp_h, exp_t) -> elab_cons_exp ctx plaintyp_expect exp_h exp_t
  | CatE (exp_l, exp_r) -> elab_cat_exp ctx plaintyp_expect exp_l exp_r
  | ParenE exp -> elab_paren_exp ctx plaintyp_expect exp
  | TupleE exps -> elab_tuple_exp ctx plaintyp_expect exps
  | IterE (exp, iter) -> elab_iter_exp ctx plaintyp_expect exp iter
  | _ ->
      fail at
        (F.asprintf "(TODO elab_exp_plain) %s"
           (El.Print.string_of_exp (exp $ at)))

(* Elaboration of episilon expressions *)

and elab_eps_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) :
    (Ctx.t * Il.exp') attempt =
  let* _plaintyp_expect, iter_expect = as_iter_plaintyp ctx plaintyp_expect in
  let exp_il =
    match iter_expect with Opt -> Il.OptE None | List -> Il.ListE []
  in
  Ok (ctx, exp_il)

(* Elaboration of list expressions *)

and elab_list_exp_elementwise (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (exps : exp list) : (Ctx.t * Il.exp list) attempt =
  match exps with
  | [] -> Ok (ctx, [])
  | exp :: exps ->
      let* ctx, exp_il = elab_exp ctx plaintyp_expect exp in
      let* ctx, exps_il = elab_list_exp_elementwise ctx plaintyp_expect exps in
      Ok (ctx, exp_il :: exps_il)

and elab_list_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list) :
    (Ctx.t * Il.exp') attempt =
  let* plaintyp_expect, iter_expect = as_iter_plaintyp ctx plaintyp_expect in
  match iter_expect with
  | Opt -> fail_elab_plain no_region "list expression with optional iteration"
  | List ->
      let* ctx, exps_il = elab_list_exp_elementwise ctx plaintyp_expect exps in
      let exp_il = Il.ListE exps_il in
      Ok (ctx, exp_il)

(* Elaboration of cons expressions *)

and elab_cons_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_h : exp)
    (exp_t : exp) : (Ctx.t * Il.exp') attempt =
  let* plaintyp_expect, iter_expect = as_iter_plaintyp ctx plaintyp_expect in
  let* ctx, exp_il_h = elab_exp ctx plaintyp_expect exp_h in
  let* ctx, exp_il_t =
    elab_exp ctx
      (IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at)
      exp_t
  in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il)

(* Elaboration of concatenation expressions *)

and elab_cat_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp') attempt =
  choose_sequential
    [
      (fun () ->
        let* plaintyp_expect, iter_expect =
          as_iter_plaintyp ctx plaintyp_expect
        in
        let plaintyp_expect =
          IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at
        in
        let* ctx, exp_il_l = elab_exp ctx plaintyp_expect exp_l in
        let* ctx, exp_il_r = elab_exp ctx plaintyp_expect exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
    ]

(* Elaboration of tuple expressions *)

and elab_tuple_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list)
    : (Ctx.t * Il.exp') attempt =
  let* plaintyps_expect = as_tuple_plaintyp ctx plaintyp_expect in
  let* ctx, exps_il = elab_exps ctx plaintyps_expect exps in
  let exp_il = Il.TupleE exps_il in
  Ok (ctx, exp_il)

(* Elaboration of parenthesized expressions *)

and elab_paren_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    (Ctx.t * Il.exp') attempt =
  let* ctx, exp_il = elab_exp ctx plaintyp_expect exp in
  Ok (ctx, exp_il.it)

(* Elaboration of iterated expressions *)

and elab_iter_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp)
    (iter : iter) : (Ctx.t * Il.exp') attempt =
  let* plaintyp_expect, iter_expect = as_iter_plaintyp ctx plaintyp_expect in
  if iter <> iter_expect then fail_elab_plain exp.at "iteration mismatch"
  else
    let* ctx, exp_il = elab_exp ctx plaintyp_expect exp in
    let iter_il_expect = elab_iter iter_expect in
    let exp_il = Il.IterE (exp_il, (iter_il_expect, [])) in
    Ok (ctx, exp_il)

(* Elaboration of notation expressions *)

and fail_elab_not (at : region) (msg : string) : (Ctx.t * Il.notexp) attempt =
  fail at ("cannot elaborate notation expression because " ^ msg)

and elab_exp_not (ctx : Ctx.t) (typ : typ) (exp : exp) :
    (Ctx.t * Il.notexp) attempt =
  match typ with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let* ctx, exp_il = elab_exp ctx plaintyp exp in
      let notexp_il = (mixop, [ exp_il ]) in
      Ok (ctx, notexp_il)
  | NotationT nottyp -> (
      match (nottyp.it, exp.it) with
      | _, ParenE exp -> elab_exp_not ctx typ exp
      | AtomT atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
          fail_elab_not exp.at "atom does not match"
      | AtomT atom_t, AtomE _ ->
          let mixop = [ [ atom_t ] ] in
          let notexp_il = (mixop, []) in
          Ok (ctx, notexp_il)
      | SeqT [], SeqE [] ->
          let mixop = [ [] ] in
          let exps_il = [] in
          let notexp_il = (mixop, exps_il) in
          Ok (ctx, notexp_il)
      | SeqT (typ :: typs), SeqE (exp :: exps) ->
          let* ctx, (mixop_h, exps_il_h) = elab_exp_not ctx typ exp in
          let* ctx, (mixop_t, exps_il_t) =
            elab_exp_not ctx
              (NotationT (SeqT typs $ nottyp.at))
              (SeqE exps $ exp.at)
          in
          let mixop = Mixop.merge mixop_h mixop_t in
          let exps_il = exps_il_h @ exps_il_t in
          let notexp_il = (mixop, exps_il) in
          Ok (ctx, notexp_il)
      | SeqT (_ :: _), SeqE [] -> fail_elab_not exp.at "omitted sequence tail"
      | SeqT [], SeqE (_ :: _) -> fail_elab_not exp.at "expression is not empty"
      | InfixT (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it
        ->
          fail_elab_not exp.at "atoms do not match"
      | InfixT (typ_l, atom_t, typ_r), InfixE (exp_l, _, exp_r) ->
          let* ctx, (mixop_l, exps_il_l) = elab_exp_not ctx typ_l exp_l in
          let* ctx, (mixop_r, exps_il_r) = elab_exp_not ctx typ_r exp_r in
          let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
          let mixop = Mixop.merge mixop_l mixop_r in
          let exps_il = exps_il_l @ exps_il_r in
          let notexp_il = (mixop, exps_il) in
          Ok (ctx, notexp_il)
      | BrackT (atom_t_l, _, atom_t_r), BrackE (atom_e_l, _, atom_e_r)
        when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
          fail_elab_not exp.at "atoms do not match"
      | BrackT (_, typ, _), BrackE (atom_e_l, exp, atom_e_r) ->
          let* ctx, (mixop, exps_il) = elab_exp_not ctx typ exp in
          let mixop_l = Mixop.merge [ [ atom_e_l ] ] mixop in
          let mixop = Mixop.merge mixop_l [ [ atom_e_r ] ] in
          let notexp_il = (mixop, exps_il) in
          Ok (ctx, notexp_il)
      | _ -> fail_elab_not exp.at "expression does not match notation")

(* Elaboration of struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  fail at ("cannot elaborate struct expression because " ^ msg)

and elab_expfields (ctx : Ctx.t) (at : region)
    (typfields : (atom * plaintyp) list) (expfields : (atom * exp) list) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  match (typfields, expfields) with
  | [], [] -> Ok (ctx, [])
  | [], (atom_e, _) :: _ ->
      fail_elab_struct atom_e.at "expression has extra fields"
  | _ :: _, [] -> fail_elab_struct at "expression omitted struct fields"
  | (atom_t, _) :: _, (atom_e, _) :: _ when atom_t.it <> atom_e.it ->
      fail_elab_struct atom_e.at "atom does not match"
  | (atom_t, plaintyp) :: typfields, (_, exp) :: expfields ->
      let* ctx, exp_il = elab_exp ctx plaintyp exp in
      let* ctx, expfields_il = elab_expfields ctx at typfields expfields in
      Ok (ctx, (atom_t, exp_il) :: expfields_il)

and elab_exp_struct (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (typfields : typfield list) (exp : exp) : (Ctx.t * Il.exp) attempt =
  let* ctx, expfields_il = elab_exp_struct' ctx typfields exp in
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  let exp_il = Il.StrE expfields_il $$ (exp.at, typ_il.it) in
  Ok (ctx, exp_il)

and elab_exp_struct' (ctx : Ctx.t) (typfields : typfield list) (exp : exp) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  let typfields =
    List.map (fun (atom, plaintyp, _) -> (atom, plaintyp)) typfields
  in
  match exp.it with
  | StrE expfields -> elab_expfields ctx exp.at typfields expfields
  | _ -> fail_elab_struct exp.at "expression is not a struct"

(* Elaboration of variant expressions

   This finds a single case that matches the expression,
   where it has the smallest possible type, according to the variant type subtyping rule

   Finding the smallest type is important because the interpreter needs to
   propagate the type information while evaluating expressions,
   and it has to perform runtime type checks of whether a value is a subtype of some particular type *)

and fail_elab_variant (at : region) (msg : string) : (Ctx.t * Il.exp) attempt =
  fail at ("cannot elaborate variant case because " ^ msg)

and elab_exp_variant (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (typcases : ((nottyp * hint list) * plaintyp) list) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let ctx, exps_il =
    List.fold_left
      (fun (ctx, exps_il) ((nottyp, _), plaintyp) ->
        elab_exp_not ctx (NotationT nottyp) exp |> function
        | Ok (ctx, ((mixops, notexp_exps) as notexp_il)) ->
            let exp_il =
              let typ_il = elab_plaintyp ctx plaintyp in
              let at =
                (* if mixops is empty, compute the region with the terminals that make up the variant *)
                if List.flatten mixops = [] then exp_list_region notexp_exps
                else List.concat_map (List.map at) mixops |> over_region
              in
              Il.CaseE notexp_il $$ (at, typ_il.it)
            in
            let+ exp_il = cast_exp ctx plaintyp_expect plaintyp exp_il in
            (ctx, exps_il @ [ exp_il ])
        | Fail _ -> (ctx, exps_il))
      (ctx, []) typcases
  in
  match exps_il with
  | [ exp_il ] -> Ok (ctx, exp_il)
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Elaboration of paths *)

and is_pure_path (path_il : Il.path) : bool =
  match path_il.it with
  | RootP -> true
  | IdxP (path_il, exp_il) -> is_pure_path path_il && is_pure_exp exp_il
  | SliceP (path_il, exp_il_i, exp_il_n) ->
      is_pure_path path_il && is_pure_exp exp_il_i && is_pure_exp exp_il_n
  | DotP (path_il, _) -> is_pure_path path_il

and elab_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path) :
    (Ctx.t * Il.path * plaintyp) attempt =
  let* ctx, path_il, plaintyp = elab_path' ctx plaintyp_expect path.it in
  let plaintyp = plaintyp $ plaintyp_expect.at in
  let typ_il = elab_plaintyp ctx plaintyp in
  let path_il = path_il $$ (path.at, typ_il.it) in
  Ok (ctx, path_il, plaintyp)

and elab_path' (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path') :
    (Ctx.t * Il.path' * plaintyp') attempt =
  match path with
  | RootP -> elab_root_path ctx plaintyp_expect
  | IdxP (path, exp) -> elab_idx_path ctx plaintyp_expect path exp
  | SliceP (path, exp_l, exp_h) ->
      elab_slice_path ctx plaintyp_expect path exp_l exp_h
  | DotP (path, atom) -> elab_dot_path ctx plaintyp_expect path atom

(* Elaboration of root paths *)

and elab_root_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) :
    (Ctx.t * Il.path' * plaintyp') attempt =
  Ok (ctx, Il.RootP, plaintyp_expect.it)

(* Elaboration of index paths *)

and elab_idx_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp : exp) : (Ctx.t * Il.path' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, path_il, plaintyp = elab_path ctx plaintyp_expect path in
        let* ctx, exp_il = elab_exp ctx (NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* plaintyp = as_list_plaintyp ctx plaintyp in
        Ok (ctx, path_il, plaintyp.it));
      (fun () ->
        let* ctx, path_il, plaintyp = elab_path ctx plaintyp_expect path in
        let* ctx, exp_il = elab_exp ctx (NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* _ = as_text_plaintyp ctx plaintyp in
        Ok (ctx, path_il, plaintyp.it));
    ]

(* Elaboration of slice paths *)

and elab_slice_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp_l : exp) (exp_h : exp) : (Ctx.t * Il.path' * plaintyp') attempt =
  choose_sequential
    [
      (fun () ->
        let* ctx, path_il, plaintyp = elab_path ctx plaintyp_expect path in
        let* ctx, exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_list_plaintyp ctx plaintyp in
        Ok (ctx, path_il, plaintyp.it));
      (fun () ->
        let* ctx, path_il, plaintyp = elab_path ctx plaintyp_expect path in
        let* ctx, exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_text_plaintyp ctx plaintyp in
        Ok (ctx, path_il, plaintyp.it));
    ]

(* Elaboration of dot paths *)

and elab_dot_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (atom : atom) : (Ctx.t * Il.path' * plaintyp') attempt =
  let* ctx, path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* typfields = as_struct_plaintyp ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail atom.at "cannot infer type of field"
  in
  let path_il = Il.DotP (path_il, atom) in
  Ok (ctx, path_il, plaintyp.it)

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

and elab_arg ?(as_def = false) (ctx : Ctx.t) (param : param) (arg : arg) :
    Ctx.t * Il.arg =
  match (param.it, arg.it) with
  | ExpP plaintyp, ExpA exp ->
      let+ ctx, exp_il = elab_exp ctx plaintyp exp in
      let arg_il = Il.ExpA exp_il $ arg.at in
      (ctx, arg_il)
  | DefP (id_p, tparams_p, params_p, plaintyp_p), DefA id_a when as_def ->
      check (id_p.it = id_a.it) arg.at
        (F.asprintf
           "function argument does not match the declared function parameter %s"
           (Id.to_string id_p));
      let ctx =
        Ctx.add_defined_func_dec ctx id_p tparams_p params_p plaintyp_p
      in
      let arg_il = Il.DefA id_a $ arg.at in
      (ctx, arg_il)
  | DefP (id_p, tparams_p, params_p, plaintyp_p), DefA id_a ->
      let tparams_a, params_a, plaintyp_a = Ctx.find_func_signature ctx id_a in
      check
        (Types.Equiv.equiv_functyp ctx.tdenv arg.at tparams_p params_p
           plaintyp_p tparams_a params_a plaintyp_a)
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
    (params : param list) (args : arg list) : Ctx.t * Il.arg list =
  check (List.length args = List.length params) at "arguments do not match";
  List.fold_left2
    (fun (ctx, args_il) param arg ->
      let ctx, arg_il = elab_arg ~as_def ctx param arg in
      (ctx, args_il @ [ arg_il ]))
    (ctx, []) params args

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

let rec is_pure_prem (prem_il : Il.prem) : bool =
  match prem_il.it with
  | RulePr _ | IfPr _ | IfHoldPr _ | IfNotHoldPr _ -> false
  | LetPr (_, exp_r_il) -> is_pure_exp exp_r_il
  | IterPr (prem_il, _) -> is_pure_prem prem_il
  | DebugPr exp_il -> is_pure_exp exp_il

let is_pure_prem_internal (prem_internal : prem_internal) : bool =
  match prem_internal.it with
  | SomePr prem_il -> is_pure_prem (prem_il $ prem_internal.at)
  | VarPr -> true
  | ElsePr -> false

let check_prems_internal (at : region) (prems_internal : prem_internal list) :
    unit =
  let prems_non_else_internal =
    prems_internal
    |> List.filter (fun prem_internal ->
           not (is_else_prem_internal prem_internal))
  in
  if List.length prems_internal = List.length prems_non_else_internal then ()
  else if List.length prems_internal = List.length prems_non_else_internal + 1
  then
    check
      (List.for_all is_pure_prem_internal prems_non_else_internal)
      at "cannot have non-pure premises alongside an otherwise premise"
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

and elab_prem_with_bind (ctx : Ctx.t) (prem : prem) : Ctx.t * prem_internal list
    =
  let ctx, prem_internal = elab_prem ctx prem in
  match prem_internal.it with
  | SomePr prem_il ->
      let ctx, prem_il, sideconditions_il =
        Dataflow.Analysis.analyze_prem ctx (prem_il $ prem_internal.at)
      in
      let prems_il = prem_il :: sideconditions_il in
      let prems_internal =
        List.map (fun prem_il -> SomePr prem_il.it $ prem_il.at) prems_il
      in
      (ctx, prems_internal)
  | VarPr -> (ctx, [ prem_internal ])
  | ElsePr -> (ctx, [ prem_internal ])

and elab_prems_with_bind (ctx : Ctx.t) (prems : prem list) :
    Ctx.t * prem_internal list =
  List.fold_left
    (fun (ctx, prems_internal_acc) prem ->
      let ctx, prems_internal = elab_prem_with_bind ctx prem in
      (ctx, prems_internal_acc @ prems_internal))
    (ctx, []) prems

and elab_prem_il_with_bind (ctx : Ctx.t) (prem_il : Il.prem) :
    Ctx.t * Il.prem list =
  let ctx, prem_il, sideconditions_il =
    Dataflow.Analysis.analyze_prem ctx prem_il
  in
  let prems_il = prem_il :: sideconditions_il in
  (ctx, prems_il)

and elab_prems_il_with_bind (ctx : Ctx.t) (prems_il : Il.prem list) :
    Ctx.t * Il.prem list =
  List.fold_left
    (fun (ctx, prems_il_analyzed) prem_il ->
      let ctx, prems_il = elab_prem_il_with_bind ctx prem_il in
      (ctx, prems_il_analyzed @ prems_il))
    (ctx, []) prems_il

(* Elaboration of variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Elaboration of rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let nottyp, _, inputs = Ctx.find_rel_signature ctx id in
  let+ ctx, notexp_il = elab_exp_not ctx (NotationT nottyp) exp in
  let _, exps_il = notexp_il in
  if Hints.Input.is_conditional inputs exps_il then
    let prem_il = Il.IfHoldPr (id, notexp_il) in
    (ctx, prem_il)
  else
    let prem_il = Il.RulePr (id, notexp_il, inputs) in
    (ctx, prem_il)

(* Elaboration of negated rule premises *)

and elab_rule_not_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let nottyp, _, inputs = Ctx.find_rel_signature ctx id in
  let+ ctx, notexp_il = elab_exp_not ctx (NotationT nottyp) exp in
  let _, exps_il = notexp_il in
  check
    (Hints.Input.is_conditional inputs exps_il)
    exp.at "negated rule premises do not take inputs";
  let prem_il = Il.IfNotHoldPr (id, notexp_il) in
  (ctx, prem_il)

(* Elaboration of if premises *)

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.prem' =
  let+ ctx, exp_il = elab_exp ctx (BoolT $ exp.at) exp in
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

type rulepath_internal = SomePath of Il.rulepath | ElsePath of Il.rulepath

type rulepaths_internal =
  | SomePaths of Il.rulepath list
  | ElsePaths of Il.rulepath

type rulegroup_internal =
  | SomeGroup of Il.rulegroup
  | ElseGroup of Il.elsegroup

let is_else_rulepath_internal (rulepath_internal : rulepath_internal) : bool =
  match rulepath_internal with ElsePath _ -> true | SomePath _ -> false

let elab_rule_input_with_bind (ctx : Ctx.t) (exps_il : Il.exp list) :
    Ctx.t * Il.exp list * Il.prem list =
  Dataflow.Analysis.analyze_exps_as_bind ctx exps_il

let elab_rule_signature (ctx : Ctx.t) (exps_il : Il.exp list) : Il.exp list =
  Dataflow.Analysis.analyze_exps_as_bound ctx exps_il

let elab_rule_output_with_bind (ctx : Ctx.t) (exps_il : Il.exp list) :
    Il.exp list =
  Dataflow.Analysis.analyze_exps_as_bound ctx exps_il

let rec elab_rulematch (ctx : Ctx.t) (ctxs_local : Ctx.t list)
    (exps_il_input_group : Il.exp list list) :
    Ctx.t list * Il.rulematch * Il.prem list list =
  let ctx_local_unified =
    let ctx_local_unified = { ctx with frees = IdSet.empty } in
    let frees =
      ctxs_local
      |> List.map (fun (ctx_local : Ctx.t) -> ctx_local.frees)
      |> List.fold_left IdSet.union IdSet.empty
    in
    Ctx.add_frees ctx_local_unified frees
  in
  let ctx_local_unified, exps_il_input_unified, prems_il_unified_group =
    Antiunify.antiunify ctx_local_unified exps_il_input_group
  in
  let ctx_local_unified, exps_il_input_unified_match, prems_il_match =
    elab_rule_input_with_bind ctx_local_unified exps_il_input_unified
  in
  let exps_il_unified_signature =
    elab_rule_signature ctx_local_unified exps_il_input_unified
  in
  let ctxs_local =
    List.map
      (fun (ctx_local : Ctx.t) ->
        {
          ctx_local with
          frees = ctx_local_unified.frees;
          venv = ctx_local_unified.venv;
        })
      ctxs_local
  in
  let ctxs_local, prems_il_unified_group =
    List.map2
      (fun ctx_local prems_il_unified ->
        let ctx_local, prems_il_unified =
          elab_prems_il_with_bind ctx_local prems_il_unified
        in
        (ctx_local, prems_il_unified))
      ctxs_local prems_il_unified_group
    |> List.split
  in
  let rulematch_il =
    (exps_il_unified_signature, exps_il_input_unified_match, prems_il_match)
  in
  (ctxs_local, rulematch_il, prems_il_unified_group)

and elab_rulepath (ctx_local : Ctx.t) (id_rule : id)
    (prems_il_unified : Il.prem list) (prems : prem list)
    (exps_il_output : Il.exp list) : rulepath_internal =
  let prems_internal_unified = List.map internalize_prem prems_il_unified in
  let ctx_local, prems_internal = elab_prems_with_bind ctx_local prems in
  let prems_internal = prems_internal_unified @ prems_internal in
  check_prems_internal id_rule.at prems_internal;
  let is_else_path = List.exists is_else_prem_internal prems_internal in
  let prems_il = List.filter_map externalize_prem prems_internal in
  let exps_il_output = elab_rule_output_with_bind ctx_local exps_il_output in
  let rulepath_il = (id_rule, prems_il, exps_il_output) in
  if is_else_path then ElsePath rulepath_il else SomePath rulepath_il

and elab_rulepaths (at : region) (ctxs_local : Ctx.t list)
    (id_rule_group : id list) (prems_il_unified_group : Il.prem list list)
    (prems_group : prem list list) (exps_il_output_group : Il.exp list list) :
    rulepaths_internal =
  let rulepaths_internal =
    ctxs_local |> List.map elab_rulepath
    |> List.map2
         (fun id_rule elab_rulepath -> elab_rulepath id_rule)
         id_rule_group
    |> List.map2
         (fun prems_il_unified elab_rulepath -> elab_rulepath prems_il_unified)
         prems_il_unified_group
    |> List.map2 (fun prems elab_rulepath -> elab_rulepath prems) prems_group
    |> List.map2
         (fun exps_il_output elab_rulepath -> elab_rulepath exps_il_output)
         exps_il_output_group
  in
  let rulepaths_else_internal =
    rulepaths_internal |> List.filter is_else_rulepath_internal
  in
  match rulepaths_else_internal with
  | [] ->
      let rulepaths_il =
        rulepaths_internal
        |> List.map (function
             | SomePath rulepath_il -> rulepath_il
             | _ -> assert false)
      in
      SomePaths rulepaths_il
  | [ ElsePath rulepath_il_else ] ->
      check
        (List.length rulepaths_internal = 1)
        at "cannot have other rule paths alongside an otherwise rule path";
      ElsePaths rulepath_il_else
  | _ -> error at "cannot use multiple otherwise rule paths in a rule group"

and elab_rulegroup (ctx : Ctx.t) (at : region) (id_rel : id) (id_rulegroup : id)
    (rules : rule list) : rulegroup_internal =
  let nottyp, _, inputs, _, _ = Ctx.find_defined_rel ctx id_rel in
  let ctxs_local =
    List.map
      (fun rule ->
        let ctx_local = { ctx with frees = IdSet.empty } in
        El.Free.free_rule rule |> Ctx.add_frees ctx_local)
      rules
  in
  let id_rule_group, exp_group, prems_group =
    List.fold_left
      (fun (id_rule_group, exp_group, prems_group) rule ->
        let id_rel_, id_rule, exp, prems = rule.it in
        check (Id.eq id_rel id_rel_) id_rule.at
          "rule group identifier does not match relation identifier";
        let id_rule_group = id_rule_group @ [ id_rule ] in
        let exp_group = exp_group @ [ exp ] in
        let prems_group = prems_group @ [ prems ] in
        (id_rule_group, exp_group, prems_group))
      ([], [], []) rules
  in
  let ctxs_local, notexps_il =
    List.map2
      (fun ctx_local exp ->
        let+ ctx_local, notexp_il =
          elab_exp_not ctx_local (NotationT nottyp) exp
        in
        (ctx_local, notexp_il))
      ctxs_local exp_group
    |> List.split
  in
  let exps_il_input_group, exps_il_output_group =
    List.map
      (fun notexp_il ->
        let _, exps_il = notexp_il in
        Hints.Input.split inputs exps_il)
      notexps_il
    |> List.split
  in
  let ctxs_local, rulematch_il, prems_il_unified_group =
    elab_rulematch ctx ctxs_local exps_il_input_group
  in
  let rulepaths_internal =
    elab_rulepaths at ctxs_local id_rule_group prems_il_unified_group
      prems_group exps_il_output_group
  in
  match rulepaths_internal with
  | SomePaths rulepaths_il ->
      let rulegroup_il = (id_rulegroup, rulematch_il, rulepaths_il) $ at in
      SomeGroup rulegroup_il
  | ElsePaths rulepath_il ->
      let elsegroup_il = (id_rulegroup, rulematch_il, rulepath_il) $ at in
      ElseGroup elsegroup_il

(* Elaboration of clauses *)

type clause_internal = SomeClause of Il.clause | ElseClause of Il.clause

let elab_clause_input_with_bind (ctx : Ctx.t) (at : region)
    (params : param list) (args : arg list) : Ctx.t * Il.arg list * Il.prem list
    =
  let ctx, args_il = elab_args ~as_def:true at ctx params args in
  let ctx, args_il, sideconditions_il =
    Dataflow.Analysis.analyze_args_as_bind ctx args_il
  in
  (ctx, args_il, sideconditions_il)

let elab_clause_output_with_bind (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp)
    : Ctx.t * Il.exp =
  let+ ctx, exp_il = elab_exp ctx plaintyp exp in
  let exp_il = Dataflow.Analysis.analyze_exp_as_bound ctx exp_il in
  (ctx, exp_il)

let elab_clause (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : clause_internal =
  let tparams_expected, params, plaintyp, _, _ = Ctx.find_defined_func ctx id in
  check
    (List.length tparams = List.length tparams_expected
    && List.for_all2 ( = ) (List.map it tparams) (List.map it tparams_expected)
    )
    id.at "type arguments do not match";
  check (List.length params = List.length args) at "arguments do not match";
  let ctx_local = { ctx with frees = IdSet.empty } in
  let ctx_local =
    let def = FuncDefD (id, tparams, args, exp, prems) $ at in
    El.Free.free_id_def def |> Ctx.add_frees ctx_local
  in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let ctx_local, args_il, sideconditions_il =
    elab_clause_input_with_bind ctx_local at params args
  in
  let ctx_local, prems_internal = elab_prems_with_bind ctx_local prems in
  let sideconditions_internal = List.map internalize_prem sideconditions_il in
  let prems_internal = sideconditions_internal @ prems_internal in
  check_prems_internal at prems_internal;
  let is_else_clause = List.exists is_else_prem_internal prems_internal in
  let prems_il = List.filter_map externalize_prem prems_internal in
  let _ctx_local, exp_il =
    elab_clause_output_with_bind ctx_local plaintyp exp
  in
  let clause_il = (args_il, exp_il, prems_il) $ at in
  if is_else_clause then ElseClause clause_il else SomeClause clause_il

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
  | VarD (id, plaintyp, _hints) -> elab_var_def ctx id plaintyp |> wrap_none
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
  let plaintyp = VarT (id, []) $ id.at in
  let ctx = Ctx.add_metavar ctx id plaintyp in
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
        let plaintyp = VarT (id, []) $ id.at in
        Ctx.add_metavar ctx id plaintyp
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
          let plaintyp = VarT (id, []) $ id.at in
          Ctx.add_metavar ctx id plaintyp
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

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Elaboration of relations *)

and fetch_rel_input_hint (at : region) (nottyp_il : Il.nottyp)
    (hints : hint list) : int list =
  let len = nottyp_il.it |> snd |> List.length in
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
  let ctx = Ctx.add_extern_rel ctx id nottyp nottyp_il inputs in
  let def_il = Il.ExternRelD (id, nottyp_il, inputs, hints) $ at in
  (ctx, def_il)

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.def =
  let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
  let inputs = fetch_rel_input_hint at nottyp_il hints in
  let ctx = Ctx.add_defined_rel ctx id nottyp nottyp_il inputs in
  let def_il = Il.RelD (id, nottyp_il, inputs, [], None, hints) $ at in
  (ctx, def_il)

(* Elaboration of rule groups *)

and elab_rulegroup_def (ctx : Ctx.t) (at : region) (id_rel : id)
    (id_rulegroup : id) (rules : rule list) : Ctx.t =
  let rulegroup_internal = elab_rulegroup ctx at id_rel id_rulegroup rules in
  match rulegroup_internal with
  | SomeGroup rulegroup_il -> Ctx.add_defined_rulegroup ctx id_rel rulegroup_il
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
  let ctx = Ctx.add_extern_func_dec ctx id tparams params plaintyp in
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
  let ctx = Ctx.add_builtin_func_dec ctx id tparams params plaintyp in
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
  check (typ_il.it = BoolT) typ_il.at "table must return a boolean type";
  let ctx = Ctx.add_table_func_dec ctx id params plaintyp in
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
  let ctx = Ctx.add_defined_func_dec ctx id tparams params plaintyp in
  (ctx, def_il)

(* Elaboration of table function definitions *)

and elab_tablerow_input_with_bind (ctx : Ctx.t) (args_il : Il.arg list) :
    Ctx.t * Il.arg list * Il.prem list =
  Dataflow.Analysis.analyze_args_as_bind_shallow ctx args_il

and elab_tablerow_signature (ctx : Ctx.t) (args_il : Il.arg list) : Il.arg list
    =
  Dataflow.Analysis.analyze_args_as_bound_shallow ctx args_il

and elab_tablerow_output_with_bind (ctx : Ctx.t) (plaintyp : plaintyp)
    (exp : exp) : Ctx.t * Il.exp =
  let+ ctx, exp_il = elab_exp ctx plaintyp exp in
  let exp_il = Dataflow.Analysis.analyze_exp_as_bound ctx exp_il in
  (ctx, exp_il)

and elab_tablerow (ctx : Ctx.t) (at : region) (id : id) (params : param list)
    (plaintyp : plaintyp) (tablerow : tablerow) : Il.tablerow =
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
  let ctx_local, args_il = elab_args ~as_def:true at ctx_local params args in
  let ctx_local, args_il_input, sideconditions_il =
    elab_tablerow_input_with_bind ctx_local args_il
  in
  let args_il_signature = elab_tablerow_signature ctx_local args_il in
  let exps_il_signature =
    List.map
      (fun arg_il ->
        match arg_il.it with Il.ExpA exp_il -> exp_il | _ -> assert false)
      args_il_signature
  in
  let _ctx_local, exp_il =
    elab_tablerow_output_with_bind ctx_local plaintyp exp_body
  in
  let tablerow_il =
    (exps_il_signature, args_il_input, exp_il, sideconditions_il) $ tablerow.at
  in
  tablerow_il

and pattern_set_covered_by_typ (ctx : Ctx.t) (typ_il : Il.typ) :
    Pattern.PatternSet.t =
  match typ_il.it with
  | VarT (vid, _) -> (
      match Ctx.find_typdef_opt ctx vid with
      | Some (Defined (_, `Variant typcases)) ->
          let nottyps = typcases |> List.split |> fst |> List.split |> fst in
          let nottyps_il =
            List.map
              (fun nottyp -> elab_nottyp ctx (El.NotationT nottyp))
              nottyps
          in
          nottyps_il |> Pattern.PatternSet.of_list
      | _ ->
          error typ_il.at
            ("unknown variant type id: " ^ Il.Print.string_of_typid vid))
  | _ -> error typ_il.at "expected variable type"

and pattern_set_covered_by_exp (ctx : Ctx.t) (exp_il : Il.exp) :
    Pattern.PatternSet.t =
  match exp_il.it with
  | VarE _ -> pattern_set_covered_by_typ ctx (exp_il.note $ exp_il.at)
  | UpCastE (_, { it = VarE _; note; at }) ->
      pattern_set_covered_by_typ ctx (note $ at)
  | UpCastE (_, { it = CaseE notexp_il; at; _ }) ->
      let mixop, exps_il = notexp_il in
      [ (mixop, List.map (fun exp_il -> exp_il.note $ exp_il.at) exps_il) $ at ]
      |> Pattern.PatternSet.of_list
  | _ -> assert false

and check_valid_match_tablerows (ctx : Ctx.t) (at : region)
    (typs_il_match : Il.typ list) (tablerows_il : Il.tablerow list) : unit =
  (* Split the last wildcard row (a "closer") if it exists *)
  let split_last_wildcard_tablerows tablerows_il =
    let rec split_last_wildcard_tablerows' tablerows_il_rev = function
      | [] -> (None, tablerows_il)
      | [ tablerow_il ] ->
          let exps_il_signature, _, _, _ = tablerow_il.it in
          if
            List.for_all
              (fun exp_il_signature ->
                match exp_il_signature.it with
                | Il.VarE id when Id.is_underscored id -> true
                | _ -> false)
              exps_il_signature
          then (Some tablerow_il, List.rev tablerows_il_rev)
          else (None, tablerows_il)
      | tablerow_il_h :: tablerows_il_t ->
          split_last_wildcard_tablerows'
            (tablerow_il_h :: tablerows_il_rev)
            tablerows_il_t
    in
    split_last_wildcard_tablerows' [] tablerows_il
  in
  let closer_opt, tablerows_il = split_last_wildcard_tablerows tablerows_il in
  (* Check that table rows have exclusive patterns *)
  let pattern_sets_tablerows =
    List.map
      (fun tablerow_il ->
        let exps_il_signature, _, _, _ = tablerow_il.it in
        List.map (pattern_set_covered_by_exp ctx) exps_il_signature)
      tablerows_il
  in
  let pattern_set_overlap_opt = Pattern.find_overlap pattern_sets_tablerows in
  check
    (Option.is_none pattern_set_overlap_opt)
    at
    (Format.asprintf "table rows have overlapping patterns: %s"
       (match pattern_set_overlap_opt with
       | Some (pattern_sets_l, pattern_sets_r) ->
           Pattern.PatternSets.to_string pattern_sets_l
           ^ " and "
           ^ Pattern.PatternSets.to_string pattern_sets_r
       | None -> ""));
  (* Check that table rows are exhaustive *)
  let pattern_sets_total =
    List.map (pattern_set_covered_by_typ ctx) typs_il_match
  in
  let pattern_sets_group_missing =
    Pattern.find_missing pattern_sets_total pattern_sets_tablerows
  in
  check
    (Option.is_some closer_opt || pattern_sets_group_missing = [])
    at
    (Format.asprintf "table rows are missing patterns: %s"
       (String.concat ", "
          (List.map Pattern.PatternSets.to_string pattern_sets_group_missing)))

and elab_tablerows (ctx : Ctx.t) (at : region) (id : id) (params : param list)
    (plaintyp : plaintyp) (tablerows : tablerow list) : Il.tablerow list =
  let tablerows_il =
    List.map (elab_tablerow ctx at id params plaintyp) tablerows
  in
  let typs_il_match =
    params
    |> List.map (elab_param ctx)
    |> List.map (fun param_il ->
           match param_il.it with Il.ExpP typ_il -> typ_il | _ -> assert false)
  in
  check_valid_match_tablerows ctx at typs_il_match tablerows_il;
  tablerows_il

and elab_table_def_def (ctx : Ctx.t) (at : region) (id : id)
    (tablerows : tablerow list) : Ctx.t =
  let params, plaintyp, _ = Ctx.find_table_func ctx id in
  let tablerows_il = elab_tablerows ctx at id params plaintyp tablerows in
  Ctx.add_table_func_tablerows ctx id tablerows_il

(* Elaboration of function definitions *)

and elab_func_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let clause_internal = elab_clause ctx at id tparams args exp prems in
  match clause_internal with
  | SomeClause clause_il -> Ctx.add_defined_func_clause ctx id clause_il
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
      let _, _, _, rulegroups_il, elsegroup_il_opt =
        Ctx.find_defined_rel ctx id
      in
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
