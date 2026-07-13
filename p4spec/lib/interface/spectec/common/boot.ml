module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
open Lang.Xl
open Mixops
open Typs
open Util.Source

module Il_typs = Il_typs

module Make (V : Runtime.Valrep.SAFE) = struct
  (* Operators: local generic replacements for [Value.Make]'s [<|!]/[<<|!]/
     [<<<|] conveniences, which are [Value.Make]-only (not part of
     [Runtime.Valrep.SAFE.Make]). [<|!] is plain pairing, so it is unchanged.
     [<<|!]/[case_at] wrap [V.Make.( <<| )], which now takes an optional
     [~at] (added to [SAFE.Make] alongside this task, since case construction
     had no way to carry a source region otherwise).

     [#@@] (retag an already-built value's noted type, e.g. text-as-"id") has
     no [SAFE] equivalent: it mutates a [Value.t]'s [note.typ] in place, and
     [SAFE] never exposes a value's note (V_native has none). It is dropped
     below (see boot_id/boot_atom/boot_targ/boot_num_exp) — boot/unboot's own
     round trip dispatches purely on the case's mixop, never on note.typ, so
     this doesn't affect this module pair; see the task report for the
     (narrower) risk to external Value.t-specific consumers of note.typ. *)
  let ( <|! ) (mixop : Mixop.t) (args : V.t list) : Mixop.t * V.t list =
    (mixop, args)

  let ( <<|! ) (pair : Mixop.t * V.t list) (typ : Il.typ) : V.t =
    V.Make.( <<| ) pair typ

  let case_at (at : region) (pair : Mixop.t * V.t list) (typ : Il.typ) : V.t =
    V.Make.( <<| ) ~at pair typ

  (* Identifiers *)

  let boot_id (id : Il.id) : V.t = V.Make.text ~at:id.at id.it

  (* Atoms *)

  let boot_atom (atom : Il.atom) : V.t =
    atom.it |> Atom.string_of_atom |> V.Make.text ~at:atom.at

  (* Mixfix operators *)

  let boot_mixop (mixop : Il.mixop) : V.t =
    let atoms_matrix = Mixop.atoms_matrix mixop in
    let values_atoms =
      List.map
        (fun atoms ->
          let values_atoms = List.map boot_atom atoms in
          let typ_atoms =
            Runtime.Type.Typ.Make.var ("atom" $ no_region) []
            |> Runtime.Type.Typ.Make.list
          in
          V.Make.list typ_atoms values_atoms)
        atoms_matrix
    in
    let typ_atoms_matrix =
      Runtime.Type.Typ.Make.var ("atom" $ no_region) []
      |> Runtime.Type.Typ.Make.list
    in
    V.Make.list typ_atoms_matrix values_atoms

  (* Iterators *)

  let boot_iter (iter : Il.iter) : V.t =
    match iter with
    | Opt -> mop_quest <|! [] <<|! typ_iter
    | List -> mop_star <|! [] <<|! typ_iter

  let boot_iters (iters : Il.iter list) : V.t =
    let values_iters = List.map boot_iter iters in
    V.Make.list (Runtime.Type.Typ.Make.list typ_iter) values_iters

  (* Variables *)

  let rec boot_var (var : Il.var) : V.t =
    let id, typ, iters = var in
    let value_id = boot_id id in
    let value_typ = boot_typ typ in
    let value_iters = boot_iters iters in
    mop_vari <|! [ value_id; value_typ; value_iters ] <<|! typ_vari

  and boot_vars (vars : Il.var list) : V.t =
    let values_vars = List.map boot_var vars in
    V.Make.list (Runtime.Type.Typ.Make.list typ_vari) values_vars

  (* Types *)

  and boot_typ (typ : Il.typ) : V.t =
    let at = typ.at in
    match typ.it with
    | BoolT -> boot_bool_typ at
    | NumT numtyp -> boot_num_typ at numtyp
    | TextT -> boot_text_typ at
    | VarT (id, targs) -> boot_var_typ at id targs
    | TupleT typs -> boot_tuple_typ at typs
    | IterT (typ, iter) -> boot_iter_typ at typ iter
    | FuncT (_, _, _) -> boot_func_typ at

  and boot_typs (typs : Il.typ list) : V.t =
    let values_typs = List.map boot_typ typs in
    V.Make.list (Runtime.Type.Typ.Make.list typ_typ) values_typs

  and boot_bool_typ (at : region) : V.t =
    case_at at (mop_bool_typ <|! []) typ_optyp

  and boot_num_typ (at : region) (numtyp : Num.typ) : V.t =
    match numtyp with
    | `NatT -> case_at at (mop_num_typ_nat <|! []) typ_numtyp
    | `IntT -> case_at at (mop_num_typ_int <|! []) typ_numtyp

  and boot_text_typ (at : region) : V.t =
    case_at at (mop_text_typ <|! []) typ_optyp

  and boot_var_typ (at : region) (id : Il.id) (targs : Il.targ list) : V.t =
    let value_id = boot_id id in
    let value_targs = boot_targs targs in
    case_at at (mop_var_typ <|! [ value_id; value_targs ]) typ_typ

  and boot_tuple_typ (at : region) (typs : Il.typ list) : V.t =
    let value_typs = boot_typs typs in
    case_at at (mop_tuple_typ <|! [ value_typs ]) typ_typ

  and boot_iter_typ (at : region) (typ : Il.typ) (iter : Il.iter) : V.t =
    let value_typ = boot_typ typ in
    let value_iter = boot_iter iter in
    case_at at (mop_iter_typ <|! [ value_typ; value_iter ]) typ_typ

  and boot_func_typ (at : region) : V.t =
    case_at at (mop_func_typ <|! []) typ_typ

  (* Defined types *)

  and boot_deftyp (deftyp : Il.deftyp) : V.t =
    let at = deftyp.at in
    match deftyp.it with
    | PlainT typ -> boot_plain_deftyp at typ
    | StructT typfields -> boot_struct_deftyp at typfields
    | VariantT typcases -> boot_variant_deftyp at typcases

  and boot_plain_deftyp (at : region) (typ : Il.typ) : V.t =
    let value_typ = boot_typ typ in
    case_at at (mop_plain_deftyp <|! [ value_typ ]) typ_deftyp

  and boot_typfield (typfield : Il.typfield) : V.t =
    let atom, typ = typfield in
    let value_atom = boot_atom atom in
    let value_typ = boot_typ typ in
    mop_typfield <|! [ value_atom; value_typ ] <<|! typ_typfield

  and boot_typfields (typfields : Il.typfield list) : V.t =
    let values_typfields = List.map boot_typfield typfields in
    V.Make.list (Runtime.Type.Typ.Make.list typ_typfield) values_typfields

  and boot_struct_deftyp (at : region) (typfields : Il.typfield list) : V.t =
    let value_typfields = boot_typfields typfields in
    case_at at (mop_struct_deftyp <|! [ value_typfields ]) typ_deftyp

  and boot_typcase (typcase : Il.typcase) : V.t =
    let nottyp, _, _ = typcase in
    let mixop, typs = Mixfix.split nottyp.it in
    let value_mixop = boot_mixop mixop in
    let value_typs = boot_typs typs in
    case_at nottyp.at (mop_typcase <|! [ value_mixop; value_typs ]) typ_typcase

  and boot_typcases (typcases : Il.typcase list) : V.t =
    let values_typcases = List.map boot_typcase typcases in
    V.Make.list (Runtime.Type.Typ.Make.list typ_typcase) values_typcases

  and boot_variant_deftyp (at : region) (typcases : Il.typcase list) : V.t =
    let value_typcases = boot_typcases typcases in
    case_at at (mop_variant_deftyp <|! [ value_typcases ]) typ_deftyp

  (* Values *)

  and boot_value (value : Il.value) : V.t =
    let at = value.at in
    match value.it with
    | BoolV b -> boot_bool_value at b
    | NumV num -> boot_num_value at num
    | TextV t -> boot_text_value at t
    | StructV valuefields -> boot_struct_value at valuefields
    | CaseV valuecase -> boot_case_value at valuecase
    | TupleV values -> boot_tuple_value at values
    | OptV value_opt -> boot_opt_value at value_opt
    | ListV values -> boot_list_value at values
    | FuncV id -> boot_func_value at id
    | ExternV json -> boot_extern_value at json

  and boot_value_opt (value_opt : Il.value option) : V.t =
    V.Make.opt (Runtime.Type.Typ.Make.opt typ_val) (Option.map boot_value value_opt)

  and boot_values (values : Il.value list) : V.t =
    let values_values = List.map boot_value values in
    V.Make.list (Runtime.Type.Typ.Make.list typ_val) values_values

  and boot_bool_value (at : region) (b : bool) : V.t =
    case_at at (mop_bool_value <|! [ V.Make.bool b ]) typ_val

  and boot_num_value (at : region) (num : Il.num) : V.t =
    match num with
    | `Nat n -> case_at at (mop_num_value_nat <|! [ V.Make.nat n ]) typ_num
    | `Int i -> case_at at (mop_num_value_int <|! [ V.Make.int i ]) typ_num

  and boot_text_value (at : region) (t : string) : V.t =
    case_at at (mop_text_value <|! [ V.Make.text t ]) typ_val

  and boot_valuefield (vf : Il.valuefield) : V.t =
    let atom, value = vf in
    let value_atom = boot_atom atom in
    let value_value = boot_value value in
    mop_valuefield <|! [ value_atom; value_value ] <<|! typ_valfield

  and boot_valuefields (valuefields : Il.valuefield list) : V.t =
    let values_valuefields = List.map boot_valuefield valuefields in
    V.Make.list (Runtime.Type.Typ.Make.list typ_valfield) values_valuefields

  and boot_struct_value (at : region) (valuefields : Il.valuefield list) : V.t =
    let value_valuefields = boot_valuefields valuefields in
    case_at at (mop_struct_value <|! [ value_valuefields ]) typ_val

  and boot_valuecase (valuecase : Il.valuecase) : V.t =
    let mixop, values = Mixfix.split valuecase in
    let value_mixop = boot_mixop mixop in
    let value_values = boot_values values in
    mop_valuecase <|! [ value_mixop; value_values ] <<|! typ_valcase

  and boot_case_value (at : region) (vc : Il.valuecase) : V.t =
    let value_valuecase = boot_valuecase vc in
    case_at at (mop_case_value <|! [ value_valuecase ]) typ_val

  and boot_tuple_value (at : region) (values : Il.value list) : V.t =
    let value_values = boot_values values in
    case_at at (mop_tuple_value <|! [ value_values ]) typ_val

  and boot_opt_value (at : region) (value_opt : Il.value option) : V.t =
    let value_value_opt = boot_value_opt value_opt in
    case_at at (mop_opt_value <|! [ value_value_opt ]) typ_val

  and boot_list_value (at : region) (values : Il.value list) : V.t =
    let value_values = boot_values values in
    case_at at (mop_list_value <|! [ value_values ]) typ_val

  and boot_func_value (at : region) (id : Il.id) : V.t =
    let value_id = boot_id id in
    case_at at (mop_func_value <|! [ value_id ]) typ_val

  and boot_extern_value (at : region) (json : Yojson.Safe.t) : V.t =
    let typ_json = Runtime.Type.Typ.Make.var ("json" $ no_region) [] in
    let value_json = V.Make.extern typ_json json in
    case_at at (mop_extern_value <|! [ value_json ]) typ_val

  (* Operators *)

  and boot_unop (unop : Il.unop) : V.t =
    match unop with
    | #Bool.unop as unop -> boot_unop_bool unop
    | #Num.unop as unop -> boot_unop_num unop

  and boot_unop_bool (unop : Bool.unop) : V.t =
    match unop with `NotOp -> mop_not_unop <|! [] <<|! typ_boolunop

  and boot_unop_num (unop : Num.unop) : V.t =
    match unop with
    | `PlusOp -> mop_plus_unop <|! [] <<|! typ_numunop
    | `MinusOp -> mop_minus_unop <|! [] <<|! typ_numunop

  and boot_binop (binop : Il.binop) : V.t =
    match binop with
    | #Bool.binop as binop -> boot_binop_bool binop
    | #Num.binop as binop -> boot_binop_num binop

  and boot_binop_bool (binop : Bool.binop) : V.t =
    match binop with
    | `AndOp -> mop_and_binop <|! [] <<|! typ_boolbinop
    | `OrOp -> mop_or_binop <|! [] <<|! typ_boolbinop
    | `ImplOp -> mop_impl_binop <|! [] <<|! typ_boolbinop
    | `EquivOp -> mop_equiv_binop <|! [] <<|! typ_boolbinop

  and boot_binop_num (binop : Num.binop) : V.t =
    match binop with
    | `AddOp -> mop_add_binop <|! [] <<|! typ_numbinop
    | `SubOp -> mop_sub_binop <|! [] <<|! typ_numbinop
    | `MulOp -> mop_mul_binop <|! [] <<|! typ_numbinop
    | `DivOp -> mop_div_binop <|! [] <<|! typ_numbinop
    | `ModOp -> mop_mod_binop <|! [] <<|! typ_numbinop
    | `PowOp -> mop_pow_binop <|! [] <<|! typ_numbinop

  and boot_cmpop (cmpop : Il.cmpop) : V.t =
    match cmpop with
    | #Bool.cmpop as cmpop -> boot_cmpop_bool cmpop
    | #Num.cmpop as cmpop -> boot_cmpop_num cmpop

  and boot_cmpop_bool (cmpop : Bool.cmpop) : V.t =
    match cmpop with
    | `EqOp -> mop_eq_cmpop <|! [] <<|! typ_polycmpop
    | `NeOp -> mop_ne_cmpop <|! [] <<|! typ_polycmpop

  and boot_cmpop_num (cmpop : Num.cmpop) : V.t =
    match cmpop with
    | `LtOp -> mop_lt_cmpop <|! [] <<|! typ_numcmpop
    | `LeOp -> mop_le_cmpop <|! [] <<|! typ_numcmpop
    | `GtOp -> mop_gt_cmpop <|! [] <<|! typ_numcmpop
    | `GeOp -> mop_ge_cmpop <|! [] <<|! typ_numcmpop

  (* Type arguments *)

  and boot_targ (targ : Il.targ) : V.t = boot_typ targ

  and boot_targs (targs : Il.targ list) : V.t =
    let values_targs = List.map boot_targ targs in
    V.Make.list
      (Runtime.Type.Typ.Make.list
         (Runtime.Type.Typ.Make.var ("targ" $ no_region) []))
      values_targs

  (* Type parameters *)

  and boot_tparam (tparam : Il.tparam) : V.t = boot_id tparam

  and boot_tparams (tparams : Il.tparam list) : V.t =
    let values_tparams = List.map boot_tparam tparams in
    V.Make.list
      (Runtime.Type.Typ.Make.list
         (Runtime.Type.Typ.Make.var ("tparam" $ no_region) []))
      values_tparams

  (* Arguments *)

  and boot_arg (arg : Il.arg) : V.t =
    let at = arg.at in
    match arg.it with
    | ExpA e ->
        let value_exp = boot_exp e in
        case_at at (mop_exp_arg <|! [ value_exp ]) typ_arg
    | DefA id ->
        let value_id = boot_id id in
        case_at at (mop_def_arg <|! [ value_id ]) typ_arg

  and boot_args (args : Il.arg list) : V.t =
    let values_args = List.map boot_arg args in
    V.Make.list (Runtime.Type.Typ.Make.list typ_arg) values_args

  (* Expressions *)

  and boot_exp (exp : Il.exp) : V.t =
    let at = exp.at in
    match exp.it with
    | BoolE b -> boot_bool_exp at b
    | NumE num -> boot_num_exp at num
    | TextE t -> boot_text_exp at t
    | VarE id -> boot_var_exp at id
    | UnE (unop, _, e) -> boot_un_exp at unop e
    | BinE (binop, _, el, er) -> boot_bin_exp at binop el er
    | CmpE (cmpop, _, el, er) -> boot_cmp_exp at cmpop el er
    | UpCastE (typ, e) -> boot_upcast_exp at typ e
    | DownCastE (typ, e) -> boot_downcast_exp at typ e
    | SubE (e, typ) -> boot_sub_exp at e typ
    | MatchE (e, pattern) -> boot_match_exp at e pattern
    | TupleE exps -> boot_tuple_exp at exps
    | CaseE notexp -> boot_case_exp at notexp
    | StrE expfields -> boot_struct_exp at expfields
    | OptE exp_opt -> boot_opt_exp at exp_opt
    | ListE exps -> boot_list_exp at exps
    | ConsE (eh, et) -> boot_cons_exp at eh et
    | CatE (el, er) -> boot_cat_exp at el er
    | MemE (ee, es) -> boot_mem_exp at ee es
    | LenE e -> boot_len_exp at e
    | DotE (e, atom) -> boot_dot_exp at e atom
    | IdxE (eb, ei) -> boot_idx_exp at eb ei
    | SliceE (eb, ei, en) -> boot_slice_exp at eb ei en
    | UpdE (eb, path, en) -> boot_upd_exp at eb path en
    | CallE (id, targs, args) -> boot_call_exp at id targs args
    | IterE (e, ie) -> boot_iter_exp at e ie

  and boot_bool_exp (at : region) (b : bool) : V.t =
    case_at at (mop_bool_exp <|! [ V.Make.bool b ]) typ_exp

  and boot_num_exp (at : region) (num : Il.num) : V.t = boot_num_value at num

  and boot_text_exp (at : region) (t : string) : V.t =
    case_at at (mop_text_exp <|! [ V.Make.text t ]) typ_exp

  and boot_var_exp (at : region) (id : Il.id) : V.t =
    let value_id = boot_id id in
    case_at at (mop_var_exp <|! [ value_id ]) typ_exp

  and boot_un_exp (at : region) (unop : Il.unop) (exp : Il.exp) : V.t =
    let value_unop = boot_unop unop in
    let value_exp = boot_exp exp in
    case_at at (mop_un_exp <|! [ value_unop; value_exp ]) typ_exp

  and boot_bin_exp (at : region) (binop : Il.binop) (exp_l : Il.exp)
      (exp_r : Il.exp) : V.t =
    let value_binop = boot_binop binop in
    let value_exp_l = boot_exp exp_l in
    let value_exp_r = boot_exp exp_r in
    case_at at
      (mop_bin_exp <|! [ value_binop; value_exp_l; value_exp_r ])
      typ_exp

  and boot_cmp_exp (at : region) (cmpop : Il.cmpop) (exp_l : Il.exp)
      (exp_r : Il.exp) : V.t =
    let value_cmpop = boot_cmpop cmpop in
    let value_exp_l = boot_exp exp_l in
    let value_exp_r = boot_exp exp_r in
    case_at at
      (mop_cmp_exp <|! [ value_cmpop; value_exp_l; value_exp_r ])
      typ_exp

  and boot_upcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : V.t =
    let value_typ = boot_typ typ in
    let value_exp = boot_exp exp in
    case_at at (mop_upcast_exp <|! [ value_typ; value_exp ]) typ_exp

  and boot_downcast_exp (at : region) (typ : Il.typ) (exp : Il.exp) : V.t =
    let value_typ = boot_typ typ in
    let value_exp = boot_exp exp in
    case_at at (mop_downcast_exp <|! [ value_typ; value_exp ]) typ_exp

  and boot_sub_exp (at : region) (exp : Il.exp) (typ : Il.typ) : V.t =
    let value_exp = boot_exp exp in
    let value_typ = boot_typ typ in
    case_at at (mop_sub_exp <|! [ value_exp; value_typ ]) typ_exp

  and boot_match_exp (at : region) (exp : Il.exp) (pattern : Il.pattern) : V.t
      =
    let value_exp = boot_exp exp in
    let value_pattern = boot_pattern pattern in
    case_at at (mop_match_exp <|! [ value_exp; value_pattern ]) typ_exp

  and boot_tuple_exp (at : region) (exps : Il.exp list) : V.t =
    let value_exps = boot_exps exps in
    case_at at (mop_tuple_exp <|! [ value_exps ]) typ_exp

  and boot_expcase (notexp : Il.notexp) : V.t =
    let mixop, exps = Mixfix.split notexp in
    let value_mixop = boot_mixop mixop in
    let value_exps = boot_exps exps in
    mop_expcase <|! [ value_mixop; value_exps ] <<|! typ_expcase

  and boot_case_exp (at : region) (notexp : Il.notexp) : V.t =
    let value_expcase = boot_expcase notexp in
    case_at at (mop_case_exp <|! [ value_expcase ]) typ_exp

  and boot_expfield ((atom, exp) : Il.atom * Il.exp) : V.t =
    let value_atom = boot_atom atom in
    let value_exp = boot_exp exp in
    mop_expfield <|! [ value_atom; value_exp ] <<|! typ_expfield

  and boot_expfields (expfields : (Il.atom * Il.exp) list) : V.t =
    let values_expfields = List.map boot_expfield expfields in
    V.Make.list (Runtime.Type.Typ.Make.list typ_expfield) values_expfields

  and boot_struct_exp (at : region) (expfields : (Il.atom * Il.exp) list) :
      V.t =
    let value_expfields = boot_expfields expfields in
    case_at at (mop_struct_exp <|! [ value_expfields ]) typ_exp

  and boot_opt_exp (at : region) (exp_opt : Il.exp option) : V.t =
    let value_exp_opt = boot_exp_opt exp_opt in
    case_at at (mop_opt_exp <|! [ value_exp_opt ]) typ_exp

  and boot_list_exp (at : region) (exps : Il.exp list) : V.t =
    let value_exps = boot_exps exps in
    case_at at (mop_list_exp <|! [ value_exps ]) typ_exp

  and boot_cons_exp (at : region) (exp_h : Il.exp) (exp_t : Il.exp) : V.t =
    let value_exp_h = boot_exp exp_h in
    let value_exp_t = boot_exp exp_t in
    case_at at (mop_cons_exp <|! [ value_exp_h; value_exp_t ]) typ_exp

  and boot_cat_exp (at : region) (exp_l : Il.exp) (exp_r : Il.exp) : V.t =
    let value_exp_l = boot_exp exp_l in
    let value_exp_r = boot_exp exp_r in
    case_at at (mop_cat_exp <|! [ value_exp_l; value_exp_r ]) typ_exp

  and boot_mem_exp (at : region) (exp_e : Il.exp) (exp_s : Il.exp) : V.t =
    let value_exp_e = boot_exp exp_e in
    let value_exp_s = boot_exp exp_s in
    case_at at (mop_mem_exp <|! [ value_exp_e; value_exp_s ]) typ_exp

  and boot_len_exp (at : region) (exp : Il.exp) : V.t =
    let value_exp = boot_exp exp in
    case_at at (mop_len_exp <|! [ value_exp ]) typ_exp

  and boot_dot_exp (at : region) (exp : Il.exp) (atom : Il.atom) : V.t =
    let value_exp = boot_exp exp in
    let value_atom = boot_atom atom in
    case_at at (mop_dot_exp <|! [ value_exp; value_atom ]) typ_exp

  and boot_idx_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp) : V.t =
    let value_exp_b = boot_exp exp_b in
    let value_exp_i = boot_exp exp_i in
    case_at at (mop_idx_exp <|! [ value_exp_b; value_exp_i ]) typ_exp

  and boot_slice_exp (at : region) (exp_b : Il.exp) (exp_i : Il.exp)
      (exp_n : Il.exp) : V.t =
    let value_exp_b = boot_exp exp_b in
    let value_exp_i = boot_exp exp_i in
    let value_exp_n = boot_exp exp_n in
    case_at at
      (mop_slice_exp <|! [ value_exp_b; value_exp_i; value_exp_n ])
      typ_exp

  and boot_upd_exp (at : region) (exp_b : Il.exp) (path : Il.path)
      (exp_n : Il.exp) : V.t =
    let value_exp_b = boot_exp exp_b in
    let value_path = boot_path path in
    let value_exp_n = boot_exp exp_n in
    case_at at
      (mop_upd_exp <|! [ value_exp_b; value_path; value_exp_n ])
      typ_exp

  and boot_call_exp (at : region) (id : Il.id) (targs : Il.targ list)
      (args : Il.arg list) : V.t =
    let value_id = boot_id id in
    let value_targs = boot_targs targs in
    let value_args = boot_args args in
    case_at at
      (mop_call_exp <|! [ value_id; value_targs; value_args ])
      typ_exp

  and boot_iter_exp (at : region) (exp : Il.exp) (iterexp : Il.iterexp) : V.t =
    let value_exp = boot_exp exp in
    let value_iterexp = boot_iterexp iterexp in
    case_at at (mop_iter_exp <|! [ value_exp; value_iterexp ]) typ_exp

  and boot_exps (exps : Il.exp list) : V.t =
    let values_exps = List.map boot_exp exps in
    V.Make.list (Runtime.Type.Typ.Make.list typ_exp) values_exps

  and boot_exp_opt (exp_opt : Il.exp option) : V.t =
    V.Make.opt (Runtime.Type.Typ.Make.opt typ_exp) (Option.map boot_exp exp_opt)

  (* Paths *)

  and boot_path (path : Il.path) : V.t =
    let at = path.at in
    match path.it with
    | RootP -> case_at at (mop_root_path <|! []) typ_path
    | IdxP (path, exp) ->
        let value_path = boot_path path in
        let value_exp = boot_exp exp in
        case_at at (mop_idx_path <|! [ value_path; value_exp ]) typ_path
    | SliceP (path, exp_i, exp_n) ->
        let value_path = boot_path path in
        let value_exp_i = boot_exp exp_i in
        let value_exp_n = boot_exp exp_n in
        case_at at
          (mop_slice_path <|! [ value_path; value_exp_i; value_exp_n ])
          typ_path
    | DotP (path, atom) ->
        let value_path = boot_path path in
        let value_atom = boot_atom atom in
        case_at at (mop_dot_path <|! [ value_path; value_atom ]) typ_path

  (* Patterns *)

  and boot_pattern (pattern : Il.pattern) : V.t =
    match pattern with
    | CaseP mixop ->
        let value_mixop = boot_mixop mixop in
        mop_case_pattern <|! [ value_mixop ] <<|! typ_pattern
    | ListP `Cons -> mop_list_cons_pattern <|! [] <<|! typ_listpattern
    | ListP (`Fixed n) ->
        mop_list_fixed_pattern
        <|! [ V.Make.nat (Bigint.of_int n) ]
        <<|! typ_listpattern
    | ListP `Nil -> mop_list_nil_pattern <|! [] <<|! typ_listpattern
    | OptP `Some -> mop_opt_some_pattern <|! [] <<|! typ_optpattern
    | OptP `None -> mop_opt_none_pattern <|! [] <<|! typ_optpattern

  (* Iter expressions *)

  and boot_iterexp ((iter, vars) : Il.iterexp) : V.t =
    let value_iter = boot_iter iter in
    let value_vars = boot_vars vars in
    mop_iterexp <|! [ value_iter; value_vars ] <<|! typ_iterexp
end
