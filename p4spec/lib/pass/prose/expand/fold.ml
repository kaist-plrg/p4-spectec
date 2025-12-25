open Lang
open Il
open Util.Source

(* Fold logic: takes the results of sub-expressions and combines them into a new result *)

type ('a_exp, 'a_arg, 'a_path) folder = {
  f_BoolE : typ' -> region -> bool -> 'a_exp;
  f_NumE : typ' -> region -> num -> 'a_exp;
  f_TextE : typ' -> region -> text -> 'a_exp;
  f_VarE : typ' -> region -> id -> 'a_exp;
  f_UnE : typ' -> region -> unop -> optyp -> 'a_exp -> 'a_exp;
  f_BinE : typ' -> region -> binop -> optyp -> 'a_exp -> 'a_exp -> 'a_exp;
  f_CmpE : typ' -> region -> cmpop -> optyp -> 'a_exp -> 'a_exp -> 'a_exp;
  f_UpCastE : typ' -> region -> typ -> 'a_exp -> 'a_exp;
  f_DownCastE : typ' -> region -> typ -> 'a_exp -> 'a_exp;
  f_SubE : typ' -> region -> 'a_exp -> typ -> 'a_exp;
  f_MatchE : typ' -> region -> 'a_exp -> pattern -> 'a_exp;
  f_TupleE : typ' -> region -> 'a_exp list -> 'a_exp;
  f_CaseE : typ' -> region -> mixop -> 'a_exp list -> 'a_exp;
  f_StrE : typ' -> region -> (atom * 'a_exp) list -> 'a_exp;
  f_OptE : typ' -> region -> 'a_exp option -> 'a_exp;
  f_ListE : typ' -> region -> 'a_exp list -> 'a_exp;
  f_ConsE : typ' -> region -> 'a_exp -> 'a_exp -> 'a_exp;
  f_CatE : typ' -> region -> 'a_exp -> 'a_exp -> 'a_exp;
  f_MemE : typ' -> region -> 'a_exp -> 'a_exp -> 'a_exp;
  f_LenE : typ' -> region -> 'a_exp -> 'a_exp;
  f_DotE : typ' -> region -> 'a_exp -> atom -> 'a_exp;
  f_IdxE : typ' -> region -> 'a_exp -> 'a_exp -> 'a_exp;
  f_SliceE : typ' -> region -> 'a_exp -> 'a_exp -> 'a_exp -> 'a_exp;
  f_UpdE : typ' -> region -> 'a_exp -> 'a_path -> 'a_exp -> 'a_exp;
  f_CallE : typ' -> region -> id -> targ list -> 'a_arg list -> 'a_exp;
  f_IterE : typ' -> region -> 'a_exp -> iterexp -> 'a_exp;
  f_RootP : typ' -> region -> 'a_path;
  f_IdxP : typ' -> region -> 'a_path -> 'a_exp -> 'a_path;
  f_SliceP : typ' -> region -> 'a_path -> 'a_exp -> 'a_exp -> 'a_path;
  f_DotP : typ' -> region -> 'a_path -> atom -> 'a_path;
  f_ExpA : region -> 'a_exp -> 'a_arg;
  f_DefA : region -> id -> 'a_arg;
}

(* Default folder that takes the parts of each expression and constructs the expression *)

let identity : (exp, arg, path) folder =
  {
    f_BoolE = (fun note at b -> { it = BoolE b; at; note });
    f_NumE = (fun note at n -> { it = NumE n; at; note });
    f_TextE = (fun note at t -> { it = TextE t; at; note });
    f_VarE = (fun note at id -> { it = VarE id; at; note });
    f_UnE =
      (fun note at unop optyp exp -> { it = UnE (unop, optyp, exp); at; note });
    f_BinE =
      (fun note at binop optyp exp_l exp_r ->
        { it = BinE (binop, optyp, exp_l, exp_r); at; note });
    f_CmpE =
      (fun note at cmpop optyp exp_l exp_r ->
        { it = CmpE (cmpop, optyp, exp_l, exp_r); at; note });
    f_UpCastE = (fun note at typ exp -> { it = UpCastE (typ, exp); at; note });
    f_DownCastE =
      (fun note at typ exp -> { it = DownCastE (typ, exp); at; note });
    f_SubE = (fun note at exp typ -> { it = SubE (exp, typ); at; note });
    f_MatchE =
      (fun note at exp pattern -> { it = MatchE (exp, pattern); at; note });
    f_TupleE = (fun note at exps -> { it = TupleE exps; at; note });
    f_CaseE = (fun note at mixop exps -> { it = CaseE (mixop, exps); at; note });
    f_StrE = (fun note at expfields -> { it = StrE expfields; at; note });
    f_OptE = (fun note at exp_opt -> { it = OptE exp_opt; at; note });
    f_ListE = (fun note at exps -> { it = ListE exps; at; note });
    f_ConsE =
      (fun note at exp_h exp_t -> { it = ConsE (exp_h, exp_t); at; note });
    f_CatE = (fun note at exp_l exp_r -> { it = CatE (exp_l, exp_r); at; note });
    f_MemE = (fun note at exp_e exp_s -> { it = MemE (exp_e, exp_s); at; note });
    f_LenE = (fun note at exp -> { it = LenE exp; at; note });
    f_DotE = (fun note at exp atom -> { it = DotE (exp, atom); at; note });
    f_IdxE = (fun note at exp_b exp_i -> { it = IdxE (exp_b, exp_i); at; note });
    f_SliceE =
      (fun note at exp_b exp_l exp_h ->
        { it = SliceE (exp_b, exp_l, exp_h); at; note });
    f_UpdE =
      (fun note at exp_b path exp_f ->
        { it = UpdE (exp_b, path, exp_f); at; note });
    f_CallE =
      (fun note at funcprose targs args ->
        { it = CallE (funcprose, targs, args); at; note });
    f_IterE =
      (fun note at exp iterexp -> { it = IterE (exp, iterexp); at; note });
    f_RootP = (fun note at -> { it = RootP; at; note });
    f_IdxP = (fun note at path exp -> { it = IdxP (path, exp); at; note });
    f_SliceP =
      (fun note at path exp_l exp_h ->
        { it = SliceP (path, exp_l, exp_h); at; note });
    f_DotP = (fun note at path atom -> { it = DotP (path, atom); at; note });
    f_ExpA = (fun at exp -> { it = ExpA exp; at; note = () });
    f_DefA = (fun at id -> { it = DefA id; at; note = () });
  }

let rec fold_exp (alg : ('a_exp, 'a_arg, 'a_path) folder) (e : exp) : 'a_exp =
  let { it; note; at } = e in
  match it with
  | BoolE b -> alg.f_BoolE note at b
  | NumE n -> alg.f_NumE note at n
  | TextE t -> alg.f_TextE note at t
  | VarE id -> alg.f_VarE note at id
  | UnE (unop, optyp, exp) ->
      let acc = fold_exp alg exp in
      alg.f_UnE note at unop optyp acc
  | BinE (binop, optyp, exp_l, exp_r) ->
      let acc_l = fold_exp alg exp_l in
      let acc_r = fold_exp alg exp_r in
      alg.f_BinE note at binop optyp acc_l acc_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let acc_l = fold_exp alg exp_l in
      let acc_r = fold_exp alg exp_r in
      alg.f_CmpE note at cmpop optyp acc_l acc_r
  | UpCastE (typ, exp) ->
      let acc = fold_exp alg exp in
      alg.f_UpCastE note at typ acc
  | DownCastE (typ, exp) ->
      let acc = fold_exp alg exp in
      alg.f_DownCastE note at typ acc
  | SubE (exp, typ) ->
      let acc = fold_exp alg exp in
      alg.f_SubE note at acc typ
  | MatchE (exp, pattern) ->
      let acc = fold_exp alg exp in
      alg.f_MatchE note at acc pattern
  | TupleE exps ->
      let accs = List.map (fold_exp alg) exps in
      alg.f_TupleE note at accs
  | CaseE (mixop, exps) ->
      let accs = List.map (fold_exp alg) exps in
      alg.f_CaseE note at mixop accs
  | StrE expfields ->
      let accs =
        List.map (fun (atom, exp) -> (atom, fold_exp alg exp)) expfields
      in
      alg.f_StrE note at accs
  | OptE exp_opt ->
      let acc_opt = Option.map (fold_exp alg) exp_opt in
      alg.f_OptE note at acc_opt
  | ListE exps ->
      let accs = List.map (fold_exp alg) exps in
      alg.f_ListE note at accs
  | ConsE (exp_h, exp_t) ->
      let acc_h = fold_exp alg exp_h in
      let acc_t = fold_exp alg exp_t in
      alg.f_ConsE note at acc_h acc_t
  | CatE (exp_l, exp_r) ->
      let acc_l = fold_exp alg exp_l in
      let acc_r = fold_exp alg exp_r in
      alg.f_CatE note at acc_l acc_r
  | MemE (exp_e, exp_s) ->
      let acc_e = fold_exp alg exp_e in
      let acc_s = fold_exp alg exp_s in
      alg.f_MemE note at acc_e acc_s
  | LenE exp ->
      let acc = fold_exp alg exp in
      alg.f_LenE note at acc
  | DotE (exp, atom) ->
      let acc = fold_exp alg exp in
      alg.f_DotE note at acc atom
  | IdxE (exp_b, exp_i) ->
      let acc_b = fold_exp alg exp_b in
      let acc_i = fold_exp alg exp_i in
      alg.f_IdxE note at acc_b acc_i
  | SliceE (exp_b, exp_l, exp_h) ->
      let acc_b = fold_exp alg exp_b in
      let acc_l = fold_exp alg exp_l in
      let acc_h = fold_exp alg exp_h in
      alg.f_SliceE note at acc_b acc_l acc_h
  | UpdE (exp_b, path, exp_f) ->
      let acc_b = fold_exp alg exp_b in
      let acc_p = fold_path alg path in
      let acc_f = fold_exp alg exp_f in
      alg.f_UpdE note at acc_b acc_p acc_f
  | CallE (funcprose, targs, args) ->
      let accs = List.map (fold_arg alg) args in
      alg.f_CallE note at funcprose targs accs
  | IterE (exp, iterexp) ->
      let acc = fold_exp alg exp in
      alg.f_IterE note at acc iterexp

and fold_arg (alg : ('a_exp, 'a_arg, 'a_path) folder) (arg : arg) : 'a_arg =
  let { it; at; _ } = arg in
  match it with
  | ExpA exp ->
      let acc = fold_exp alg exp in
      alg.f_ExpA at acc
  | DefA id -> alg.f_DefA at id

and fold_args (alg : ('a_exp, 'a_arg, 'a_path) folder) (args : arg list) :
    'a_arg list =
  List.map (fold_arg alg) args

and fold_path (alg : ('a_exp, 'a_arg, 'a_path) folder) (path : path) : 'a_path =
  let { it; note; at } = path in
  match it with
  | RootP -> alg.f_RootP note at
  | IdxP (path, exp) ->
      let acc_p = fold_path alg path in
      let acc_e = fold_exp alg exp in
      alg.f_IdxP note at acc_p acc_e
  | SliceP (path, exp_l, exp_h) ->
      let acc_p = fold_path alg path in
      let acc_l = fold_exp alg exp_l in
      let acc_h = fold_exp alg exp_h in
      alg.f_SliceP note at acc_p acc_l acc_h
  | DotP (path, atom) ->
      let acc_p = fold_path alg path in
      alg.f_DotP note at acc_p atom

and fold_paths (alg : ('a_exp, 'a_arg, 'a_path) folder) (paths : path list) :
    'a_path list =
  List.map (fold_path alg) paths
