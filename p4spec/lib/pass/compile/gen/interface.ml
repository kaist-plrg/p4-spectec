open Domain
open Lang
module Typdef = Runtime.Type.Typdef
open Util.Source

(* Type-to-name mapping for generated function suffixes *)

let rec interface_name (typ : Sl.typ) : string =
  match typ.it with
  | BoolT -> "bool"
  | NumT `NatT -> "nat"
  | NumT `IntT -> "int"
  | TextT -> "text"
  | VarT (id, []) -> Names.var_of_id id
  | VarT (id, targs) ->
      Names.var_of_id id ^ "__"
      ^ String.concat "__" (List.map interface_name targs)
  | TupleT typs -> String.concat "_" (List.map interface_name typs) ^ "_tup"
  | IterT (t, Il.Opt) -> interface_name t ^ "__opt"
  | IterT (t, Il.List) -> interface_name t ^ "__list"
  | FuncT _ -> "func"

(* Runtime Typ.t codegen *)

let rec typ_make_expr (typ : Sl.typ) : Ml.expr =
  match typ.it with
  | BoolT -> Ml.LitE "Typ.Make.bool"
  | NumT `NatT -> Ml.LitE "Typ.Make.nat"
  | NumT `IntT -> Ml.LitE "Typ.Make.int"
  | TextT -> Ml.LitE "Typ.Make.text"
  | VarT (id, []) ->
      Ml.AppE (Ml.LitE "make_typ_var_", [ Ml.StrE id.it; Ml.ListE [] ])
  | VarT (id, targs) ->
      Ml.AppE
        ( Ml.LitE "make_typ_var_",
          [ Ml.StrE id.it; Ml.ListE (List.map typ_make_expr targs) ] )
  | TupleT typs ->
      Ml.AppE
        (Ml.LitE "Typ.Make.tuple", [ Ml.ListE (List.map typ_make_expr typs) ])
  | IterT (t, Il.Opt) -> Ml.AppE (Ml.LitE "Typ.Make.opt", [ typ_make_expr t ])
  | IterT (t, Il.List) -> Ml.AppE (Ml.LitE "Typ.Make.list", [ typ_make_expr t ])
  | FuncT _ -> Ml.LitE "Typ.Make.bool"

(* Runtime Atom.t/Mixop.t codegen, for marshalling *)

let atom_it_lit (atom : Atom.t) : string =
  match atom with
  | Atom.Atom s -> Printf.sprintf "Atom.Atom \"%s\"" (String.escaped s)
  | Atom.SilentAtom s ->
      Printf.sprintf "Atom.SilentAtom \"%s\"" (String.escaped s)
  | Atom.Sub -> "Atom.Sub"
  | Atom.Sup -> "Atom.Sup"
  | Atom.Turnstile -> "Atom.Turnstile"
  | Atom.Tilesturn -> "Atom.Tilesturn"
  | Atom.Tick -> "Atom.Tick"
  | Atom.DoubleQuote -> "Atom.DoubleQuote"
  | Atom.Underscore -> "Atom.Underscore"
  | Atom.Arrow `Plain -> "Atom.Arrow `Plain"
  | Atom.Arrow `Tick -> "Atom.Arrow `Tick"
  | Atom.ArrowSub -> "Atom.ArrowSub"
  | Atom.DoubleArrow -> "Atom.DoubleArrow"
  | Atom.DoubleArrowSub -> "Atom.DoubleArrowSub"
  | Atom.DoubleArrowLong -> "Atom.DoubleArrowLong"
  | Atom.SqArrow -> "Atom.SqArrow"
  | Atom.SqArrowStar -> "Atom.SqArrowStar"
  | Atom.Dot `Plain -> "Atom.Dot `Plain"
  | Atom.Dot `Tick -> "Atom.Dot `Tick"
  | Atom.Dot2 `Plain -> "Atom.Dot2 `Plain"
  | Atom.Dot2 `Tick -> "Atom.Dot2 `Tick"
  | Atom.Dot3 `Plain -> "Atom.Dot3 `Plain"
  | Atom.Dot3 `Tick -> "Atom.Dot3 `Tick"
  | Atom.Comma -> "Atom.Comma"
  | Atom.Semicolon `Plain -> "Atom.Semicolon `Plain"
  | Atom.Semicolon `Tick -> "Atom.Semicolon `Tick"
  | Atom.Colon `Plain -> "Atom.Colon `Plain"
  | Atom.Colon `Tick -> "Atom.Colon `Tick"
  | Atom.ColonEq `Plain -> "Atom.ColonEq `Plain"
  | Atom.ColonEq `Tick -> "Atom.ColonEq `Tick"
  | Atom.Hash -> "Atom.Hash"
  | Atom.Dollar -> "Atom.Dollar"
  | Atom.At -> "Atom.At"
  | Atom.Quest -> "Atom.Quest"
  | Atom.Bang -> "Atom.Bang"
  | Atom.BangEq -> "Atom.BangEq"
  | Atom.Tilde -> "Atom.Tilde"
  | Atom.Tilde2 `Plain -> "Atom.Tilde2 `Plain"
  | Atom.Tilde2 `Tick -> "Atom.Tilde2 `Tick"
  | Atom.LAngle `Tick -> "Atom.LAngle `Tick"
  | Atom.LAngle `Tick2 -> "Atom.LAngle `Tick2"
  | Atom.LAngle2 -> "Atom.LAngle2"
  | Atom.LAngleEq -> "Atom.LAngleEq"
  | Atom.LAngle2Eq -> "Atom.LAngle2Eq"
  | Atom.RAngle `Plain -> "Atom.RAngle `Plain"
  | Atom.RAngle `Tick2 -> "Atom.RAngle `Tick2"
  | Atom.RAngle2 -> "Atom.RAngle2"
  | Atom.RAngleEq -> "Atom.RAngleEq"
  | Atom.RAngle2Eq -> "Atom.RAngle2Eq"
  | Atom.LParen -> "Atom.LParen"
  | Atom.RParen -> "Atom.RParen"
  | Atom.LBrack `Tick -> "Atom.LBrack `Tick"
  | Atom.LBrack `Tick2 -> "Atom.LBrack `Tick2"
  | Atom.RBrack `Plain -> "Atom.RBrack `Plain"
  | Atom.RBrack `Tick2 -> "Atom.RBrack `Tick2"
  | Atom.LBrace `Tick -> "Atom.LBrace `Tick"
  | Atom.LBrace `Tick2 -> "Atom.LBrace `Tick2"
  | Atom.LBraceHashRBrace -> "Atom.LBraceHashRBrace"
  | Atom.RBrace `Plain -> "Atom.RBrace `Plain"
  | Atom.RBrace `Tick2 -> "Atom.RBrace `Tick2"
  | Atom.Plus -> "Atom.Plus"
  | Atom.Plus2 -> "Atom.Plus2"
  | Atom.PlusEq -> "Atom.PlusEq"
  | Atom.PlusColon -> "Atom.PlusColon"
  | Atom.Minus -> "Atom.Minus"
  | Atom.MinusEq -> "Atom.MinusEq"
  | Atom.Star -> "Atom.Star"
  | Atom.StarEq -> "Atom.StarEq"
  | Atom.Slash -> "Atom.Slash"
  | Atom.SlashEq -> "Atom.SlashEq"
  | Atom.Backslash -> "Atom.Backslash"
  | Atom.Percent -> "Atom.Percent"
  | Atom.PercentEq -> "Atom.PercentEq"
  | Atom.Eq -> "Atom.Eq"
  | Atom.Eq2 -> "Atom.Eq2"
  | Atom.Amp -> "Atom.Amp"
  | Atom.Amp2 -> "Atom.Amp2"
  | Atom.Amp3 -> "Atom.Amp3"
  | Atom.AmpEq -> "Atom.AmpEq"
  | Atom.Up -> "Atom.Up"
  | Atom.UpEq -> "Atom.UpEq"
  | Atom.Bar -> "Atom.Bar"
  | Atom.Bar2 -> "Atom.Bar2"
  | Atom.BarEq -> "Atom.BarEq"
  | Atom.SPlus -> "Atom.SPlus"
  | Atom.SPlusEq -> "Atom.SPlusEq"
  | Atom.SMinus -> "Atom.SMinus"
  | Atom.SMinusEq -> "Atom.SMinusEq"

let atom_phrase_lit (atom : Atom.t phrase) : string =
  Printf.sprintf "{it = %s; at = no_region; note = ()}" (atom_it_lit atom.it)

let rec mixop_lit (mixop : Mixop.t) : string =
  match mixop with
  | Mixfix.Arg () -> "Mixfix.Arg ()"
  | Mixfix.Atom atom -> Printf.sprintf "Mixfix.Atom (%s)" (atom_phrase_lit atom)
  | Mixfix.Brack (open_a, inner, close_a) ->
      Printf.sprintf "Mixfix.Brack (%s, %s, %s)" (atom_phrase_lit open_a)
        (mixop_lit inner) (atom_phrase_lit close_a)
  | Mixfix.Infix (left, atom, right) ->
      Printf.sprintf "Mixfix.Infix (%s, %s, %s)" (mixop_lit left)
        (atom_phrase_lit atom) (mixop_lit right)
  | Mixfix.Seq parts ->
      Printf.sprintf "Mixfix.Seq [%s]"
        (String.concat "; " (List.map mixop_lit parts))

let mixop_expr (mixop : Mixop.t) : Ml.expr = Ml.LitE (mixop_lit mixop)

(* Runtime Atom.t/Mixop.t codegen, for unmarshalling --- *)

let atom_phrase_pat (atom : Atom.t phrase) : string =
  Printf.sprintf "{it = %s; _}" (atom_it_lit atom.it)

let rec mixop_pat_lit (mixop : Mixop.t) : string =
  match mixop with
  | Mixfix.Arg () -> "Mixfix.Arg _"
  | Mixfix.Atom atom -> Printf.sprintf "Mixfix.Atom (%s)" (atom_phrase_pat atom)
  | Mixfix.Brack (open_a, inner, close_a) ->
      Printf.sprintf "Mixfix.Brack (%s, %s, %s)" (atom_phrase_pat open_a)
        (mixop_pat_lit inner) (atom_phrase_pat close_a)
  | Mixfix.Infix (left, atom, right) ->
      Printf.sprintf "Mixfix.Infix (%s, %s, %s)" (mixop_pat_lit left)
        (atom_phrase_pat atom) (mixop_pat_lit right)
  | Mixfix.Seq parts ->
      Printf.sprintf "Mixfix.Seq [%s]"
        (String.concat "; " (List.map mixop_pat_lit parts))

(* Pattern for value Mixfix.t: binds each Arg node to a named variable.
   Returns (pattern_string, arg_var_names_in_left_to_right_order). *)
let mixop_value_arg_pat (mixop : Mixop.t) : string * string list =
  let counter = ref 0 in
  let args_rev = ref [] in
  let rec go = function
    | Mixfix.Arg () ->
        let i = !counter in
        incr counter;
        let name = "p" ^ string_of_int i in
        args_rev := name :: !args_rev;
        Printf.sprintf "Mixfix.Arg %s" name
    | Mixfix.Atom atom ->
        Printf.sprintf "Mixfix.Atom (%s)" (atom_phrase_pat atom)
    | Mixfix.Brack (open_a, inner, close_a) ->
        let s_inner = go inner in
        Printf.sprintf "Mixfix.Brack (%s, %s, %s)" (atom_phrase_pat open_a)
          s_inner (atom_phrase_pat close_a)
    | Mixfix.Infix (left, atom, right) ->
        let s_left = go left in
        let s_right = go right in
        Printf.sprintf "Mixfix.Infix (%s, %s, %s)" s_left (atom_phrase_pat atom)
          s_right
    | Mixfix.Seq parts ->
        Printf.sprintf "Mixfix.Seq [%s]"
          (String.concat "; " (List.map go parts))
  in
  let pat = go mixop in
  (pat, List.rev !args_rev)

(* BFS over all types reachable from function signatures *)

(* Transitive marshal/unmarshal dependency closure of [seeds] — the same sub-type
   edges [marshal_T]/[unmarshal_T] recurse along. On the typed path,
   marshal/unmarshal are reached only from [eval_program] (the program type) and
   the persisted state types, so generation is seeded from those instead of the
   whole func/rel I/O surface; everything outside this closure was dead. *)
let close_types (ctx : Ctx.t) (seeds : Sl.typ list) : Sl.typ list =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let queue : Sl.typ Queue.t = Queue.create () in
  let enqueue typ =
    let name = interface_name typ in
    if not (Hashtbl.mem seen name) then (
      Hashtbl.replace seen name ();
      Queue.push typ queue)
  in
  List.iter enqueue seeds;
  let result = ref [] in
  while not (Queue.is_empty queue) do
    let typ = Queue.pop queue in
    result := typ :: !result;
    match typ.it with
    | Il.BoolT | Il.NumT _ | Il.TextT | Il.FuncT _ -> ()
    | Il.TupleT typs -> List.iter enqueue typs
    | Il.IterT (t, _) -> enqueue t
    | Il.VarT (id, targs) -> (
        match Ctx.find_typdef_opt ctx id with
        | None
        | Some Typdef.Extern
        | Some Typdef.Param
        | Some (Typdef.Defining _) ->
            ()
        | Some (Typdef.Defined (tparams, deftyp))
          when List.length tparams = List.length targs -> (
            let theta = Mono.Specialize.build_theta tparams targs in
            let sub t = Mono.Subst.subst_typ theta t in
            match deftyp.it with
            | Il.PlainT t -> enqueue (sub t)
            | Il.StructT typfields ->
                List.iter (fun (_, t) -> enqueue (sub t)) typfields
            | Il.VariantT typcases ->
                List.iter
                  (fun (nottyp, _, _) ->
                    List.iter (fun t -> enqueue (sub t)) (Mixfix.args nottyp.it))
                  typcases)
        | _ -> ())
  done;
  List.rev !result

(* Substitution theta from a typedef's tparams and caller's targs *)

let build_theta ctx id targs =
  match Ctx.find_typdef_opt ctx id with
  | Some (Typdef.Defined (tparams, _) | Typdef.Defining tparams)
    when List.length tparams = List.length targs ->
      Mono.Specialize.build_theta tparams targs
  | _ -> Mono.Specialize.build_theta [] []

(* Module-level constant pool for hoisting marshal templates out of function bodies *)

type const_pool = {
  mutable consts : (string * Ml.expr) list;
  seen_mixops : (string, string) Hashtbl.t;
  seen_typs : (string, string) Hashtbl.t;
  mutable ctr : int;
}

let make_pool () =
  {
    consts = [];
    seen_mixops = Hashtbl.create 128;
    seen_typs = Hashtbl.create 128;
    ctr = 0;
  }

let intern_mixop (pool : const_pool) (mixop : Mixop.t) : string =
  let key = mixop_lit mixop in
  match Hashtbl.find_opt pool.seen_mixops key with
  | Some n -> n
  | None ->
      let n = Printf.sprintf "_mo_%d_" pool.ctr in
      pool.ctr <- pool.ctr + 1;
      Hashtbl.replace pool.seen_mixops key n;
      pool.consts <- (n, mixop_expr mixop) :: pool.consts;
      n

let intern_typ (pool : const_pool) (key : string) (expr : Ml.expr) : string =
  match Hashtbl.find_opt pool.seen_typs key with
  | Some n -> n
  | None ->
      let n = "_ty_" ^ key ^ "_" in
      Hashtbl.replace pool.seen_typs key n;
      pool.consts <- (n, expr) :: pool.consts;
      n

(* Marshalling *)

module Marshal = struct
  (* Booleans *)

  let compile_bool = Ml.AppE (Ml.LitE "Value.Make.bool", [ Ml.VarE "x" ])

  (* Numbers *)

  let compile_num = function
    | `NatT -> Ml.AppE (Ml.LitE "Value.Make.nat", [ Ml.VarE "x" ])
    | `IntT -> Ml.AppE (Ml.LitE "Value.Make.int", [ Ml.VarE "x" ])

  (* Texts *)

  let expr_text_ml = Ml.AppE (Ml.LitE "Value.Make.text", [ Ml.VarE "x" ])

  (* Variable types *)

  (* Structs *)

  let compile_struct (typ_ref : string) (subst : Sl.typ -> Sl.typ)
      (typfields : Sl.typfield list) : Ml.expr =
    let field_exprs =
      List.map
        (fun (atom, t) ->
          let atom_str = Names.Ctor.atom atom in
          let ocaml_field = Names.field atom in
          Ml.TupleE
            [
              Ml.AppE (Ml.LitE "make_atom_", [ Ml.StrE atom_str ]);
              Ml.AppE
                ( Ml.VarE ("marshal_" ^ interface_name (subst t)),
                  [ Ml.FieldE (Ml.VarE "x", ocaml_field) ] );
            ])
        typfields
    in
    Ml.AppE (Ml.LitE "Value.Make.str", [ Ml.VarE typ_ref; Ml.ListE field_exprs ])

  (* Variants *)

  let compile_variant (ctx : Ctx.t) (id : Sl.id) (typ_ref : string)
      (subst : Sl.typ -> Sl.typ) (pool : const_pool) : Ml.expr =
    let ctors_info = Ctx.find_ctors_full ctx id in
    let arms =
      List.map
        (fun (mixop, ctor_ml, payload_typs) ->
          let payload_typs' = List.map subst payload_typs in
          let pvars =
            List.mapi (fun i _ -> "p_" ^ string_of_int i) payload_typs'
          in
          let pat =
            Ml.VariantP (`Poly (ctor_ml, List.map (fun v -> Ml.VarP v) pvars))
          in
          let marshal_calls =
            List.map2
              (fun t pvar ->
                Ml.AppE
                  (Ml.VarE ("marshal_" ^ interface_name t), [ Ml.VarE pvar ]))
              payload_typs' pvars
          in
          let mo_ref = intern_mixop pool mixop in
          ( pat,
            Ml.AppE
              ( Ml.LitE "make_case_",
                [ Ml.VarE mo_ref; Ml.ListE marshal_calls; Ml.VarE typ_ref ] ) ))
        ctors_info
    in
    Ml.MatchE (Ml.VarE "x", arms)

  let compile_var (ctx : Ctx.t) (id : Sl.id) (targs : Sl.targ list)
      (typ_ref : string) (pool : const_pool) : Ml.expr =
    let theta = build_theta ctx id targs in
    let subst typ = Mono.Subst.subst_typ theta typ in
    let td = Ctx.find_typdef ctx id in
    match td with
    | Typdef.Param | Typdef.Defining _ -> Ml.UnitE
    | Typdef.Defined (_, deftyp) -> (
        match deftyp.it with
        | Il.PlainT typ_alias ->
            Ml.AppE
              ( Ml.VarE ("marshal_" ^ interface_name (subst typ_alias)),
                [ Ml.VarE "x" ] )
        | Il.StructT typfields -> compile_struct typ_ref subst typfields
        | Il.VariantT _ -> compile_variant ctx id typ_ref subst pool)
    | Typdef.Extern ->
        Ml.AppE (Ml.LitE "Value.Make.extern", [ Ml.VarE typ_ref; Ml.VarE "x" ])

  (* Tuples *)

  let compile_tuple (typ_ref : string) (typs : Sl.typ list) : Ml.expr =
    let vars = List.mapi (fun i _ -> "x" ^ string_of_int i) typs in
    let marshal_calls =
      List.map2
        (fun t v ->
          Ml.AppE (Ml.VarE ("marshal_" ^ interface_name t), [ Ml.VarE v ]))
        typs vars
    in
    Ml.LetE
      ( Ml.TupleP (List.map (fun v -> Ml.VarP v) vars),
        Ml.VarE "x",
        Ml.AppE
          ( Ml.LitE "Value.Make.tuple",
            [ Ml.VarE typ_ref; Ml.ListE marshal_calls ] ) )

  (* Iterations *)

  let compile_iter_opt (typ_ref : string) (t : Sl.typ) : Ml.expr =
    Ml.AppE
      ( Ml.LitE "Value.Make.opt",
        [
          Ml.VarE typ_ref;
          Ml.AppE
            ( Ml.LitE "Option.map",
              [ Ml.VarE ("marshal_" ^ interface_name t); Ml.VarE "x" ] );
        ] )

  let compile_iter_list (typ_ref : string) (t : Sl.typ) : Ml.expr =
    Ml.AppE
      ( Ml.LitE "Value.Make.list",
        [
          Ml.VarE typ_ref;
          Ml.AppE
            ( Ml.LitE "List.map",
              [ Ml.VarE ("marshal_" ^ interface_name t); Ml.VarE "x" ] );
        ] )

  let compile_iter (typ_ref : string) (typ : Sl.typ) (iter : Sl.iter) : Ml.expr
      =
    match iter with
    | Opt -> compile_iter_opt typ_ref typ
    | List -> compile_iter_list typ_ref typ

  let compile_body (ctx : Ctx.t) (pool : const_pool) (typ : Sl.typ) : Ml.expr =
    match typ.it with
    | Il.BoolT -> compile_bool
    | Il.NumT numtyp -> compile_num numtyp
    | Il.TextT -> expr_text_ml
    | Il.FuncT _ -> Ml.UnitE
    | _ -> (
        let typ_ref =
          intern_typ pool (interface_name typ) (typ_make_expr typ)
        in
        match typ.it with
        | Il.VarT (id, targs) -> compile_var ctx id targs typ_ref pool
        | Il.TupleT typs -> compile_tuple typ_ref typs
        | Il.IterT (t, iter) -> compile_iter typ_ref t iter
        | _ -> assert false)

  let compile (ctx : Ctx.t) (pool : const_pool) (typ : Sl.typ) : Ml.funcdef =
    let name = "marshal_" ^ interface_name typ in
    let ml_typ = Type.compile_typ ~tparams:[] typ in
    let body = compile_body ctx pool typ in
    (name, [ ("x", Some ml_typ) ], Some (Ml.NameT "Value.t"), body)
end

(* Unmarshalling *)

module Unmarshal = struct
  (* Booleans *)

  let compile_bool = Ml.AppE (Ml.LitE "Value.Get.bool", [ Ml.VarE "v" ])

  (* Numbers *)

  let compile_num =
    Ml.MatchE
      ( Ml.AppE (Ml.LitE "Value.Get.num", [ Ml.VarE "v" ]),
        [
          (Ml.VariantP (`Poly ("Nat", [ Ml.VarP "n_" ])), Ml.VarE "n_");
          (Ml.VariantP (`Poly ("Int", [ Ml.VarP "i_" ])), Ml.VarE "i_");
        ] )

  (* Texts *)

  let expr_text_ml = Ml.AppE (Ml.LitE "Value.Get.text", [ Ml.VarE "v" ])

  (* Variable typs *)

  (* Structs *)

  let compile_struct (sub : Sl.typ -> Sl.typ) (typfields : Sl.typfield list) :
      Ml.expr =
    let field_bindings =
      List.map
        (fun (atom, t) ->
          let atom_str = Names.Ctor.atom atom in
          let ocaml_field = Names.field atom in
          ( ocaml_field,
            Ml.AppE
              ( Ml.VarE ("unmarshal_" ^ interface_name (sub t)),
                [
                  Ml.AppE
                    ( Ml.LitE "get_field_",
                      [ Ml.VarE "fields_"; Ml.StrE atom_str ] );
                ] ) ))
        typfields
    in
    Ml.LetE
      ( Ml.VarP "fields_",
        Ml.AppE (Ml.LitE "Value.Get.str", [ Ml.VarE "v" ]),
        Ml.RecordE field_bindings )

  (* Variants *)

  let compile_variant (ctx : Ctx.t) (id : Sl.id) (name : string)
      (sub : Sl.typ -> Sl.typ) : Ml.expr =
    let ctors_info = Ctx.find_ctors_full ctx id in
    let arms_ctor_ml =
      List.map
        (fun (mixop, ctor_ml, payload_typs) ->
          let payload_typs' = List.map sub payload_typs in
          let pat_str, ids_arg_ml = mixop_value_arg_pat mixop in
          let exprs_payload_ml =
            List.map2
              (fun typ id_arg_ml ->
                Ml.AppE
                  ( Ml.VarE ("unmarshal_" ^ interface_name typ),
                    [ Ml.VarE id_arg_ml ] ))
              payload_typs' ids_arg_ml
          in
          (Ml.LitP pat_str, Ml.VariantE (ctor_ml, exprs_payload_ml)))
        ctors_info
    in
    let arm_wild_ml =
      ( Ml.WildP,
        Common.raise_unmatch (Printf.sprintf "unmarshal_%s: unknown case" name)
      )
    in
    Ml.MatchE
      ( Ml.FieldE (Ml.VarE "v", "it"),
        [
          ( Ml.VariantP (`Mono ("CaseV", [ Ml.VarP "vc_" ])),
            Ml.MatchE (Ml.VarE "vc_", arms_ctor_ml @ [ arm_wild_ml ]) );
          (Ml.WildP, Common.raise_unmatch ("unmarshal_" ^ name));
        ] )

  let compile_var (ctx : Ctx.t) (id : Sl.id) (targs : Sl.targ list)
      (name : string) : Ml.expr =
    let theta = build_theta ctx id targs in
    let subst typ = Mono.Subst.subst_typ theta typ in
    let td = Ctx.find_typdef ctx id in
    match td with
    | Typdef.Param | Typdef.Defining _ ->
        Common.raise_unmatch ("unmarshal_" ^ name)
    | Typdef.Defined (_, deftyp) -> (
        match deftyp.it with
        | Il.PlainT typ_alias ->
            Ml.AppE
              ( Ml.VarE ("unmarshal_" ^ interface_name (subst typ_alias)),
                [ Ml.VarE "v" ] )
        | Il.StructT typfields -> compile_struct subst typfields
        | Il.VariantT _ -> compile_variant ctx id name subst)
    | Typdef.Extern -> Ml.AppE (Ml.LitE "Value.Get.extern", [ Ml.VarE "v" ])

  (* Tuples *)

  let compile_tuple (name : string) (typs : Sl.typ list) : Ml.expr =
    let n = List.length typs in
    let vars = List.init n (fun i -> "v" ^ string_of_int i) in
    let unmarshal_calls =
      List.mapi
        (fun i t ->
          Ml.AppE
            ( Ml.VarE ("unmarshal_" ^ interface_name t),
              [ Ml.VarE (List.nth vars i) ] ))
        typs
    in
    Ml.MatchE
      ( Ml.AppE (Ml.LitE "Value.Get.tuple", [ Ml.VarE "v" ]),
        [
          ( Ml.ListP (List.map (fun v -> Ml.VarP v) vars),
            Ml.TupleE unmarshal_calls );
          (Ml.WildP, Common.raise_unmatch ("unmarshal_" ^ name));
        ] )

  (* Iterations *)

  let compile_iter_opt (t : Sl.typ) : Ml.expr =
    Ml.MatchE
      ( Ml.AppE (Ml.LitE "Value.Get.opt", [ Ml.VarE "v" ]),
        [
          (Ml.OptP None, Ml.OptE None);
          ( Ml.OptP (Some (Ml.VarP "v_inner_")),
            Ml.OptE
              (Some
                 (Ml.AppE
                    ( Ml.VarE ("unmarshal_" ^ interface_name t),
                      [ Ml.VarE "v_inner_" ] ))) );
        ] )

  let compile_iter_list (t : Sl.typ) : Ml.expr =
    Ml.AppE
      ( Ml.LitE "List.map",
        [
          Ml.VarE ("unmarshal_" ^ interface_name t);
          Ml.AppE (Ml.LitE "Value.Get.list", [ Ml.VarE "v" ]);
        ] )

  let compile_iter (t : Sl.typ) (iter : Sl.iter) : Ml.expr =
    match iter with Opt -> compile_iter_opt t | List -> compile_iter_list t

  let compile_body (ctx : Ctx.t) (typ : Sl.typ) : Ml.expr =
    let name = interface_name typ in
    match typ.it with
    | Il.BoolT -> compile_bool
    | Il.NumT _ -> compile_num
    | Il.TextT -> expr_text_ml
    | Il.VarT (id, targs) -> compile_var ctx id targs name
    | Il.TupleT typs -> compile_tuple name typs
    | Il.IterT (t, iter) -> compile_iter t iter
    | Il.FuncT _ -> Common.raise_unmatch "unmarshal_func"

  let compile (ctx : Ctx.t) (typ : Sl.typ) : Ml.funcdef =
    let name = "unmarshal_" ^ interface_name typ in
    let ml_typ = Type.compile_typ ~tparams:[] typ in
    let body = compile_body ctx typ in
    (name, [ ("v", Some (Ml.NameT "Value.t")) ], Some ml_typ, body)
end

(* Shallow typed mixop bridges *)

(* Two functions emitted into the compiled output, used by [V_native]:
   - [make_case_typed]: the typed mirror of [make_case_] (which builds a
     [Value.t]). Given a canonical mixop string, an [Obj.t list] of already-typed
     args, and the raw spec type id, it builds the typed variant directly
     ([Obj.repr (`Ctor (..))]) with O(1) [Obj.obj] casts — no marshalling.
   - [case_of_typed]: the shallow inverse of [make_case_typed]. It projects a
     typed variant into its one-level [Obj.t Mixfix.t] shell, leaving the args as
     [Obj.t] (un-recursed), so it never reintroduces deep conversion.

   Scope: only non-generic [VarT] whose typedef is a [VariantT] — the value
   shapes the externs construct/inspect. Keyed at runtime by the raw spec type id
   ([id.it], what the externs pass), matched against the OCaml type ([var_of_id
   id]). The inner key is the canonical mixop string ([Mixop.string_of_mixop]). *)
module Typed = struct
  let variant_ids (ctx : Ctx.t) (typs : Sl.typ list) : (Sl.id * Sl.typ) list =
    List.filter_map
      (fun typ ->
        match typ.it with
        | Il.VarT (id, []) -> (
            match Ctx.find_typdef ctx id with
            | Typdef.Defined ([], deftyp) -> (
                match deftyp.it with
                | Il.VariantT _ -> Some (id, typ)
                | _ -> None)
            | _ -> None)
        | _ -> None)
      typs

  (* [Obj.obj (List.nth args i)] *)
  let obj_obj_nth (i : int) : Ml.expr =
    Ml.AppE
      ( Ml.LitE "Obj.obj",
        [
          Ml.AppE
            (Ml.LitE "List.nth", [ Ml.VarE "args"; Ml.LitE (string_of_int i) ]);
        ] )

  (* The bridges dispatch on a value's spec type, threaded from the (hand-written,
     [SAFE]-generic) extern as a structured [Il.typ] — not a bare string (which was
     fragile: a stale typename only failed at runtime). The per-type arms still key
     on the type's name, so we extract [id.it] from the [VarT] head once at the top
     and let the existing string-keyed match stand:
       [match typ.it with Il.VarT (id, _) -> id.it | _ -> ""]. *)
  let typename_of_expr : Ml.expr =
    Ml.MatchE
      ( Ml.FieldE (Ml.VarE "typ", "it"),
        [
          ( Ml.VariantP (`Mono ("Il.VarT", [ Ml.VarP "id"; Ml.WildP ])),
            Ml.FieldE (Ml.VarE "id", "it") );
          (Ml.WildP, Ml.StrE "");
        ] )

  (* [set]/[pair]/[map]/[res] are parametric poly-variants ([`Set of 'k list],
     [`Pair of 'k * 'v], [map = pair set], [`OK_X of 'x | `FAIL]) absent from
     [variant_ids]: their decls carry tparams, so the normal arm-builder's
     per-payload [compile_typ] annotation would dangle a free type var. But
     the ctor is type-uniform — 'k/'v/'x erase to [Obj.t] — so ONE
     annotation-free arm per head covers every instantiation. The real ctor +
     mixop come from [Ctx] (keys then match the spec, hence [V_native]'s
     threaded mixop); [map]'s value is a [`Set] of pairs, so it reuses the
     "set" ctor. [res] is included for the same reason as the other three —
     FINDINGS.md §2c: [valres]/[valsres] (= [res<val>]/[res<val*>]) are
     concrete aliases of the parametric [res<X>], and nothing in
     [all_typ_refs]'s closure unfolds through a parametric alias target, so
     they never got a [case_of_typed]/[make_case_typed] arm despite being
     exactly as type-uniform as [set]/[pair]/[map]. *)
  let parametric_heads = [ "set"; "pair"; "map"; "res" ]

  let parametric_ctors (ctx : Ctx.t) (head : string) =
    let src = if head = "map" then "set" else head in
    Ctx.find_ctors_full ctx (src $ no_region)

  (* set/map -> [Obj.t set], pair -> [(Obj.t, Obj.t) pair], res -> [Obj.t res]. *)
  let parametric_scrut_typ (head : string) : Ml.typ =
    match head with
    | "pair" -> Ml.AppT ("pair", [ Ml.NameT "Obj.t"; Ml.NameT "Obj.t" ])
    | "res" -> Ml.AppT ("res", [ Ml.NameT "Obj.t" ])
    | _ -> Ml.AppT ("set", [ Ml.NameT "Obj.t" ])

  let make_parametric_arms (ctx : Ctx.t) : Ml.arm list =
    List.map
      (fun head ->
        let inner_arms =
          List.map
            (fun (mixop, ctor_ml, payload_typs) ->
              let canon = Mixop.string_of_mixop mixop in
              let args = List.mapi (fun i _ -> obj_obj_nth i) payload_typs in
              ( Ml.LitP (Printf.sprintf "%S" canon),
                Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VariantE (ctor_ml, args) ]) ))
            (parametric_ctors ctx head)
        in
        let wild =
          ( Ml.WildP,
            Ml.AppE
              ( Ml.LitE "failwith",
                [
                  Ml.BinopE
                    ( "^",
                      Ml.StrE ("make_case_typed: bad mixop for " ^ head ^ ": "),
                      Ml.VarE "mixop" );
                ] ) )
        in
        ( Ml.LitP (Printf.sprintf "%S" head),
          Ml.MatchE (Ml.VarE "mixop", inner_arms @ [ wild ]) ))
      parametric_heads

  let case_parametric_arms (ctx : Ctx.t) (pool : const_pool) : Ml.arm list =
    List.map
      (fun head ->
        let inner_arms =
          List.map
            (fun (mixop, ctor_ml, payload_typs) ->
              let pvars =
                List.mapi (fun i _ -> "p" ^ string_of_int i) payload_typs
              in
              let pat =
                Ml.VariantP
                  (`Poly (ctor_ml, List.map (fun v -> Ml.VarP v) pvars))
              in
              let mo_ref = intern_mixop pool mixop in
              let repr_args =
                List.map
                  (fun v -> Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE v ]))
                  pvars
              in
              ( pat,
                Ml.AppE
                  (Ml.LitE "Mixfix.fill", [ Ml.VarE mo_ref; Ml.ListE repr_args ])
              ))
            (parametric_ctors ctx head)
        in
        let scrut =
          Ml.AnnotE
            ( Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]),
              parametric_scrut_typ head )
        in
        (Ml.LitP (Printf.sprintf "%S" head), Ml.MatchE (scrut, inner_arms)))
      parametric_heads

  let compile_make_case (ctx : Ctx.t) (variants : (Sl.id * Sl.typ) list) :
      Ml.funcdef =
    let outer_arms =
      List.map
        (fun (id, _typ) ->
          let ctors = Ctx.find_ctors_full ctx id in
          let inner_arms =
            List.map
              (fun (mixop, ctor_ml, payload_typs) ->
                let canon = Mixop.string_of_mixop mixop in
                let arg_exprs =
                  List.mapi
                    (fun i pt ->
                      Ml.AnnotE (obj_obj_nth i, Type.compile_typ ~tparams:[] pt))
                    payload_typs
                in
                ( Ml.LitP (Printf.sprintf "%S" canon),
                  Ml.AppE
                    (Ml.LitE "Obj.repr", [ Ml.VariantE (ctor_ml, arg_exprs) ])
                ))
              ctors
          in
          let inner_wild =
            ( Ml.WildP,
              Ml.AppE
                ( Ml.LitE "failwith",
                  [
                    Ml.BinopE
                      ( "^",
                        Ml.StrE
                          ("make_case_typed: bad mixop for " ^ id.it ^ ": "),
                        Ml.VarE "mixop" );
                  ] ) )
          in
          ( Ml.LitP (Printf.sprintf "%S" id.it),
            Ml.MatchE (Ml.VarE "mixop", inner_arms @ [ inner_wild ]) ))
        variants
    in
    let outer_wild =
      ( Ml.WildP,
        Ml.AppE
          ( Ml.LitE "failwith",
            [
              Ml.BinopE
                ("^", Ml.StrE "make_case_typed: unknown typ ", Ml.VarE "typ");
            ] ) )
    in
    ( "make_case_typed",
      [
        ("mixop", Some (Ml.NameT "Il.mixop"));
        ("args", Some (Ml.AppT ("list", [ Ml.NameT "Obj.t" ])));
        ("typ", Some (Ml.NameT "Il.typ"));
      ],
      Some (Ml.NameT "Obj.t"),
      Ml.LetE
        ( Ml.VarP "typ",
          typename_of_expr,
          Ml.LetE
            ( Ml.VarP "mixop",
              Ml.AppE (Ml.LitE "Mixop.string_of_mixop", [ Ml.VarE "mixop" ]),
              Ml.MatchE
                ( Ml.VarE "typ",
                  outer_arms @ make_parametric_arms ctx @ [ outer_wild ] ) ) )
    )

  let compile_case_of (ctx : Ctx.t) (pool : const_pool)
      (variants : (Sl.id * Sl.typ) list) : Ml.funcdef =
    let outer_arms =
      List.map
        (fun (id, typ) ->
          let ctors = Ctx.find_ctors_full ctx id in
          let inner_arms =
            List.map
              (fun (mixop, ctor_ml, payload_typs) ->
                let pvars =
                  List.mapi (fun i _ -> "p" ^ string_of_int i) payload_typs
                in
                let pat =
                  Ml.VariantP
                    (`Poly (ctor_ml, List.map (fun v -> Ml.VarP v) pvars))
                in
                let mo_ref = intern_mixop pool mixop in
                let repr_args =
                  List.map
                    (fun v -> Ml.AppE (Ml.LitE "Obj.repr", [ Ml.VarE v ]))
                    pvars
                in
                ( pat,
                  Ml.AppE
                    ( Ml.LitE "Mixfix.fill",
                      [ Ml.VarE mo_ref; Ml.ListE repr_args ] ) ))
              ctors
          in
          let scrut =
            Ml.AnnotE
              ( Ml.AppE (Ml.LitE "Obj.obj", [ Ml.VarE "x" ]),
                Type.compile_typ ~tparams:[] typ )
          in
          (Ml.LitP (Printf.sprintf "%S" id.it), Ml.MatchE (scrut, inner_arms)))
        variants
    in
    let outer_wild =
      ( Ml.WildP,
        Ml.AppE
          ( Ml.LitE "failwith",
            [
              Ml.BinopE
                ("^", Ml.StrE "case_of_typed: unknown typ ", Ml.VarE "typ");
            ] ) )
    in
    ( "case_of_typed",
      [ ("x", Some (Ml.NameT "Obj.t")); ("typ", Some (Ml.NameT "Il.typ")) ],
      Some (Ml.AppT ("Mixfix.t", [ Ml.NameT "Obj.t" ])),
      Ml.LetE
        ( Ml.VarP "typ",
          typename_of_expr,
          Ml.MatchE
            ( Ml.VarE "typ",
              outer_arms @ case_parametric_arms ctx pool @ [ outer_wild ] ) ) )

  let compile (ctx : Ctx.t) (pool : const_pool) (typs : Sl.typ list) :
      Ml.funcdef list =
    let variants = variant_ids ctx typs in
    [ compile_make_case ctx variants; compile_case_of ctx pool variants ]
end

(* Direct dependencies of marshal/unmarshal for a given type:
   the sub-types that marshal_T calls marshal_S for. *)
let interface_typ_deps (ctx : Ctx.t) (typ : Sl.typ) : Sl.typ list =
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT | Il.FuncT _ -> []
  | Il.TupleT typs -> typs
  | Il.IterT (t, _) -> [ t ]
  | Il.VarT (id, targs) -> (
      let theta = build_theta ctx id targs in
      let sub t = Mono.Subst.subst_typ theta t in
      match Ctx.find_typdef ctx id with
      | Typdef.Param | Typdef.Defining _ | Typdef.Extern -> []
      | Typdef.Defined (_, deftyp) -> (
          match deftyp.it with
          | Il.PlainT t -> [ sub t ]
          | Il.StructT typfields -> List.map (fun (_, t) -> sub t) typfields
          | Il.VariantT typcases ->
              List.concat_map
                (fun (nottyp, _, _) ->
                  List.map sub (Domain.Mixfix.args nottyp.it))
                typcases))

(* Compute SCCs on the marshal/unmarshal call graph and return groups in
   topological order (dependencies first). Each group becomes one Ml.LetRec. *)
let compute_groups (ctx : Ctx.t) (typs : Sl.typ list) : Sl.typ list list =
  let n = List.length typs in
  if n = 0 then []
  else
    let typs_arr = Array.of_list typs in
    let name_idx : (string, int) Hashtbl.t = Hashtbl.create (n * 2) in
    Array.iteri
      (fun i typ -> Hashtbl.replace name_idx (interface_name typ) i)
      typs_arr;
    let adj = Array.make n [] in
    Array.iteri
      (fun i typ ->
        let deps = interface_typ_deps ctx typ in
        let edges : (int, unit) Hashtbl.t = Hashtbl.create 4 in
        List.iter
          (fun dep ->
            let dep_name = interface_name dep in
            match Hashtbl.find_opt name_idx dep_name with
            | Some j when j <> i -> Hashtbl.replace edges j ()
            | _ -> ())
          deps;
        adj.(i) <- Hashtbl.fold (fun j () acc -> j :: acc) edges [])
      typs_arr;
    let sccs = Scc.Tarjan.tarjan n adj in
    List.map (fun scc -> List.map (fun i -> typs_arr.(i)) scc) sccs

(* All non-parametric type references declared in the spec. The typed mixop
   bridges ([make_case_typed]/[case_of_typed]) must cover every variant type an
   extern may construct or inspect — not just the marshal-reachable closure,
   because extern-constructed result types (e.g. [returnResult], [callResult])
   need not appear as a marshaled field of any seed type and so are absent from
   that closure. [Typed.variant_ids] filters these to the variant typedefs. *)
let all_typ_refs (spec : Sl.spec) : Sl.typ list =
  List.filter_map
    (fun (def : Sl.def) ->
      match def.it with
      | Sl.TypD (id, [], _, _) -> Some (Il.VarT (id, []) $ id.at)
      | _ -> None)
    spec

(* [marshal_typed]/[unmarshal_typed]: the per-type [V_native] persist bridge.
   Dispatched by matching the value's spec type [Typ.t] directly — call sites pass
   the type they already hold (backend-sim's [Typs.*], the builtins' element-type
   targ), no string convention. Total over the marshal closure [typs]: every named
   ([VarT]) closure type gets a marshal and unmarshal arm. No curated entry-point
   list, so a new persist/builtin marshal target needs no codegen change. *)
let compile_marshal_dispatch (typs : Sl.typ list) : Ml.funcdef list =
  let keys =
    List.filter_map
      (fun (t : Sl.typ) ->
        match t.it with
        | Il.VarT (id, _) -> Some (id.it, interface_name t)
        | _ -> None)
      typs
    |> List.sort_uniq compare
  in
  (* Match [typ.it] against the named-type constructor for each closure type. *)
  let scrut = Ml.FieldE (Ml.VarE "typ", "it") in
  let var_pat key =
    Ml.LitP (Printf.sprintf "Il.VarT ({ it = %S; _ }, _)" key)
  in
  let marshal_arms =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.VarE ("marshal_" ^ iname),
              [ Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE "x" ]) ] ) ))
      keys
  in
  let unmarshal_arms =
    List.map
      (fun (key, iname) ->
        ( var_pat key,
          Ml.AppE
            ( Ml.LitE "Obj.repr",
              [ Ml.AppE (Ml.VarE ("unmarshal_" ^ iname), [ Ml.VarE "v" ]) ] ) ))
      keys
  in
  let wild name =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "failwith",
          [
            Ml.BinopE
              ( "^",
                Ml.StrE (name ^ ": unknown type "),
                Ml.AppE (Ml.LitE "Typ.to_string", [ Ml.VarE "typ" ]) );
          ] ) )
  in
  [
    ( "marshal_typed",
      [ ("typ", Some (Ml.NameT "Typ.t")); ("x", Some (Ml.NameT "Obj.t")) ],
      Some (Ml.NameT "Value.t"),
      Ml.MatchE (scrut, marshal_arms @ [ wild "marshal_typed" ]) );
    ( "unmarshal_typed",
      [ ("typ", Some (Ml.NameT "Typ.t")); ("v", Some (Ml.NameT "Value.t")) ],
      Some (Ml.NameT "Obj.t"),
      Ml.MatchE (scrut, unmarshal_arms @ [ wild "unmarshal_typed" ]) );
  ]

let compile (ctx : Ctx.t) (spec : Sl.spec) ~(tid_program : string) :
    Ml.toplevel list
    * Ml.funcdef list list
    * Ml.funcdef list list
    * Ml.funcdef list =
  let all_refs = all_typ_refs spec in
  ignore tid_program;
  (* Marshal/unmarshal are generated for every non-parametric type declared in
     the spec ([all_refs]) — the same closure [make_case_typed]/[case_of_typed]
     already use (see [all_typ_refs]'s comment). This used to be seeded from
     just [eval_program]'s program type plus the types backend-sim persists,
     as a code-size optimization. But [V.marshal] has a second caller besides
     backend-sim's persistence: [builtin/maps.ml]'s [eq_v] calls it to derive
     element equality for ANY map/set key type, and a spec can have map/set
     keys that never reach that narrower persist closure — e.g. spec-meta/sl's
     own [venv = map<varr, val>], whose key type [varr] is pure interpreter
     bookkeeping, never part of the target program's AST or persisted state.
     That miss failed at runtime ([marshal_typed: unknown type varr]) instead
     of never existing. *)
  let typs = close_types ctx all_refs in
  let groups = compute_groups ctx typs in
  let pool = make_pool () in
  let marshal_groups = List.map (List.map (Marshal.compile ctx pool)) groups in
  let unmarshal_groups = List.map (List.map (Unmarshal.compile ctx)) groups in
  (* Typed mixop bridges over ALL spec variant types (see [all_typ_refs]).
     Reuses [pool] so [case_of_typed]'s interned mixops join [const_decls]. *)
  let typed_bridges =
    Typed.compile ctx pool all_refs @ compile_marshal_dispatch typs
  in
  let const_decls = List.rev_map (fun (n, e) -> Ml.Let (n, e)) pool.consts in
  (const_decls, marshal_groups, unmarshal_groups, typed_bridges)
