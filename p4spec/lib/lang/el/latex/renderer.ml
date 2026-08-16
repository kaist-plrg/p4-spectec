open Domain
open Domain.Lib
open Ast
open Util.Source
open Tex

(* Helpers *)

(* Parenthesize

   x -> (x) *)

let tex_of_parenthesized (tex : Doc.t) : Doc.t = Doc.delimited Paren tex

(* Breakable infix layout

   x + y -> x + y or x newline-indented + y *)

let tex_of_breakable_infix (tex_l : Doc.t) (tex_op : Doc.t) (tex_r : Doc.t) :
    Doc.t =
  if Doc.is_empty tex_l || Doc.is_empty tex_op || Doc.is_empty tex_r then
    Doc.concat_spaced [ tex_l; tex_op; tex_r ]
  else
    let tex_continuation =
      Doc.concat [ Doc.soft_space; tex_op; Doc.space; tex_r ]
    in
    let tex_continuation = Doc.nest 4 tex_continuation in
    let tex_infix = Doc.concat [ tex_l; tex_continuation ] in
    Doc.layout_group tex_infix

(* Separated blocks

   x; y; z -> x; y; z *)

let rec tex_of_separated_blocks (blocks : Doc.block list) : Doc.block list =
  match blocks with
  | [] -> []
  | [ block ] -> [ block ]
  | block :: blocks -> block :: Doc.gap :: tex_of_separated_blocks blocks

(* Links

   eval(x) -> \href{#eval}{\mathrm{eval}}\left(\mathsf{x}\right) *)

let tex_of_link (s_anchor_opt : string option) (tex : Doc.t) : Doc.t =
  match s_anchor_opt with
  | None -> tex
  | Some s_anchor ->
      let target = Link.target_of_string s_anchor in
      Link.link_unowned_doc target tex

(* Identifiers

   TC_0 -> {\mathsf{TC}}_{\mathsf{0}}
   find_type -> \mathrm{find\_type} *)

let tex_of_varid (id_var : id) : Doc.t =
  if Id.is_underscored id_var then Doc.styled_mathsf "_"
  else
    match String.split_on_char '_' id_var.it with
    | [] -> Error.error id_var.at "empty variable identifier decomposition"
    | [ var_type ] -> Doc.styled_mathsf var_type
    | var_type :: var_subscripts ->
        let tex_b = Doc.styled_mathsf var_type in
        let s_subscript = String.concat "_" var_subscripts in
        let tex_sub = Doc.styled_mathsf s_subscript in
        Doc.subscript tex_b tex_sub

let tex_of_typid (id_typ : id) : Doc.t = Doc.styled_mathsf id_typ.it
let tex_of_defid (id_def : id) : Doc.t = Doc.styled_mathrm id_def.it

(* Numbers

   -42 -> -42
   0x10000000000000000 -> \mathtt{0x10000000000000000} *)

let tex_of_number (numop : numop) (num : num) : Doc.t =
  let n_abs, tex_sign =
    match num with
    | `Nat n -> (n, Doc.empty)
    | `Int n when Bigint.(n < zero) -> (Bigint.abs n, Doc.fixed Minus)
    | `Int n -> (n, Doc.fixed Plus)
  in
  let tex_abs =
    match numop with
    | `DecOp -> Doc.decimal n_abs
    | `HexOp -> Doc.hexadecimal n_abs
  in
  Doc.concat [ tex_sign; tex_abs ]

(* Atoms

   Tag -> ""
   ArrowSub n -> {\to}_{\mathsf{n}} *)

type atom_interm = EmptyAtom | PlainAtom of Doc.t | SubscriptedAtom of Doc.t

let render_atom (atom : Atom.t) : atom_interm =
  match atom with
  | Atom.Keyword s_keyword -> PlainAtom (Doc.styled_mathsf s_keyword)
  | Atom.Tag _ -> EmptyAtom
  | Atom.Operator op -> PlainAtom (Doc.mathbin (Doc.styled_mathtt op))
  | Atom.Sub ->
      PlainAtom (Doc.mathrel (Doc.concat [ Doc.fixed Less; Doc.fixed Colon ]))
  | Atom.Sup ->
      PlainAtom
        (Doc.mathrel (Doc.concat [ Doc.fixed Colon; Doc.fixed Greater ]))
  | Atom.Turnstile -> PlainAtom (Doc.mathrel (Doc.fixed Turnstile))
  | Atom.Tilesturn -> PlainAtom (Doc.mathrel (Doc.fixed Tilesturn))
  | Atom.Arrow -> PlainAtom (Doc.fixed To)
  | Atom.ArrowSub -> SubscriptedAtom (Doc.fixed To)
  | Atom.DoubleArrowSub -> SubscriptedAtom (Doc.fixed Rightarrow)
  | Atom.DoubleArrowLong -> PlainAtom (Doc.fixed Longrightarrow)
  | Atom.SqArrow -> PlainAtom (Doc.fixed Hookrightarrow)
  | Atom.SqArrowStar ->
      PlainAtom (Doc.superscript (Doc.fixed Hookrightarrow) (Doc.fixed Ast))
  | Atom.Dot -> PlainAtom (Doc.group (Doc.fixed Dot))
  | Atom.Dot2 -> PlainAtom (Doc.fixed Dot2)
  | Atom.Dot3 -> PlainAtom (Doc.fixed Ellipsis)
  | Atom.Semicolon -> PlainAtom (Doc.fixed Semicolon)
  | Atom.Colon -> PlainAtom (Doc.fixed Colon)
  | Atom.ColonEq ->
      PlainAtom (Doc.mathrel (Doc.concat [ Doc.fixed Colon; Doc.fixed Equal ]))
  | Atom.Tilde2 -> PlainAtom (Doc.fixed Sim)
  | Atom.Backslash -> PlainAtom (Doc.fixed Setminus)
  | Atom.LAngle -> PlainAtom (Doc.fixed Less)
  | Atom.RAngle -> PlainAtom (Doc.fixed Greater)
  | Atom.LParen -> PlainAtom (Doc.fixed LeftParen)
  | Atom.RParen -> PlainAtom (Doc.fixed RightParen)
  | Atom.LBrack -> PlainAtom (Doc.fixed LeftBracket)
  | Atom.RBrack -> PlainAtom (Doc.fixed RightBracket)
  | Atom.LBrace -> PlainAtom (Doc.fixed LeftBrace)
  | Atom.RBrace -> PlainAtom (Doc.fixed RightBrace)

let tex_of_atom (atom : atom) : Doc.t =
  let atom_interm = render_atom atom.it in
  match atom_interm with
  | EmptyAtom -> Doc.empty
  | PlainAtom tex | SubscriptedAtom tex -> tex

(* Brackets *)

let tex_of_bracket (atom_l : atom) (tex_body : Doc.t) (atom_r : atom) : Doc.t =
  match (atom_l.it, atom_r.it) with
  | Atom.LParen, Atom.RParen -> Doc.delimited Paren tex_body
  | Atom.LBrack, Atom.RBrack -> Doc.delimited Bracket tex_body
  | Atom.LBrace, Atom.RBrace -> Doc.delimited Brace tex_body
  | Atom.LAngle, Atom.RAngle -> Doc.delimited Angle tex_body
  | _ ->
      let tex_l = tex_of_atom atom_l in
      let tex_r = tex_of_atom atom_r in
      Doc.concat [ tex_l; tex_body; tex_r ]

(* Iterators

   t? -> {\mathsf{t}}^{?}
   t* -> {\mathsf{t}}^{\ast} *)

let tex_of_iter (iter : iter) : Doc.t =
  match iter with Opt -> Doc.fixed Question | List -> Doc.fixed Ast

(* Types *)

let rec tex_of_typ (typ : typ) : Doc.t =
  match typ with
  | PlainT plaintyp -> tex_of_plaintyp plaintyp
  | NotationT nottyp -> tex_of_nottyp nottyp

(* Plain types

   bool -> \mathbb{B} *)

and tex_of_plaintyp (plaintyp : plaintyp) : Doc.t =
  match plaintyp.it with
  | BoolT -> tex_of_bool_type ()
  | NumT numtyp -> tex_of_num_type numtyp
  | TextT -> tex_of_text_type ()
  | VarT (id_typ, targs) -> tex_of_var_typ id_typ targs
  | ParenT plaintyp -> tex_of_paren_typ plaintyp
  | TupleT plaintyps -> tex_of_tuple_typ plaintyps
  | IterT (plaintyp, iter) -> tex_of_iter_typ plaintyp iter

(* Boolean types

   bool -> \mathbb{B} *)

and tex_of_bool_type () : Doc.t = Doc.styled_mathbb "B"

(* Number types

   nat -> \mathbb{N}
   int -> \mathbb{Z} *)

and tex_of_num_type (numtyp : Xl.Num.typ) : Doc.t =
  match numtyp with
  | `NatT -> Doc.styled_mathbb "N"
  | `IntT -> Doc.styled_mathbb "Z"

(* Text types

   text -> \mathsf{Text} *)

and tex_of_text_type () : Doc.t = Doc.styled_mathbb "T"

(* Type applications

   list<nat> -> \mathsf{list}\left\langle\mathbb{N}\right\rangle *)

and tex_of_var_typ (id_typ : id) (targs : targ list) : Doc.t =
  let tex_typid = tex_of_typid id_typ in
  let tex_targs = tex_of_targs targs in
  Doc.concat [ tex_typid; tex_targs ]

(* Parenthesized types

   (nat) -> \left(\mathbb{N}\right) *)

and tex_of_paren_typ (plaintyp : plaintyp) : Doc.t =
  let tex = tex_of_plaintyp plaintyp in
  tex_of_parenthesized tex

(* Tuple types

   (nat, bool) -> \left(\mathbb{N}, \mathbb{B}\right) *)

and tex_of_tuple_typ (plaintyps : plaintyp list) : Doc.t =
  let texs = List.map tex_of_plaintyp plaintyps in
  let tex_body = Doc.layout_group_soft_comma_separated texs in
  Doc.delimited Paren tex_body

(* Iterated types

   nat* -> {\mathbb{N}}^{\ast} *)

and tex_of_iter_typ (plaintyp : plaintyp) (iter : iter) : Doc.t =
  let tex_b = tex_of_plaintyp plaintyp in
  let tex_sup = tex_of_iter iter in
  Doc.superscript tex_b tex_sup

(* Notation types

   x : nat -> \mathsf{x} : \mathbb{N} *)

and tex_of_nottyp (nottyp : nottyp) : Doc.t =
  match nottyp.it with
  | AtomT atom -> tex_of_atom atom
  | SeqT typs -> tex_of_seq_typ typs
  | InfixT (typ_l, atom, typ_r) -> tex_of_infix_typ typ_l atom typ_r
  | BrackT (atom_l, typ, atom_r) -> tex_of_brack_typ atom_l typ atom_r

(* Type sequences

   x y -> \mathsf{x}\,\mathsf{y} *)

and tex_of_seq_typ (typs : typ list) : Doc.t =
  Doc.concat_juxtaposed (List.map tex_of_typ typs)

(* Bracketed types

   (nat) -> \left(\mathbb{N}\right) *)

and tex_of_brack_typ (atom_l : atom) (typ : typ) (atom_r : atom) : Doc.t =
  let tex_body = tex_of_typ typ in
  tex_of_bracket atom_l tex_body atom_r

(* Infix types

   x -> y -> \mathsf{x} \to \mathsf{y} *)

and tex_of_infix_typ (typ_l : typ) (atom : atom) (typ_r : typ) : Doc.t =
  let tex_l = tex_of_typ typ_l in
  let atom_interm = render_atom atom.it in
  match (atom_interm, typ_r) with
  | ( SubscriptedAtom tex_op,
      NotationT ({ it = SeqT (typ_sub :: typs); _ } as nottyp_r) ) ->
      let tex_sub = tex_of_typ typ_sub in
      let tex_op = Doc.subscript tex_op tex_sub in
      let tex_r = tex_of_typ (NotationT (SeqT typs $ nottyp_r.at)) in
      Doc.concat_spaced [ tex_l; tex_op; tex_r ]
  | SubscriptedAtom tex_op, NotationT { it = SeqT []; _ } ->
      Doc.concat_spaced [ tex_l; tex_op ]
  | SubscriptedAtom tex_op, typ_sub ->
      let tex_sub = tex_of_typ typ_sub in
      let tex_op = Doc.subscript tex_op tex_sub in
      Doc.concat_spaced [ tex_l; tex_op ]
  | atom_interm, _ ->
      let tex_op =
        match atom_interm with
        | EmptyAtom -> Doc.empty
        | PlainAtom tex | SubscriptedAtom tex -> tex
      in
      let tex_r = tex_of_typ typ_r in
      Doc.concat_spaced [ tex_l; tex_op; tex_r ]

(* Type arguments

   nat -> \mathbb{N} *)

and tex_of_targ (targ : targ) : Doc.t = tex_of_plaintyp targ

and tex_of_targs (targs : targ list) : Doc.t =
  let texs = List.map tex_of_targ targs in
  match texs with
  | [] -> Doc.empty
  | _ ->
      let tex_body = Doc.layout_group_soft_comma_separated texs in
      Doc.delimited Angle tex_body

(* Definition types *)

and tex_of_deftyp (deftyp : deftyp) : Doc.t =
  match deftyp.it with
  | PlainTD plaintyp -> tex_of_plaintyp plaintyp
  | StructTD typfields -> tex_of_struct_typ typfields
  | VariantTD typcases -> tex_of_variant_typ typcases

(* Struct types

   { FIELD nat } -> \left\{\mathsf{FIELD}\ \mathbb{N}\right\} *)

and tex_of_struct_typ (typfields : typfield list) : Doc.t =
  let texs = List.map tex_of_typfield typfields in
  let tex_body = Doc.layout_group_soft_comma_separated texs in
  Doc.delimited Brace tex_body

(* Variant types

   A | B -> \begin{gathered}\mathsf{A}\\\mathsf{B}\end{gathered} *)

and tex_of_typcase ((typ, _hints) : typcase) : Doc.t = tex_of_typ typ

and tex_of_variant_typ (typcases : typcase list) : Doc.t =
  let blocks =
    List.map
      (fun typcase ->
        let tex = tex_of_typcase typcase in
        Doc.line tex)
      typcases
  in
  Doc.gathered blocks

(* Struct fields

   FIELD nat -> \mathsf{FIELD}\ \mathbb{N} *)

and tex_of_typfield ((atom, plaintyp, _hints) : typfield) : Doc.t =
  let tex_atom = tex_of_atom atom in
  let tex_typ = tex_of_plaintyp plaintyp in
  Doc.concat_spaced [ tex_atom; tex_typ ]

(* Operators *)

let tex_of_unop (unop : unop) : Doc.t =
  match unop with
  | `NotOp -> Doc.fixed Neg
  | `PlusOp -> Doc.fixed Plus
  | `MinusOp -> Doc.fixed Minus

type binop_interm = InfixBinop of Doc.t | ExponentBinop

let render_binop (binop : binop) : binop_interm =
  match binop with
  | `AndOp -> InfixBinop (Doc.fixed Land)
  | `OrOp -> InfixBinop (Doc.fixed Lor)
  | `ImplOp -> InfixBinop (Doc.fixed Rightarrow)
  | `EquivOp -> InfixBinop (Doc.fixed Leftrightarrow)
  | `AddOp -> InfixBinop (Doc.fixed Plus)
  | `SubOp -> InfixBinop (Doc.fixed Minus)
  | `MulOp -> InfixBinop (Doc.fixed Cdot)
  | `DivOp -> InfixBinop (Doc.fixed Slash)
  | `ModOp -> InfixBinop (Doc.fixed Bmod)
  | `PowOp -> ExponentBinop

let tex_of_cmpop (cmpop : cmpop) : Doc.t =
  match cmpop with
  | `EqOp -> Doc.fixed Equal
  | `NeOp -> Doc.fixed NotEqual
  | `LtOp -> Doc.fixed Less
  | `GtOp -> Doc.fixed Greater
  | `LeOp -> Doc.fixed LessEqual
  | `GeOp -> Doc.fixed GreaterEqual

(* Expressions *)

type anchors = { func : string -> string option; rel : string -> string option }
type exp_interm = { tex : Doc.t; category : Prec.category }

let rec tex_of_exp ?(anchors : anchors option) (exp : exp) : Doc.t =
  let exp_interm = render_exp ?anchors exp in
  exp_interm.tex

and tex_of_nested_exp ~(prec_parent : Prec.t) ~(side : Prec.side)
    (exp_interm : exp_interm) : Doc.t =
  let category_parent, assoc = prec_parent in
  if
    Prec.needs_parentheses ~category_parent ~assoc ~side
      ~category_child:exp_interm.category
  then tex_of_parenthesized exp_interm.tex
  else exp_interm.tex

and render_exp ?(anchors : anchors option) (exp : exp) : exp_interm =
  match exp.it with
  | BoolE b -> render_bool_exp b
  | NumE (numop, num) -> render_num_exp numop num
  | TextE text -> render_text_exp text
  | VarE id_var -> render_var_exp id_var
  | UnE (unop, exp) -> render_un_exp ~anchors unop exp
  | BinE (exp_l, `PowOp, exp_r) -> render_pow_exp ~anchors exp_l exp_r
  | BinE (exp_l, binop, exp_r) -> render_bin_exp ~anchors binop exp_l exp_r
  | CmpE (exp_l, cmpop, exp_r) -> render_cmp_exp ~anchors cmpop exp_l exp_r
  | ArithE exp -> render_arith_exp ~anchors exp
  | EpsE -> render_eps_exp ()
  | ListE exps -> render_list_exp ~anchors exps
  | ConsE (exp_l, exp_r) -> render_cons_exp ~anchors exp_l exp_r
  | CatE (exp_l, exp_r) -> render_cat_exp ~anchors exp_l exp_r
  | IdxE (exp_b, exp_i) -> render_idx_exp ~anchors exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> render_slice_exp ~anchors exp_b exp_l exp_h
  | LenE exp -> render_len_exp ~anchors exp
  | MemE (exp_e, exp_s) -> render_mem_exp ~anchors exp_e exp_s
  | StrE expfields -> render_str_exp ~anchors expfields
  | DotE (exp_b, atom) -> render_dot_exp ~anchors exp_b atom
  | UpdE (exp_b, path, exp_f) -> render_upd_exp ~anchors exp_b path exp_f
  | ParenE exp -> render_paren_exp ~anchors exp
  | TupleE exps -> render_tuple_exp ~anchors exps
  | CallE (id_def, targs, args) -> render_call_exp ~anchors id_def targs args
  | IterE (exp, iter) -> render_iter_exp ~anchors exp iter
  | SubE (exp, plaintyp) -> render_sub_exp ~anchors exp plaintyp
  | AtomE atom -> render_atom_exp atom
  | SeqE exps -> render_seq_exp ~anchors exps
  | InfixE (exp_l, atom, exp_r) -> render_infix_exp ~anchors exp_l atom exp_r
  | BrackE (atom_l, exp, atom_r) -> render_brack_exp ~anchors atom_l exp atom_r
  | HoleE _ -> render_hole_exp exp.at
  | FuseE _ -> render_fuse_exp exp.at
  | UnparenE _ -> render_unparen_exp exp.at
  | LatexE _ -> render_latex_exp exp.at

(* Atomic expression layout

   x -> { tex = \mathsf{x}; category = Atomic } *)

and render_atomic_exp (tex : Doc.t) : exp_interm =
  { tex; category = Prec.Atomic }

(* Binary-expression layout

   x + y -> precedence-aware infix document *)

and render_binary_exp ~(anchors : anchors option) ((category, assoc) : Prec.t)
    (tex_op : Doc.t) (exp_l : exp) (exp_r : exp) : exp_interm =
  let exp_l_interm = render_exp ?anchors exp_l in
  let exp_r_interm = render_exp ?anchors exp_r in
  let tex_l =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.LeftChild
      exp_l_interm
  in
  let tex_r =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.RightChild
      exp_r_interm
  in
  { tex = tex_of_breakable_infix tex_l tex_op tex_r; category }

(* Postfix-expression layout

   xs[i] -> precedence-aware postfix document *)

and render_postfix_exp ~(anchors : anchors option) (exp_b : exp)
    (tex_suffix : Doc.t) : exp_interm =
  let exp_b_interm = render_exp ?anchors exp_b in
  let tex_b =
    tex_of_nested_exp ~prec_parent:(Prec.Postfix, Prec.Left)
      ~side:Prec.LeftChild exp_b_interm
  in
  { tex = Doc.concat [ tex_b; tex_suffix ]; category = Prec.Postfix }

(* Boolean expressions

   true -> \mathsf{true} *)

and render_bool_exp (b : bool) : exp_interm =
  let s_bool = string_of_bool b in
  let tex = Doc.styled_mathsf s_bool in
  render_atomic_exp tex

(* Numeric expressions

   0xff -> \mathtt{0xff} *)

and render_num_exp (numop : numop) (num : num) : exp_interm =
  let tex = tex_of_number numop num in
  render_atomic_exp tex

(* Text expressions

   "ok" -> \texttt{"ok"} *)

and render_text_exp (text : string) : exp_interm =
  let s_text = "\"" ^ text ^ "\"" in
  let tex = Doc.styled_texttt s_text in
  render_atomic_exp tex

(* Variable expressions

   TC_0 -> {\mathsf{TC}}_{\mathsf{0}} *)

and render_var_exp (id_var : id) : exp_interm =
  let tex = tex_of_varid id_var in
  render_atomic_exp tex

(* Unary expressions

   not x -> \neg\ \mathsf{x} *)

and render_un_exp ~(anchors : anchors option) (unop : unop) (exp : exp) :
    exp_interm =
  let exp_interm = render_exp ?anchors exp in
  let tex_op = tex_of_unop unop in
  let tex_exp =
    tex_of_nested_exp ~prec_parent:(Prec.Unary, Prec.Right)
      ~side:Prec.RightChild exp_interm
  in
  { tex = Doc.concat_spaced [ tex_op; tex_exp ]; category = Prec.Unary }

(* Exponent expressions

   x ^ y -> {\mathsf{x}}^{\mathsf{y}} *)

and render_pow_exp ~(anchors : anchors option) (exp_l : exp) (exp_r : exp) :
    exp_interm =
  let exp_l_interm = render_exp ?anchors exp_l in
  let exp_r_interm = render_exp ?anchors exp_r in
  let category, assoc = Prec.of_binop `PowOp in
  let tex_l =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.LeftChild
      exp_l_interm
  in
  let tex_r =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.RightChild
      exp_r_interm
  in
  { tex = Doc.superscript tex_l tex_r; category }

(* Binary expressions

   x + y -> \mathsf{x} + \mathsf{y} *)

and render_bin_exp ~(anchors : anchors option) (binop : binop) (exp_l : exp)
    (exp_r : exp) : exp_interm =
  let prec = Prec.of_binop binop in
  let binop_interm = render_binop binop in
  let tex_op =
    match binop_interm with
    | InfixBinop tex -> tex
    | ExponentBinop ->
        Error.error exp_l.at
          "exponentiation reached binary expression rendering"
  in
  render_binary_exp ~anchors prec tex_op exp_l exp_r

(* Comparison expressions

   x <= y -> \mathsf{x} \leq \mathsf{y} *)

and render_cmp_exp ~(anchors : anchors option) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : exp_interm =
  let prec = Prec.of_cmpop cmpop in
  let tex_op = tex_of_cmpop cmpop in
  render_binary_exp ~anchors prec tex_op exp_l exp_r

(* Arithmetic wrappers

   ArithE x -> \mathsf{x} *)

and render_arith_exp ~(anchors : anchors option) (exp : exp) : exp_interm =
  render_exp ?anchors exp

(* Empty-sequence expressions

   eps -> \epsilon *)

and render_eps_exp () : exp_interm =
  let tex = Doc.fixed Epsilon in
  render_atomic_exp tex

(* List expressions

   [x, y] -> \left[\mathsf{x}, \mathsf{y}\right] *)

and render_list_exp ~(anchors : anchors option) (exps : exp list) : exp_interm =
  exps
  |> List.map (tex_of_exp ?anchors)
  |> Doc.layout_group_soft_comma_separated |> Doc.delimited Bracket
  |> render_atomic_exp

(* Cons expressions

   x :: xs -> \mathsf{x} \mathbin{::} \mathsf{xs} *)

and render_cons_exp ~(anchors : anchors option) (exp_l : exp) (exp_r : exp) :
    exp_interm =
  let tex_op = Doc.mathbin (Doc.fixed DoubleColon) in
  render_binary_exp ~anchors (Prec.Cons, Prec.Right) tex_op exp_l exp_r

(* Concatenation expressions

   xs ++ ys -> \mathsf{xs} \mathbin{+\!\!+} \mathsf{ys} *)

and render_cat_exp ~(anchors : anchors option) (exp_l : exp) (exp_r : exp) :
    exp_interm =
  let tex_op = Doc.mathbin (Doc.fixed Cat) in
  render_binary_exp ~anchors (Prec.Additive, Prec.Left) tex_op exp_l exp_r

(* Index expressions

   xs[i] -> \mathsf{xs}\left[\mathsf{i}\right] *)

and render_idx_exp ~(anchors : anchors option) (exp_b : exp) (exp_i : exp) :
    exp_interm =
  let tex_i = tex_of_exp ?anchors exp_i in
  let tex_suffix = Doc.delimited Bracket tex_i in
  render_postfix_exp ~anchors exp_b tex_suffix

(* Slice expressions

   xs[i:j] -> \mathsf{xs}\left[\mathsf{i} : \mathsf{j}\right] *)

and render_slice_exp ~(anchors : anchors option) (exp_b : exp) (exp_l : exp)
    (exp_h : exp) : exp_interm =
  let tex_l = tex_of_exp ?anchors exp_l in
  let tex_h = tex_of_exp ?anchors exp_h in
  let tex_body = Doc.concat_spaced [ tex_l; Doc.fixed Colon; tex_h ] in
  let tex_suffix = Doc.delimited Bracket tex_body in
  render_postfix_exp ~anchors exp_b tex_suffix

(* Length expressions

   |xs| -> \left|\mathsf{xs}\right| *)

and render_len_exp ~(anchors : anchors option) (exp : exp) : exp_interm =
  let tex_body = tex_of_exp ?anchors exp in
  { tex = Doc.delimited Bar tex_body; category = Prec.Unary }

(* Membership expressions

   x in xs -> \mathsf{x} \in \mathsf{xs} *)

and render_mem_exp ~(anchors : anchors option) (exp_e : exp) (exp_s : exp) :
    exp_interm =
  let tex_op = Doc.fixed In in
  render_binary_exp ~anchors (Prec.Comparison, Prec.Right) tex_op exp_e exp_s

(* Record fields

   FIELD x -> \mathsf{FIELD}\ \mathsf{x} *)

and tex_of_exp_field ~(anchors : anchors option) ((atom, exp) : atom * exp) :
    Doc.t =
  let tex_atom = tex_of_atom atom in
  let tex_exp = tex_of_exp ?anchors exp in
  Doc.concat_spaced [ tex_atom; tex_exp ]

(* Record expressions

   { FIELD x } -> \left\{\mathsf{FIELD}\ \mathsf{x}\right\} *)

and render_str_exp ~(anchors : anchors option) (expfields : (atom * exp) list) :
    exp_interm =
  expfields
  |> List.map (tex_of_exp_field ~anchors)
  |> Doc.layout_group_soft_comma_separated |> Doc.delimited Brace
  |> render_atomic_exp

(* Field-access expressions

   x.FIELD -> {\mathsf{x}}_{\mathsf{FIELD}} *)

and render_dot_exp ~(anchors : anchors option) (exp_b : exp) (atom : atom) :
    exp_interm =
  let tex_field = tex_of_atom atom in
  if Doc.is_empty tex_field then render_exp ?anchors exp_b
  else
    let exp_b_interm = render_exp ?anchors exp_b in
    let tex_b =
      tex_of_nested_exp ~prec_parent:(Prec.Postfix, Prec.Left)
        ~side:Prec.LeftChild exp_b_interm
    in
    { tex = Doc.subscript tex_b tex_field; category = Prec.Postfix }

(* Update expressions

   x[FIELD = y] -> \mathsf{x}\left[\mathsf{FIELD} = \mathsf{y}\right] *)

and render_upd_exp ~(anchors : anchors option) (exp_b : exp) (path : path)
    (exp_f : exp) : exp_interm =
  let tex_path = tex_of_path ?anchors path in
  let tex_f = tex_of_exp ?anchors exp_f in
  let tex_body = Doc.concat_spaced [ tex_path; Doc.fixed Equal; tex_f ] in
  let tex_suffix = Doc.delimited Bracket tex_body in
  render_postfix_exp ~anchors exp_b tex_suffix

(* Parenthesized expressions

   (x) -> \left(\mathsf{x}\right) *)

and render_paren_exp ~(anchors : anchors option) (exp : exp) : exp_interm =
  let tex = tex_of_exp ?anchors exp in
  let tex = tex_of_parenthesized tex in
  render_atomic_exp tex

(* Tuple expressions

   (x, y) -> \left(\mathsf{x}, \mathsf{y}\right) *)

and render_tuple_exp ~(anchors : anchors option) (exps : exp list) : exp_interm
    =
  exps
  |> List.map (tex_of_exp ?anchors)
  |> Doc.layout_group_soft_comma_separated |> Doc.delimited Paren
  |> render_atomic_exp

(* Function calls

   f(x) -> \mathrm{f}\left(\mathsf{x}\right) *)

and render_call_exp ~(anchors : anchors option) (id_def : id)
    (targs : targ list) (args : arg list) : exp_interm =
  let s_anchor_opt =
    Option.bind anchors (fun anchors -> anchors.func id_def.it)
  in
  let tex_name = tex_of_defid id_def in
  let tex_name = tex_of_link s_anchor_opt tex_name in
  let tex_targs = tex_of_targs targs in
  let texs_args = List.map (tex_of_arg ?anchors) args in
  let tex_args = Doc.layout_group_soft_comma_separated texs_args in
  let tex_args = Doc.delimited Paren tex_args in
  let tex = Doc.concat [ tex_name; tex_targs; tex_args ] in
  render_atomic_exp tex

(* Iterated expressions

   x* -> {\mathsf{x}}^{\ast} *)

and render_iter_exp ~(anchors : anchors option) (exp : exp) (iter : iter) :
    exp_interm =
  let exp_interm = render_exp ?anchors exp in
  let tex_b =
    tex_of_nested_exp ~prec_parent:(Prec.Postfix, Prec.Left)
      ~side:Prec.LeftChild exp_interm
  in
  let tex_iter = tex_of_iter iter in
  { tex = Doc.superscript tex_b tex_iter; category = Prec.Postfix }

(* Subtype expressions

   x <: t -> \mathsf{x} \mathrel{<:} \mathsf{t} *)

and render_sub_exp ~(anchors : anchors option) (exp : exp) (plaintyp : plaintyp)
    : exp_interm =
  let exp_interm = render_exp ?anchors exp in
  let tex_l =
    tex_of_nested_exp ~prec_parent:(Prec.Colon, Prec.Left) ~side:Prec.LeftChild
      exp_interm
  in
  let tex_op = Doc.mathrel (Doc.concat [ Doc.fixed Less; Doc.fixed Colon ]) in
  let tex_r = tex_of_plaintyp plaintyp in
  { tex = tex_of_breakable_infix tex_l tex_op tex_r; category = Prec.Colon }

(* Atom expressions

   Arrow -> \to *)

and render_atom_exp (atom : atom) : exp_interm =
  let tex = tex_of_atom atom in
  render_atomic_exp tex

(* Expression sequences

   x y -> \mathsf{x}\,\mathsf{y} *)

and render_seq_exp ~(anchors : anchors option) (exps : exp list) : exp_interm =
  let texs =
    List.map
      (fun exp ->
        let exp_interm = render_exp ?anchors exp in
        tex_of_nested_exp ~prec_parent:(Prec.Sequence, Prec.Left)
          ~side:Prec.RightChild exp_interm)
      exps
  in
  { tex = Doc.fill ~separator:Doc.thin_space texs; category = Prec.Sequence }

(* Bracketed expressions

   [x] -> \left[\mathsf{x}\right] *)

and render_brack_exp ~(anchors : anchors option) (atom_l : atom) (exp : exp)
    (atom_r : atom) : exp_interm =
  let tex_body = tex_of_exp ?anchors exp in
  let tex = tex_of_bracket atom_l tex_body atom_r in
  render_atomic_exp tex

(* Hole-expression errors

   HoleE _ -> LatexError *)

and render_hole_exp (at : region) : 'a =
  Error.error at "LaTeX rendering is undefined for a hole expression"

(* Fuse-expression errors

   FuseE _ -> LatexError *)

and render_fuse_exp (at : region) : 'a =
  Error.error at "LaTeX rendering is undefined for a fuse expression"

(* Unparen-expression errors

   UnparenE _ -> LatexError *)

and render_unparen_exp (at : region) : 'a =
  Error.error at "LaTeX rendering is undefined for an unparen expression"

(* Raw-LaTeX errors

   LatexE _ -> LatexError *)

and render_latex_exp (at : region) : 'a =
  Error.error at "raw LaTeX expressions are not allowed in canonical rendering"

(* Infix expressions

   p |- e -> \mathsf{p} \mathrel{\vdash} \mathsf{e} *)

and render_infix_exp ~(anchors : anchors option) (exp_l : exp) (atom : atom)
    (exp_r : exp) : exp_interm =
  let category, assoc = Prec.of_infix atom.it in
  let exp_l_interm = render_exp ?anchors exp_l in
  let atom_interm = render_atom atom.it in
  let exp_r_interm, tex_op =
    match (atom_interm, exp_r.it) with
    | SubscriptedAtom tex_op, SeqE (exp_sub :: exps) ->
        let exp_r = SeqE exps $ exp_r.at in
        let exp_r_interm = render_exp ?anchors exp_r in
        let tex_sub = tex_of_exp ?anchors exp_sub in
        let tex_op = Doc.subscript tex_op tex_sub in
        (exp_r_interm, tex_op)
    | SubscriptedAtom tex_op, SeqE [] ->
        ({ tex = Doc.empty; category = Prec.Atomic }, tex_op)
    | SubscriptedAtom tex_op, _ ->
        let exp_r_interm = { tex = Doc.empty; category = Prec.Atomic } in
        let tex_sub = tex_of_exp ?anchors exp_r in
        let tex_op = Doc.subscript tex_op tex_sub in
        (exp_r_interm, tex_op)
    | atom_interm, _ ->
        let tex_op =
          match atom_interm with
          | EmptyAtom -> Doc.empty
          | PlainAtom tex | SubscriptedAtom tex -> tex
        in
        let exp_r_interm = render_exp ?anchors exp_r in
        (exp_r_interm, tex_op)
  in
  let tex_l =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.LeftChild
      exp_l_interm
  in
  let tex_r =
    tex_of_nested_exp ~prec_parent:(category, assoc) ~side:Prec.RightChild
      exp_r_interm
  in
  { tex = tex_of_breakable_infix tex_l tex_op tex_r; category }

(* Paths

   record.FIELD[i]
   -> {\mathsf{record}}_{\mathsf{FIELD}}\left[\mathsf{i}\right] *)

and tex_of_path ?(anchors : anchors option) (path : path) : Doc.t =
  match path.it with
  | RootP -> tex_of_root_path ()
  | IdxP (path, exp) -> tex_of_idx_path ~anchors path exp
  | SliceP (path, exp_l, exp_h) -> tex_of_slice_path ~anchors path exp_l exp_h
  | DotP (path, atom) -> tex_of_dot_path ~anchors path atom

(* Root paths

   RootP -> "" *)

and tex_of_root_path () : Doc.t = Doc.empty

(* Index paths

   [i] -> \left[\mathsf{i}\right] *)

and tex_of_idx_path ~(anchors : anchors option) (path : path) (exp : exp) :
    Doc.t =
  let tex_path = tex_of_path ?anchors path in
  let tex_exp = tex_of_exp ?anchors exp in
  let tex_suffix = Doc.delimited Bracket tex_exp in
  Doc.concat [ tex_path; tex_suffix ]

(* Slice paths

   [i:j] -> \left[\mathsf{i} : \mathsf{j}\right] *)

and tex_of_slice_path ~(anchors : anchors option) (path : path) (exp_l : exp)
    (exp_h : exp) : Doc.t =
  let tex_path = tex_of_path ?anchors path in
  let tex_l = tex_of_exp ?anchors exp_l in
  let tex_h = tex_of_exp ?anchors exp_h in
  let tex_body = Doc.concat_spaced [ tex_l; Doc.fixed Colon; tex_h ] in
  let tex_suffix = Doc.delimited Bracket tex_body in
  Doc.concat [ tex_path; tex_suffix ]

(* Field paths

   FIELD -> \mathsf{FIELD} *)

and tex_of_dot_path ~(anchors : anchors option) (path : path) (atom : atom) :
    Doc.t =
  match path.it with
  | RootP -> tex_of_atom atom
  | IdxP _ | SliceP _ | DotP _ ->
      let tex_field = tex_of_atom atom in
      if Doc.is_empty tex_field then tex_of_path ?anchors path
      else
        let tex_path = tex_of_path ?anchors path in
        Doc.concat [ tex_path; Doc.fixed Dot; tex_field ]

(* Type parameters

   T, U -> \left\langle\mathsf{T}, \mathsf{U}\right\rangle *)

and tex_of_tparam (tparam : tparam) : Doc.t = tex_of_typid tparam

and tex_of_tparams (tparams : tparam list) : Doc.t =
  let texs = List.map tex_of_tparam tparams in
  match texs with
  | [] -> Doc.empty
  | _ ->
      let tex_body = Doc.layout_group_soft_comma_separated texs in
      Doc.delimited Angle tex_body

(* Arguments *)

and tex_of_arg ?(anchors : anchors option) (arg : arg) : Doc.t =
  match arg.it with
  | ExpA exp -> tex_of_exp_arg ~anchors exp
  | DefA id_def -> tex_of_def_arg id_def

and tex_of_args ?(anchors : anchors option) (args : arg list) : Doc.t =
  let texs = List.map (tex_of_arg ?anchors) args in
  let tex_body = Doc.layout_group_soft_comma_separated texs in
  Doc.delimited Paren tex_body

(* Expression arguments

   ExpA x -> \mathsf{x} *)

and tex_of_exp_arg ~(anchors : anchors option) (exp : exp) : Doc.t =
  tex_of_exp ?anchors exp

(* Definition arguments

   DefA f -> \mathrm{f} *)

and tex_of_def_arg (id_def : id) : Doc.t = tex_of_defid id_def

(* Parameters *)

and tex_of_param (param : param) : Doc.t =
  match param.it with
  | ExpP plaintyp -> tex_of_exp_param plaintyp
  | DefP (id_def, tparams, params, plaintyp_result) ->
      tex_of_def_param id_def tparams params plaintyp_result

and tex_of_params (params : param list) : Doc.t =
  let texs = List.map tex_of_param params in
  let tex_body = Doc.layout_group_soft_comma_separated texs in
  Doc.delimited Paren tex_body

(* Expression parameters

   ExpP nat -> \mathbb{N} *)

and tex_of_exp_param (plaintyp : plaintyp) : Doc.t = tex_of_plaintyp plaintyp

(* Definition parameters

   DefP f(nat) -> \mathrm{f}\left(\mathbb{N}\right) *)

and tex_of_def_param (id_def : id) (tparams : tparam list) (params : param list)
    (plaintyp_result : plaintyp) : Doc.t =
  let tex_name = tex_of_defid id_def in
  let tex_tparams = tex_of_tparams tparams in
  let tex_params = tex_of_params params in
  let tex_head = Doc.concat [ tex_name; tex_tparams; tex_params ] in
  let tex_result = tex_of_plaintyp plaintyp_result in
  Doc.concat_spaced [ tex_head; Doc.fixed Colon; tex_result ]

(* Premises *)

and tex_of_prem ?(anchors : anchors option) (prem : prem) : Doc.t =
  match prem.it with
  | VarPr (id_var, plaintyp) -> tex_of_var_prem id_var plaintyp
  | RulePr (id_rel, exp) -> tex_of_rule_prem ~anchors id_rel exp
  | RuleNotPr (id_rel, exp) -> tex_of_rule_not_prem ~anchors id_rel exp
  | IfPr exp -> tex_of_if_prem ~anchors exp
  | ElsePr -> tex_of_else_prem ()
  | IterPr (prem, iter) -> tex_of_iter_prem ~anchors prem iter
  | DebugPr exp -> tex_of_debug_prem ~anchors exp

and texs_of_prems ?(anchors : anchors option) (prems : prem list) : Doc.t list =
  prems
  |> List.map (tex_of_prem ?anchors)
  |> List.filter (Fun.negate Doc.is_empty)

(* Variable premises

   x : nat -> \mathsf{x} : \mathbb{N} *)

and tex_of_var_prem (id_var : id) (plaintyp : plaintyp) : Doc.t =
  let tex_var = tex_of_varid id_var in
  let tex_typ = tex_of_plaintyp plaintyp in
  Doc.concat_spaced [ tex_var; Doc.fixed Colon; tex_typ ]

(* Relation premises

   Eval: p |- e -> \href{#Eval}{\mathsf{p} \mathrel{\vdash} \mathsf{e}} *)

and tex_of_rule_prem ~(anchors : anchors option) (id_rel : id) (exp : exp) :
    Doc.t =
  let s_anchor_opt =
    Option.bind anchors (fun anchors -> anchors.rel id_rel.it)
  in
  let tex_exp = tex_of_exp ?anchors exp in
  tex_of_link s_anchor_opt tex_exp

(* Negative relation premises

   not Eval: p |- e -> \neg\ \href{#Eval}{\mathsf{p} \mathrel{\vdash} \mathsf{e}} *)

and tex_of_rule_not_prem ~(anchors : anchors option) (id_rel : id) (exp : exp) :
    Doc.t =
  let s_anchor_opt =
    Option.bind anchors (fun anchors -> anchors.rel id_rel.it)
  in
  let exp_interm = render_exp ?anchors exp in
  let tex_judgment =
    tex_of_nested_exp ~prec_parent:(Prec.Unary, Prec.Right)
      ~side:Prec.RightChild exp_interm
  in
  let tex_judgment = tex_of_link s_anchor_opt tex_judgment in
  Doc.concat_spaced [ Doc.fixed Neg; tex_judgment ]

(* Conditional premises

   if x -> \mathsf{x} *)

and tex_of_if_prem ~(anchors : anchors option) (exp : exp) : Doc.t =
  tex_of_exp ?anchors exp

(* Else premises

   else -> \mathrm{otherwise} *)

and tex_of_else_prem () : Doc.t = Doc.styled_mathrm "otherwise"

(* Iterated premises

   (x : nat)* -> {\left(\mathsf{x} : \mathbb{N}\right)}^{\ast} *)

and tex_of_iter_prem ~(anchors : anchors option) (prem : prem) (iter : iter) :
    Doc.t =
  let tex_base =
    match prem.it with
    | IterPr _ -> tex_of_prem ?anchors prem
    | VarPr _ | RulePr _ | RuleNotPr _ | IfPr _ | ElsePr | DebugPr _ ->
        let tex_prem = tex_of_prem ?anchors prem in
        tex_of_parenthesized tex_prem
  in
  let tex_iter = tex_of_iter iter in
  Doc.superscript tex_base tex_iter

(* Debug premises

   debug x -> \mathrm{debug}\ \mathsf{x} *)

and tex_of_debug_prem ~(anchors : anchors option) (exp : exp) : Doc.t =
  let tex_exp = tex_of_exp ?anchors exp in
  Doc.concat_spaced [ Doc.styled_mathrm "debug"; tex_exp ]
