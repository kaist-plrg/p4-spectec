open Domain
open Lang
open Util.Source

(* Runtime Typ.t codegen *)

let rec typ (t : Sl.typ) : Ml.expr =
  match t.it with
  | BoolT -> Ml.LitE "Typ.Make.bool"
  | NumT `NatT -> Ml.LitE "Typ.Make.nat"
  | NumT `IntT -> Ml.LitE "Typ.Make.int"
  | TextT -> Ml.LitE "Typ.Make.text"
  | VarT (id, []) ->
      Ml.AppE (Ml.LitE "make_typ_var_", [ Ml.StrE id.it; Ml.ListE [] ])
  | VarT (id, targs) ->
      Ml.AppE
        ( Ml.LitE "make_typ_var_",
          [ Ml.StrE id.it; Ml.ListE (List.map typ targs) ] )
  | TupleT typs ->
      Ml.AppE (Ml.LitE "Typ.Make.tuple", [ Ml.ListE (List.map typ typs) ])
  | IterT (t, Il.Opt) -> Ml.AppE (Ml.LitE "Typ.Make.opt", [ typ t ])
  | IterT (t, Il.List) -> Ml.AppE (Ml.LitE "Typ.Make.list", [ typ t ])
  | FuncT _ -> Ml.LitE "Typ.Make.bool"

(* Runtime Atom.t/Mixop.t codegen, for marshalling *)

let atom_lit (atom : Atom.t) : string =
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
  Printf.sprintf "{it = %s; at = no_region; note = ()}" (atom_lit atom.it)

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
  Printf.sprintf "{it = %s; _}" (atom_lit atom.it)

(* Pattern for value Mixfix.t: binds each Arg node to a named variable.
   Returns (pattern_string, arg_var_names_in_left_to_right_order). *)

let mixop_pat (mixop : Mixop.t) : string * string list =
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
