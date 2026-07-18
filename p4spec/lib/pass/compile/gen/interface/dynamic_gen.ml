open Domain
open Lang
open Util.Source

(* Runtime Typ.t codegen *)

let rec make_typ_expr ?(tparams : string list = []) (typ : Sl.typ) : Ml.expr =
  match typ.it with
  | BoolT -> Ml.LitE "Typ.Make.bool"
  | NumT `NatT -> Ml.LitE "Typ.Make.nat"
  | NumT `IntT -> Ml.LitE "Typ.Make.int"
  | TextT -> Ml.LitE "Typ.Make.text"
  | VarT (id, []) when List.mem id.it tparams ->
      Ml.VarE (Naming.name_typ (Names.tvar id))
  | VarT (id, targs) ->
      Ml.AppE
        ( Ml.LitE "Typ.Make.var",
          [
            Common.make_phrase (Printf.sprintf "\"%s\"" (String.escaped id.it));
            Ml.ListE (List.map (make_typ_expr ~tparams) targs);
          ] )
  | TupleT typs ->
      Ml.AppE
        ( Ml.LitE "Typ.Make.tuple",
          [ Ml.ListE (List.map (make_typ_expr ~tparams) typs) ] )
  | IterT (typ, Il.Opt) ->
      Ml.AppE (Ml.LitE "Typ.Make.opt", [ make_typ_expr ~tparams typ ])
  | IterT (typ, Il.List) ->
      Ml.AppE (Ml.LitE "Typ.Make.list", [ make_typ_expr ~tparams typ ])
  | FuncT _ -> Ml.LitE "Typ.Make.bool"

(* Runtime Atom.t codegen *)

let make_atom_string (atom : Atom.t) : string =
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

let make_atom_phrase_string (atom : Atom.t phrase) : string =
  Printf.sprintf "{it = %s; at = no_region; note = ()}"
    (make_atom_string atom.it)

let make_atom_phrase_pat_string (atom : Atom.t phrase) : string =
  Printf.sprintf "{it = %s; _}" (make_atom_string atom.it)

(* Runtime Mixop.t codegen *)

let rec make_mixop_string (mixop : Mixop.t) : string =
  match mixop with
  | Mixfix.Arg () -> "Mixfix.Arg ()"
  | Mixfix.Atom atom ->
      Printf.sprintf "Mixfix.Atom (%s)" (make_atom_phrase_string atom)
  | Mixfix.Brack (atom_l, mixop_inner, atom_r) ->
      Printf.sprintf "Mixfix.Brack (%s, %s, %s)"
        (make_atom_phrase_string atom_l)
        (make_mixop_string mixop_inner)
        (make_atom_phrase_string atom_r)
  | Mixfix.Infix (mixop_l, atom, mixop_r) ->
      Printf.sprintf "Mixfix.Infix (%s, %s, %s)"
        (make_mixop_string mixop_l)
        (make_atom_phrase_string atom)
        (make_mixop_string mixop_r)
  | Mixfix.Seq mixops ->
      Printf.sprintf "Mixfix.Seq [%s]"
        (String.concat "; " (List.map make_mixop_string mixops))

let make_mixop_expr (mixop : Mixop.t) : Ml.expr =
  Ml.LitE (make_mixop_string mixop)

let make_mixop_pat_string (mixop : Mixop.t) : string * string list =
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
        Printf.sprintf "Mixfix.Atom (%s)" (make_atom_phrase_pat_string atom)
    | Mixfix.Brack (open_a, inner, close_a) ->
        let s_inner = go inner in
        Printf.sprintf "Mixfix.Brack (%s, %s, %s)"
          (make_atom_phrase_pat_string open_a)
          s_inner
          (make_atom_phrase_pat_string close_a)
    | Mixfix.Infix (left, atom, right) ->
        let s_left = go left in
        let s_right = go right in
        Printf.sprintf "Mixfix.Infix (%s, %s, %s)" s_left
          (make_atom_phrase_pat_string atom)
          s_right
    | Mixfix.Seq parts ->
        Printf.sprintf "Mixfix.Seq [%s]"
          (String.concat "; " (List.map go parts))
  in
  let pat = go mixop in
  (pat, List.rev !args_rev)
