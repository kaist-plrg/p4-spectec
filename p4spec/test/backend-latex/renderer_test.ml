open Domain
open Lang
open El
open Backend_latex_test_support

let string_of_atom atom =
  match Renderer.render_atom atom with
  | Renderer.PlainAtom doc -> Serialize.to_string doc
  | Renderer.SubscriptedAtom doc -> "<sub>" ^ Serialize.to_string doc

let string_of_binop binop =
  match Renderer.render_binop binop with
  | Renderer.InfixBinop doc -> Serialize.to_string doc
  | Renderer.ExponentBinop -> "<superscript>"

let () =
  [
    Renderer.tex_of_bool_type ();
    Renderer.tex_of_num_type `NatT;
    Renderer.tex_of_num_type `IntT;
    Renderer.tex_of_text_type ();
  ]
  |> Doc.concat_spaced |> print_doc "base-types";
  [ `NotOp; `PlusOp; `MinusOp ]
  |> List.map Renderer.tex_of_unop
  |> Doc.concat_spaced
  |> print_doc "unary-operators";
  [
    `AndOp;
    `OrOp;
    `ImplOp;
    `EquivOp;
    `AddOp;
    `SubOp;
    `MulOp;
    `DivOp;
    `ModOp;
    `PowOp;
  ]
  |> List.map string_of_binop |> String.concat " "
  |> Printf.printf "[binary-operators]\n%s\n";
  [ `EqOp; `NeOp; `LtOp; `GtOp; `LeOp; `GeOp ]
  |> List.map Renderer.tex_of_cmpop
  |> Doc.concat_spaced
  |> print_doc "comparison-operators";
  [
    Atom.Keyword "BOOL";
    Atom.Tag "META";
    Atom.Operator "<+>";
    Atom.Sub;
    Atom.Sup;
    Atom.Turnstile;
    Atom.Tilesturn;
    Atom.Arrow;
    Atom.ArrowSub;
    Atom.DoubleArrowSub;
    Atom.DoubleArrowLong;
    Atom.SqArrow;
    Atom.SqArrowStar;
    Atom.Dot;
    Atom.Dot2;
    Atom.Dot3;
    Atom.Semicolon;
    Atom.Colon;
    Atom.ColonEq;
    Atom.Tilde2;
    Atom.Backslash;
    Atom.LAngle;
    Atom.RAngle;
    Atom.LParen;
    Atom.RParen;
    Atom.LBrack;
    Atom.RBrack;
    Atom.LBrace;
    Atom.RBrace;
  ]
  |> List.map string_of_atom |> String.concat " | " |> print_endline
