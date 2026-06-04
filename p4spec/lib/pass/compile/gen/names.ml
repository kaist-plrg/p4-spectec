open Domain
open Lib
open Lang
open Runtime.Dynamic_OCaml
open Util.Source

(* Type variables *)

let tvar (id : Id.t) = id.it |> Sanitize.apply |> String.lowercase_ascii

(* Variables *)

let var_of_id (id : Id.t) = id.it |> Sanitize.apply

let var_of_var (var_ : Var.t) =
  let id, iters = var_ in
  var_of_id
    (id.it
     ^ String.concat ""
         (List.map
            (fun iter ->
              match iter with Il.Opt -> "__quest" | Il.List -> "__star")
            iters)
    $ no_region)

(* Functions *)

let func (id : Id.t) = "f__" ^ var_of_id id

(* Relations *)

let rel (id : Id.t) = "r__" ^ var_of_id id

(* Fields and constructors *)

module Ctor = struct
  let atom (atom_ : Sl.atom) : string =
    let atom (atom_ : Atom.t) : string =
      match atom_ with
      | Atom id -> id
      | SilentAtom id -> id
      | Sub -> "Sub"
      | Sup -> "Sup"
      | Turnstile -> "Turnstile"
      | Tilesturn -> "Tilesturn"
      | Tick -> "Tick"
      | DoubleQuote -> "Dquote"
      | Underscore -> "Under"
      | Arrow `Plain -> "Arrow"
      | Arrow `Tick -> "TickArrow"
      | ArrowSub -> "ArrowSub"
      | DoubleArrow -> "Darrow"
      | DoubleArrowSub -> "DarrowSub"
      | DoubleArrowLong -> "LongArrow"
      | SqArrow -> "SqArrow"
      | SqArrowStar -> "SqArrowStar"
      | Dot `Plain -> "Dot"
      | Dot `Tick -> "TickDot"
      | Dot2 `Plain -> "Dot2"
      | Dot2 `Tick -> "TickDot2"
      | Dot3 `Plain -> "Dot3"
      | Dot3 `Tick -> "TickDot3"
      | Comma -> "Comma"
      | Semicolon `Plain -> "Semi"
      | Semicolon `Tick -> "TickSemi"
      | Colon `Plain -> "Colon"
      | Colon `Tick -> "TickColon"
      | ColonEq `Plain -> "Assign"
      | ColonEq `Tick -> "TickAssign"
      | Hash -> "Hash"
      | Dollar -> "Dollar"
      | At -> "At"
      | Quest -> "Quest"
      | Bang -> "Bang"
      | BangEq -> "BangEq"
      | Tilde -> "Tilde"
      | Tilde2 `Plain -> "Tilde2"
      | Tilde2 `Tick -> "TickTilde2"
      | LAngle `Tick -> "Lt"
      | LAngle `Tick2 -> "Lt2"
      | LAngle2 -> "Lshift"
      | LAngleEq -> "Le"
      | LAngle2Eq -> "LshiftEq"
      | RAngle `Plain -> "Gt"
      | RAngle `Tick2 -> "Gt2"
      | RAngle2 -> "Rshift"
      | RAngleEq -> "Ge"
      | RAngle2Eq -> "RshiftEq"
      | LParen -> "Lparen"
      | RParen -> "Rparen"
      | LBrack `Tick -> "Lbracket"
      | LBrack `Tick2 -> "Lbracket2"
      | RBrack `Plain -> "Rbracket"
      | RBrack `Tick2 -> "Rbracket2"
      | LBrace `Tick -> "Lbrace"
      | LBrace `Tick2 -> "Lbrace2"
      | LBraceHashRBrace -> "HashSet"
      | RBrace `Plain -> "Rbrace"
      | RBrace `Tick2 -> "Rbrace2"
      | Plus -> "Plus"
      | Plus2 -> "Plus2"
      | PlusEq -> "PlusEq"
      | PlusColon -> "PlusColon"
      | Minus -> "Minus"
      | MinusEq -> "MinusEq"
      | Star -> "Star"
      | StarEq -> "StarEq"
      | Slash -> "Slash"
      | SlashEq -> "SlashEq"
      | Backslash -> "Bslash"
      | Percent -> "Pct"
      | PercentEq -> "PctEq"
      | Eq -> "Eq"
      | Eq2 -> "Eq2"
      | Amp -> "Amp"
      | Amp2 -> "And"
      | Amp3 -> "Amp3"
      | AmpEq -> "AmpEq"
      | Up -> "Up"
      | UpEq -> "UpEq"
      | Bar -> "Bar"
      | Bar2 -> "Bar2"
      | BarEq -> "BarEq"
      | SPlus -> "Splus"
      | SPlusEq -> "SplusEq"
      | SMinus -> "Sminus"
      | SMinusEq -> "SminusEq"
    in
    atom atom_.it

  let rec typ (typ_ : Sl.typ) : string =
    match typ_.it with
    | Il.BoolT -> "Bool"
    | Il.NumT _ -> "Int"
    | Il.TextT -> "Text"
    | Il.VarT (id, _) -> String.capitalize_ascii id.it
    | Il.TupleT typs -> typs |> List.map typ |> String.concat "_"
    | Il.IterT (typ_, Il.List) -> typ typ_ ^ "_List"
    | Il.IterT (typ_, Il.Opt) -> typ typ_ ^ "_Opt"
    | Il.FuncT _ -> "Func"
end

let field (atom : Sl.atom) : string = atom |> Ctor.atom |> Sanitize.apply

let ctor (nottyp : Sl.nottyp) : string =
  let capitalize_first (s : string) : string =
    match s with "" -> "" | _ -> String.capitalize_ascii s
  in
  let typs_arg = Mixfix.args nottyp.it in
  let n_args = List.length typs_arg in
  let parts =
    nottyp.it |> Mixfix.atoms_matrix
    |> List.mapi (fun idx_slot atom_group ->
           let atom_parts =
             List.filter_map
               (fun atom ->
                 let part = Ctor.atom atom in
                 if part = "" then None else Some (capitalize_first part))
               atom_group
           in
           let arg_parts =
             if idx_slot < n_args then
               let typ_arg = List.nth typs_arg idx_slot in
               let part = Ctor.typ typ_arg in
               if part = "" then [] else [ capitalize_first part ]
             else []
           in
           atom_parts @ arg_parts)
    |> List.concat
  in
  String.concat "_" parts
