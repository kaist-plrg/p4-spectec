open Domain
open Ast
open Atom

(* Expression precedence decides when nested EL expressions need parentheses.

   x + y * z -> \mathsf{x} + \mathsf{y} \cdot \mathsf{z}
   (x + y) * z -> \left(\mathsf{x} + \mathsf{y}\right) \cdot \mathsf{z} *)

(* Category: syntactic class of an expression

   x + y -> Additive
   x * y -> Multiplicative

   Constructors are ordered from weakest to strongest *)

type category =
  | Implication
  | Disjunction
  | Conjunction
  | Turnstile
  | Tilesturn
  | SquigglyArrow
  | Colon
  | Comparison
  | Cons
  | Arrow
  | Semicolon
  | Dot
  | Additive
  | Multiplicative
  | Unary
  | Sequence
  | Power
  | Postfix
  | Atomic

(* Associativity: grouping of operators in the same category

   x - y - z -> (x - y) - z
   x => y => z -> x => (y => z) *)

type assoc = Left | Right | Non

(* Precedence: category and associativity of an expression *)

type t = category * assoc

let compare (category_l : category) (category_r : category) : int =
  Stdlib.compare category_l category_r

(* Check if a child expression needs parentheses
   when nested inside a parent expression *)

type side = LeftChild | RightChild

let needs_parentheses ~(category_parent : category) ~(assoc : assoc)
    ~(side : side) ~(category_child : category) : bool =
  compare category_child category_parent < 0
  || compare category_child category_parent = 0
     &&
     match (assoc, side) with
     | Left, RightChild | Right, LeftChild | Non, _ -> true
     | Left, LeftChild | Right, RightChild -> false

(* Precedence of operators and atoms *)

let of_infix (atom : Atom.t) : t =
  match atom with
  | DoubleArrowSub | DoubleArrowLong -> (Implication, Right)
  | Turnstile -> (Turnstile, Non)
  | Tilesturn -> (Tilesturn, Non)
  | SqArrow | SqArrowStar -> (SquigglyArrow, Right)
  | Sub | Sup | Colon | ColonEq | Tilde2 -> (Colon, Left)
  | Arrow | ArrowSub -> (Arrow, Right)
  | Semicolon -> (Semicolon, Left)
  | Dot | Dot2 | Dot3 -> (Dot, Left)
  | Backslash -> (Multiplicative, Left)
  | Keyword _ | Tag _ | Operator _ | LAngle | RAngle | LParen | RParen | LBrack
  | RBrack | LBrace | RBrace ->
      (Colon, Non)

let of_binop (binop : binop) : t =
  match binop with
  | `ImplOp | `EquivOp -> (Implication, Right)
  | `OrOp -> (Disjunction, Left)
  | `AndOp -> (Conjunction, Left)
  | `AddOp | `SubOp -> (Additive, Left)
  | `MulOp | `DivOp | `ModOp -> (Multiplicative, Left)
  | `PowOp -> (Power, Left)

let of_cmpop (_cmpop : cmpop) : t = (Comparison, Right)
