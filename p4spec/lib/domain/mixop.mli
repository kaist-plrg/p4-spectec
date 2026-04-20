(** Backward-compatibility wrapper for the shape-only mixop. *)
type atom = Mixfix.atom

type 'a tree = 'a Mixfix.t =
  | Arg of 'a
  | Atom of atom
  | Brack of atom * 'a tree * atom
  | Infix of 'a tree * atom * 'a tree
  | Seq of 'a tree list

type mixop = Mixfix.mixop
type t = mixop

(** Shape-Only Operations *)

val compare : t -> t -> int
val eq : t -> t -> bool
val arity : t -> int
val atoms : t -> atom list

(** Rendering *)

val string_of_mixop : t -> string
val assemble : string_of_atom:(atom -> string) -> t -> string list -> string
