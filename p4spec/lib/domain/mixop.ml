type atom = Mixfix.atom

type 'a tree = 'a Mixfix.t =
  | Arg of 'a
  | Atom of atom
  | Brack of atom * 'a tree * atom
  | Infix of 'a tree * atom * 'a tree
  | Seq of 'a tree list

type mixop = Mixfix.mixop
type t = mixop

let compare = Mixfix.compare_mixop
let eq = Mixfix.eq_mixop
let arity = Mixfix.arity
let atoms = Mixfix.atoms
let string_of_mixop = Mixfix.to_string

let assemble ~(string_of_atom : atom -> string) (mixop : t) (args : string list)
    : string =
  Mixfix.render ~string_of_atom ~string_of_arg:Fun.id (Mixfix.fill mixop args)
