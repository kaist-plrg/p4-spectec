(** Tree-shaped mixfix notation with inline payloads.

    ['a t] is the canonical notation representation. The shape-only view is
    [mixop = unit t]. *)
type atom = Atom.t Util.Source.phrase [@@deriving yojson]

type 'a t =
  | Arg of 'a
  | Atom of atom
  | Brack of atom * 'a t * atom
  | Infix of 'a t * atom * 'a t
  | Seq of 'a t list
[@@deriving yojson]

type mixop = unit t [@@deriving yojson]

exception Arity_mismatch of string

(** Equality and Comparison *)

val compare : compare_arg:('a -> 'b -> int) -> 'a t -> 'b t -> int
val eq : eq_arg:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val compare_mixop : 'a t -> 'b t -> int
val eq_mixop : 'a t -> 'b t -> bool

(** Tree Traversal *)

val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val map_atoms : (atom -> atom) -> 'a t -> 'a t
val iter_atoms : (atom -> unit) -> 'a t -> unit

(** Projections *)

val arity : 'a t -> int
val atoms : 'a t -> atom list
val args : 'a t -> 'a list

(** [to_mixop mixfix] replaces every argument with a hole. *)
val to_mixop : 'a t -> mixop

(** Mixop Conversion *)

(** [fill mixop args] fills the holes of a [mixop] with [args], producing a
    concrete mixfix tree. *)
val fill : mixop -> 'a list -> 'a t

(** [split mixfix] separates a populated mixfix into its [mixop] and its arguments. *)
val split : 'a t -> mixop * 'a list

(** Rendering *)
val render :
  string_of_atom:(atom -> string) ->
  string_of_arg:('a -> string) ->
  'a t ->
  string

val to_string : 'a t -> string
