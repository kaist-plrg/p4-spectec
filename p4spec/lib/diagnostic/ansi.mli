type style = Bold | Dim | Red | Yellow | Blue | Cyan

(** [t] records whether ANSI styling is enabled. *)
type t

val plain : t
val color : t

(** [auto] returns [color] when [tty] holds and [NO_COLOR] is unset, and
    [plain] otherwise. *)
val auto : tty:bool -> t

(** [style ansi styles s] wraps [s] in [styles] unless styling is disabled or
    [styles] is empty. *)
val style : t -> style list -> string -> string
