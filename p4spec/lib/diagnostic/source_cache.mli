(** [t] caches source lines lazily for diagnostic snippets. *)

type t

val create : unit -> t

(** [get_line cache file line] returns the one-indexed line. It returns [None]
    when the file is unreadable or the line is out of range. *)
val get_line : t -> string -> int -> string option
