open Util.Source

(* Errors *)

type error

val string_of_error : error -> string
val to_region_msg : error -> region * string

(* Stages *)

val parse_string : string -> (Lang.El.spec, error) result
val elab_spec : Lang.El.spec -> (Lang.Il.spec, error) result
val algo_spec : Lang.Il.spec -> (Lang.Al.spec, error) result
val struct_spec : final:bool -> Lang.Al.spec -> (Lang.Sl.spec, error) result
val annotate_spec : Lang.Sl.spec -> (Lang.Pl.spec, error) result

(* Cached pipeline *)

val parse : string list -> (Lang.El.spec, error) result
val elab : string list -> (Lang.Il.spec, error) result
val algo : string list -> (Lang.Al.spec, error) result
val structure : final:bool -> string list -> (Lang.Sl.spec, error) result
val annotate : string list -> (Lang.Pl.spec, error) result
