open Util.Source

type error

val to_region_msg : error -> region * string
val parse_files : string list -> (Lang.El.spec, error) result
val parse_string : string -> (Lang.El.spec, error) result
val parse_mixop : string -> Domain.Mixfix.mixop
