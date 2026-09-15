type error = Diagnostic.t

val parse_files : string list -> (Lang.El.spec, error) result
val parse_string : string -> (Lang.El.spec, error) result
val parse_mixop : string -> Domain.Mixfix.mixop
