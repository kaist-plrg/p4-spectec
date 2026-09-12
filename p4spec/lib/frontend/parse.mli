type error = Diagnostic.t
type spec_source = { filename : string; contents : string }

val parse_files : string list -> (Lang.El.spec, error) result
val parse_sources : spec_source list -> (Lang.El.spec, error) result
val parse_string : string -> (Lang.El.spec, error) result
val parse_mixop : string -> Domain.Mixfix.mixop
