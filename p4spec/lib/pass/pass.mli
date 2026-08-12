open Util.Source

(* Pass submodules. The deep, raising entry points (e.g. [Elaborate.Elab.elab_spec])
   are still consumed directly by the interface layer. *)

module Elaborate = Elaborate
module Algo = Algo
module Structure = Structure
module Annotate = Annotate

(* Errors *)

type error

val string_of_error : error -> string
val to_region_msg : error -> region * string

(* Pipeline. Each stage returns its result or the first pass failure. *)

val parse : string list -> (Lang.El.spec, error) result
val elab : string list -> (Lang.Il.spec, error) result
val algo : string list -> (Lang.Al.spec, error) result
val structure : final:bool -> string list -> (Lang.Sl.spec, error) result
val annotate : string list -> (Lang.Pl.spec, error) result
