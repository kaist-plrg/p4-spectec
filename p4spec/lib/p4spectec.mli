(* Entry points for the p4spectec tool, reporting every failure as Error.t *)

module Error = Error

type 'a result = ('a, Error.t) Stdlib.result

(* Spec transformations *)

val parse : string list -> Lang.El.spec result
val elab : string list -> Lang.Il.spec result
val algo : string list -> Lang.Al.spec result
val structure : final:bool -> string list -> Lang.Sl.spec result
val annotate : string list -> Lang.Pl.spec result

val spec_of_mode :
  Runtime.Dynamic_Runner.Signature.mode ->
  string list ->
  Runtime.Dynamic_Runner.Signature.spec result

(* Simulator, for the P4 target *)

val build_sim :
  ?cache:bool ->
  ?det:bool ->
  ?guard:bool ->
  ?arch:string ->
  Runtime.Sim.Signature.spec ->
  (module Runtime.Sim.Signature.SIM) result

(* Runners, for the meta-circular interpreter *)

val build_null :
  ?cache:bool ->
  ?det:bool ->
  ?guard:bool ->
  Backend_boot.Config.interface ->
  Runtime.Dynamic_Runner.Signature.spec ->
  (module Runtime.Dynamic_Runner.Signature.RUNNER) result

val tower_of_file :
  string -> Backend_boot.Config.target -> Backend_boot.Config.tower result

(* Builds every level of the tower, returning the boot spec and its runner *)
val build_tower :
  ?cache:bool ->
  ?det:bool ->
  ?guard:bool ->
  Backend_boot.Config.tower ->
  (Runtime.Dynamic_Runner.Signature.spec
  * (module Runtime.Dynamic_Runner.Signature.RUNNER))
  result

(* Negative test generation *)

val fuzzer :
  int ->
  Lang.Sl.spec ->
  string ->
  string list ->
  string ->
  string option ->
  int option ->
  Backend_testgen_neg.Modes.logmode ->
  Backend_testgen_neg.Modes.bootmode ->
  Backend_testgen_neg.Modes.mutationmode ->
  Backend_testgen_neg.Modes.covermode ->
  unit result

val debug_dangling :
  Lang.Sl.spec ->
  string ->
  string list ->
  string ->
  string ->
  int ->
  unit result
