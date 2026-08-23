open Util.Source

type severity = Error | Warning
type related = { region : region; message : string }

(** [trace_node] is the rendering form of a [Util.Attempt.failtrace]. *)
type trace_node = {
  region : region;
  message : string;
  children : trace_node list;
}

type t = private {
  severity : severity;
  region : region;
  code : string option;
  message : string;
  detail : string option;
  related : related list;
  trace : trace_node list;
  source : string;
      (** The component that produced the diagnostic, such as ["parse"] or
          ["runtime"]. *)
}

(** [quote value] uses inline-code delimiters when [value] contains no
    backticks or controls, and an escaped double-quoted form otherwise. *)
val quote : string -> string

val error :
  ?code:string ->
  ?detail:string ->
  ?related:related list ->
  ?trace:trace_node list ->
  source:string ->
  region ->
  string ->
  t

(** [of_failtraces] uses [fallback] unless the failtrace list has one root. *)
val of_failtraces :
  source:string -> fallback:string -> Util.Attempt.failtrace list -> t

val region_msg : t -> region * string

module Report : sig
  type diagnostic = t
  type t

  val singleton : diagnostic -> t
  val merge : t -> t -> t

  (** [to_sorted_list] sorts by severity and then by source location. *)
  val to_sorted_list : t -> diagnostic list

  val is_empty : t -> bool
end

val collect : (unit -> 'a) -> 'a * Report.t

val warn :
  ?code:string ->
  ?detail:string ->
  ?related:related list ->
  source:string ->
  region ->
  string ->
  unit
