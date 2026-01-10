(* Signature for splicing modules *)

module type SPLICE = sig
  type key
  type value

  val name : string
  val prefix : string
  val suffix : string
  val parse_keys : Source.t -> key list
  val find_values : Ctx.t -> key list -> value list
  val render : key list -> value list -> string
end
