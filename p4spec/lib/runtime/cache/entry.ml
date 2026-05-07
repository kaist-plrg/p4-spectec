(* Cache entry *)

module type ENTRY = sig
  type t

  val default : t
  val equal : t -> t -> bool
  val hash : t -> int
end
