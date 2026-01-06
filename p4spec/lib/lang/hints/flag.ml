(* Flag hints *)

type t = bool

let to_string (hint : t) : string = if hint then "hint(flag)" else ""

(* Creating hints *)

let init (hints : El.hint list) (hid : string) : t =
  List.exists (fun (hint : El.hint) -> hid = hint.hintid.it) hints
