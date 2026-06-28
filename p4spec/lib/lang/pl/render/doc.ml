open Adoc

(* Document representation *)

type t =
  | Text of string
  | Seq of t list
  | Code of t
  | Link of string * t (* target, body *)

let text (s : string) : t = Text s
let seq (ts : t list) : t = Seq ts
let code (t : t) : t = Code t
let link ~(target : string) (t : t) : t = Link (target, t)
let empty : t = Seq []
let ( ++ ) (a : t) (b : t) : t = Seq [ a; b ]

let rec serialize ~(in_code : bool) ~(in_link : bool) (t : t) : string =
  match t with
  | Text s -> s
  | Seq ts -> String.concat "" (List.map (serialize ~in_code ~in_link) ts)
  | Code inner ->
      let s = serialize ~in_code:true ~in_link inner in
      if in_code then s else adoc_mono_chopped s
  | Link (target, inner) ->
      let s = serialize ~in_code ~in_link:true inner in
      if in_link then s else adoc_link ~link:target s

(* Serialize at the top level: neither a code span nor a link is open *)

let to_adoc (t : t) : string = serialize ~in_code:false ~in_link:false t

(* Serialize as if already inside a code span *)

let to_adoc_code (t : t) : string = serialize ~in_code:true ~in_link:false t

(* Serialize as if already inside a link *)

let to_adoc_in_link (t : t) : string = serialize ~in_code:false ~in_link:true t
