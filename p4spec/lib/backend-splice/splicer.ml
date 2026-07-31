open Lang
open Error
open Util.Source

(* Splice key and values *)

module type KEY = sig
  type t

  val to_string : t -> string
  val parse : Source.t -> t list
  val compare : t -> t -> int
end

module type VALUE = sig
  type t

  val render : t list -> string
end

module type INIT = sig
  type key
  type value

  val init : El.spec -> Pl.spec -> (key * value) list
end

(* Splice lookups *)

module type STORE = sig
  type key
  type value
  type t

  val cardinal : t -> int
  val add : key -> value -> t -> t
  val find_opt : t -> key -> value option
  val use : t -> key -> unit
  val used : t -> key -> bool
  val unused : t -> key list
  val empty : t
  val init : El.spec -> Pl.spec -> t
end

module Make_store
    (K : KEY)
    (V : VALUE)
    (I : INIT with type key = K.t and type value = V.t) :
  STORE with type key = K.t and type value = V.t = struct
  module M = Map.Make (K)

  type key = K.t
  type value = V.t
  type entry = { mutable used : bool; data : V.t }
  type t = entry M.t

  let cardinal (sto : t) : int = M.cardinal sto

  let add (key : K.t) (data : V.t) (sto : t) : t =
    M.add key { used = false; data } sto

  let find_opt (sto : t) (key : K.t) : V.t option =
    match M.find_opt key sto with Some entry -> Some entry.data | None -> None

  let use (sto : t) (key : K.t) : unit =
    let entry = M.find key sto in
    entry.used <- true

  let used (sto : t) (key : K.t) : bool =
    let entry = M.find key sto in
    entry.used

  let unused (sto : t) : K.t list =
    M.fold
      (fun key entry keys_unused ->
        if entry.used then keys_unused else key :: keys_unused)
      sto []
    |> List.rev

  let empty : t = M.empty

  let init (spec_el : El.spec) (spec_pl : Pl.spec) : t =
    I.init spec_el spec_pl
    |> List.fold_left (fun sto (key, data) -> add key data sto) empty
end

(* Splice anchor *)

let prefix_source =
  "ifdef::backend-html5[]\n"
  ^ ".Click to view the specification source\n[%collapsible]\n====\n----\n"

let suffix_source = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n" ^ "endif::[]"
let prefix_prose = "****\n"
let suffix_prose = "\n****"

module type ANCHOR = sig
  val name : string
  val prefix : string
  val suffix : string
  val header : bool
end

(* Splicer *)

module type SPLICER = sig
  include ANCHOR

  type key
  type value

  val init : El.spec -> Pl.spec -> unit
  val splice : Source.t -> string
  val warn_unused : unit -> unit
end

module Make
    (K : KEY)
    (V : VALUE)
    (I : INIT with type key = K.t and type value = V.t)
    (A : ANCHOR) : SPLICER with type key = K.t and type value = V.t = struct
  include A

  type key = K.t
  type value = V.t

  (* Store *)

  module S = Make_store (K) (V) (I)

  let sto = ref S.empty

  let init (spec_el : El.spec) (spec_pl : Pl.spec) : unit =
    sto := S.init spec_el spec_pl

  (* Splicer functions *)

  let parse (source : Source.t) : K.t list = K.parse source

  let render (keys : K.t list) : string =
    let keys, values =
      keys
      |> List.filter_map (fun key ->
             let value_opt = S.find_opt !sto key in
             match value_opt with
             | Some value -> Some (key, value)
             | None ->
                 warn no_region
                   (Format.asprintf "%s splice key not found: %s" name
                      (K.to_string key));
                 None)
      |> List.split
    in
    let headers =
      if header then
        (keys
        |> List.filter_map (fun key ->
               if not (S.used !sto key) then Some ("[[" ^ K.to_string key ^ "]]")
               else None)
        |> String.concat "\n")
        ^ "\n"
      else ""
    in
    List.iter (S.use !sto) keys;
    headers ^ prefix ^ V.render values ^ suffix

  let splice (source : Source.t) : string =
    let keys = parse source in
    render keys

  let warn_unused () : unit =
    let keys_unused = S.unused !sto in
    let count_unused = List.length keys_unused in
    let total = S.cardinal !sto in
    let percentage =
      if total = 0 then 0.0
      else float_of_int count_unused /. float_of_int total *. 100.0
    in
    Format.asprintf "unused %d %s splices out of %d (%.2f%%)" count_unused name
      total percentage
    |> warn no_region;
    let s =
      keys_unused
      |> List.mapi (fun idx key -> (idx, key))
      |> List.fold_left
           (fun s (idx, key) ->
             let s =
               if idx mod 5 = 0 && idx > 0 then (
                 warn no_region ("\t" ^ s);
                 "")
               else s
             in
             let s = s ^ K.to_string key in
             s ^ if idx mod 5 < 4 && idx < count_unused - 1 then ", " else "")
           ""
    in
    warn no_region ("\t" ^ s)
end
