open Lang
open Error
open Util.Source

(* Splice key and values *)

module type KEY = sig
  type t

  val to_string : t -> string
  val to_anchor : t -> string
  val parse : Source.t -> t list
  val compare : t -> t -> int
end

module type VALUE = sig
  type t

  val render : Ctx.t -> t list -> string
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

(* Splice configuration *)

let prefix_source =
  "ifdef::backend-html5[]\n"
  ^ ".Click to view the specification source\n[%collapsible]\n====\n"
  ^ "[source,watsup]\n----\n"

let suffix_source = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\n" ^ "endif::[]"
let prefix_latex = "ifdef::backend-html5[]\n[latexmath]\n++++\n"
let suffix_latex = "\n++++\nendif::[]"
let prefix_prose = "****\n"
let suffix_prose = "\n****"

(* Splice name, output wrapper, and optional link target *)

module type CONFIG = sig
  val name : string
  val prefix : string
  val suffix : string
  val anchor : Ctx.t -> string -> string option
end

(* Splicer *)

module type SPLICER = sig
  include CONFIG

  type key
  type value

  val init : ?context:Ctx.t -> El.spec -> Pl.spec -> unit
  val splice : Source.t -> string
  val warn_unused : unit -> unit
end

(* Each splice kind owns its store and usage accounting *)

module Make
    (K : KEY)
    (V : VALUE)
    (I : INIT with type key = K.t and type value = V.t)
    (C : CONFIG) : SPLICER with type key = K.t and type value = V.t = struct
  include C
  module S = Make_store (K) (V) (I)

  type key = K.t
  type value = V.t

  let sto : S.t ref = ref S.empty
  let render_context : Ctx.t ref = ref Ctx.empty

  let init ?(context : Ctx.t = Ctx.empty) (spec_el : El.spec)
      (spec_pl : Pl.spec) : unit =
    sto := S.init spec_el spec_pl;
    render_context := context

  let parse (source : Source.t) : K.t list = K.parse source

  let render (keys : K.t list) : string =
    let keys, values =
      keys
      |> List.filter_map (fun key ->
             match S.find_opt !sto key with
             | Some value -> Some (key, value)
             | None ->
                 warn no_region
                   (Format.asprintf "%s splice key not found: %s" name
                      (K.to_string key));
                 None)
      |> List.split
    in
    let headers =
      let anchors =
        keys
        |> List.filter_map (fun key ->
               C.anchor !render_context (K.to_anchor key))
        |> List.filter (Ctx.claim_anchor !render_context)
      in
      match anchors with
      | [] -> ""
      | _ ->
          "++++\n"
          ^ (anchors
            |> List.map (fun anchor -> "<span id=\"" ^ anchor ^ "\"></span>")
            |> String.concat "\n")
          ^ "\n++++\n"
    in
    List.iter (S.use !sto) keys;
    headers ^ prefix ^ V.render !render_context values ^ suffix

  let splice (source : Source.t) : string = render (parse source)

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
