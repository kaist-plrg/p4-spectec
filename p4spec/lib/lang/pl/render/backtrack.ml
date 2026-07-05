module F = Format

(* Backtrack utils *)

(* Namespaced anchor ids and display labels for backtracking blocks *)

module Label : sig
  type t = { id : string; display : string }

  val set_namespace : string -> unit
  val fresh : unit -> t
end = struct
  type t = { id : string; display : string }

  let namespace : string option ref = ref None

  (* Per-namespace counter for unique label numbers *)

  let counters : (string, int) Hashtbl.t = Hashtbl.create 64
  let set_namespace (namespace_ : string) = namespace := Some namespace_

  (* Fresh label, counted within the current namespace *)

  let fresh () =
    let namespace = !namespace in
    let key = Option.value namespace ~default:"" in
    let name = (try Hashtbl.find counters key with Not_found -> 0) + 1 in
    Hashtbl.replace counters key name;
    let id =
      match namespace with
      | None -> F.asprintf "bk-%d" name
      | Some namespace -> F.asprintf "bk-%s-%d" namespace name
    in
    { id; display = F.asprintf "#%d" name }
end

(* Where a failed instruction backtracks to: the next arm, or out of the block *)

type target = NextArm of string | OutOfBlock

(* A backtracking block paired with the target of its current arm *)

type ctx = { label : Label.t; target : target }

(* Asciidoctor ordered-list styles, cycled by nesting depth. *)

type ordered_style =
  | Arabic
  | Loweralpha
  | Lowerroman
  | Upperalpha
  | Upperroman

let style_at_level (level : int) : ordered_style =
  let cycle = [| Arabic; Loweralpha; Lowerroman; Upperalpha; Upperroman |] in
  cycle.(((level mod 5) + 5) mod 5)

(* Must mirror asciidoctor's own numbering exactly, or arm cross-links break *)

let arm_letter (level : int) (idx : int) : string =
  let to_roman ?(upper = false) (n : int) =
    let units =
      [| ""; "i"; "ii"; "iii"; "iv"; "v"; "vi"; "vii"; "viii"; "ix" |]
    in
    let tens =
      [| ""; "x"; "xx"; "xxx"; "xl"; "l"; "lx"; "lxx"; "lxxx"; "xc" |]
    in
    let n = max 1 n in
    let s = tens.(n / 10 mod 10) ^ units.(n mod 10) in
    if upper then String.uppercase_ascii s else s
  in
  let n = idx + 1 in
  match style_at_level level with
  | Arabic -> string_of_int n
  | Loweralpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'a' + idx))
  | Upperalpha when idx < 26 -> String.make 1 (Char.chr (Char.code 'A' + idx))
  | Lowerroman -> to_roman n
  | Upperroman -> to_roman ~upper:true n
  | Loweralpha | Upperalpha -> F.asprintf "arm%d" n

let update ~(label : Label.t) ~(level : int) ~(total : int) (idx : int) : ctx =
  let target =
    if idx + 1 < total then NextArm (arm_letter level (idx + 1)) else OutOfBlock
  in
  { label; target }

(* Renderers *)

let prose_of_label (label : Label.t) : Adoc.prose =
  Adoc.text_prose
    (F.asprintf "pass:[<strong id=\"%s\" class=\"bk-label\">%s</strong>]"
       label.id label.display)

let prose_of_fallthrough_link (backtrack : ctx option) : Adoc.prose =
  match backtrack with
  | None -> Adoc.empty_prose
  | Some { label; target } ->
      let text, id_target =
        match target with
        | NextArm letter ->
            ( F.asprintf "else %s-%s" label.display letter,
              F.asprintf "%s-%s" label.id letter )
        | OutOfBlock -> (F.asprintf "fail %s" label.display, label.id)
      in
      Adoc.text_prose
        (F.asprintf
           "+++<sub class=\"bk-mark\">[<a href=\"#%s\">%s</a>]</sub>+++"
           id_target text)

let prose_of_arm_anchor ~(label : Label.t) ~(level : int) (idx : int) :
    Adoc.prose =
  (* bk-arm-anchor sets scroll-margin-top so fragment links land on the arm header *)
  Adoc.text_prose
    (F.asprintf "+++<span class=\"bk-arm-anchor\" id=\"%s-%s\"></span>+++"
       label.id (arm_letter level idx))
