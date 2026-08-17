module F = Format

(* Block utils *)

(* Per-namespace counter for unique block anchor ids *)

let counters : (string, int) Hashtbl.t = Hashtbl.create 64

(* Fresh block anchor id, counted within the given namespace *)

let fresh_label (namespace : string) : string =
  let name = (try Hashtbl.find counters namespace with Not_found -> 0) + 1 in
  Hashtbl.replace counters namespace name;
  F.asprintf "bk-%s-%d" namespace name

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

(* Renderers *)

let prose_of_arm_anchor ~(label : string) ~(level : int) (idx : int) :
    Adoc.prose =
  (* bk-arm-anchor sets scroll-margin-top so fragment links land on the arm header *)
  Adoc.text
    (F.asprintf "+++<span class=\"bk-arm-anchor\" id=\"%s-%s\"></span>+++" label
       (arm_letter level idx))
