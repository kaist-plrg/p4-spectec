open Ast
open Util.Source
module F = Format

(* Fallthrough label rendering *)

(* Types *)

type namespace = string
type anchor = string
type letter = string
type next = anchor * letter

(* Context threaded through rendering to resolve fallthrough labels *)

type ctx = { namespace : namespace; next : next option }

(* Anchor ids that fallthrough labels link to *)

let anchor_of_group (namespace : string) (id_rulegroup : string) : anchor =
  let sanitize s = String.map (fun c -> if c = '/' then '-' else c) s in
  sanitize namespace ^ "-" ^ sanitize id_rulegroup

let anchor_of_else (namespace : string) : anchor = namespace ^ "-else"

(* Renderers *)

let prose_of_fallthrough ~(anchor : anchor) ~(text : string) : Adoc.prose =
  Adoc.text
    (F.asprintf "+++<sub class=\"bk-mark\">[<a href=\"#%s\">→ %s</a>]</sub>+++"
       anchor text)

let prose_of_fallthrough_link ~(ctx_fallthrough : ctx) (instr : instr) :
    Adoc.prose =
  match instr.node.note.fallthrough with
  | None -> Adoc.empty_prose
  | Some FallNext ->
      let anchor, letter = Option.get ctx_fallthrough.next in
      prose_of_fallthrough ~anchor ~text:letter
  | Some (FallGroup id_rulegroup) ->
      let name = id_rulegroup.it in
      let anchor = anchor_of_group ctx_fallthrough.namespace name in
      prose_of_fallthrough ~anchor ~text:name
  | Some FallElse ->
      let anchor = anchor_of_else ctx_fallthrough.namespace in
      prose_of_fallthrough ~anchor ~text:"⋅"
  | Some FallFail -> Adoc.text "+++<sub class=\"bk-mark\">[FAIL]</sub>+++"
