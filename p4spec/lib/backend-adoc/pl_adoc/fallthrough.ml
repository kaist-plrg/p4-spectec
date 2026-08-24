open Lang
open Pl
open Util.Source
module F = Format

(* Fallthrough label rendering *)

(* Types *)

type namespace = string
type anchor = string
type next = anchor

(* Context threaded through rendering to resolve fallthrough labels *)

type ctx = { namespace : namespace; next : next option }

(* Anchor ids that fallthrough labels link to *)

let block_counters : (namespace, int) Hashtbl.t = Hashtbl.create 64

let fresh_block_anchor (namespace : namespace) : anchor =
  let count =
    Option.value (Hashtbl.find_opt block_counters namespace) ~default:0
  in
  let count = count + 1 in
  Hashtbl.replace block_counters namespace count;
  F.asprintf "bk-%s-%d" namespace count

let anchor_of_arm (block_anchor : anchor) (idx : int) : anchor =
  F.asprintf "%s-arm-%d" block_anchor (idx + 1)

let anchor_of_group (namespace : string) (id_rulegroup : string) : anchor =
  let sanitize s = String.map (fun c -> if c = '/' then '-' else c) s in
  sanitize namespace ^ "-" ^ sanitize id_rulegroup

let anchor_of_else (namespace : string) : anchor = namespace ^ "-else"

(* Renderer *)

let prose_of_fallthrough_link ~(ctx_fallthrough : ctx) (instr : _ instr) :
    Adoc.prose =
  match instr.node.note.fallthrough with
  | None -> Adoc.empty_prose
  | Some FallNext ->
      let anchor = Option.get ctx_fallthrough.next in
      Adoc.fallthrough_prose ~anchor ~label:Adoc.Derived
  | Some (FallGroup id_rulegroup) ->
      let name = id_rulegroup.it in
      let anchor = anchor_of_group ctx_fallthrough.namespace name in
      Adoc.fallthrough_prose ~anchor ~label:(Adoc.Explicit name)
  | Some FallElse ->
      let anchor = anchor_of_else ctx_fallthrough.namespace in
      Adoc.fallthrough_prose ~anchor ~label:(Adoc.Explicit "⋅")
  | Some FallFail -> Adoc.text "+++<sub class=\"bk-mark\">[FAIL]</sub>+++"
