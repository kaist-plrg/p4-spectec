open Lang
open Util.Source
module Adoc = Backend_adoc__Pl_adoc__Adoc
module Render = Backend_adoc__Pl_adoc__Render

let semantic_anchor = function
  | Adoc.Function "present_function" -> Some "function_prose_present_function"
  | Adoc.Relation "Present_relation" -> Some "relation_prose_Present_relation"
  | _ -> None

let print label value = Printf.printf "[%s]\n%s\n" label value

let num_exp (num : int) : Pl.exp =
  Pl.NumE (`Nat (Bigint.of_int num))
  $$ (no_region, Il.NumT `NatT) |> Pl.Annot.no_hints

let var_exp (name : string) : Pl.exp =
  Pl.VarE (name $ no_region) $$ (no_region, Il.NumT `NatT) |> Pl.Annot.no_hints

let () =
  let open Adoc in
  link_subject_code (Function "present_function") (token "present_function")
  |> ser_code ~anchor:semantic_anchor
  |> print "present-function";
  link_subject_code (Function "missing_function") (token "missing_function")
  |> ser_code ~anchor:semantic_anchor
  |> print "missing-function";
  link_subject_code (Function "present_function") (token "present function")
  |> code_prose
  |> ser_prose ~anchor:semantic_anchor
  |> print "present-function-prose";
  token " left  right " |> code_prose
  |> ser_prose ~anchor:semantic_anchor
  |> print "code-spacing";
  seq_code [ token "value"; token "^{asterisk}^" ]
  |> code_prose
  |> ser_prose ~anchor:semantic_anchor
  |> print "adjacent-code";
  seq_code [ token "\"left\""; token " "; token "\"right\"" ]
  |> code_prose
  |> ser_prose ~anchor:semantic_anchor
  |> print "quoted-code";
  empty_code |> code_prose
  |> ser_prose ~anchor:semantic_anchor
  |> print "empty-code";
  link_subject_prose (Relation "Present_relation") (text "present relation")
  |> ser_prose ~anchor:semantic_anchor
  |> print "present-relation";
  link_subject_prose (Relation "Missing_relation") (text "missing relation")
  |> ser_prose ~anchor:semantic_anchor
  |> print "missing-relation";
  link_prose ~target:"direct-arm" (text "direct arm")
  |> ser_prose ~anchor:semantic_anchor
  |> print "direct";
  seq_block
    [
      item_ordered_block ~level:0 ~anchor:"arm-one"
        ~block_body:
          (item_ordered_block ~level:1
             (fallthrough_prose ~anchor:"arm-two" ~label:Derived))
        (text "Try:");
      item_ordered_block ~level:0 ~anchor:"arm-two"
        ~block_body:(item_ordered_block ~level:1 (text "Return."))
        (text "Then, try:");
    ]
  |> ser_block ~anchor:semantic_anchor
  |> print "ordered-items";
  [
    ("plus-operator", Domain.Atom.Operator "+");
    ("offset-operator", Domain.Atom.Operator "+:");
    ("left-paren", Domain.Atom.LParen);
    ("backslash", Domain.Atom.Backslash);
  ]
  |> List.iter (fun (label, atom) ->
         Render.string_of_atom (atom $ no_region) |> print label);
  [
    ("silent-atom", Domain.Atom.Tag "B"); ("empty-atom", Domain.Atom.Tag "EMPTY");
  ]
  |> List.iter (fun (label, atom) ->
         Render.code_of_atom (atom $ no_region)
         |> Adoc.code_prose |> Adoc.ser_prose |> print label);
  Pl.BinE (`AddOp, `NatT, var_exp "n_idx", num_exp 1)
  $$ (no_region, Il.NumT `NatT) |> Pl.Annot.no_hints |> Render.code_of_exp
  |> Adoc.code_prose |> Adoc.ser_prose |> print "addition"
