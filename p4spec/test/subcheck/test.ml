module Mixfix = Domain.Mixfix
open Lang
open Il
module Typ = Runtime.Type.Typ
module Typdef = Runtime.Type.Typdef
module Value = Runtime.Value
open Util.Source

let tid name = name $ no_region
let typ_var name = Typ.Make.var (tid name) []

let typcase tid mixop =
  (Mixfix.fill mixop [] $ no_region, (tid, []) $ no_region, [])

let fail message = raise (Failure message)

let () =
  let mixop_foo = Value.Mixops.of_string "FOO" in
  let mixop_bar = Value.Mixops.of_string "BAR" in
  let tid_wide = tid "Wide" in
  let tid_narrow = tid "Narrow" in
  let typdef_wide =
    Typdef.Defined
      ( [],
        VariantT [ typcase tid_wide mixop_foo; typcase tid_wide mixop_bar ]
        $ no_region )
  in
  let typdef_narrow =
    Typdef.Defined ([], VariantT [ typcase tid_narrow mixop_foo ] $ no_region)
  in
  let find_typdef_opt tid =
    if tid.it = tid_wide.it then Some typdef_wide
    else if tid.it = tid_narrow.it then Some typdef_narrow
    else None
  in
  let typ_wide = typ_var "Wide" in
  let typ_narrow = typ_var "Narrow" in
  let typ_source = Typ.Make.tuple [ typ_wide; Typ.Make.list typ_wide ] in
  let typ_target = Typ.Make.tuple [ typ_narrow; Typ.Make.list typ_narrow ] in
  let subcheck =
    Runtime.Type.Sub.compile find_typdef_opt ~typ_source ~typ_target
  in
  (match subcheck with
  | TupleSC [ MixopSC [ mixop_tuple ]; IterSC (List, MixopSC [ mixop_list ]) ]
    when Mixfix.eq_mixop mixop_foo mixop_tuple
         && Mixfix.eq_mixop mixop_foo mixop_list ->
      ()
  | _ -> fail "nested variant narrowing did not compile recursively");
  let value_case mixop = Value.Make.case typ_wide (Mixfix.fill mixop []) in
  let value_foo = value_case mixop_foo in
  let value_bar = value_case mixop_bar in
  let value_tuple value_item =
    let value_list = Value.Make.list (Typ.Make.list typ_wide) [ value_item ] in
    Value.Make.tuple typ_source [ value_foo; value_list ]
  in
  let cache_sub_var = Hashtbl.create 8 in
  let find_func _ = fail "unexpected function lookup" in
  let check value =
    Value.Match.check cache_sub_var find_typdef_opt find_func subcheck value
  in
  if not (check (value_tuple value_foo)) then
    fail "recursive subcheck rejected matching cases";
  if check (value_tuple value_bar) then
    fail "recursive subcheck accepted a nested excluded case"
