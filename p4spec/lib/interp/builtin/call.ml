module Fresh_ = Fresh
open Lang
open Il
open Error
open Util.Source

(* Initializer *)

let init (printer : value -> string) : unit =
  Fresh_.ctr := 0;
  Printer.printer := printer

(* Builtin calls *)

module Funcs = Map.Make (String)

let funcs =
  Funcs.empty
  (* Printing *)
  |> Funcs.add "print_" Printer.print
  (* Nats *)
  |> Funcs.add "sum_nat" Nats.sum_nat
  |> Funcs.add "max_nat" Nats.max_nat
  |> Funcs.add "min_nat" Nats.min_nat
  (* Ints *)
  |> Funcs.add "sum_int" Ints.sum_int
  |> Funcs.add "max_int" Ints.max_int
  |> Funcs.add "min_int" Ints.min_int
  (* Texts *)
  |> Funcs.add "text_to_int" Texts.text_to_int
  |> Funcs.add "int_to_text" Texts.int_to_text
  |> Funcs.add "split_text" Texts.split_text
  |> Funcs.add "strip_prefix" Texts.strip_prefix
  |> Funcs.add "strip_suffix" Texts.strip_suffix
  |> Funcs.add "strip_all_whitespace" Texts.strip_all_whitespace
  (* Lists *)
  |> Funcs.add "rev_" Lists.rev_
  |> Funcs.add "concat_" Lists.concat_
  |> Funcs.add "distinct_" Lists.distinct_
  |> Funcs.add "partition_" Lists.partition_
  |> Funcs.add "assoc_" Lists.assoc_
  |> Funcs.add "sort_" Lists.sort_
  (* Sets *)
  |> Funcs.add "intersect_set" Sets.intersect_set
  |> Funcs.add "union_set" Sets.union_set
  |> Funcs.add "unions_set" Sets.unions_set
  |> Funcs.add "diff_set" Sets.diff_set
  |> Funcs.add "sub_set" Sets.sub_set
  |> Funcs.add "eq_set" Sets.eq_set
  (* Maps *)
  |> Funcs.add "find_map" Maps.find_map
  |> Funcs.add "find_maps" Maps.find_maps
  |> Funcs.add "add_map" Maps.add_map
  |> Funcs.add "adds_map" Maps.adds_map
  |> Funcs.add "update_map" Maps.update_map
  (* Fresh type id *)
  |> Funcs.add "fresh_typeId" Fresh_.fresh_typeId
  (* Numerics *)
  |> Funcs.add "shl" Numerics.shl
  |> Funcs.add "shr" Numerics.shr
  |> Funcs.add "shr_arith" Numerics.shr_arith
  |> Funcs.add "pow2" Numerics.pow2
  |> Funcs.add "bitstr_to_int" Numerics.bitstr_to_int
  |> Funcs.add "int_to_bitstr" Numerics.int_to_bitstr
  |> Funcs.add "bits_to_int_unsigned" Numerics.bits_to_int_unsigned
  |> Funcs.add "bits_to_int_signed" Numerics.bits_to_int_signed
  |> Funcs.add "int_to_bits_unsigned" Numerics.int_to_bits_unsigned
  |> Funcs.add "int_to_bits_signed" Numerics.int_to_bits_signed
  |> Funcs.add "bneg" Numerics.bneg
  |> Funcs.add "band" Numerics.band
  |> Funcs.add "bxor" Numerics.bxor
  |> Funcs.add "bor" Numerics.bor
  |> Funcs.add "bitacc" Numerics.bitacc
  |> Funcs.add "bitacc_replace" Numerics.bitacc_replace

let invoke (add : value -> unit) (id : id) (targs : targ list)
    (args : value list) : value =
  let func = Funcs.find_opt id.it funcs in
  check (Option.is_some func) id.at
    (Format.asprintf "implementation for builtin %s is missing" id.it);
  let func = Option.get func in
  func add id.at targs args
