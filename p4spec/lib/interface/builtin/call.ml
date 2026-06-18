module Fresh_ = Fresh
open Lang
open Il
module Typ = Runtime.Type.Typ
open Error
open Util.Source

(* Shared builtin state counter. Lifted to lib level (out of the functors) so the
   V_value (interface) and V_typed (backend-sim) instances increment the same
   counter — modes never overlap, so a single ref is correct. Used by
   [fresh_typeId] and by [checkpoint]/[seff] state-effect detection. *)
let ctr : int ref = ref 0

(* The builtin lib, generic over the value representation [V]. [interface]
   instantiates it at [Valrep.V_value] (IL/SL, Value.t); [backend-sim]
   instantiates it at [V_typed] (the compiled ML path, Obj.t). *)
module Make_funcs (V : Valrep.VAL) = struct
  module Nats = Nats.Make (V)
  module Ints = Ints.Make (V)
  module Texts = Texts.Make (V)
  module Lists = Lists.Make (V)
  module Sets = Sets.Make (V)
  module Maps = Maps.Make (V)
  module Numerics = Numerics.Make (V)
  module Fresh = Fresh_.Make (V)

  (* Extensibility point: extra or override builtins per interface *)

  type impl = (V.t -> unit) -> region -> Typ.t list -> V.t list -> V.t

  module type EXT = sig
    val entries : (string * impl) list
  end

  module No_ext : EXT = struct
    let entries = []
  end

  (* Create a BUILTIN from an EXT module containing extensions *)

  module Make (Ext : EXT) () = struct
    (* Initializer *)

    let init () : unit = ctr := 0

    (* State management *)

    let checkpoint () : int = !ctr
    let seff (before : int) (after : int) : bool = before <> after

    (* Builtin calls *)

    module Funcs = Map.Make (String)

    let funcs =
      Funcs.empty
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
      |> Funcs.add "transpose_" Lists.transpose_
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
      |> Funcs.add "fresh_typeId" (Fresh.fresh_typeId ctr)
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
      (* Ext entries merged last — allow interface-specific overrides *)
      |> fun m ->
      List.fold_left (fun acc (k, v) -> Funcs.add k v acc) m Ext.entries

    let invoke (add : V.t -> unit) (id : id) (targs : targ list)
        (args : V.t list) : V.t =
      let func = Funcs.find_opt id.it funcs in
      check (Option.is_some func) id.at
        (Format.asprintf "implementation for builtin %s is missing" id.it);
      let func = Option.get func in
      func add id.at targs args
  end
end
