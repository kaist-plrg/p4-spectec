(* Typed value representation at the compiled-spec <-> extern boundary (C4).

   [t = Obj.t] holding the compiled spec's native OCaml typed values. Boundary
   crossings become O(1) box/unbox ([Obj.repr]/[Obj.obj]) instead of the deep
   marshal/unmarshal the [V_value] path pays — this is the currency the perf
   flip (C5) routes the compiled (ML) extern calls through.

   Soundness rests on the same invariant the generated [unmarshal] dispatch
   already trusts: a given relation/function argument slot has one spec type, so
   the boxed [Obj.t] carries exactly the OCaml type a projection expects. A wrong
   cast fails fast and is caught by the sim suite (see API.md §4, §7).

   STATUS (C5d): fully implemented but still UNINSTANTIATED — [build.ml] binds
   [V_value], so nothing here runs yet. Every op is now real: [to_value]/[of_value]
   are identity casts (C5 decision 1: typed [Obj.t] is smuggled through the
   [Value.t]-typed interfaces, no marshal on the live path); [Get.( |>>? )] shapes
   via the shallow [case_of_typed] and matches the mixop string; [to_string]
   marshals the one [value] it prints back via the generated [marshal_value]
   re-export. The per-mode extern topology that instantiates this at ML lands with
   C5's codegen flip + [make.ml] rewrite (the atomic core). *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Num = Lang.Xl.Num
open Util.Source

module V_typed : Val.VAL with type t = Obj.t = struct
  type t = Obj.t

  (* [to_string] is used only by [log_msg] on a [value]; marshal that one value
     back to the concrete [Value.t] (via the generated [marshal_value] re-export)
     and print it. This is the lone real marshal in [V_typed], on a cold path. *)
  let to_string (x : t) : string =
    Value.to_string (Spec_parts.Dispatch.marshal_value (Obj.obj x))

  (* Cold bridge to/from [Value.t] is an identity cast: under [V_typed] the whole
     ML pipe carries typed [Obj.t] smuggled through the [Value.t]-typed interfaces,
     so there is nothing to marshal at the state-persist edge (C5 decision 1). *)
  let to_value (x : t) : Value.t = Obj.obj x
  let of_value (v : Value.t) : t = Obj.repr v

  module Get = struct
    let text (x : t) : string = (Obj.obj x : string)

    (* Typed nums drop the Nat/Int tag (both compile to [Bigint.t]); every caller
       collapses the tag (e.g. [Spec_Func.sizeof_*], [extract_varsize]), so an
       arbitrary tag is safe here. *)
    let num (x : t) : Num.t = `Int (Obj.obj x : Bigint.t)

    let bool (x : t) : bool = (Obj.obj x : bool)
    let list (x : t) : t list = (Obj.obj x : Obj.t list)
    let opt (x : t) : t option = (Obj.obj x : Obj.t option)

    (* A typed tuple is a plain OCaml tuple block; project its fields (a nullary
       tuple is the immediate unit). *)
    let tuple (x : t) : t list =
      if Obj.is_int x then [] else List.init (Obj.size x) (fun i -> Obj.field x i)

    (* The sole [Get.case] site ([extract_varsize]) inspects a [value]; the [VAL]
       surface carries no typename, so pin it here until C5 threads the type. *)
    let case (x : t) : t Mixfix.t = Spec_parts.Dispatch.case_of_typed x "value"

    let extern (x : t) : Yojson.Safe.t = (Obj.obj x : Yojson.Safe.t)

    (* Extractors over [t list] — representation-agnostic, mirror [Value.Get]. *)
    let nth (n : int) (xs : t list) : t = List.nth xs n

    let one : t list -> t = function
      | [ x ] -> x
      | _ -> failwith "V_typed.Get.one: expected exactly one value"

    let two : t list -> t * t = function
      | [ a; b ] -> (a, b)
      | _ -> failwith "V_typed.Get.two: expected exactly two values"

    let three : t list -> t * t * t = function
      | [ a; b; c ] -> (a, b, c)
      | _ -> failwith "V_typed.Get.three: expected exactly three values"

    (* Project a typed variant's args by arity, trusting the value is the
       expected constructor (well-typedness, same trust as [Obj.obj]). A poly
       variant with [n] args is [hash; arg] for n=1 and [hash; (a0..an-1)] for
       n>=2; a nullary one is the immediate hash. *)
    let args_by_arity (x : t) (n : int) : t list =
      if n = 0 then []
      else if n = 1 then [ Obj.field x 1 ]
      else
        let payload = Obj.field x 1 in
        List.init n (fun i -> Obj.field payload i)

    let ( |>> ) (x : t) (s_mixop : string) : t list =
      args_by_arity x (Mixop.arity (Value.Mixops.of_string s_mixop))

    (* Sole site: [rejectTransitionResult] (spec 8.02 [REJECT errorValue]); shape
       it via the shallow [case_of_typed] (args stay typed [Obj.t], un-recursed)
       and match the expected mixop by its canonical string. *)
    let ( |>>? ) (x : t) (s_mixop : string) : t list option =
      let mixop, args =
        Mixfix.split
          (Spec_parts.Dispatch.case_of_typed x "rejectTransitionResult")
      in
      let canon = Mixop.string_of_mixop (Value.Mixops.of_string s_mixop) in
      if Mixop.string_of_mixop mixop = canon then Some args else None
  end

  module Make = struct
    let text ?(at = no_region) (s : string) : t =
      ignore at;
      Obj.repr s

    let int ?(at = no_region) (i : Bigint.t) : t =
      ignore at;
      Obj.repr i

    let nat ?(at = no_region) (n : Bigint.t) : t =
      ignore at;
      Obj.repr n

    let bool ?(at = no_region) (b : bool) : t =
      ignore at;
      Obj.repr b

    let opt ?(at = no_region) (_typ : Typ.t) (o : t option) : t =
      ignore at;
      Obj.repr o

    let list ?(at = no_region) (_typ : Typ.t) (xs : t list) : t =
      ignore at;
      Obj.repr xs

    let tuple ?(at = no_region) (_typ : Typ.t) (xs : t list) : t =
      ignore at;
      match xs with
      | [] -> Obj.repr ()
      | _ ->
          let n = List.length xs in
          let b = Obj.new_block 0 n in
          List.iteri (fun i v -> Obj.set_field b i v) xs;
          b

    let extern ?(at = no_region) (_typ : Typ.t) (y : Yojson.Safe.t) : t =
      ignore at;
      Obj.repr y

    let ( <| ) (s_mixop : string) (args : t list) : string * t list =
      (s_mixop, args)

    let ( <<| ) ((s_mixop, args) : string * t list) (typ : string) : t =
      (* [make_case_typed] keys on the canonical mixop string. *)
      let canon = Mixop.string_of_mixop (Value.Mixops.of_string s_mixop) in
      Spec_parts.Dispatch.make_case_typed canon args typ
  end
end
