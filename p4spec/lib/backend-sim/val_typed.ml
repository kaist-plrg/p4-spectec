(* Typed value representation at the compiled-spec <-> extern boundary (C4).

   [t = Obj.t] holding the compiled spec's native OCaml typed values. Boundary
   crossings become O(1) box/unbox ([Obj.repr]/[Obj.obj]) instead of the deep
   marshal/unmarshal the [V_value] path pays — this is the currency the perf
   flip (C5) routes the compiled (ML) extern calls through.

   Soundness rests on the same invariant the generated [unmarshal] dispatch
   already trusts: a given relation/function argument slot has one spec type, so
   the boxed [Obj.t] carries exactly the OCaml type a projection expects. A wrong
   cast fails fast and is caught by the sim suite (see API.md §4, §7).

   STATUS (C4): defined but UNINSTANTIATED — [build.ml] still binds [V_value], so
   nothing here runs yet. The straight typed projections/constructors are real;
   the few ops that need a spec typename the [VAL] surface does not carry are left
   as explicit C5 placeholders ([to_value]/[of_value] need per-type marshal
   dispatch; [Get.( |>>? )] needs constructor identity; [to_string] needs a typed
   printer). C5 wires them together with the per-mode extern topology. *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Num = Lang.Xl.Num
open Util.Source

module V_typed : Val.VAL with type t = Obj.t = struct
  type t = Obj.t

  let todo_c5 (what : string) : 'a =
    failwith ("V_typed." ^ what ^ ": wired in C5 (typed compiled path)")

  (* Needs a typed printer / per-type marshal — deferred to C5. *)
  let to_string (_ : t) : string = todo_c5 "to_string"

  (* The cold bridge to/from [Value.t] is a per-type marshal/unmarshal at the
     state-persist edge (API.md §3.1). A single generic [Obj.t <-> Value.t] cast
     is unsound across spec types; the real routing lands with C5's topology. *)
  let to_value (_ : t) : Value.t = todo_c5 "to_value"
  let of_value (_ : Value.t) : t = todo_c5 "of_value"

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

    (* Detecting a constructor *mismatch* needs the value's tag vs the expected
       constructor's poly-variant hash (derivable only with the compiler-side
       OCaml ctor name). Deferred to C5. *)
    let ( |>>? ) (_x : t) (_s_mixop : string) : t list option = todo_c5 "( |>>? )"
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
