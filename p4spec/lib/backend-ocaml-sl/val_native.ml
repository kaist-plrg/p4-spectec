(* Native value representation at the compiled-spec <-> extern boundary.

   [t = Obj.t] holding the compiled spec's native OCaml values. A boundary
   crossing is an O(1) box/unbox ([Obj.repr]/[Obj.obj]) instead of the deep
   conversion the [V_value] path pays.

   Soundness rests on the invariant the generated code already relies on: a
   given relation/function argument slot has one spec type, so the boxed [Obj.t]
   carries exactly the OCaml type a projection expects. A wrong cast fails fast
   and is caught by the sim suite. *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
module Il = Lang.Il
module Num = Lang.Xl.Num

(* This library's own generated dispatch tables (spec-meta/sl), not P4's. *)
module Spec_parts = Spec_parts_sl
open Util.Source

module V_native : Runtime.Valrep.VAL with type t = Obj.t = struct
  type t = Obj.t

  (* Pass straight back to compiled code, never decoded: identity cast. *)
  let to_value (x : t) : Value.t = Obj.obj x
  let of_value (v : Value.t) : t = Obj.repr v

  (* Compiled native values carry no per-node region; matches every
     [V_native.Make.*] constructor already discarding [~at]. *)
  let at (_ : t) : region = no_region

  (* Convert to/from a real [Value.t] when the value is stored in a serialized
     field. Dispatch a per-type [marshal_<typ>]/[unmarshal_<typ>] by the
     caller-supplied spec type. *)
  let marshal (typ : Typ.t) (x : t) : Value.t =
    Spec_parts.Dispatch.marshal_typed typ x

  let unmarshal (typ : Typ.t) (v : Value.t) : t =
    Spec_parts.Dispatch.unmarshal_typed typ v

  module Get = struct
    let text (x : t) : string = (Obj.obj x : string)

    (* Native nums drop the Nat/Int tag (both are [Bigint.t]); every caller
       collapses the tag, so an arbitrary tag is safe. *)
    let num (x : t) : Num.t = `Int (Obj.obj x : Bigint.t)
    let bool (x : t) : bool = (Obj.obj x : bool)
    (* Composites are note-wrapped [(body, vnote) note_phrase]; the body is
       field 0 ([it]) of the info block, so unwrap before projecting. *)
    let list (x : t) : t list = (Obj.obj (Obj.field x 0) : Obj.t list)
    let opt (x : t) : t option = (Obj.obj (Obj.field x 0) : Obj.t option)

    (* A native tuple's body is a plain OCaml tuple block; project its fields (a
       nullary tuple is the immediate unit). *)
    let tuple (x : t) : t list =
      let body = Obj.field x 0 in
      if Obj.is_int body then []
      else List.init (Obj.size body) (fun i -> Obj.field body i)

    (* Shallow one-level destructure of the native variant of spec type [typ]
       into its mixop shell (args left as native [Obj.t]). *)
    let case (x : t) (typ : Il.typ) : t Mixfix.t =
      Spec_parts.Dispatch.case_of_typed x typ

    let extern (x : t) : Yojson.Safe.t = (Obj.obj x : Yojson.Safe.t)

    (* Extractors over [t list] — representation-agnostic, mirror [Value.Get]. *)
    let nth (n : int) (xs : t list) : t = List.nth xs n

    let one : t list -> t = function
      | [ x ] -> x
      | _ -> failwith "V_native.Get.one: expected exactly one value"

    let two : t list -> t * t = function
      | [ a; b ] -> (a, b)
      | _ -> failwith "V_native.Get.two: expected exactly two values"

    let three : t list -> t * t * t = function
      | [ a; b; c ] -> (a, b, c)
      | _ -> failwith "V_native.Get.three: expected exactly three values"

    (* Project a native variant's args by arity, trusting the value is the
       expected constructor (well-typedness, same trust as [Obj.obj]). A poly
       variant with [n] args is [hash; arg] for n=1 and [hash; (a0..an-1)] for
       n>=2; a nullary one is the immediate hash. *)
    let args_by_arity (x : t) (n : int) : t list =
      (* unwrap the note_phrase to the poly-variant body ([it], field 0) *)
      let x = Obj.field x 0 in
      if n = 0 then []
      else if n = 1 then [ Obj.field x 1 ]
      else
        let payload = Obj.field x 1 in
        List.init n (fun i -> Obj.field payload i)

    let ( |>> ) (x : t) (s_mixop : string) : t list =
      args_by_arity x (Mixop.arity (Value.Mixops.of_string s_mixop))

    (* Shape [x] (spec type [typ]) into its mixop shell and test whether it is
       the expected constructor. [typ] must be the value's actual (possibly
       union) type, not a leaf single-ctor type: a single-ctor type compiles
       [case_of_typed] to an unchecked projection that segfaults on a different
       runtime ctor. Shallow, so returned args stay native [Obj.t]. *)
    let ( |>>? ) (x : t) ((mixop_expect, typ) : Il.mixop * Il.typ) :
        t list option =
      let mixop, args =
        Mixfix.split (Spec_parts.Dispatch.case_of_typed x typ)
      in
      if Mixop.eq mixop mixop_expect then Some args else None
  end

  (* Note-wrapping for [Make]. A composite built at the boundary must carry a
     note whose [vhash] matches what the compiled [mk_<t>] would compute, so
     structural eq/hashing stays consistent across the boundary: prims hash by
     [Hashtbl.hash] (same as the generated [hash_<prim>]), wrapped children read
     their slot ([expr_slot_hash]), and the fold mirrors the prelude
     [hash_list]/[hash_opt]/tuple combine. *)

  (* Hash of one child, dispatched purely on representation so no spec type is
     needed: a wrapped child (the [note_phrase] record is the only tag-0, size-3
     block) reads its slot; anything else is bare and hashes flat by
     [Hashtbl.hash] (matching the generated [hash_<prim>]/extern hash). *)
  let child_hash (x : t) : int =
    if Obj.is_block x && Obj.tag x = 0 && Obj.size x = 3 then
      (Obj.obj x : (t, Il.vnote) note_phrase).note.vhash
    else Hashtbl.hash x

  (* Stamp [body] with a fresh note carrying [vhash] and the reified [typ]. *)
  let wrap_note (typ : Typ.t) (vhash : int) (body : t) : t =
    let note = { Il.vid = Value.fresh (); typ = typ.it; vhash } in
    Obj.repr (body $$ (no_region, note))

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

    let opt ?(at = no_region) (typ : Typ.t) (o : t option) : t =
      ignore at;
      let vhash = match o with None -> 0 | Some x -> 31 + child_hash x in
      wrap_note typ vhash (Obj.repr o)

    let list ?(at = no_region) (typ : Typ.t) (xs : t list) : t =
      ignore at;
      let vhash = List.fold_left (fun h x -> (h * 31) + child_hash x) 1 xs in
      wrap_note typ vhash (Obj.repr xs)

    let tuple ?(at = no_region) (typ : Typ.t) (xs : t list) : t =
      ignore at;
      let body =
        match xs with
        | [] -> Obj.repr ()
        | _ ->
            let n = List.length xs in
            let b = Obj.new_block 0 n in
            List.iteri (fun i v -> Obj.set_field b i v) xs;
            b
      in
      let vhash =
        List.fold_left (fun h x -> (h * 31) + child_hash x) (List.length xs) xs
      in
      wrap_note typ vhash body

    let extern ?(at = no_region) (_typ : Typ.t) (y : Yojson.Safe.t) : t =
      ignore at;
      Obj.repr y

    let ( <| ) (s_mixop : string) (args : t list) : Il.mixop * t list =
      (Value.Mixops.of_string s_mixop, args)

    let ( <<| ) ?(at = no_region) ((mixop, args) : Il.mixop * t list)
        (typ : Il.typ) : t =
      ignore at;
      Spec_parts.Dispatch.make_case_typed mixop args typ
  end
end
