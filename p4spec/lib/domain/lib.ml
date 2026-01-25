open Util.Source

(* Generic hash table functor *)

module HashTable (K : sig
  type t

  val eq : t -> t -> bool
  val hash : t -> int
end) (V : sig
  type t
end) =
struct
  module T = Hashtbl.Make (struct
    type t = K.t

    let equal = K.eq
    let hash = K.hash
  end)

  type t = V.t T.t

  let create ~size = T.create size
  let add k v tbl = T.add tbl k v
  let find k tbl = T.find tbl k
  let find_opt k tbl = T.find_opt tbl k
  let size tbl = T.length tbl
end

(* Int-based identifiers *)

(* Value identifiers *)

module VId = struct
  type t = int

  let to_string t = string_of_int t
  let compare t_a t_b = compare t_a t_b
  let eq t_a t_b = compare t_a t_b = 0
  let hash t = Hashtbl.hash t
end

module VIdSet = struct
  include Set.Make (VId)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map VId.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module VIdMap = struct
  include Map.Make (VId)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> VIdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = VId.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

module VIdTbl (V : sig
  type t
end) =
  HashTable (VId) (V)

(* Instruction indentifiers *)

module IId = VId
module IIdSet = VIdSet
module IIdMap = VIdMap
module IIdTbl = VIdTbl

(* Phantom identifiers *)

module PId = VId
module PIdSet = VIdSet
module PIdMap = VIdMap
module PIdTbl = VIdTbl

(* String-based identifiers *)

(* Variable identifiers *)

module Id = struct
  type t = string phrase

  let to_string t = t.it
  let compare t_a t_b = compare t_a.it t_b.it
  let eq t_a t_b = compare t_a t_b = 0
  let hash t = Hashtbl.hash t.it
  let is_underscored t = String.starts_with ~prefix:"_" t.it

  let rec strip_underscore t =
    if is_underscored t then
      String.sub t.it 1 (String.length t.it - 1) $ t.at |> strip_underscore
    else t
end

module IdSet = struct
  include Set.Make (Id)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map Id.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> IdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = Id.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

module IdTbl (V : sig
  type t
end) =
  HashTable (Id) (V)

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap
module TIdTbl = IdTbl

(* Relation identifiers *)

module RId = Id
module RIdSet = IdSet
module RIdMap = IdMap
module RIdTbl = IdTbl

(* Function identifiers *)

module FId = Id
module FIdSet = IdSet
module FIdMap = IdMap
module FIdTbl = IdTbl

(* Hint identifiers *)

module HId = Id
module HIdSet = IdSet
module HIdMap = IdMap

(* Atom-based identifiers *)

(* Mixop identifiers *)

module MixId = struct
  type t = Mixop.t

  let to_string mixop = Mixop.string_of_mixop mixop
  let compare mixop_a mixop_b = Mixop.compare mixop_a mixop_b
  let eq mixop_a mixop_b = compare mixop_a mixop_b = 0
  let hash mixop = Hashtbl.hash mixop
end

module MixIdSet = struct
  include Set.Make (MixId)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map MixId.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module MixIdMap = struct
  include Map.Make (MixId)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> MixIdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = MixId.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

module MixIdTbl (V : sig
  type t
end) =
  HashTable (MixId) (V)

(* Type case identifiers *)

module CaseId = struct
  type t = TId.t * MixId.t

  let to_string (tid, mixid) = TId.to_string tid ^ MixId.to_string mixid

  let compare (tid_a, mixid_a) (tid_b, mixid_b) =
    let c = TId.compare tid_a tid_b in
    if c <> 0 then c else MixId.compare mixid_a mixid_b

  let eq caseid_a caseid_b = compare caseid_a caseid_b = 0
  let hash (tid, mixid) = Hashtbl.hash (TId.hash tid, MixId.hash mixid)
end

module CaseIdSet = struct
  include Set.Make (CaseId)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map CaseId.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module CaseIdMap = struct
  include Map.Make (CaseId)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> CaseIdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = CaseId.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

module CaseIdTbl (V : sig
  type t
end) =
  HashTable (CaseId) (V)

(* Environment/table functor *)

(* Int-based *)

module MakeVIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include VIdMap

  type t = V.t VIdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    VIdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeVIdTbl (V : sig
  type t
end) =
  VIdTbl (V)

module MakeIIdEnv = MakeVIdEnv
module MakeIIdTbl = MakeVIdTbl
module MakePIdEnv = MakeVIdEnv
module MakePIdTbl = MakeVIdTbl

(* String-based *)

module MakeIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    IdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeIdTbl (V : sig
  type t
end) =
  IdTbl (V)

module MakeTIdEnv = MakeIdEnv
module MakeTIdTbl = MakeIdTbl
module MakeRIdEnv = MakeIdEnv
module MakeRIdTbl = MakeIdTbl
module MakeFIdEnv = MakeIdEnv
module MakeFIdTbl = MakeIdTbl
module MakeHIdEnv = MakeIdEnv
module MakeHIdTbl = MakeIdTbl

(* Atom-based *)

module MakeMixIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include MixIdMap

  type t = V.t MixIdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    MixIdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeMixIdTbl (V : sig
  type t
end) =
  MixIdTbl (V)

module MakeCaseIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include CaseIdMap

  type t = V.t CaseIdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    CaseIdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeCaseIdTbl (V : sig
  type t
end) =
  CaseIdTbl (V)
