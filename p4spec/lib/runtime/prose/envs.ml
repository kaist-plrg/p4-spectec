open Lang
open Domain
open Lib

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation input environment *)

module IHEnv = MakeHIdEnv (Hints.Input)

(* Prose hint environemnt

   This implements a 3-level mapping:
    - Indexed by hint id (e.g., "prose", "prose_in")
    - Indexed by case/relation/function id (i.e., CaseId.t, FId.t, RId.t)
    - Kind of hint (Alter, Fields) *)

module HEnv = struct
  type t = Hintkinds.t HIdMap.t

  let empty = HIdMap.empty

  (* Key for hints *)

  type key = [ `Typ of CaseId.t | `Func of FId.t | `Rel of RId.t ]

  (* Adders and finders for hints *)

  let add (henv : t) (hid : HId.t) (key : key) (hint : Hintkinds.Kind.t) : t =
    let kinds =
      HIdMap.find_opt hid henv |> Option.value ~default:Hintkinds.empty
    in
    let kinds =
      match key with
      | `Typ cid -> Hintkinds.add_typ cid hint kinds
      | `Func fid -> Hintkinds.add_func fid hint kinds
      | `Rel rid -> Hintkinds.add_rel rid hint kinds
    in
    HIdMap.add hid kinds henv

  let add_alter (henv : t) (hid : HId.t) (key : key)
      (hint_alter : Hints.Alter.t) : t =
    add henv hid key (Hintkinds.Kind.Alter hint_alter)

  let add_fields (henv : t) (hid : HId.t) (key : key)
      (hint_fields : Hints.Fields.t) : t =
    add henv hid key (Hintkinds.Kind.Fields hint_fields)

  let find (henv : t) (hid : HId.t) (key : key) : Hintkinds.Kind.t option =
    match HIdMap.find_opt hid henv with
    | Some kinds -> (
        match key with
        | `Typ cid -> Hintkinds.find_typ cid kinds
        | `Func fid -> Hintkinds.find_func fid kinds
        | `Rel rid -> Hintkinds.find_rel rid kinds)
    | None -> None

  let find_alter (henv : t) (hid : HId.t) (key : key) : Hints.Alter.t option =
    match find henv hid key with
    | Some (Hintkinds.Kind.Alter hint_alter) -> Some hint_alter
    | _ -> None

  let find_fields (henv : t) (hid : HId.t) (key : key) : Hints.Fields.t option =
    match find henv hid key with
    | Some (Hintkinds.Kind.Fields hint_fields) -> Some hint_fields
    | _ -> None
end
