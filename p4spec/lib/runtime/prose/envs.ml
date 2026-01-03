open Lang
open Domain
open Lib

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation input environment *)

module IHEnv = MakeHIdEnv (Hints.Input)

(* Hint environemnt *)

module HEnv = struct
  type t = Kinds.t HIdMap.t

  let empty = HIdMap.empty

  (* Key for hints *)

  type key = [ `Func of FId.t | `Rel of RId.t | `Typ of TId.t * Mixop.t ]

  (* Adders and finders for hints *)

  let add (hid : HId.t) (key : key) (hint : Hints.Hint.t) (henv : t) : t =
    let kinds = HIdMap.find_opt hid henv |> Option.value ~default:Kinds.empty in
    let kinds =
      match key with
      | `Typ (tid, mixop) -> Kinds.add_typ tid mixop hint kinds
      | `Func fid -> Kinds.add_func fid hint kinds
      | `Rel rid -> Kinds.add_rel rid hint kinds
    in
    HIdMap.add hid kinds henv

  let find (hid : HId.t) (key : key) (henv : t) : Hints.Hint.t option =
    match HIdMap.find_opt hid henv with
    | Some kinds -> (
        match key with
        | `Typ (tid, mixop) -> Kinds.find_typ tid mixop kinds
        | `Func fid -> Kinds.find_func fid kinds
        | `Rel rid -> Kinds.find_rel rid kinds)
    | None -> None
end
