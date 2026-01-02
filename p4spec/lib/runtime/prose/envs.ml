open Domain
open Lib

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation input environment *)

module IEnv = Static.Envs.IEnv

(* Hint environemnt *)

module HEnv = struct
  type t = Hints.t HIdMap.t

  let empty = HIdMap.empty

  (* Key for hints *)

  type key = [ `Func of FId.t | `Rel of RId.t | `Typ of TId.t * Mixop.t ]

  (* Adders and finders for hints *)

  let add (hid : HId.t) (key : key) (hint : Hints.Hint.t) (henv : t) : t =
    let hints = HIdMap.find_opt hid henv |> Option.value ~default:Hints.empty in
    let hints =
      match key with
      | `Typ (tid, mixop) -> Hints.add_typ tid mixop hint hints
      | `Func fid -> Hints.add_func fid hint hints
      | `Rel rid -> Hints.add_rel rid hint hints
    in
    HIdMap.add hid hints henv

  let find (hid : HId.t) (key : key) (henv : t) : Hints.Hint.t option =
    match HIdMap.find_opt hid henv with
    | Some hints -> (
        match key with
        | `Typ (tid, mixop) -> Hints.find_typ tid mixop hints
        | `Func fid -> Hints.find_func fid hints
        | `Rel rid -> Hints.find_rel rid hints)
    | None -> None
end
