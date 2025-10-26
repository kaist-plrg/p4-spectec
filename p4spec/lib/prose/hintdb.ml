open Domain.Lib
module HintIdMap = Map.Make (String)

type t = Hintenv.t HintIdMap.t

let empty = HintIdMap.empty

type def_id = [ `Func of FId.t | `Rel of RId.t | `Typ of TId.t ]

let add (hintid : string) (id : def_id) (exp : El.Ast.exp) (db : t) : t =
  let hint_env =
    HintIdMap.find_opt hintid db |> Option.value ~default:Hintenv.empty
  in
  let new_hint_env =
    match id with
    | `Typ tid -> Hintenv.add_typ tid exp hint_env
    | `Func fid -> Hintenv.add_func fid exp hint_env
    | `Rel rid -> Hintenv.add_rel rid exp hint_env
  in
  HintIdMap.add hintid new_hint_env db

let get (hintid : string) (id : def_id) (db : t) : El.Ast.exp option =
  match HintIdMap.find_opt hintid db with
  | None -> None
  | Some hint_env -> (
      match id with
      | `Typ tid -> Hintenv.get_typ tid hint_env
      | `Func fid -> Hintenv.get_func fid hint_env
      | `Rel rid -> Hintenv.get_rel rid hint_env)
