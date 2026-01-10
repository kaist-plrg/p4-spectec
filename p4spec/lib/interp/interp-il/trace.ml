open Lang
open Il

(* Execution trace *)

type t =
  | Rel of {
      id_rel : id;
      id_rule : id;
      values_input : value list;
      subtraces : t list;
    }
  | Dec of {
      id_func : id;
      idx_clause : int;
      values_input : value list;
      subtraces : t list;
    }
  | Iter of { inner : string; subtraces : t list }
  | Prem of prem
  | Empty

(* Openers *)

let open_rel (id_rel : id) (id_rule : id) (values_input : value list) : t =
  Rel { id_rel; id_rule; values_input; subtraces = [] }

let open_dec (id_func : id) (idx_clause : int) (values_input : value list) : t =
  Dec { id_func; idx_clause; values_input; subtraces = [] }

let open_iter (inner : string) : t = Iter { inner; subtraces = [] }

(* Committing *)

let commit (trace : t) (trace_sub : t) : t =
  match trace with
  | Rel { id_rel; id_rule; values_input; subtraces; _ } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Rel { id_rel; id_rule; values_input; subtraces }
  | Dec { id_func; idx_clause; values_input; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Dec { id_func; idx_clause; values_input; subtraces }
  | Iter { inner; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Iter { inner; subtraces }
  | Prem _ -> assert false
  | Empty -> trace_sub

(* Extension *)

let extend (trace : t) (prem : prem) : t =
  match trace with
  | Rel { id_rel; id_rule; values_input; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Rel { id_rel; id_rule; values_input; subtraces }
  | Dec { id_func; idx_clause; values_input; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Dec { id_func; idx_clause; values_input; subtraces }
  | Iter { inner; subtraces } ->
      let subtraces = subtraces @ [ Prem prem ] in
      Iter { inner; subtraces }
  | Prem _ | Empty -> assert false
