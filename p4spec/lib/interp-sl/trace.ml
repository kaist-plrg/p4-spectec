open Sl.Ast
open Sl.Print
open Util.Source

(* Execution trace for SL interpreter - simplified version without timing *)

type t =
  | Rel of {
      id_rel : id;
      values_input : value list;
      subtraces : t list;
    }
  | Func of {
      id_func : id;
      values_input : value list;
      subtraces : t list;
    }
  | Instr of { instr : instr; subtraces : t list }
  | Empty

(* Openers *)

let open_rel (id_rel : id) (values_input : value list) : t =
  Rel { id_rel; values_input; subtraces = [] }

let open_func (id_func : id) (values_input : value list) : t =
  Func { id_func; values_input; subtraces = [] }

let open_instr (instr : instr) : t =
  Instr { instr; subtraces = [] }

(* Closers *)

let close (trace : t) : t = trace

(* Committing *)

let commit (trace : t) (trace_sub : t) : t =
  match trace with
  | Rel { id_rel; values_input; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Rel { id_rel; values_input; subtraces }
  | Func { id_func; values_input; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Func { id_func; values_input; subtraces }
  | Instr { instr; subtraces } ->
      let subtraces = subtraces @ [ trace_sub ] in
      Instr { instr; subtraces }
  | Empty -> trace_sub

(* Replacement for caching *)

let rec wipe_trace (trace : t) : t =
  match trace with
  | Rel { id_rel; values_input; subtraces } ->
      let subtraces = List.map wipe_trace subtraces in
      Rel { id_rel; values_input; subtraces }
  | Func { id_func; values_input; subtraces } ->
      let subtraces = List.map wipe_trace subtraces in
      Func { id_func; values_input; subtraces }
  | Instr { instr; subtraces } ->
      let subtraces = List.map wipe_trace subtraces in
      Instr { instr; subtraces }
  | Empty -> Empty

let wipe_subtraces (trace : t) : t list =
  match trace with
  | Rel { subtraces; _ } | Func { subtraces; _ } | Instr { subtraces; _ } ->
      List.map wipe_trace subtraces
  | Empty -> []

let replace_subtraces (trace : t) (subtraces : t list) : t =
  match trace with
  | Rel { id_rel; values_input; _ } -> Rel { id_rel; values_input; subtraces }
  | Func { id_func; values_input; _ } -> Func { id_func; values_input; subtraces }
  | Instr { instr; _ } -> Instr { instr; subtraces }
  | Empty -> Empty

(* Convert trace to failtrace for error reporting *)

let rec to_failtrace (trace : t) : Util.Attempt.failtrace list =
  match trace with
  | Rel { id_rel; subtraces; _ } ->
      let subfailtraces = List.concat_map to_failtrace subtraces in
      [
        Util.Attempt.Failtrace
          ( id_rel.at,
            Format.asprintf "invocation of relation %s failed" id_rel.it,
            subfailtraces );
      ]
  | Func { id_func; subtraces; _ } ->
      let subfailtraces = List.concat_map to_failtrace subtraces in
      [
        Util.Attempt.Failtrace
          ( id_func.at,
            Format.asprintf "invocation of function %s failed" id_func.it,
            subfailtraces );
      ]
  | Instr { instr; subtraces; _ } ->
      let subfailtraces = List.concat_map to_failtrace subtraces in
      [
        Util.Attempt.Failtrace
          ( instr.at,
            Format.asprintf "instruction %s failed"
              (String.sub (string_of_instr instr) 0
                 (min 50 (String.length (string_of_instr instr)))),
            subfailtraces );
      ]
  | Empty -> []
