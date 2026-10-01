open Util.Source
module Lsp = Linol_eio
module Diagnostic = P4spectec.Diagnostic

let ( let* ) = Result.bind

let position ~position_encoding ~sources (p : pos) : Lsp.Position.t =
  let line = max 0 (p.line - 1) in
  let character =
    match (position_encoding, List.assoc_opt p.file sources) with
    | `UTF8, _ | `UTF16, None -> max 0 p.column
    | `UTF16, Some lines ->
        if line >= Array.length lines then 0
        else
          Uutf.String.fold_utf_8
            (fun column offset uchar ->
              if offset >= p.column then column
              else
                column
                +
                match uchar with
                | `Uchar u when Uchar.to_int u > 0xffff -> 2
                | _ -> 1)
            0 lines.(line)
  in
  Lsp.Position.create ~line ~character

let range ~position_encoding ~sources (r : region) : Lsp.Range.t =
  Lsp.Range.create
    ~start:(position ~position_encoding ~sources r.left)
    ~end_:(position ~position_encoding ~sources r.right)

let severity = function
  | Diagnostic.Error -> Lsp.DiagnosticSeverity.Error
  | Diagnostic.Warning -> Lsp.DiagnosticSeverity.Warning

let rec trace_lines indent (nodes : Diagnostic.trace_node list) =
  List.concat_map
    (fun (node : Diagnostic.trace_node) ->
      (indent ^ node.message) :: trace_lines (indent ^ "  ") node.children)
    nodes

let rec trace_locations (nodes : Diagnostic.trace_node list) =
  List.concat_map
    (fun (node : Diagnostic.trace_node) ->
      Diagnostic.{ region = node.region; message = node.message }
      :: trace_locations node.children)
    nodes

let related ~position_encoding ~sources (label : Diagnostic.related) =
  if label.region.left.file = "" then None
  else
    Some
      (Lsp.DiagnosticRelatedInformation.create
         ~location:
           (Lsp.Location.create
              ~uri:(Lsp.DocumentUri.of_path label.region.left.file)
              ~range:(range ~position_encoding ~sources label.region))
         ~message:label.message)

let of_diagnostic ~position_encoding ~sources (d : Diagnostic.t) :
    Lsp.Diagnostic.t =
  let message =
    String.concat "\n"
      ((d.message :: Option.to_list d.detail) @ trace_lines "  " d.trace)
  in
  let related_information =
    List.filter_map
      (related ~position_encoding ~sources)
      (d.related @ trace_locations d.trace)
  in
  Lsp.Diagnostic.create
    ~range:(range ~position_encoding ~sources d.region)
    ~severity:(severity d.severity) ~source:d.source
    ?code:(Option.map (fun c -> `String c) d.code)
    ~message:(`String message)
    ?relatedInformation:
      (if related_information = [] then None else Some related_information)
    ()

let read_file path = In_channel.with_open_bin path In_channel.input_all

let spec_files_of open_path =
  match P4spectec.spec_root_of_file open_path with
  | Some root -> P4spectec.collect_spec_files ~include_file:open_path root
  | None -> [ open_path ]

let retag_as ~open_path (d : Diagnostic.t) : Diagnostic.t =
  let related = Diagnostic.{ region = d.region; message = d.message } in
  Diagnostic.error ?code:d.code ?detail:d.detail ~related:(related :: d.related)
    ~trace:d.trace ~source:d.source (region_of_file open_path)
    (Printf.sprintf "in %s: %s"
       (Filename.basename d.region.left.file)
       d.message)

let report_for ~position_encoding ~sources ~open_path (d : Diagnostic.t) =
  if String.equal d.region.left.file open_path then
    Some (of_diagnostic ~position_encoding ~sources d)
  else
    match d.severity with
    | Diagnostic.Error ->
        Some (of_diagnostic ~position_encoding ~sources (retag_as ~open_path d))
    | Diagnostic.Warning -> None

let unreadable ~open_path file message =
  Diagnostic.error ~source:"io" (region_of_file open_path)
    (Printf.sprintf "cannot read spec file %s: %s" (Filename.basename file)
       message)

let internal_error ~open_path exn =
  Diagnostic.error ~source:"internal" (region_of_file open_path)
    ("internal error: " ^ Printexc.to_string exn)

let diagnose ~position_encoding ~open_path text =
  let read filename =
    if String.equal filename open_path then
      Either.Right P4spectec.{ filename; contents = text }
    else
      match read_file filename with
      | contents -> Either.Right P4spectec.{ filename; contents }
      | exception Sys_error message -> Either.Left (filename, message)
  in
  let unreadable_files, spec_sources =
    List.partition_map read (spec_files_of open_path)
  in
  let sources =
    List.map
      (fun (source : P4spectec.spec_source) ->
        ( source.filename,
          Array.of_list (String.split_on_char '\n' source.contents) ))
      spec_sources
  in
  match unreadable_files with
  | _ :: _ ->
      List.map
        (fun (filename, message) ->
          of_diagnostic ~position_encoding ~sources
            (unreadable ~open_path filename message))
        unreadable_files
  | [] ->
      let _, report =
        P4spectec.with_diagnostics (fun () ->
            let* spec = P4spectec.parse_sources spec_sources in
            P4spectec.elab_spec spec)
      in
      List.filter_map
        (report_for ~position_encoding ~sources ~open_path)
        (Diagnostic.Report.to_sorted_list report)

let run ?(position_encoding = `UTF16) ~path text =
  let open_path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  try diagnose ~position_encoding ~open_path text
  with exn ->
    [
      of_diagnostic ~position_encoding ~sources:[]
        (internal_error ~open_path exn);
    ]
