open Util.Source

type severity = Error | Warning

let severity_rank = function Error -> 0 | Warning -> 1
let compare_severity a b = Int.compare (severity_rank a) (severity_rank b)

let compare_location_start region_l region_r =
  if region_l.left.file = region_r.left.file then
    if region_l.left.line = region_r.left.line then
      compare region_l.left.column region_r.left.column
    else compare region_l.left.line region_r.left.line
  else compare region_l.left.file region_r.left.file

type related = { region : region; message : string }

type trace_node = {
  region : region;
  message : string;
  children : trace_node list;
}

type t = {
  severity : severity;
  region : region;
  code : string option;
  message : string;
  detail : string option;
  related : related list;
  trace : trace_node list;
  source : string;
}

let quote value =
  let inline_safe character =
    character <> '`' && character >= ' ' && character <> '\x7f'
  in
  if String.for_all inline_safe value then "`" ^ value ^ "`"
  else
    let buffer = Buffer.create (String.length value + 2) in
    Buffer.add_char buffer '"';
    String.iter
      (function
        | '`' -> Buffer.add_string buffer "\\x60"
        | '\n' -> Buffer.add_string buffer "\\n"
        | '\r' -> Buffer.add_string buffer "\\r"
        | '\t' -> Buffer.add_string buffer "\\t"
        | '"' -> Buffer.add_string buffer "\\\""
        | '\\' -> Buffer.add_string buffer "\\\\"
        | character
          when Char.code character < Char.code ' ' || character = '\x7f' ->
            Buffer.add_string buffer
              (Printf.sprintf "\\x%02X" (Char.code character))
        | character -> Buffer.add_char buffer character)
      value;
    Buffer.add_char buffer '"';
    Buffer.contents buffer

let error ?code ?detail ?(related = []) ?(trace = []) ~source region message =
  { severity = Error; region; code; message; detail; related; trace; source }

let warning ?code ?detail ~source region message =
  {
    severity = Warning;
    region;
    code;
    message;
    detail;
    related = [];
    trace = [];
    source;
  }

let rec trace_of_failtrace (failtrace : Util.Attempt.failtrace) : trace_node =
  let (Util.Attempt.Failtrace (region, message, children)) = failtrace in
  {
    region;
    message = message ();
    children = List.map trace_of_failtrace children;
  }

let traces_of_failtraces = List.map trace_of_failtrace

let of_failtraces ~source ~fallback (failtraces : Util.Attempt.failtrace list) :
    t =
  let at = Util.Attempt.region_of_failtraces failtraces in
  let message, trace =
    match failtraces with
    | [] -> (fallback, [])
    | [ failtrace ] ->
        let { message; children; _ } = trace_of_failtrace failtrace in
        (message, children)
    | _ -> (fallback, traces_of_failtraces failtraces)
  in
  error ~source ~trace at message

module Report = struct
  type diagnostic = t
  type t = diagnostic list

  let singleton d = [ d ]

  (* Reports are stored in reverse insertion order. *)
  let merge earlier later = later @ earlier

  let to_sorted_list ds =
    List.rev ds
    |> List.sort (fun a b ->
           match compare_severity a.severity b.severity with
           | 0 -> compare_location_start a.region b.region
           | c -> c)

  let is_empty = function [] -> true | _ -> false
end
