open Util.Source

(* Every p4spectec entry point reports its failure as a diagnostic *)

type t = Diagnostic.t

(* [code] identifies a command-line diagnostic across runs. *)

type code =
  | Boot_source_conflict
  | Boot_source_required
  | Splice_file_count_mismatch

let string_of_code = function
  | Boot_source_conflict -> "boot-source-conflict"
  | Boot_source_required -> "boot-source-required"
  | Splice_file_count_mismatch -> "splice-file-count-mismatch"

let render_code (code : code) : string = "command/" ^ string_of_code code

(* Command-line errors *)

let error ?(code : code option) (msg : string) : t =
  Diagnostic.error
    ?code:(Option.map render_code code)
    ~source:"command" no_region msg

let error_boot_source_conflict =
  error ~code:Boot_source_conflict
    "options `-boot-dir` and `-boot-file` cannot be used together"

let error_boot_source_required =
  error ~code:Boot_source_required
    "either `-boot-dir` or `-boot-file` is required"

let error_splice_file_count_mismatch inputs outputs =
  let plural count = if count = 1 then "" else "s" in
  error ~code:Splice_file_count_mismatch
    (Format.asprintf
       "splice expects equal numbers of input and output files, but got %d \
        input file%s and %d output file%s"
       inputs (plural inputs) outputs (plural outputs))

(* Rendering *)

let to_report (error : t) : Diagnostic.Report.t =
  Diagnostic.Report.singleton error

let to_string (error : t) : string =
  let at, msg = Diagnostic.region_msg error in
  Util.Error.string_of_error at msg
