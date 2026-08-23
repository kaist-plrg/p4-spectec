module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

(* Failures reported by the p4spectec entry points *)

type t =
  | PassError of Pass.error
  | RunError of Run.error
  | CommandError of string
  | CommandDiagnostic of Diagnostic.t

type command_code =
  | Boot_source_conflict
  | Boot_source_required
  | Splice_file_count_mismatch

let string_of_command_code = function
  | Boot_source_conflict -> "boot-source-conflict"
  | Boot_source_required -> "boot-source-required"
  | Splice_file_count_mismatch -> "splice-file-count-mismatch"

let command_diagnostic code message =
  Diagnostic.error
    ~code:("command/" ^ string_of_command_code code)
    ~source:"command" no_region message

let boot_source_conflict =
  CommandDiagnostic
    (command_diagnostic Boot_source_conflict
       "options `-boot-dir` and `-boot-file` cannot be used together")

let boot_source_required =
  CommandDiagnostic
    (command_diagnostic Boot_source_required
       "either `-boot-dir` or `-boot-file` is required")

let splice_file_count_mismatch inputs outputs =
  let plural count = if count = 1 then "" else "s" in
  CommandDiagnostic
    (command_diagnostic Splice_file_count_mismatch
       (Format.asprintf
          "splice expects equal numbers of input and output files, but got %d \
           input file%s and %d output file%s"
          inputs (plural inputs) outputs (plural outputs)))

let to_diagnostic = function
  | PassError e -> e
  | RunError e -> e
  | CommandError msg -> Diagnostic.error ~source:"command" no_region msg
  | CommandDiagnostic diagnostic -> diagnostic

let to_diagnostics (e : t) : Diagnostic.Report.t =
  Diagnostic.Report.singleton (to_diagnostic e)

let to_string (e : t) : string =
  let at, msg = Diagnostic.region_msg (to_diagnostic e) in
  Util.Error.string_of_error at msg
