module Diagnostic = P4spectec.Diagnostic

let render_diagnostics (report : Diagnostic.Report.t) : unit =
  if not (Diagnostic.Report.is_empty report) then
    let ansi = Diagnostic.Ansi.auto ~tty:(Unix.isatty Unix.stderr) in
    Printf.eprintf "%s\n%!" (Diagnostic.Render.render_report ~ansi report)

(* A failure from either stage renders as a diagnostic and exits with 1. *)
let run_with_diagnostics ~(action : unit -> 'a P4spectec.result)
    ~(on_success : 'a -> unit P4spectec.result) : unit =
  let result, report = P4spectec.with_diagnostics action in
  render_diagnostics report;
  match result with
  | Error _ -> exit 1
  | Ok value ->
      let result, report =
        P4spectec.with_diagnostics (fun () -> on_success value)
      in
      render_diagnostics report;
      if Result.is_error result then exit 1
