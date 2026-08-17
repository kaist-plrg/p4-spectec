module Diagnostic = P4spectec.Diagnostic

let render_diagnostics (report : Diagnostic.Report.t) : unit =
  if not (Diagnostic.Report.is_empty report) then
    let ansi = Diagnostic.Ansi.auto ~tty:(Unix.isatty Unix.stderr) in
    Printf.eprintf "%s\n%!" (Diagnostic.Render.render_report ~ansi report)

let run_with_diagnostics ~(on_success : 'a -> unit)
    (action : unit -> 'a P4spectec.result) : unit =
  let result, report = P4spectec.with_diagnostics action in
  render_diagnostics report;
  match result with
  | Ok value ->
      let (), report = P4spectec.with_warnings (fun () -> on_success value) in
      render_diagnostics report
  | Error _ -> ()
