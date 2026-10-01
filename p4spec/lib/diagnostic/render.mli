(** [render_report] renders diagnostics sorted by severity and source location.
    It reads referenced source files for snippets. [show_trace] defaults to
    [true]. *)
val render_report : ?show_trace:bool -> ansi:Ansi.t -> Record.Report.t -> string
