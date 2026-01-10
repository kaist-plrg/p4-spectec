let now () =
  Core.Time_ns.now () |> Core.Time_ns.to_span_since_epoch
  |> Core.Time_ns.Span.to_sec
