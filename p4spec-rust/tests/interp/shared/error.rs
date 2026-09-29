use crate::interp::report::ReportExt;
use p4spec_rust::{
    diagnostic::Report,
    interp::shared::backtrack::Failure,
    lang::common::source::{Position, Span},
};

#[test]
fn rendering_preserves_branch_order_and_locations() {
    let span = Span::new(Position::new("spec", 3, 4), Position::new("spec", 3, 5));
    let report = Failure::Mismatch(vec![
        Report::frame(span, "first mismatch", vec![]),
        Report::frame(Span::default(), "second mismatch", vec![]),
    ])
    .into_report();
    let text = report.render();
    assert!(text.find("first mismatch").unwrap() < text.find("second mismatch").unwrap());
    assert!(text.contains("spec:3:5"), "{text}");
}

#[test]
fn deep_frames_render_and_drop_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(128 * 1024)
        .spawn(|| {
            let mut report = Report::frame(Span::default(), "leaf", vec![]);
            for idx in (0..20_000).rev() {
                report = Report::frame(Span::default(), format!("frame {idx}"), vec![report]);
            }
            let text = report.render();
            assert!(text.contains("frame 0"));
            assert!(text.contains("further reports omitted"));
            drop(report);
        })
        .unwrap()
        .join()
        .unwrap();
}
