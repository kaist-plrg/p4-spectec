use super::{report, span};
use p4spec_rust::{
    diagnostic::{Label, LabelStyle, RenderConfig, RenderError, Renderer},
    lang::common::source::Span,
};
use std::{
    fs,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

struct File(PathBuf);

impl File {
    fn new(bytes: &[u8]) -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let path = std::env::temp_dir().join(format!(
            "p4spec-diagnostic-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::write(&path, bytes).unwrap();
        Self(path)
    }

    fn name(&self) -> &str {
        self.0.to_str().unwrap()
    }
}

impl Drop for File {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.0);
    }
}

#[test]
fn two_files_keep_label_roles_messages_and_notes() {
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("input.watsup", "foo bar\n");
    renderer.insert_source("decl.watsup", "var foo : nat\n");
    let mut report = report(span("input.watsup", 1, 0, 1, 3));
    super::cause_mut(&mut report).labels.push(Label {
        style: LabelStyle::Secondary,
        span: span("decl.watsup", 1, 4, 1, 7),
        message: "first declared here".to_owned(),
        line_only: false,
    });
    let text = renderer.render_to_string(&report).unwrap();
    for part in [
        "input.watsup:1:1",
        "decl.watsup:1:5",
        "^^^ invalid escape",
        "--- first declared here",
        "= use a supported escape",
    ] {
        assert!(text.contains(part), "{part}: {text}");
    }
    assert!(!text.contains('\u{1b}'));
}

#[test]
fn overrides_and_successful_disk_reads_are_cached() {
    let file = File::new(b"disk\n");
    let report = report(span(file.name(), 1, 0, 1, 4));
    let mut renderer = Renderer::new(RenderConfig::default());
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.contains("disk"));
    fs::write(&file.0, b"changed\n").unwrap();
    assert_eq!(renderer.render_to_string(&report).unwrap(), text);
    renderer.insert_source(file.name(), "override\n");
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.contains("override"));
    assert!(!text.contains("changed"));
}

#[test]
fn byte_columns_crlf_tabs_multiline_and_eof_resolve() {
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("utf8", "é\tx\r\nnext\n");
    let text = renderer
        .render_to_string(&report(span("utf8", 1, 3, 1, 4)))
        .unwrap();
    assert!(text.contains("utf8:1:3"), "{text}");
    assert!(text.contains("^ invalid escape"), "{text}");
    let text = renderer
        .render_to_string(&report(span("utf8", 1, 3, 2, 4)))
        .unwrap();
    assert!(text.contains("next"), "{text}");
    let text = renderer
        .render_to_string(&report(span("utf8", 3, 0, 3, 0)))
        .unwrap();
    assert!(text.contains("utf8:3:1"), "{text}");
    renderer.insert_source("empty", "");
    assert!(
        renderer
            .render_to_string(&report(span("empty", 1, 0, 1, 0)))
            .unwrap()
            .contains("empty:1:1")
    );
}

#[test]
fn unavailable_sources_keep_the_responsible_location() {
    let file = File::new(&[0xff]);
    let file_missing = File::new(b"");
    fs::remove_file(&file_missing.0).unwrap();
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("related", "abc");
    for (span, loc) in [
        (Span::default(), "generated"),
        (span("file-only", 0, 0, 0, 0), "file-only"),
        (span(file_missing.name(), 2, 1, 2, 3), file_missing.name()),
        (span(file.name(), 1, 0, 1, 1), file.name()),
    ] {
        let mut report = report(span.clone());
        super::cause_mut(&mut report).labels.push(Label {
            style: LabelStyle::Secondary,
            span: super::span("related", 1, 0, 1, 3),
            message: "related declaration".to_owned(),
            line_only: false,
        });
        let text = renderer.render_to_string(&report).unwrap();
        assert!(text.contains(loc), "{text}");
        assert!(text.contains("at "), "{text}");
        assert!(text.contains("invalid escape"), "{text}");
        assert_eq!(super::cause(&report).labels[0].span, span);
    }
    fs::write(&file_missing.0, b"now present\n").unwrap();
    let text = renderer
        .render_to_string(&report(span(file_missing.name(), 2, 1, 2, 3)))
        .unwrap();
    assert!(text.contains("unavailable"), "{text}");
    renderer.insert_source(file_missing.name(), "one\ntwo\n");
    assert!(
        renderer
            .render_to_string(&report(span(file_missing.name(), 2, 1, 2, 3)))
            .unwrap()
            .contains("two")
    );
}

#[test]
fn malformed_known_text_spans_fail_instead_of_being_clamped() {
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("known", "éx\n");
    for span in [
        span("known", 1, 1, 1, 2),
        span("known", 1, 0, 1, 99),
        span("known", 3, 0, 3, 0),
        span("known", 1, 3, 1, 2),
        span("known", 0, 1, 1, 2),
        Span::new(super::span("known", 1, 0, 1, 1).left, super::span("other", 1, 0, 1, 1).right),
    ] {
        assert!(matches!(
            renderer.render_to_string(&report(span)),
            Err(RenderError::InvalidSpan { .. })
        ));
    }
}

#[test]
fn extreme_byte_columns_do_not_panic_while_reporting_bad_locations() {
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("known", "abc");
    let error = renderer
        .render_to_string(&report(span("known", 1, usize::MAX, 1, usize::MAX)))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("byte column is outside the line")
    );
    let file = File::new(b"");
    fs::remove_file(&file.0).unwrap();
    let text = renderer
        .render_to_string(&report(span(file.name(), 1, usize::MAX, 1, usize::MAX)))
        .unwrap();
    assert!(text.contains("source unavailable"));
}

#[test]
fn fallback_locations_use_readable_roles_and_colon_coordinates() {
    let mut renderer = Renderer::new(RenderConfig::default());
    let mut report = report(span("missing-input", 2, 3, 2, 5));
    super::cause_mut(&mut report).labels.push(Label {
        style: LabelStyle::Secondary,
        span: span("missing-decl", 4, 0, 4, 2),
        message: "declared here".to_owned(),
        line_only: false,
    });
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.contains("at missing-input:2:4-missing-input:2:6: invalid escape"), "{text}");
    assert!(
        text.contains("related location at missing-decl:4:1-missing-decl:4:3: declared here"),
        "{text}"
    );
    assert!(!text.contains("primary"), "{text}");
    let text = renderer
        .render_to_string(&super::report(span("missing-input", 0, 0, 0, 0)))
        .unwrap();
    assert!(text.contains("at missing-input: invalid escape"), "{text}");
    assert!(!text.contains("no source range"), "{text}");
}

#[test]
fn control_characters_in_source_never_reach_the_terminal() {
    for control in ['\u{000b}', '\u{001b}', '\u{007f}', '\u{0085}', '\r'] {
        let text = format!("abc\n{control}x\n");
        let file = File::new(text.as_bytes());
        let mut renderer = Renderer::new(RenderConfig::default());
        // Context lines must be safe even when the label itself is printable
        let report = report(span(file.name(), 1, 0, 1, 3));
        let rendered = renderer.render_to_string(&report).unwrap();
        assert!(!rendered.contains(control), "{rendered:?}");
        assert!(
            rendered.contains("snippet omitted: source contains control characters"),
            "{rendered}"
        );
        assert!(
            rendered.contains(&format!("at {}:1:1-{}:1:4", file.name(), file.name())),
            "{rendered}"
        );
        renderer.insert_source(file.name(), "safe\n");
        assert!(renderer.render_to_string(&report).unwrap().contains("safe"));
        renderer.insert_source(file.name(), text);
        assert!(
            !renderer
                .render_to_string(&report)
                .unwrap()
                .contains(control)
        );
        // Source suppression must not mask malformed producer coordinates
        assert!(matches!(
            renderer.render_to_string(&super::report(span(file.name(), 1, 0, 1, 99))),
            Err(RenderError::InvalidSpan { .. })
        ));
    }
}

#[test]
fn source_names_escape_terminal_controls_without_changing_source_identity() {
    let file = "input\u{1b}[31m\n.watsup";
    let mut renderer = Renderer::new(RenderConfig::default());
    let report = report(span(file, 1, 0, 1, 1));
    for available in [false, true] {
        if available {
            renderer.insert_source(file, "x");
        }
        let text = renderer.render_to_string(&report).unwrap();
        assert!(!text.contains('\u{1b}'), "{text:?}");
        assert!(text.contains(&file.escape_debug().to_string()), "{text}");
        assert_eq!(super::cause(&report).labels[0].span.left.file.as_ref(), file);
    }
    let text = renderer
        .render_to_string(&super::report(span(file, 0, 0, 0, 0)))
        .unwrap();
    assert!(!text.contains('\u{1b}'), "{text:?}");
    assert!(text.contains(&file.escape_debug().to_string()), "{text}");
}

#[test]
fn logical_line_labels_do_not_apply_expanded_columns_to_source() {
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("original.p4", "x\n");
    let span = span("original.p4", 73, 250, 73, 300);
    let mut report = report(span.clone());
    let label = &mut super::cause_mut(&mut report).labels[0];
    *label = label.clone().with_line_only();
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.contains("at original.p4:73"), "{text}");
    assert!(text.contains("column unavailable after preprocessing"), "{text}");
    assert!(!text.contains(":251"), "{text}");
    assert!(!text.contains("┌─"), "{text}");
    assert_eq!(super::cause_mut(&mut report).labels[0].span, span);
}
