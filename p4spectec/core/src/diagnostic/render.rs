//! Source resolution and codespan output at presentation boundaries
//!
//! A renderer caches successful and unavailable source loads for its lifetime.
//! Explicit text overrides replace cached disk contents.
//! Reports keep their original spans even when source text is unavailable.
//! Child snippets follow tree connections; deep chains restart at the trace base
//! only when no pending sibling connection would be lost.
//! Rendering prepares output before writing stderr so invalid spans cannot
//! leave a partially printed diagnostic.

use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
};

use codespan_reporting::{
    diagnostic::{Diagnostic as CodeDiagnostic, Label as CodeLabel},
    files::{self, Files, SimpleFiles},
    term::{
        self,
        termcolor::{Buffer, BufferWriter, ColorSpec, WriteColor},
    },
};

use crate::lang::common::source::{Position, Span};

use super::{
    ColorChoice, Diagnostic, DisplayStyle, Label, LabelStyle, Report, ReportKind, Severity,
    SnippetConfig,
};

// = Helpers

/// Formats a span as file:line:column with one-based display columns.
fn span_location(span: &Span) -> String {
    let loc = |pos: &Position| {
        // Widen before adding one so even u32::MAX can be displayed
        format!("{}:{}:{}", pos.file.name().escape_debug(), pos.line, u64::from(pos.column) + 1)
    };
    if span.left == span.right {
        loc(&span.left)
    } else {
        format!("{}-{}", loc(&span.left), loc(&span.right))
    }
}

// = Tree presentation

const TRACE_FOLD_DEPTH: usize = 4;

/// Retains a sibling group's indentation anchor while deeper branches render.
#[derive(Clone, Copy)]
struct TraceLayout {
    // Depth whose children restart at the trace base
    anchor: usize,
    // No ancestor below the anchor has a pending sibling
    foldable: bool,
}

/// Builds the ancestor connections below the current indentation anchor.
fn trace_prefix(ancestors: &[bool], vertical: &str) -> String {
    let mut prefix = String::new();
    for has_next in ancestors {
        prefix.push_str(if *has_next { vertical } else { "   " });
    }
    prefix
}

/// Adds tree connections to every line without changing diagnostic colors.
struct TraceWriter<'a> {
    buffer: &'a mut Buffer,
    prefix_first: String,
    prefix_rest: String,
    first_line: bool,
    line_start: bool,
    color: ColorSpec,
}

impl Write for TraceWriter<'_> {
    /// Prefixes each physical line even when codespan writes it in pieces.
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        for line in bytes.split_inclusive(|byte| *byte == b'\n') {
            // Draw connections without inheriting a source or severity color
            if self.line_start {
                let prefix = if self.first_line { &self.prefix_first } else { &self.prefix_rest };
                self.buffer.reset()?;
                let prefix = if line == b"\n" { prefix.trim_end() } else { prefix };
                self.buffer.write_all(prefix.as_bytes())?;
                self.buffer.set_color(&self.color)?;
                self.line_start = false;
                self.first_line = false;
            }
            // A newline in this write starts a new prefixed line next time
            self.buffer.write_all(line)?;
            self.line_start = line.ends_with(b"\n");
        }
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.buffer.flush()
    }
}

impl WriteColor for TraceWriter<'_> {
    fn supports_color(&self) -> bool {
        self.buffer.supports_color()
    }

    fn set_color(&mut self, color: &ColorSpec) -> io::Result<()> {
        self.color.clone_from(color);
        self.buffer.set_color(color)
    }

    fn reset(&mut self) -> io::Result<()> {
        self.color.clear();
        self.buffer.reset()
    }
}

// = Configuration

/// Configures snippet presentation and the visible trace budget.
#[derive(Debug, Clone)]
pub struct RenderConfig {
    /// Delegates source layout policy to codespan.
    pub snippet: SnippetConfig,
    /// Overrides frame presentation, inheriting the snippet style when absent.
    pub frame_style: Option<DisplayStyle>,
    /// Selects color behavior for stderr output.
    pub color: ColorChoice,
    /// Limits visible trace nodes without modifying the report.
    pub trace_limit: usize,
}

impl Default for RenderConfig {
    fn default() -> Self {
        Self {
            snippet: SnippetConfig::default(),
            frame_style: None,
            color: ColorChoice::Auto,
            trace_limit: 64,
        }
    }
}

// = Rendering errors

/// Distinguishes inconsistent source coordinates from output failures.
#[derive(Debug, thiserror::Error)]
pub enum RenderError {
    /// Rejects a malformed span rather than changing its coordinates.
    #[error("invalid diagnostic span {}: {reason}", span_location(.span))]
    SpanInvalid {
        /// Preserves the rejected coordinates.
        span: Box<Span>,
        /// Explains the violated coordinate contract.
        reason: &'static str,
    },
    /// Preserves an error from codespan or the output writer.
    #[error(transparent)]
    Output(#[from] files::Error),
}

// = Source cache and renderer

/// Records whether cached source text can be printed safely as a snippet.
#[derive(Clone, Copy)]
struct Source {
    id: usize,
    printable: bool,
}

/// Resolves source text and renders reports without changing semantic state.
pub struct Renderer {
    config: RenderConfig,
    files: SimpleFiles<String, String>,
    cache: HashMap<String, Option<Source>>,
}

impl Renderer {
    // - invalid_*: span errors

    fn invalid_span(span: &Span, reason: &'static str) -> RenderError {
        RenderError::SpanInvalid { span: Box::new(*span), reason }
    }

    // - register_*: source storage

    /// Registers source text and marks whether its snippet is safe to print.
    fn register_source(&mut self, file: String, text: String) -> Source {
        // Codespan may print context lines outside the labelled range
        let mut chars = text.chars().peekable();
        let mut printable = true;
        while let Some(ch) = chars.next() {
            if ch.is_control()
                && ch != '\n'
                && ch != '\t'
                && !(ch == '\r' && chars.peek() == Some(&'\n'))
            {
                printable = false;
                break;
            }
        }
        // Keep the source untouched so span validation uses original byte offsets
        Source { id: self.files.add(file.escape_debug().to_string(), text), printable }
    }

    // - resolve_*: sources and offsets

    /// Caches both successful and unavailable reads under the original file name.
    fn resolve_source(&mut self, file: &str) -> Option<Source> {
        // An explicit override or previous load always wins
        if let Some(source) = self.cache.get(file) {
            return *source;
        }

        // Unreadable and non-UTF-8 files retain location-only diagnostics
        let source = fs::read_to_string(file)
            .ok()
            .map(|text| self.register_source(file.to_owned(), text));
        self.cache.insert(file.to_owned(), source);
        source
    }

    /// Resolves a one-based line and byte column without clamping either.
    fn resolve_offset(&self, id: usize, pos: &Position, span: &Span) -> Result<usize, RenderError> {
        let text = self.files.source(id)?;
        let (line, column) = (pos.line as usize, pos.column as usize);
        // SimpleFiles permits an extra sentinel line; source spans do not
        let line_max = self.files.line_index(id, text.len())? + 1;
        if line == 0 || line > line_max {
            return Err(Self::invalid_span(span, "line is outside the source"));
        }

        // A newline starts the next line; CR remains an original source byte
        let range = self.files.line_range(id, line - 1)?;
        let text_line = text[range.clone()]
            .strip_suffix('\n')
            .unwrap_or(&text[range.clone()]);
        if column > text_line.len() {
            return Err(Self::invalid_span(span, "byte column is outside the line"));
        }

        // Coordinates must lie between complete UTF-8 characters
        let offset = range.start + column;
        if !text.is_char_boundary(offset) {
            return Err(Self::invalid_span(span, "byte column splits a UTF-8 character"));
        }
        Ok(offset)
    }

    // - append_*: labels and location notes

    /// Retains a label's role and location when a snippet cannot be produced.
    fn append_location_note(
        label: &Label,
        loc: &str,
        reason: Option<&str>,
        diagnostic: &mut CodeDiagnostic<usize>,
    ) {
        let role = match label.style {
            LabelStyle::Primary => "at",
            LabelStyle::Secondary => "related location at",
        };
        let message =
            if label.message.is_empty() { String::new() } else { format!(": {}", label.message) };
        let reason = reason.map_or_else(String::new, |reason| format!(" ({reason})"));
        diagnostic
            .notes
            .push(format!("{role} {loc}{message}{reason}"));
    }

    /// Converts a label or preserves its location as an explicit fallback note.
    fn append_label(
        &mut self,
        label: &Label,
        diagnostic: &mut CodeDiagnostic<usize>,
    ) -> Result<(), RenderError> {
        let span = &label.span;
        // One codespan label cannot describe two source identities
        if span.left.file != span.right.file {
            return Err(Self::invalid_span(span, "endpoints name different files"));
        }

        // Generated and file-only spans have no line to underline
        let file_only = span.left.line == 0 && span.left.column == 0 && span.left == span.right;
        if file_only {
            let loc = if span.left.file.name().is_empty() {
                "generated source".to_owned()
            } else {
                span.left.file.name().escape_debug().to_string()
            };
            Self::append_location_note(label, &loc, None, diagnostic);
            return Ok(());
        }

        // Missing source must not hide a responsible location behind related labels
        let Some(source) = self.resolve_source(span.left.file.name()) else {
            Self::append_location_note(
                label,
                &span_location(span),
                Some("source unavailable"),
                diagnostic,
            );
            return Ok(());
        };
        let start = self.resolve_offset(source.id, &span.left, span)?;
        let end = self.resolve_offset(source.id, &span.right, span)?;
        if start > end {
            return Err(Self::invalid_span(span, "end precedes start"));
        }

        // Preserve coordinates without sending invisible controls to the terminal
        if !source.printable {
            Self::append_location_note(
                label,
                &span_location(span),
                Some("snippet omitted: source contains control characters"),
                diagnostic,
            );
            return Ok(());
        }

        // Preserve the producer's role, range, and explanation
        diagnostic.labels.push(CodeLabel {
            style: label.style,
            file_id: source.id,
            range: start..end,
            message: label.message.clone(),
        });
        Ok(())
    }

    // - convert_*: codespan diagnostics

    /// Converts diagnostic data without inspecting the report tree.
    fn convert_diagnostic(
        &mut self,
        diagnostic: &Diagnostic,
    ) -> Result<CodeDiagnostic<usize>, RenderError> {
        let mut rendered = CodeDiagnostic::new(diagnostic.severity);
        rendered.code.clone_from(&diagnostic.code);
        rendered.message.clone_from(&diagnostic.message);
        rendered.notes.clone_from(&diagnostic.notes);
        // Uncoded failures still identify their author
        if diagnostic.code.is_none() && !diagnostic.source.is_empty() {
            rendered
                .notes
                .push(format!("source: {}", diagnostic.source));
        }
        // Convert each label independently to retain cross-file relationships
        for label in &diagnostic.labels {
            self.append_label(label, &mut rendered)?;
        }
        Ok(rendered)
    }

    /// Includes the frame's source location in Short and Medium headers.
    fn convert_report_kind(
        &mut self,
        kind: &ReportKind,
        style: &DisplayStyle,
    ) -> Result<CodeDiagnostic<usize>, RenderError> {
        match kind {
            // Render context as a note with its own source location
            ReportKind::Frame { span, message } => {
                let mut rendered = CodeDiagnostic::new(Severity::Note).with_message(message);
                if *span != Span::default() {
                    self.append_label(
                        &Label {
                            style: if matches!(style, DisplayStyle::Rich) {
                                LabelStyle::Secondary
                            } else {
                                LabelStyle::Primary
                            },
                            span: *span,
                            message: String::new(),
                        },
                        &mut rendered,
                    )?;
                }
                Ok(rendered)
            }
            // Keep each cause's code, severity, labels, and notes
            ReportKind::Cause(diagnostic) => self.convert_diagnostic(diagnostic),
        }
    }

    // - Construction and source overrides

    /// Constructs a renderer with an empty source cache.
    pub fn new(config: RenderConfig) -> Self {
        Self { config, files: SimpleFiles::new(), cache: HashMap::new() }
    }

    /// Supplies source text that takes precedence over disk contents.
    pub fn insert_source(&mut self, file: impl Into<String>, text: impl Into<String>) {
        let file = file.into();
        let source = self.register_source(file.clone(), text.into());
        self.cache.insert(file, Some(source));
    }

    // - render_to_*: output destinations

    /// Renders a node, preserving locations when snippets are unavailable.
    fn render_kind(
        &mut self,
        writer: &mut impl WriteColor,
        kind: &ReportKind,
    ) -> Result<(), RenderError> {
        let mut config = self.config.snippet.clone();
        // Override only frames, including roots and frames without children
        if matches!(kind, ReportKind::Frame { .. })
            && let Some(style) = &self.config.frame_style
        {
            config.display_style = style.clone();
        }
        let diagnostic = self.convert_report_kind(kind, &config.display_style)?;
        // Unavailable and generated sources store their frame location as a note
        if matches!(kind, ReportKind::Frame { .. })
            && matches!(config.display_style, DisplayStyle::Short)
            && !diagnostic.notes.is_empty()
        {
            config.display_style = DisplayStyle::Medium;
        }
        term::emit_to_write_style(writer, &config, &self.files, &diagnostic)?;
        Ok(())
    }

    /// Emits the root and traverses visible causes in depth-first branch order.
    fn render_to_buffer(
        &mut self,
        buffer: &mut Buffer,
        report: &Report,
    ) -> Result<(), RenderError> {
        self.render_kind(buffer, &report.kind)?;

        // Match the snippet character set for terminals using ASCII borders
        let (branch, last, vertical, continuation) =
            if self.config.snippet.chars.source_border_left.is_ascii() {
                ("|- ", "`- ", "|  ", "...")
            } else {
                ("├─ ", "└─ ", "│  ", "⋮")
            };
        // Store cursors and ancestor continuations without recursive rendering
        let mut pending = vec![(report.children.iter(), TraceLayout { anchor: 0, foldable: true })];
        let mut ancestors = Vec::new();
        let mut count = 0;
        while let Some((children, layout)) = pending.last_mut() {
            // Restore the parent's layout when this sibling group is exhausted
            let Some(child) = children.next() else {
                pending.pop();
                ancestors.pop();
                continue;
            };
            // Restart deep chains without cutting a pending sibling connection
            let level = ancestors.len() + 1;
            if count < self.config.trace_limit
                && layout.foldable
                && level - layout.anchor > TRACE_FOLD_DEPTH
            {
                buffer
                    .set_color(ColorSpec::new().set_dimmed(true))
                    .map_err(files::Error::from)?;
                writeln!(buffer, "{continuation} (depth {level}, continued)")
                    .map_err(files::Error::from)?;
                buffer.reset().map_err(files::Error::from)?;
                layout.anchor = level - 1;
            }
            let prefix = trace_prefix(&ancestors[layout.anchor..], vertical);
            // Close each remaining branch at the limit without changing reports
            if count == self.config.trace_limit {
                writeln!(
                    buffer,
                    "{prefix}{last}... further reports omitted (trace limit: {count})"
                )
                .map_err(files::Error::from)?;
                *children = [].iter();
                continue;
            }
            count += 1;
            let has_next = !children.as_slice().is_empty();
            // Keep snippets and multiline notes connected to the same branch
            let mut writer = TraceWriter {
                buffer,
                prefix_first: format!("{prefix}{}", if has_next { branch } else { last }),
                prefix_rest: format!("{prefix}{}", if has_next { vertical } else { "   " }),
                first_line: true,
                line_start: true,
                color: ColorSpec::new(),
            };
            self.render_kind(&mut writer, &child.kind)?;
            // Any unvisited sibling prevents folding until its branch is closed
            let layout_children =
                TraceLayout { anchor: layout.anchor, foldable: layout.foldable && !has_next };
            ancestors.push(has_next);
            pending.push((child.children.iter(), layout_children));
        }
        Ok(())
    }

    /// Renders a report to a string without terminal color codes.
    pub fn render_to_string(&mut self, report: &Report) -> Result<String, RenderError> {
        let mut buffer = Buffer::no_color();
        self.render_to_buffer(&mut buffer, report)?;
        // Codespan and tree connections write only UTF-8 text
        Ok(String::from_utf8(buffer.into_inner()).expect("diagnostic output is UTF-8"))
    }

    /// Renders a complete report to stderr using the configured colors.
    pub fn render_to_stderr(&mut self, report: &Report) -> Result<(), RenderError> {
        let writer = BufferWriter::stderr(self.config.color);
        let mut buffer = writer.buffer();
        self.render_to_buffer(&mut buffer, report)?;
        writer.print(&buffer).map_err(files::Error::from)?;
        Ok(())
    }
}
