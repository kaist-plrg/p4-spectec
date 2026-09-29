use p4spec_rust::{
    diagnostic::{Diagnostic, RenderConfig, Renderer, Report, ReportKind},
    lang::common::source::Span,
};

pub trait ReportExt {
    fn code(&self) -> Option<&str>;
    fn render(&self) -> String;
    fn span(&self) -> Span;
    fn diagnostic(&self) -> &Diagnostic;
    fn find_code(&self, code: &str) -> Option<&Report>;
}

pub trait IntoReport {
    fn into_report(self) -> Box<Report>;
}

impl IntoReport for Box<Report> {
    fn into_report(self) -> Box<Report> {
        self
    }
}

impl ReportExt for Report {
    fn code(&self) -> Option<&str> {
        match &self.kind {
            ReportKind::Cause(diagnostic) => diagnostic.code.as_deref(),
            ReportKind::Frame { .. } => None,
        }
    }
    fn render(&self) -> String {
        Renderer::new(RenderConfig::default())
            .render_to_string(self)
            .unwrap()
    }

    fn span(&self) -> Span {
        match &self.kind {
            ReportKind::Frame { span, .. } => span.clone(),
            ReportKind::Cause(diagnostic) => diagnostic
                .labels
                .first()
                .map(|label| label.span.clone())
                .unwrap_or_default(),
        }
    }

    fn diagnostic(&self) -> &Diagnostic {
        let ReportKind::Cause(diagnostic) = &self.kind else { panic!("expected cause") };
        diagnostic
    }

    fn find_code(&self, code: &str) -> Option<&Report> {
        let mut pending = vec![self];
        while let Some(report) = pending.pop() {
            if let ReportKind::Cause(diagnostic) = &report.kind
                && diagnostic.code.as_deref() == Some(code)
            {
                return Some(report);
            }
            pending.extend(report.children.iter().rev());
        }
        None
    }
}
