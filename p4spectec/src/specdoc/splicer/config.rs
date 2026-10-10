//! AsciiDoc wrappers around rendered splice bodies
//!
//! Source bodies become collapsible `[source,watsup]` listings inside `====`.
//! LaTeX bodies use `[latexmath]` with `++++` passthrough delimiters.
//! Both are conditional on `backend-html5`; prose bodies use `****` sidebars.
//! Prose markers add the `prose-algorithm` role to their sidebar.

/// Opens a collapsible source listing for HTML output.
pub(super) const PREFIX_SOURCE: &str = "ifdef::backend-html5[]\n.Click to view the specification source\n[%collapsible]\n====\n[source,watsup]\n----\n";
/// Closes the listing and adds the empty block used for source spacing.
pub(super) const SUFFIX_SOURCE: &str = "\n----\n====\n\n[.empty]\n--\n\n\n--\n\nendif::[]";
/// Opens a LaTeX passthrough block for HTML output.
pub(super) const PREFIX_LATEX: &str = "ifdef::backend-html5[]\n[latexmath]\n++++\n";
/// Closes the LaTeX block and its HTML conditional.
pub(super) const SUFFIX_LATEX: &str = "\n++++\nendif::[]";
/// Opens a prose sidebar.
pub(super) const PREFIX_PROSE: &str = "[.prose-algorithm]\n****\n";
/// Closes a prose sidebar.
pub(super) const SUFFIX_PROSE: &str = "\n****";
