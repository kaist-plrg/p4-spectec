//! Invocation and evaluation frames
//!
//! Frame messages are formatted only after evaluation fails.

use crate::{
    diagnostic::Report,
    lang::{
        common::source::Span,
        il::ast::{Id, Typ},
        traits::print::Print,
    },
};

/// Wraps failures in an `execution failed` frame.
pub fn execution(children: Vec<Report>) -> Box<Report> {
    Box::new(Report::frame(Span::default(), "execution failed", children))
}

/// Describes a function invocation, including supplied type arguments.
pub fn function(id: &Id, targs: &[Typ]) -> String {
    // Omit the angle brackets for monomorphic calls
    if targs.is_empty() {
        return format!("while invoking ${}", id.node);
    }
    // Format instantiated arguments only on the failure path
    format!(
        "while invoking ${}<{}>",
        id.node,
        targs
            .iter()
            .map(Print::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// Returns the maximum report depth without recursion.
pub fn depth(report: &Report) -> usize {
    let mut depth = 0;
    let mut pending = vec![(report, 1)];
    // Bound stack use for long invocation chains
    while let Some((report, depth_report)) = pending.pop() {
        depth = depth.max(depth_report);
        pending.extend(
            report
                .children
                .iter()
                .map(|report| (report, depth_report + 1)),
        );
    }
    depth
}
