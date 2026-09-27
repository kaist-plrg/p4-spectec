//! Hyperlink ownership across rendering, layout, and serialization
//!
//! ```text
//! link_unowned_doc(a, Concat([x, Link(b, y)]))   -> Concat([Link(a, x), Link(b, y)])
//! link_unowned_doc(a, Fill(_, s, [x, y]))        -> Fill(_, s, [Link(a, x), Link(a, y)])
//! link_resolved_doc(a, LeftStack([x, y]))        -> LeftStack([Link(a, x), Link(a, y)])
//! strip_links(Link(a, x))                        -> x
//! ```
//!
//! Renderer, layout, and serializer call these in that order.

use super::doc::*;
use crate::backend_doc::latex::error::{Error, Result};

// == Targets
//
//   Target::of_string("Eval_0")   -> Ok(Target("Eval_0"))
//   Target::of_string("a-b")      -> Err(InvalidLinkTarget("a-b"))

impl Target {
    /// Validates a nonempty local target containing ASCII names and primes.
    pub(crate) fn of_string(text: &str) -> Result<Target> {
        let is_target_byte =
            |byte: u8| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'\'');
        if !text.is_empty() && text.bytes().all(is_target_byte) {
            Ok(Target(text.into()))
        } else {
            Err(Error::InvalidLinkTarget(text.into()))
        }
    }
}

// == Ownership analysis
//
//   has_link_doc(Concat([x, Link(a, y)]))   -> true
//   has_boundary_doc(Fill(_, s, [x, y]))    -> true
//   has_boundary_doc(Sub(x, i))             -> false

/// Detects existing links in a document.
fn has_link_doc(doc: &Doc) -> bool {
    matches!(doc, Doc::Link(..)) || doc.children().into_iter().any(has_link_doc)
}

/// Detects regions that cannot share one enclosing fallback link.
fn has_boundary_doc(doc: &Doc) -> bool {
    match doc {
        // Explicit links keep their targets; fill items are linked one by one
        Doc::Link(..) | Doc::Fill(..) => true,
        // Other documents inherit the boundaries of their children
        doc => doc.children().into_iter().any(has_boundary_doc),
    }
}

// == Fallback insertion
//
//   link_unowned_doc(a, Concat([x, y, Link(b, z), w]))
//   -> Concat([Link(a, Concat([x, y])), Link(b, z), Link(a, w)])

/// Links unowned regions while retaining explicit targets and fill separators.
pub(crate) fn link_unowned_doc(target: &Target, doc: Doc) -> Doc {
    // Give a boundary-free region one enclosing link
    if !has_boundary_doc(&doc) {
        return Doc::link(target.clone(), doc);
    }
    match doc {
        // Coalesce adjacent boundary-free children
        Doc::Concat(docs) => link_unowned_concat(target, docs),
        // Flatten nested ownership only when an inner explicit link exists
        Doc::Link(target_existing, doc_linked) => {
            if has_link_doc(&doc_linked) {
                link_unowned_doc(&target_existing, *doc_linked)
            } else {
                Doc::Link(target_existing, doc_linked)
            }
        }
        // Push the target into every child of other boundary regions
        doc => doc.map_children(|doc| link_unowned_doc(target, doc)),
    }
}

/// Coalesces adjacent boundary-free children into one fallback region.
fn link_unowned_concat(target: &Target, docs: Vec<Doc>) -> Doc {
    let mut docs_unowned = Vec::new();
    let mut docs_linked = Vec::new();
    // Flush pending content before each explicitly owned boundary
    for doc in docs {
        if !has_boundary_doc(&doc) {
            docs_unowned.push(doc);
            continue;
        }
        if !docs_unowned.is_empty() {
            let docs_pending = std::mem::take(&mut docs_unowned);
            let doc_pending = Doc::concat(docs_pending);
            let doc_linked = Doc::link(target.clone(), doc_pending);
            docs_linked.push(doc_linked);
        }
        let doc_linked = link_unowned_doc(target, doc);
        docs_linked.push(doc_linked);
    }
    // Preserve the final unowned suffix
    if !docs_unowned.is_empty() {
        let doc_pending = Doc::concat(docs_unowned);
        let doc_linked = Doc::link(target.clone(), doc_pending);
        docs_linked.push(doc_linked);
    }
    Doc::concat(docs_linked)
}

// == Resolved documents
//
//   link_resolved_doc(a, LeftStack([x, y]))   -> LeftStack([Link(a, x), Link(a, y)])

/// Links a resolved document, giving each concrete line and grid cell its own link.
pub(super) fn link_resolved_doc(target: &Target, doc: Doc) -> Doc {
    match doc {
        // Resolved lines and grid cells each receive their own link
        Doc::LeftStack(_) | Doc::Grid(..) => doc.map_children(|doc| link_unowned_doc(target, doc)),
        // Other documents follow ordinary fallback insertion
        doc => link_unowned_doc(target, doc),
    }
}

// == Invisible geometry
//
//   strip_links(Sub(Link(a, x), i))   -> Sub(x, i)

/// Removes every hyperlink while preserving all layout structure.
pub(super) fn strip_links(doc: Doc) -> Doc {
    match doc {
        // Keep only the visible document of a link
        Doc::Link(_, doc) => strip_links(*doc),
        // Fill separators are children only here
        Doc::Fill(indent, separator, docs) => {
            let separator = strip_links(*separator);
            let docs = docs.into_iter().map(strip_links).collect();
            Doc::Fill(indent, Box::new(separator), docs)
        }
        doc => doc.map_children(strip_links),
    }
}
