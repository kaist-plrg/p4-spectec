//! Fallthrough anchors and labels for prose-language rendering
//!
//! ```text
//! Next, where the next arm bk-f-1-arm-2 is displayed as 2
//! -> +++<sub class="bk-mark">[<a href="#bk-f-1-arm-2">→ 2</a>]</sub>+++
//!
//! Group(g), in namespace Rel
//! -> +++<sub class="bk-mark">[<a href="#Rel-g">→ g</a>]</sub>+++
//!
//! Else, in namespace f
//! -> +++<sub class="bk-mark">[<a href="#f-else">→ ⋅</a>]</sub>+++
//!
//! Fail
//! -> +++<sub class="bk-mark">[FAIL]</sub>+++
//! ```

use std::collections::BTreeMap;

use crate::lang::pl::ast::{Fallthrough, Instr};

use super::doc::doc::{FallthroughLabel, Prose};

// == Context
//
//   Context::new("Rel")   -> Context { namespace: "Rel", next: None }

/// The fallthrough destination visible while rendering one instruction.
#[derive(Clone, Debug)]
pub struct Context {
    pub namespace: String,
    pub next: Option<String>,
}

impl Context {
    /// Starts a definition's context, where no enclosing arm follows.
    pub fn new(namespace: &str) -> Self {
        Context { namespace: namespace.to_owned(), next: None }
    }
}

// == Anchors

// - Block anchors
//
//   fresh_block("f"), fresh_block("f"), fresh_block("g")   -> bk-f-1, bk-f-2, bk-g-1

/// Per-document counters for backtracking blocks.
#[derive(Default)]
pub struct Anchors {
    block_counters: BTreeMap<String, usize>,
}

impl Anchors {
    /// Allocates the next block anchor in a namespace.
    pub fn fresh_block(&mut self, namespace: &str) -> String {
        let num_blocks = self.block_counters.entry(namespace.to_owned()).or_default();
        *num_blocks += 1;
        format!("bk-{namespace}-{num_blocks}")
    }
}

// - Target anchors
//
//   anchor_of_arm("bk-f-1", 0)        -> bk-f-1-arm-1
//   anchor_of_group("Rel/x", "g/h")   -> Rel-x-g-h
//   anchor_of_else("f")               -> f-else

/// Returns the anchor of an ordered arm.
pub fn anchor_of_arm(anchor_block: &str, idx: usize) -> String {
    let num = idx + 1;
    format!("{anchor_block}-arm-{num}")
}

/// Returns a rule-group anchor, sanitizing path separators.
pub fn anchor_of_group(namespace: &str, id_group: &str) -> String {
    let namespace_sanitized = namespace.replace('/', "-");
    let id_group_sanitized = id_group.replace('/', "-");
    format!("{namespace_sanitized}-{id_group_sanitized}")
}

/// Returns the anchor of an otherwise block.
pub fn anchor_of_else(namespace: &str) -> String {
    format!("{namespace}-else")
}

// == Rendering

impl Prose {
    // - Fallthrough links
    //
    //   None   -> (empty)
    //   Fail   -> +++<sub class="bk-mark">[FAIL]</sub>+++

    /// Renders an instruction's fallthrough marker.
    pub(super) fn of_fallthrough_link<Tier>(ctx: &Context, instr: &Instr<Tier>) -> Prose {
        match &instr.node.note {
            None => Prose::Empty,
            Some(Fallthrough::Next) => Prose::of_next_link(ctx),
            Some(Fallthrough::Group(id_group)) => Prose::of_group_link(ctx, &id_group.node),
            Some(Fallthrough::Else) => Prose::of_else_link(ctx),
            Some(Fallthrough::Fail) => Prose::of_fail_link(),
        }
    }

    // - Next-arm links
    //
    //   next arm bk-f-1-arm-2, displayed as 2
    //   -> +++<sub class="bk-mark">[<a href="#bk-f-1-arm-2">→ 2</a>]</sub>+++

    fn of_next_link(ctx: &Context) -> Prose {
        let anchor = ctx
            .next
            .clone()
            .expect("Fallthrough::Next has a target arm");
        Prose::fallthrough(anchor, FallthroughLabel::Derived)
    }

    // - Rule-group links
    //
    //   group g, in namespace Rel   -> +++<sub class="bk-mark">[<a href="#Rel-g">→ g</a>]</sub>+++

    fn of_group_link(ctx: &Context, id_group: &str) -> Prose {
        let anchor = anchor_of_group(&ctx.namespace, id_group);
        Prose::fallthrough(anchor, FallthroughLabel::Explicit(id_group.to_owned()))
    }

    // - Otherwise links
    //
    //   namespace f   -> +++<sub class="bk-mark">[<a href="#f-else">→ ⋅</a>]</sub>+++

    fn of_else_link(ctx: &Context) -> Prose {
        let anchor = anchor_of_else(&ctx.namespace);
        Prose::fallthrough(anchor, FallthroughLabel::Explicit("⋅".to_owned()))
    }

    // - Failure markers
    //
    //   Fail   -> +++<sub class="bk-mark">[FAIL]</sub>+++

    fn of_fail_link() -> Prose {
        Prose::text("+++<sub class=\"bk-mark\">[FAIL]</sub>+++")
    }
}
