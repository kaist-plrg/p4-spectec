//! Fallthrough anchors and labels for prose-language rendering
//!
//! ```text
//! Next, where the next arm bk-3-arm-2 is displayed as 2
//! -> +++<sub class="bk-mark">[<a href="#bk-3-arm-2">→ 2</a>]</sub>+++
//!
//! Group(g), in relation Rel
//! -> +++<sub class="bk-mark">[<a href="#Rel-g">→ g</a>]</sub>+++
//!
//! Else, in function f
//! -> +++<sub class="bk-mark">[<a href="#func:f-else">→ ⋅</a>]</sub>+++
//!
//! Fail
//! -> +++<sub class="bk-mark">[FAIL]</sub>+++
//! ```

use crate::lang::pl::ast::Fallthrough;

use crate::specdoc::anchor::{AnchorContext, DefKind};

use super::doc::doc::{FallthroughLabel, Prose};

// == Context
//
//   Context::new(Rel, "Sign")   -> Context { def_kind: Rel, id_def: "Sign", next: None }

/// The fallthrough destinations visible while rendering one instruction.
#[derive(Clone, Debug)]
pub struct Context {
    /// The kind of the definition whose body is rendered.
    pub def_kind: DefKind,
    /// The definition that owns group and otherwise destinations.
    pub id_def: String,
    /// The anchor of the next alternative arm, if one follows.
    pub next: Option<String>,
}

impl Context {
    /// Starts a definition's context, where no enclosing arm follows.
    pub fn new(def_kind: DefKind, id_def: &str) -> Self {
        Context { def_kind, id_def: id_def.to_owned(), next: None }
    }

    /// Continues within an arm whose successor has the given anchor.
    pub fn with_next(&self, next: Option<String>) -> Self {
        Context { next, ..self.clone() }
    }
}

// == Anchors
//
//   anchor_of_arm("bk-3", 0)   -> bk-3-arm-1

/// Returns the anchor of an ordered arm.
pub fn anchor_of_arm(anchor_block: &str, idx: usize) -> String {
    let num = idx + 1;
    format!("{anchor_block}-arm-{num}")
}

// == Rendering

impl Prose {
    // - Fallthrough links
    //
    //   None         -> (empty)
    //   Some(Fail)   -> +++<sub class="bk-mark">[FAIL]</sub>+++

    /// Renders a fallthrough marker, or nothing for an infallible step.
    pub(super) fn of_fallthrough_opt(
        anchor_ctx: &AnchorContext<'_>,
        ctx: &Context,
        fallthrough_opt: Option<&Fallthrough>,
    ) -> Prose {
        match fallthrough_opt {
            None => Prose::Empty,
            Some(Fallthrough::Next) => Prose::of_next_link(ctx),
            Some(Fallthrough::Group(id_group)) => {
                Prose::of_group_link(anchor_ctx, ctx, &id_group.node)
            }
            Some(Fallthrough::Else) => Prose::of_else_link(anchor_ctx, ctx),
            Some(Fallthrough::Fail) => Prose::of_fail_link(),
        }
    }

    // - Next-arm links
    //
    //   next arm bk-3-arm-2, displayed as 2
    //   -> +++<sub class="bk-mark">[<a href="#bk-3-arm-2">→ 2</a>]</sub>+++

    fn of_next_link(ctx: &Context) -> Prose {
        let anchor = ctx
            .next
            .clone()
            .expect("Fallthrough::Next has a target arm");
        Prose::fallthrough(anchor, FallthroughLabel::Derived)
    }

    // - Rule-group links
    //
    //   group g, in relation Rel   -> +++<sub class="bk-mark">[<a href="#Rel-g">→ g</a>]</sub>+++

    fn of_group_link(anchor_ctx: &AnchorContext<'_>, ctx: &Context, id_group: &str) -> Prose {
        let anchor = anchor_ctx.group(&ctx.id_def, id_group);
        Prose::fallthrough(anchor, FallthroughLabel::Explicit(id_group.to_owned()))
    }

    // - Otherwise links
    //
    //   function f   -> +++<sub class="bk-mark">[<a href="#func:f-else">→ ⋅</a>]</sub>+++

    fn of_else_link(anchor_ctx: &AnchorContext<'_>, ctx: &Context) -> Prose {
        let anchor = anchor_ctx.otherwise(ctx.def_kind, &ctx.id_def);
        Prose::fallthrough(anchor, FallthroughLabel::Explicit("⋅".to_owned()))
    }

    // - Failure markers
    //
    //   Fail   -> +++<sub class="bk-mark">[FAIL]</sub>+++

    fn of_fail_link() -> Prose {
        Prose::text("+++<sub class=\"bk-mark\">[FAIL]</sub>+++")
    }
}
