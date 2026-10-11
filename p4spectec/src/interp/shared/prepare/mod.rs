//! Prepare source syntax for slot-based execution
//!
//! `Prepare` walks IL syntax once
//! and replaces identifiers and variables by frame slots (`IdSlot`, `VarSlot`),
//! so evaluation indexes a frame instead of looking names up;
//! notations' mixops are interned as shapes in the specification's
//! `MixopArena` on the way, and containers and phrases recurse structurally.
//! `Callable::prepare` pairs a prepared definition with the collected layout.

mod il;

use std::rc::Rc;

use crate::lang::{
    common::{Id, source::NotePhrase},
    data::{
        notation::{self, MixopArena, flat, tree},
        var::{IdSlot, Var, VarSlot},
    },
};

use crate::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};

pub use il::prepare_def_typ;

/// Where preparation records slots and shapes.
pub struct PrepareContext<'a> {
    /// The callable's frame layout, filled as names resolve.
    pub layout: &'a mut FrameLayout,
    /// The specification's shapes, filled as mixops are interned.
    pub arena_mixop: &'a mut MixopArena,
    /// The runner's instruction-selection mode, used to lower SL tail calls.
    pub det: bool,
}

/// Slot resolution of one syntax node.
pub trait Prepare: Sized {
    /// The same node with identifiers resolved to slots and mixops to shapes.
    type Output;

    /// Resolves the node's identifiers and mixops through `ctx`.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output;
}

// - Callables

impl<T> Callable<T> {
    /// Prepares a definition, collecting its slot layout.
    ///
    /// Notations are interned into `arena_mixop`,
    /// which must stay with the prepared definition for evaluation.
    pub fn prepare<S: Prepare<Output = T>>(
        arena_mixop: &mut MixopArena,
        det: bool,
        source: S,
    ) -> Self {
        // The traversal fills the layout as it resolves names
        let mut layout = FrameLayout::default();
        let def = source.prepare(&mut PrepareContext { layout: &mut layout, arena_mixop, det });
        Self { def, layout: Rc::new(layout) }
    }
}

// - Names

impl Prepare for Id {
    type Output = IdSlot;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ctx.layout.resolve_id(self)
    }
}

impl Prepare for Var {
    type Output = VarSlot;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ctx.layout.resolve_var(self)
    }
}

// - Containers

impl<T: Prepare> Prepare for Vec<T> {
    type Output = Vec<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        self.into_iter().map(|syntax| syntax.prepare(ctx)).collect()
    }
}

impl<T: Prepare> Prepare for Box<T> {
    type Output = Box<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        Box::new((*self).prepare(ctx))
    }
}

impl<T: Prepare> Prepare for Option<T> {
    type Output = Option<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        self.map(|syntax| syntax.prepare(ctx))
    }
}

// - Phrases

impl<T: Prepare, N, S> Prepare for NotePhrase<T, N, S> {
    type Output = NotePhrase<T::Output, N, S>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Grow the stack for deep syntax trees
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || NotePhrase {
            node: self.node.prepare(ctx),
            note: self.note,
            span: self.span,
        })
    }
}

// - Notation

impl<T: Prepare> Prepare for tree::Mixfix<T> {
    type Output = flat::Mixfix<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let (mixop, args) = self.into_parts();
        let mixop = prepare_mixop(ctx, &mixop);
        let args = args.prepare(ctx);
        notation::flat::Mixfix::new(ctx.arena_mixop, mixop, args)
            .expect("a mixfix fills every position")
    }
}

/// Interns a shared mixop as a shape in the specification arena.
pub(crate) fn prepare_mixop(ctx: &mut PrepareContext<'_>, mixop: &Rc<tree::Mixop>) -> flat::Mixop {
    ctx.arena_mixop
        .intern(mixop)
        .expect("specification mixops fit in 32-bit shape handles")
}
