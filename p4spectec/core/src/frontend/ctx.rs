//! Parser state shared by grammar actions and contextual tokenization
//!
//! `Bindings` owns variable names that survive across related source files.
//! `Context::with_bindings` creates fresh per-source scopes, parser modes,
//! and interned positions around those bindings.
//! Grammar actions pair `enter_scope` with `exit_scope`
//! and `enter_exp` or `enter_arith` with `exit_mode`;
//! the token adapter reads `in_arith` while classifying `*`.
//! `location` turns a [`Position`] into a compact [`Location`]
//! that LALRPOP can copy and later resolve through `position` or `span`.
//!
//! # Example
//!
//! ```text
//! Bindings
//! ├── Context(file_a): scopes_a, modes_a, positions_a
//! └── Context(file_b): scopes_b, modes_b, positions_b
//! ```

use std::{cell::RefCell, collections::BTreeSet, rc::Rc};

use crate::lang::common::{
    ids::id::strip_suffix,
    source::{Position, Span},
};

/// A compact source location for LALRPOP.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Location(usize);

/// Variable bindings preserved across related SpecTec source files.
#[derive(Default)]
pub(crate) struct Bindings {
    /// Names currently bound as variables, shared by every context.
    variables: RefCell<BTreeSet<String>>,
}

/// Parser mode for contextual tokenization.
#[derive(Clone, Copy)]
enum Mode {
    /// Expression mode: `*` is postfix iteration.
    Exp,
    /// Arithmetic mode: `*` is multiplication.
    Arith,
}

/// Per-source state shared by parser actions and contextual lexing.
pub(crate) struct Context {
    /// The cross-file variable names.
    bindings: Rc<Bindings>,
    /// Names each open scope added, to remove on exit.
    scopes: RefCell<Vec<Vec<String>>>,
    /// Positions by `Location` index.
    positions: RefCell<Vec<Position>>,
    /// Open parser modes, innermost last.
    modes: RefCell<Vec<Mode>>,
}

impl Context {
    // - Construction

    /// A context over shared bindings with no open scope or mode.
    pub(crate) fn with_bindings(bindings: Rc<Bindings>) -> Self {
        Self {
            bindings,
            scopes: RefCell::default(),
            positions: RefCell::default(),
            modes: RefCell::default(),
        }
    }

    // - Source locations

    /// Interns a position and returns its handle.
    pub(crate) fn location(&self, position: Position) -> Location {
        let mut positions = self.positions.borrow_mut();
        let loc = Location(positions.len());
        positions.push(position);
        loc
    }

    /// The position behind a handle.
    pub(crate) fn position(&self, loc: Location) -> Position {
        self.positions.borrow()[loc.0]
    }

    /// The span between two handles.
    pub(crate) fn span(&self, loc_l: Location, loc_r: Location) -> Span {
        Span::new(self.position(loc_l), self.position(loc_r))
    }

    // - Variable scopes

    /// Opens a scope; names added until `exit_scope` are removed with it.
    pub(crate) fn enter_scope(&self) {
        self.scopes.borrow_mut().push(Vec::new());
    }

    /// Closes the innermost scope, unbinding the names it added.
    pub(crate) fn exit_scope(&self) {
        let ids = self
            .scopes
            .borrow_mut()
            .pop()
            .expect("parser scope actions are balanced");
        let mut variables = self.bindings.variables.borrow_mut();
        for id in ids {
            variables.remove(&id);
        }
    }

    /// Binds a variable name in the innermost scope.
    pub(crate) fn add_id(&self, id: &str) {
        let id = id.to_owned();
        // Only a newly bound name is owed to this scope
        if self.bindings.variables.borrow_mut().insert(id.clone())
            && let Some(scope) = self.scopes.borrow_mut().last_mut()
        {
            scope.push(id);
        }
    }

    /// Whether the name, minus its suffix, is a bound variable.
    pub(crate) fn find_id(&self, id: &str) -> bool {
        self.bindings.variables.borrow().contains(strip_suffix(id))
    }

    // - Parser modes

    /// Enters expression mode.
    pub(crate) fn enter_exp(&self) {
        self.modes.borrow_mut().push(Mode::Exp);
    }

    /// Enters arithmetic mode.
    pub(crate) fn enter_arith(&self) {
        self.modes.borrow_mut().push(Mode::Arith);
    }

    /// Leaves the innermost mode.
    pub(crate) fn exit_mode(&self) {
        self.modes
            .borrow_mut()
            .pop()
            .expect("parser mode actions are balanced");
    }

    /// Whether the innermost mode is arithmetic.
    pub(crate) fn in_arith(&self) -> bool {
        matches!(self.modes.borrow().last(), Some(Mode::Arith))
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::with_bindings(Rc::new(Bindings::default()))
    }
}
