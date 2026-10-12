//! Mutable state shared by the P4 lexer and parser
//!
//! The context keeps a global scope followed by nested local scopes.
//! A top-level-only grammar production
//! temporarily moves the local scopes aside,
//! then restores them without copying their namespace maps.
//! For example, a declaration parsed inside a control
//! can inspect the global type namespace
//! and then resume resolving the control's parameters.
//! Source positions are interned
//! so LALRPOP can use copyable indices while building spans.

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::BTreeMap,
};

use crate::lang::{
    common::source::{Position, Span},
    data::arena::Arena,
};

use super::error::ContextError;

// == Names and scopes

/// A copyable handle to an interned position.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Location {
    /// Index of this position.
    position: usize,
    /// Index of the end of the preceding token, where an empty span sits.
    previous: usize,
}

/// Names declared in one scope or type, with their kinds.
pub type Namespace = BTreeMap<String, IdentKind>;

/// The declared type of a variable, for resolving `var.member`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeId {
    /// No named type, so members cannot be resolved.
    Empty,
    /// A type name resolved in the current scopes.
    Local(String),
    /// A type name resolved in the global scope only, `.T`.
    Global(String),
}

/// What a declared name is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IdentKind {
    /// A type, with its type parameters and its members.
    TypeName { has_params: bool, namespace: Namespace },
    /// A variable or function, with its type parameters and its type.
    Ident { has_params: bool, type_id: TypeId },
}

/// The shared lexer and parser state over the arena.
pub struct Context<'a> {
    /// The arena parse-tree values are built in.
    arena: RefCell<&'a mut Arena>,
    /// Global namespace followed by the currently active local namespaces.
    scopes: RefCell<Vec<Namespace>>,
    /// Local namespaces set aside while parsing a top-level-only production.
    scopes_suspended: RefCell<Vec<Namespace>>,
    /// Most recently classified identifier, used to resolve a following member.
    id_prev: RefCell<Option<String>>,
    /// Namespace used to classify members of the most recent receiver.
    namespace_parent: RefCell<Option<Namespace>>,
    /// Source positions indexed by the copyable locations used by LALRPOP.
    positions: RefCell<Vec<Position>>,
}

// == Context operations

impl<'a> Context<'a> {
    // - Construction

    /// A context with only the global scope.
    pub fn new(arena: &'a mut Arena) -> Self {
        Self {
            arena: RefCell::new(arena),
            scopes: RefCell::new(vec![Namespace::new()]),
            scopes_suspended: RefCell::new(Vec::new()),
            id_prev: RefCell::new(None),
            namespace_parent: RefCell::new(None),
            positions: RefCell::new(Vec::new()),
        }
    }

    /// Borrows the arena.
    pub fn arena(&self) -> Ref<'_, Arena> {
        Ref::map(self.arena.borrow(), |arena| &**arena)
    }

    /// Borrows the arena mutably.
    pub fn arena_mut(&self) -> RefMut<'_, Arena> {
        RefMut::map(self.arena.borrow_mut(), |arena| &mut **arena)
    }

    // - Declarations

    /// Binds a name in the innermost scope.
    fn declare(&self, id: impl Into<String>, kind: IdentKind) -> Result<(), ContextError> {
        let mut scopes = self.scopes.borrow_mut();
        let scope = scopes.last_mut().ok_or(ContextError::ScopeMissing)?;
        scope.insert(id.into(), kind);
        Ok(())
    }

    /// Declares a type name with no members yet.
    pub fn declare_typ(&self, id: impl Into<String>, has_params: bool) -> Result<(), ContextError> {
        self.declare(id, IdentKind::TypeName { has_params, namespace: Namespace::new() })
    }

    /// Declares a variable or function.
    pub fn declare_var(
        &self,
        id: impl Into<String>,
        has_params: bool,
        type_id: TypeId,
    ) -> Result<(), ContextError> {
        self.declare(id, IdentKind::Ident { has_params, type_id })
    }

    // - Identifier lookup

    /// Looks a name up from the innermost scope outward.
    pub(super) fn ident_find(&self, id: &str) -> Option<IdentKind> {
        self.scopes
            .borrow()
            .iter()
            .rev()
            .find_map(|scope| scope.get(id).cloned())
    }

    /// Classifies a name for the lexer, remembering it for a following member.
    pub fn ident_kind(&self, id: &str) -> IdentKind {
        // After a `.`, look in the receiver's type; otherwise in the scopes
        let kind = match self.namespace_parent.borrow().as_ref() {
            Some(namespace) => namespace.get(id).cloned(),
            None => self.ident_find(id),
        }
        // Unknown names are plain identifiers
        .unwrap_or(IdentKind::Ident { has_params: false, type_id: TypeId::Empty });
        *self.id_prev.borrow_mut() = Some(id.to_owned());
        kind
    }

    // - Scope stack

    /// Opens a local scope.
    pub fn scope_push(&self) {
        self.scopes.borrow_mut().push(Namespace::new());
    }

    /// Closes the innermost local scope and returns its names.
    pub fn scope_pop(&self) -> Result<Namespace, ContextError> {
        let mut scopes = self.scopes.borrow_mut();
        // The global scope stays
        if scopes.len() <= 1 {
            return Err(ContextError::RootScopePopForbidden);
        }
        scopes.pop().ok_or(ContextError::ScopeMissing)
    }

    /// Sets the local scopes aside, leaving only the global one.
    pub fn scope_to_toplevel(&self) -> Result<(), ContextError> {
        let mut scopes = self.scopes.borrow_mut();
        if scopes.is_empty() {
            return Err(ContextError::ScopeMissing);
        }
        *self.scopes_suspended.borrow_mut() = scopes.split_off(1);
        Ok(())
    }

    /// Restores the scopes set aside by `scope_to_toplevel`.
    pub fn scope_to_local(&self) {
        let mut scopes_suspended = self.scopes_suspended.borrow_mut();
        let mut scopes = self.scopes.borrow_mut();
        scopes.truncate(1);
        scopes.append(&mut scopes_suspended);
    }

    // - Namespaces

    /// Records the members of a declared type, innermost declaration first.
    pub fn namespace_set_typ(&self, id: &str, namespace: Namespace) {
        let mut scopes = self.scopes.borrow_mut();
        // The innermost declaration of the type receives the members
        for scope in scopes.iter_mut().rev() {
            if let Some(IdentKind::TypeName { has_params, namespace: old_namespace }) =
                scope.get_mut(id)
            {
                let _ = has_params;
                *old_namespace = namespace;
                return;
            }
        }
    }

    /// After `x.`, selects the members of the type of `x` for the next lookup.
    pub fn namespace_set_parent(&self) {
        let id_prev = self.id_prev.borrow().clone();
        // The receiver is the last classified name; it must be a typed variable
        let type_id = id_prev
            .as_deref()
            .and_then(|id| self.ident_find(id))
            .and_then(|kind| match kind {
                IdentKind::Ident { type_id, .. } => Some(type_id),
                IdentKind::TypeName { .. } => None,
            });

        let scopes = self.scopes.borrow();
        let find_namespace = |id: &str, scopes: &[Namespace]| {
            scopes.iter().rev().find_map(|scope| match scope.get(id) {
                Some(IdentKind::TypeName { namespace, .. }) => Some(namespace.clone()),
                _ => None,
            })
        };
        // Local types resolve through the scopes, `.T` types only globally
        let namespace = type_id
            .and_then(|type_id| match type_id {
                TypeId::Empty => None,
                TypeId::Local(id) => find_namespace(&id, &scopes),
                TypeId::Global(id) => scopes
                    .first()
                    .and_then(|scope| find_namespace(&id, std::slice::from_ref(scope))),
            })
            .unwrap_or_default();
        *self.namespace_parent.borrow_mut() = Some(namespace);
    }

    /// Ends member lookup.
    pub fn namespace_clear_parent(&self) {
        self.namespace_parent.borrow_mut().take();
    }

    // - Source locations

    /// Interns a position, linking it to the preceding token's end.
    pub(crate) fn location_add(&self, position: Position, previous: Option<Location>) -> Location {
        let mut positions = self.positions.borrow_mut();
        let loc = Location {
            position: positions.len(),
            previous: previous.map_or(positions.len(), |loc| loc.position),
        };
        positions.push(position);
        loc
    }

    /// The position behind a handle.
    pub(crate) fn location_get(&self, loc: Location) -> Position {
        self.positions.borrow()[loc.position].clone()
    }

    /// The span between two handles.
    pub(crate) fn location_span(&self, loc_l: Location, loc_r: Location) -> Span {
        if loc_l == loc_r {
            // Menhir locates epsilon at the preceding token's end
            let position = self.positions.borrow()[loc_l.previous].clone();
            Span::new(position.clone(), position)
        } else {
            Span::new(self.location_get(loc_l), self.location_get(loc_r))
        }
    }
}
