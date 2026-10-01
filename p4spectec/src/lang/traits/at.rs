//! Source locations shared across language representations
//!
//! [`At`] reads a phrase's stored span or covers a composite's components.
//! Empty collections use the default span; actual default-spanned components
//! participate in coverage just like other source annotations.

use std::rc::Rc;

use crate::lang::common::source::{NotePhrase, Span};

/// Retrieves the source span of a located value.
pub trait At {
    /// Returns the stored or computed source span.
    fn at(&self) -> Span;
}

// == Source annotations

impl At for Span {
    fn at(&self) -> Span {
        self.clone()
    }
}

impl<T, N> At for NotePhrase<T, N, Span> {
    fn at(&self) -> Span {
        self.span.clone()
    }
}

// == Containers

impl<T: At + ?Sized> At for &T {
    fn at(&self) -> Span {
        (**self).at()
    }
}

impl<T: At + ?Sized> At for Box<T> {
    fn at(&self) -> Span {
        self.as_ref().at()
    }
}

impl<T: At + ?Sized> At for Rc<T> {
    fn at(&self) -> Span {
        self.as_ref().at()
    }
}

impl<T: At> At for [T] {
    fn at(&self) -> Span {
        Span::over_iter(self.iter().map(At::at))
    }
}
