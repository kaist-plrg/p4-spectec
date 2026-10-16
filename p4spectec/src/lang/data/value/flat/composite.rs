//! Borrowed constructor bodies for allocation-free interner lookups.

use super::{Value, ValueKind, ValueTag, projection::Shape};
use hashbrown::Equivalent;
use std::hash::{Hash, Hasher};

pub(super) struct Composite<'a> {
    pub shape: Shape,
    pub values: &'a [Value],
}
impl Hash for Composite<'_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self.shape {
            Shape::Tuple => ValueTag::Tuple.hash(hasher),
            Shape::List => ValueTag::List.hash(hasher),
            Shape::Case(mixop) => {
                ValueTag::Case.hash(hasher);
                mixop.hash(hasher);
            }
        }
        super::hash::exact_values(self.values, hasher);
    }
}
impl Equivalent<ValueKind> for Composite<'_> {
    fn equivalent(&self, kind: &ValueKind) -> bool {
        match (self.shape, kind) {
            (Shape::Tuple, ValueKind::Tuple(values)) | (Shape::List, ValueKind::List(values)) => {
                self.values == values
            }
            (Shape::Case(mixop), ValueKind::Case(case)) => {
                mixop == *case.mixop() && self.values == case.args()
            }
            _ => false,
        }
    }
}
