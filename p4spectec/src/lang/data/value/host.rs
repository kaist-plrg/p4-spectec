//! Host payloads carried as values of extern syntax types
//!
//! A host defines each `extern syntax` type it supplies as a `HostTypDef`,
//! e.g. a simulator's `arch_state: HostTypDef<Arch>` for `archState`,
//! and keeps its own Rust data in values of that type.
//! `HostTypDef::payload` wraps the data, `flat::make::external` makes the value,
//! and `flat::get::external` reads the data back through the same definition.
//!
//! A payload belongs to the definition that made it:
//! payloads of two definitions never compare equal,
//! even with the same name or the same Rust data type.
//! Payloads live only in the arena; encoding one fails.

use std::{
    any::Any,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    rc::Rc,
    sync::atomic::{self, AtomicU64},
};

use serde::{Deserialize, Deserializer, Serialize, Serializer, de, ser};

use super::ValueError;

// = Definitions

/// The id of the next definition made.
static ID_NEXT: AtomicU64 = AtomicU64::new(0);

/// A host's definition of one extern syntax type, with Rust data `T`.
///
/// Every `new` makes a definition of its own,
/// so a host makes each once and passes copies of it around.
pub struct HostTypDef<T> {
    typdef: HostTyp,
    /// Ties the definition to its data type; it holds no `T`.
    _repr: PhantomData<fn() -> T>,
}

// Copies regardless of `T`, which a derive would require to be `Copy`
impl<T> Clone for HostTypDef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for HostTypDef<T> {}

impl<T> HostTypDef<T> {
    /// Defines the extern syntax type `name`, apart from every other definition.
    pub fn new(name: &'static str) -> Self {
        // Ids only tell definitions apart, so no other memory is ordered
        let id = ID_NEXT.fetch_add(1, atomic::Ordering::Relaxed);
        Self { typdef: HostTyp { id, name }, _repr: PhantomData }
    }

    /// The definition with its Rust data type erased.
    pub fn erase(self) -> HostTyp {
        self.typdef
    }
}

// - Payload access

impl<T: Ord + Hash + fmt::Debug + 'static> HostTypDef<T> {
    /// Wraps `data` as a payload of this definition.
    pub fn payload(self, data: T) -> Rc<HostPayload> {
        Rc::new(HostPayload { typdef: self.typdef, data: Box::new(data) })
    }

    /// Borrows the data of `payload`, failing unless this definition made it.
    pub(super) fn data(self, payload: &HostPayload) -> Result<&T, ValueError> {
        if payload.typdef != self.typdef {
            return Err(ValueError::HostPayloadMismatch {
                expected: self.typdef.name,
                actual: payload.typdef.name,
            });
        }
        // One definition makes payloads of one Rust type
        let data = (&*payload.data as &dyn Any)
            .downcast_ref::<T>()
            .expect("a host type definition makes data of its own type");
        Ok(data)
    }
}

// - Erasure

/// A host type definition with its Rust data type erased.
///
/// Two are equal only as the same definition, told apart by id.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HostTyp {
    id: u64,
    name: &'static str,
}

impl HostTyp {
    /// The extern syntax type it defines.
    pub fn name(self) -> &'static str {
        self.name
    }
}

// Hashes the name alone: an id depends on how many definitions came before
impl Hash for HostTyp {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
    }
}

// = Payloads

/// Host data carried as an extern value, with the definition that made it.
///
/// Payloads that compare equal must be the same data:
/// interning and the call cache treat them as one value.
pub struct HostPayload {
    typdef: HostTyp,
    data: Box<dyn HostData>,
}

impl HostPayload {
    /// The definition that made the payload.
    pub fn typdef(&self) -> HostTyp {
        self.typdef
    }
}

/// Host data with its Rust type erased, still ordered and hashed.
trait HostData: Any + fmt::Debug {
    /// Orders against data of the same Rust type.
    fn cmp_dyn(&self, data_other: &dyn HostData) -> Ordering;
    /// Hashes the data.
    fn hash_dyn(&self, hasher: &mut dyn Hasher);
}

// - Erasure

impl<T: Ord + Hash + fmt::Debug + 'static> HostData for T {
    fn cmp_dyn(&self, data_other: &dyn HostData) -> Ordering {
        // A payload compares data only under one definition, so of one Rust type
        let data_other = (data_other as &dyn Any)
            .downcast_ref::<T>()
            .expect("data of one definition share a Rust type");
        Ord::cmp(self, data_other)
    }

    fn hash_dyn(&self, mut hasher: &mut dyn Hasher) {
        Hash::hash(self, &mut hasher);
    }
}

// - Printing

impl fmt::Debug for HostPayload {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(self.typdef.name).field(&self.data).finish()
    }
}

// - Equality

impl PartialEq for HostPayload {
    fn eq(&self, payload_other: &Self) -> bool {
        self.cmp(payload_other).is_eq()
    }
}

impl Eq for HostPayload {}

// - Ordering

impl PartialOrd for HostPayload {
    fn partial_cmp(&self, payload_other: &Self) -> Option<Ordering> {
        Some(self.cmp(payload_other))
    }
}

// By definition first, so that only data of one definition are compared
impl Ord for HostPayload {
    fn cmp(&self, payload_other: &Self) -> Ordering {
        self.typdef
            .cmp(&payload_other.typdef)
            .then_with(|| self.data.cmp_dyn(&*payload_other.data))
    }
}

// - Hashing

impl Hash for HostPayload {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.typdef.hash(hasher);
        self.data.hash_dyn(hasher);
    }
}

// - Encoding

// Payloads are host data without a value encoding
impl Serialize for HostPayload {
    fn serialize<S: Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        let name = self.typdef.name;
        Err(ser::Error::custom(format!("host payload of {name} cannot be encoded")))
    }
}

impl<'de> Deserialize<'de> for HostPayload {
    fn deserialize<D: Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        Err(de::Error::custom("host payloads cannot be decoded"))
    }
}
