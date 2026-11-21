//! The `register` extern, an array the program reads and writes
//!
//! Elements start at the element type's default value;
//! an out-of-range read yields that default,
//! an out-of-range write is ignored.

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::{CallResult, ExternEnv, InitEnv, P4Ops, P4Type, P4Value, P4Vars};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Register array with its element type.
pub struct Register {
    /// Element type `T`.
    pub typ: P4Type,
    /// The `size` elements.
    pub values: Vec<P4Value>,
}

impl Register {
    /// A register object is created by calling its constructor.  This
    /// creates an array of 'size' identical elements, each with type
    /// T.  The array indices are in the range [0, size-1].  For
    /// example, this constructor call:
    ///
    /// ```text
    ///     register<bit<32>>(512) my_reg;
    ///
    /// ```
    /// allocates storage for 512 values, each with type `bit<32>`.
    ///
    /// `register(bit<32> size);`
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        // Exactly one type argument, the element type
        let typ = match env.get_types() {
            [typ] => *typ,
            typs => {
                return Err(error::register_type_argument_arity_mismatch(format!(
                    "register constructor expects 1 type argument, but {} were given",
                    typs.len()
                ))
                .into());
            }
        };
        let size = env.get("size")?;
        let initial = env.default_of(&typ)?;
        let size: usize = size.try_into()?;
        Ok(Self { typ, values: vec![initial; size] })
    }

    // - Methods

    /// Runs the method `name`, whose parameters are `params`.
    pub fn call(
        &mut self,
        env: &mut ExternEnv,
        name: &str,
        params: &[&str],
    ) -> Result<CallResult, ExternError> {
        match (name, params) {
            ("read", ["result", "index"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                self.read(env, idx)?;
                Ok(CallResult::Return(None))
            }
            ("write", ["index", "value"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                let value = env.get_local("value")?;
                self.write(idx, value);
                Ok(CallResult::Return(None))
            }
            _ => env.unsupported(),
        }
    }

    /// `read()` reads the state of the register array stored at the
    /// specified index, and returns it as the value written to the
    /// result parameter.
    ///
    /// @param index The index of the register array element to be
    ///              read, normally a value in the range [0, size-1].
    /// @param result Only types T that are `bit<W>` are currently
    ///              supported.  When index is in range, the value of
    ///              result becomes the value read from the register
    ///              array element.  When index >= size, the final
    ///              value of result is not specified, and should be
    ///              ignored by the caller.
    ///
    /// `void read(out T result, in bit<32> index);`
    pub fn read(&self, env: &mut ExternEnv, idx: usize) -> Result<(), ExternError> {
        // Out of range: the default, since the result is unspecified
        let value = match self.values.get(idx) {
            Some(value) => value.clone(),
            None => env.default_of(&self.typ)?,
        };
        env.set_local("result", value)
    }

    /// `write()` writes the state of the register array at the specified
    /// index, with the value provided by the value parameter.
    ///
    /// If you wish to perform a `read()` followed later by a `write()` to
    /// the same register array element, and you wish the
    /// read-modify-write sequence to be atomic relative to other
    /// processed packets, then there may be parallel implementations
    /// of the v1model architecture for which you must execute them in
    /// a P4_16 block annotated with an `@atomic` annotation.  See the
    /// P4_16 language specification description of the `@atomic`
    /// annotation for more details.
    ///
    /// @param index The index of the register array element to be
    ///              written, normally a value in the range [0,
    ///              size-1].  If index >= size, no register state will
    ///              be updated.
    /// @param value Only types T that are `bit<W>` are currently
    ///              supported.  When index is in range, this
    ///              parameter's value is written into the register
    ///              array element specified by index.
    /// `void write(in bit<32> index, in T value);`
    pub fn write(&mut self, idx: usize, value: P4Value) {
        // Out of range: ignored
        if let Some(value_elem) = self.values.get_mut(idx) {
            *value_elem = value;
        }
    }
}
