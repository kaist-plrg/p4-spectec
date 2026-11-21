//! The PSA `Register` extern, an array the program reads and writes
//!
//! Elements start at `initial_value` or the element type's default;
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
    /// Instantiate an array of `size` registers with undefined initial values,
    /// or initialize every register to the supplied `initial_value`.
    ///
    /// ```text
    /// extern Register<T, S>
    /// Register(bit<32> size);
    /// Register(bit<32> size, T initial_value);
    /// ```
    pub fn init(env: &mut InitEnv) -> Result<Self, ExternError> {
        // Exactly two type arguments: the element and index types
        let typ = match env.get_types() {
            [typ, _] => *typ,
            typs => {
                return Err(error::register_type_argument_arity_mismatch(format!(
                    "Register constructor expects 2 type arguments, but {} were given",
                    typs.len()
                ))
                .into());
            }
        };
        let size = env.get("size")?;
        // The second constructor supplies an initial value
        let initial = match env.get_opt("initial_value")? {
            Some(value) => value,
            None => env.default_of(&typ)?,
        };
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
            ("read", ["index"]) => {
                let idx: usize = env.get_local("index")?.try_into()?;
                Ok(CallResult::Return(Some(self.read(env, idx)?)))
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

    /// Reads the element at `idx`; out of range, the element type's default.
    ///
    /// `T read(in S index);`
    pub fn read(&self, env: &mut ExternEnv, idx: usize) -> Result<P4Value, ExternError> {
        match self.values.get(idx) {
            Some(value) => Ok(value.clone()),
            None => env.default_of(&self.typ),
        }
    }

    /// Writes `value` at `idx`; out of range, the write is ignored.
    ///
    /// `void write(in S index, in T value);`
    pub fn write(&mut self, idx: usize, value: P4Value) {
        if let Some(value_elem) = self.values.get_mut(idx) {
            *value_elem = value;
        }
    }
}
