//! STF qualified names and their source spelling

use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// An STF qualified name, keeping its source spelling.
pub struct Name(String);

impl Name {
    /// Borrows the name text.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes the value into its text.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        Self(name)
    }
}

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        Self(name.to_owned())
    }
}

impl fmt::Display for Name {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}
