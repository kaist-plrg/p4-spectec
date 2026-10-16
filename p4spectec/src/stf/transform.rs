//! Target-specific normalizations used between STF parsing and simulation
//!
//! Public transforms rewrite qualified-name components, validity markers, and
//! action qualification before simulation. For example, `hdr.$valid$` becomes
//! `hdr.isValid()`.

use super::{
    ast::{Action, TableMatch},
    name::Name,
};

// == Case-insensitive string helpers

/// Whether `value` contains `pattern`, ignoring ASCII case.
fn contains_ignore_ascii_case(value: &str, pattern: &str) -> bool {
    value
        .to_ascii_lowercase()
        .contains(&pattern.to_ascii_lowercase())
}

/// Replaces the first case-insensitive match of `pattern` with `replacement`.
fn replace_first_ignore_ascii_case(value: &str, pattern: &str, replacement: &str) -> String {
    let value_lower = value.to_ascii_lowercase();
    let pattern_lower = pattern.to_ascii_lowercase();
    let Some(start) = value_lower.find(&pattern_lower) else {
        return value.to_owned();
    };
    let end = start + pattern.len();
    format!("{}{}{}", &value[..start], replacement, &value[end..])
}

// == Names

impl Name {
    /// Replaces the first segment when it contains one of `substrings`.
    pub fn rewrite_substring(self, substrings: &[&str], replacement: &str) -> Self {
        let name = self.into_string();
        let mut segments = name.split('.');
        let head = segments.next().unwrap_or_default();
        let rewritten = if substrings
            .iter()
            .any(|substring| contains_ignore_ascii_case(head, substring))
        {
            replacement
        } else {
            head
        };
        std::iter::once(rewritten)
            .chain(segments)
            .collect::<Vec<_>>()
            .join(".")
            .into()
    }

    /// Replaces the first occurrence of every requested substring.
    pub fn replace_substring(self, substrings: &[&str], replacement: &str) -> Self {
        let name = self.into_string();
        substrings
            .iter()
            .fold(name, |name, substring| {
                replace_first_ignore_ascii_case(&name, substring, replacement)
            })
            .into()
    }
}

// == Table matches

impl TableMatch {
    /// Rewrites a `$valid$` match key to an `isValid()` call.
    pub fn rewrite_valid(mut self) -> Self {
        let name = self.name.into_string();
        self.name = name.replace("$valid$", "isValid()").into();
        self
    }
}

// == Actions

impl Action {
    /// Drops all but the last dotted segment of the action name.
    pub fn into_unqualified(mut self) -> Self {
        let name = self.name.into_string();
        let unqualified = name.rsplit('.').next().unwrap_or(&name).to_owned();
        self.name = unqualified.into();
        self
    }

    /// Replaces the requested substrings in the action name.
    pub fn replace_substring(mut self, substrings: &[&str], replacement: &str) -> Self {
        self.name = self.name.replace_substring(substrings, replacement);
        self
    }
}
