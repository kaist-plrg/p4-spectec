//! Target-specific normalizations used between STF parsing and simulation
//!
//! p4c's STF tests name tables, actions, and registers
//! after p4c's own block names; `transform_stmt` rewrites them
//! to the specification's names for the architecture under test.
//! For example, v1model's `ingress.t` becomes `main.ig.t`
//! and `hdr.$valid$` becomes `hdr.isValid()`.

use crate::{
    arch::{ebpf::Ebpf, psa::Psa, v1model::V1Model},
    p4::Architecture,
};

use super::syntax::ast::{Action, Name, Statement, TableMatch};

// == Statements

/// Rewrites the names in an STF statement for the architecture named `arch`.
pub fn transform_stmt(arch: &str, stmt: Statement) -> Statement {
    if arch == Ebpf::NAME {
        transform_ebpf(stmt)
    } else if arch == Psa::NAME {
        transform_psa(stmt)
    } else if arch == V1Model::NAME {
        transform_v1model(stmt)
    } else {
        // Other architectures use the names as written
        stmt
    }
}

/// Rewrites p4c block names in an STF statement to the specification's.
///
/// `pipe` blocks become `main.filt`, `_NoAction` becomes `NoAction`.
fn transform_ebpf(mut stmt: Statement) -> Statement {
    fn transform_name(name: Name) -> Name {
        name.replace_substring(&["pipe_c1_"], "main.filt.c1.")
            .replace_substring(&["pipe_"], "main.filt.")
            .replace_substring(&["pipe"], "main.filt")
    }
    fn transform_action(action: Action) -> Action {
        action
            .replace_substring(&["pipe_c1_"], "main.filt.c1.")
            .replace_substring(&["pipe_"], "main.filt.")
            .replace_substring(&["_NoAction"], "NoAction")
            .into_unqualified()
    }
    match &mut stmt {
        // Table and action of an add or default
        Statement::Add { table, action, .. } | Statement::SetDefault { table, action } => {
            *table = transform_name(table.clone());
            *action = transform_action(action.clone());
        }
        _ => {}
    }
    stmt
}

/// Rewrites p4c register-STF block names to the specification's `ip.ig`.
fn transform_psa(mut stmt: Statement) -> Statement {
    match &mut stmt {
        Statement::RegisterRead { name, .. }
        | Statement::RegisterWrite { name, .. }
        | Statement::RegisterReset { name } => {
            *name = name.clone().rewrite_substring(&["ingress"], "ip.ig");
        }
        _ => {}
    }
    stmt
}

/// Rewrites p4c block names in an STF statement to the specification's.
///
/// `ingress`/`preqos` become `main.ig`, `egress`/`postqos`/`c3` become
/// `main.eg`, and `$valid$` in an `add` match becomes `isValid()`.
/// A bare name, such as the global name of `@name(".t")`, is kept.
fn transform_v1model(mut stmt: Statement) -> Statement {
    fn transform_name(name: Name) -> Name {
        // Keep a bare name; only a qualified name has a block prefix
        if !name.to_string().contains('.') {
            return name;
        }
        name.rewrite_substring(&["ingress", "preqos"], "main.ig")
            .rewrite_substring(&["egress", "postqos", "c3"], "main.eg")
    }
    fn transform_action(mut action: Action) -> Action {
        action.name = transform_name(action.name);
        action
    }
    match &mut stmt {
        // Table, action, and every match of an add
        Statement::Add { table, matches, action, .. } => {
            *table = transform_name(table.clone());
            *action = transform_action(action.clone());
            *matches = matches
                .iter()
                .cloned()
                .map(TableMatch::rewrite_valid)
                .collect();
        }
        // Table and action of a default
        Statement::SetDefault { table, action } => {
            *table = transform_name(table.clone());
            *action = transform_action(action.clone());
        }
        _ => {}
    }
    stmt
}

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
