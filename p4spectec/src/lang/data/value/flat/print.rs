//! Rendering flat values through their arena
//!
//! `ValueRef` implements `Print`, resolving bodies and case mixops.
//! Nested aggregates preserve field order and indentation.

use std::fmt::{self, Write};

use crate::util::text::escape_text;

use crate::lang::traits::print::{Print, Printer};

use super::{ValueField, ValueKind, ValueRef};

impl Print for ValueRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.print_inner(printer, 0)
    }
}

impl ValueRef<'_> {
    /// Prints a value with nested aggregates indented by `level`.
    fn print_inner(&self, printer: &mut Printer<'_>, level: usize) -> fmt::Result {
        match self.arena.value.kind(self.value.node) {
            ValueKind::Bool(value) => write!(printer, "{value}"),
            ValueKind::Num(value) => value.print(printer),
            ValueKind::Text(text) => printer.write_str(&escape_text(text)),
            // Empty structs stay on one line
            ValueKind::Struct(value_fields) if value_fields.is_empty() => printer.write_str("{}"),
            // One field per line, indented one level deeper
            ValueKind::Struct(value_fields) => {
                printer.write_str("{\n")?;
                for (idx, ValueField { atom, value }) in value_fields.iter().enumerate() {
                    if idx != 0 {
                        printer.write_str(";\n")?;
                    }
                    printer.write_str(&"  ".repeat(level + 1))?;
                    atom.print(printer)?;
                    printer.write_char(' ')?;
                    value.view(self.arena).print_inner(printer, level + 1)?;
                }
                printer.write_char('\n')?;
                printer.write_str(&"  ".repeat(level))?;
                printer.write_char('}')
            }
            ValueKind::Case(value_case) => {
                value_case.print_with(&self.arena.mixop, printer, |value, printer| {
                    value.view(self.arena).print_inner(printer, level + 1)
                })
            }
            ValueKind::Tuple(values) => {
                printer.write_char('(')?;
                for (idx, value) in values.iter().enumerate() {
                    if idx != 0 {
                        printer.write_str(", ")?;
                    }
                    value.view(self.arena).print_inner(printer, level + 1)?;
                }
                printer.write_char(')')
            }
            ValueKind::Opt(Some(value)) => {
                printer.write_str("Some(")?;
                value.view(self.arena).print_inner(printer, level + 1)?;
                printer.write_char(')')
            }
            ValueKind::Opt(None) => printer.write_str("None"),
            // Empty lists stay on one line
            ValueKind::List(values) if values.is_empty() => printer.write_str("[]"),
            // One element per line, indented one level deeper
            ValueKind::List(values) => {
                printer.write_str("[\n")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx != 0 {
                        printer.write_str(",\n")?;
                    }
                    printer.write_str(&"  ".repeat(level + 1))?;
                    value.view(self.arena).print_inner(printer, level + 1)?;
                }
                printer.write_char('\n')?;
                printer.write_str(&"  ".repeat(level))?;
                printer.write_char(']')
            }
            ValueKind::Func(id) => {
                printer.write_char('$')?;
                printer.write_str(&id.node)
            }
            ValueKind::Extern(_) => printer.write_str("extern"),
        }
    }
}
