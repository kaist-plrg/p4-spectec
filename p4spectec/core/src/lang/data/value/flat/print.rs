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
        match self.arena.value.kind(self.value.node) {
            ValueKind::Bool(value) => write!(printer, "{value}"),
            ValueKind::Num(value) => value.print(printer),
            ValueKind::Text(text) => printer.write_str(&escape_text(text)),
            // Empty structs stay on one line
            ValueKind::Struct(value_fields) if value_fields.is_empty() => printer.write("{}"),
            // One field per line, indented one level deeper
            ValueKind::Struct(value_fields) => {
                printer.write("{")?;
                printer.indented(|printer| {
                    for (idx, ValueField { atom, value }) in value_fields.iter().enumerate() {
                        if idx != 0 {
                            printer.write(";")?;
                        }
                        printer.newline()?;
                        atom.print(printer)?;
                        printer.write(" ")?;
                        value.view(self.arena).print(printer)?;
                    }
                    Ok(())
                })?;
                printer.newline()?;
                printer.write("}")
            }
            ValueKind::Case(value_case) => printer.indented(|printer| {
                value_case.print_with(&self.arena.mixop, printer, |value| value.view(self.arena))
            }),
            ValueKind::Tuple(values) => {
                printer.write("(")?;
                printer.indented(|printer| {
                    for (idx, value) in values.iter().enumerate() {
                        if idx != 0 {
                            printer.write(", ")?;
                        }
                        value.view(self.arena).print(printer)?;
                    }
                    Ok(())
                })?;
                printer.write(")")
            }
            ValueKind::Opt(Some(value)) => {
                printer.write("Some(")?;
                printer.indented(|printer| value.view(self.arena).print(printer))?;
                printer.write(")")
            }
            ValueKind::Opt(None) => printer.write("None"),
            // Empty lists stay on one line
            ValueKind::List(values) if values.is_empty() => printer.write("[]"),
            // One element per line, indented one level deeper
            ValueKind::List(values) => {
                printer.write("[")?;
                printer.indented(|printer| {
                    for (idx, value) in values.iter().enumerate() {
                        if idx != 0 {
                            printer.write(",")?;
                        }
                        printer.newline()?;
                        value.view(self.arena).print(printer)?;
                    }
                    Ok(())
                })?;
                printer.newline()?;
                printer.write("]")
            }
            ValueKind::Func(id) => {
                printer.write("$")?;
                printer.write(&id.node)
            }
            ValueKind::Extern(_) => printer.write("extern"),
        }
    }
}
