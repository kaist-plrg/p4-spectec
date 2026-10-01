//! Stable STF diagnostic text output
//!
//! Each syntax type writes directly to the shared language printer. Compound
//! values delegate to their components, and programs separate statements with
//! newlines. For example, an action named `drop` renders as `"drop"()`.

use std::fmt;

use crate::lang::traits::print::{Print, Printer};

use super::ast::{Action, Condition, CounterKind, CounterTarget, MatchKind, Program, Statement};

// == Lexical helpers

/// Writes a double-quoted string.
fn write_quoted(printer: &mut Printer<'_>, value: &str) -> fmt::Result {
    printer.write_fmt(format_args!("\"{value}\""))
}

// == Compound syntax

impl Print for Action {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_quoted(printer, self.name.as_str())?;
        printer.write("(")?;
        for (index, arg) in self.args.iter().enumerate() {
            if index != 0 {
                printer.write(",")?;
            }
            write_quoted(printer, &arg.id)?;
            printer.write_fmt(format_args!(":{}", arg.num))?;
        }
        printer.write(")")
    }
}

impl Print for MatchKind {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Number(num) => printer.write(num),
            Self::Slash(number_l, number_r) => {
                printer.write_fmt(format_args!("{number_l}/{number_r}"))
            }
        }
    }
}

impl Print for CounterTarget {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Id(id) => printer.write(id),
            Self::Index(num) => printer.write(num),
        }
    }
}

impl Print for Condition {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let text = match self {
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Le => "<=",
            Self::Lt => "<",
            Self::Ge => ">=",
            Self::Gt => ">",
        };
        printer.write(text)
    }
}

impl Print for CounterKind {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let text = match self {
            Self::Bytes => "bytes",
            Self::Packets => "packets",
        };
        printer.write(text)
    }
}

// == Statements and programs

impl Print for Statement {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Wait => printer.write("wait"),
            Self::RemoveAll => printer.write("remove_all"),
            Self::Expect { port, packet_expected, exact } => {
                printer.write_fmt(format_args!("expect {port}"))?;
                if packet_expected.is_some() || *exact {
                    printer.write(" ")?;
                }
                if let Some(packet_expected) = packet_expected {
                    printer.write(packet_expected)?;
                }
                if *exact {
                    printer.write("$")?;
                }
                Ok(())
            }
            Self::Packet { port, packet } => {
                printer.write_fmt(format_args!("packet {port} {packet}"))
            }
            Self::NoPacket => printer.write("no_packet"),
            Self::Add { table, priority, matches, action, id } => {
                printer.write("add ")?;
                write_quoted(printer, table.as_str())?;
                if let Some(priority) = priority {
                    printer.write_fmt(format_args!(" {priority}"))?;
                }
                for table_match in matches {
                    printer.write(" ")?;
                    write_quoted(printer, table_match.name.as_str())?;
                    printer.write(":")?;
                    table_match.kind.print(printer)?;
                }
                printer.write(" ")?;
                action.print(printer)?;
                if let Some(id) = id {
                    printer.write(" ")?;
                    write_quoted(printer, id)?;
                }
                Ok(())
            }
            Self::SetDefault { table, action } => {
                printer.write("setdefault ")?;
                write_quoted(printer, table.as_str())?;
                printer.write(" ")?;
                action.print(printer)
            }
            Self::CheckCounter { counter, target, check } => {
                printer.write("check_counter ")?;
                write_quoted(printer, counter)?;
                printer.write("(")?;
                target.print(printer)?;
                printer.write(")")?;
                if let Some(kind) = check.kind {
                    printer.write(" ")?;
                    kind.print(printer)?;
                }
                printer.write(" ")?;
                check.condition.print(printer)?;
                printer.write_fmt(format_args!(" {}", check.num))
            }
            Self::MirroringAdd { session, port } => {
                printer.write_fmt(format_args!("mirroring_add {session} {port}"))
            }
            Self::MirroringAddMc { session, group_id } => {
                printer.write_fmt(format_args!("mirroring_add_mc {session} {group_id}"))
            }
            Self::MirroringGet { session } => {
                printer.write_fmt(format_args!("mirroring_get {session}"))
            }
            Self::McGroupCreate { group_id } => {
                printer.write_fmt(format_args!("mc_mgrp_create {group_id}"))
            }
            Self::McNodeCreate { replication_id, ports } => {
                printer.write_fmt(format_args!("mc_node_create {replication_id} "))?;
                for (index, port) in ports.iter().enumerate() {
                    if index != 0 {
                        printer.write(" ")?;
                    }
                    printer.write(port)?;
                }
                Ok(())
            }
            Self::McNodeAssociate { group_id, handle } => {
                printer.write_fmt(format_args!("mc_mgrp_associate {group_id} {handle}"))
            }
            Self::RegisterRead { name, index } => {
                printer.write("register_read ")?;
                write_quoted(printer, name.as_str())?;
                printer.write_fmt(format_args!(" {index}"))
            }
            Self::RegisterWrite { name, index, value } => {
                printer.write("register_write ")?;
                write_quoted(printer, name.as_str())?;
                printer.write_fmt(format_args!(" {index} {value}"))
            }
            Self::RegisterReset { name } => {
                printer.write("register_reset ")?;
                write_quoted(printer, name.as_str())
            }
        }
    }
}

impl Print for Program {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, statement) in self.iter().enumerate() {
            if index != 0 {
                printer.newline()?;
            }
            statement.node.print(printer)?;
        }
        Ok(())
    }
}
