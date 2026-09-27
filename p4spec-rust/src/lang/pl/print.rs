//! Text rendering for prose-language data
//!
//! Prints PL as a numbered outline, one step per instruction,
//! nested blocks indented under their step.
//! The shared control flow prints once, parameterized by a tier printer
//! for the dispatch and group-body instructions.
//! `short` prints a step's heading without its blocks.

use std::fmt::{self, Write};

use crate::{
    lang::traits::print::{Print, Printer},
    util::text::escape_text,
};

use super::ast::*;

// == Printing

// - Expressions

impl<I: Print, V: Print> Print for Exp<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node.node {
            ExpKind::Bool(value) => write!(printer, "{value}"),
            ExpKind::Num(value) => value.print(printer),
            ExpKind::Text(text) => write!(printer, "\"{}\"", escape_text(text)),
            ExpKind::Id(id) => id.print(printer),
            ExpKind::Un(op, _, exp) => {
                op.print(printer)?;
                exp.print(printer)
            }
            ExpKind::Bin(op, _, exp_l, exp_r) => {
                printer.write_char('(')?;
                exp_l.print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Cmp(op, _, exp_l, exp_r) => {
                printer.write_char('(')?;
                exp_l.print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::UpCast(typ, exp) | ExpKind::DownCast(typ, exp) => {
                printer.write_char('(')?;
                exp.print(printer)?;
                printer.write_str(" as ")?;
                typ.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Sub(exp, typ, _) => {
                printer.write_char('(')?;
                exp.print(printer)?;
                printer.write_str(" has type ")?;
                typ.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Match(exp, pattern) => {
                printer.write_char('(')?;
                exp.print(printer)?;
                printer.write_str(" matches pattern ")?;
                pattern.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Tuple(exps) => {
                printer.write_char('(')?;
                printer.separated(exps, ", ")?;
                printer.write_char(')')
            }
            ExpKind::Case(not_exp) => {
                printer.write_char('(')?;
                not_exp.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Str(fields) => {
                printer.write_char('{')?;
                for (index, (atom, exp)) in fields.iter().enumerate() {
                    if index > 0 {
                        printer.write_str(", ")?;
                    }
                    atom.print(printer)?;
                    printer.write_char(' ')?;
                    exp.print(printer)?;
                }
                printer.write_char('}')
            }
            ExpKind::Opt(Some(exp)) => {
                printer.write_str("?(")?;
                exp.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Opt(None) => printer.write_str("?()"),
            ExpKind::List(exps) => {
                printer.write_char('[')?;
                printer.separated(exps, ", ")?;
                printer.write_char(']')
            }
            ExpKind::Cons(exp_head, exp_tail) => {
                exp_head.print(printer)?;
                printer.write_str(" :: ")?;
                exp_tail.print(printer)
            }
            ExpKind::Cat(exp_l, exp_r) => {
                exp_l.print(printer)?;
                printer.write_str(" ++ ")?;
                exp_r.print(printer)
            }
            ExpKind::Mem(exp_elem, exp_set) => {
                exp_elem.print(printer)?;
                printer.write_str(" is in ")?;
                exp_set.print(printer)
            }
            ExpKind::Len(exp) => {
                printer.write_char('|')?;
                exp.print(printer)?;
                printer.write_char('|')
            }
            ExpKind::Dot(exp, atom) => {
                exp.print(printer)?;
                printer.write_char('.')?;
                atom.print(printer)
            }
            ExpKind::Idx(exp_base, exp_idx) => {
                exp_base.print(printer)?;
                printer.write_char('[')?;
                exp_idx.print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                exp_base.print(printer)?;
                printer.write_char('[')?;
                exp_idx.print(printer)?;
                printer.write_str(" : ")?;
                exp_len.print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Upd(exp_base, path, exp_field) => {
                exp_base.print(printer)?;
                printer.write_char('[')?;
                path.print(printer)?;
                printer.write_str(" = ")?;
                exp_field.print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Call(id, targs, args) => {
                printer.write_char('$')?;
                id.print(printer)?;
                if !targs.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(targs, ", ")?;
                    printer.write_char('>')?;
                }
                args.as_slice().print(printer)
            }
            ExpKind::Iter(exp, iter_exp) => {
                printer.write_char('(')?;
                exp.print(printer)?;
                printer.write_char(')')?;
                std::slice::from_ref(iter_exp).print(printer)
            }
        }
    }
}

impl<I: Print, V: Print> Print for NotExp<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.print_with(printer, |exp, printer| exp.print(printer))
    }
}

// - Paths

impl<I: Print, V: Print> Print for Path<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            PathKind::Root => Ok(()),
            PathKind::Idx(path, exp_idx) => {
                path.print(printer)?;
                printer.write_char('[')?;
                exp_idx.print(printer)?;
                printer.write_char(']')
            }
            PathKind::Slice(path, exp_idx, exp_len) => {
                path.print(printer)?;
                printer.write_char('[')?;
                exp_idx.print(printer)?;
                printer.write_str(" : ")?;
                exp_len.print(printer)?;
                printer.write_char(']')
            }
            // A field of the root prints bare
            PathKind::Dot(path, atom) if matches!(path.node, PathKind::Root) => atom.print(printer),
            PathKind::Dot(path, atom) => {
                path.print(printer)?;
                printer.write_char('.')?;
                atom.print(printer)
            }
        }
    }
}

// - Parameters

impl<E: Print> Print for Param<E> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            ParamKind::Exp(_, exp) => exp.print(printer),
            ParamKind::Def(id, tparams, params, typ) => {
                printer.write_char('$')?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.as_slice().print(printer)?;
                printer.write_str(" : ")?;
                typ.print(printer)
            }
        }
    }
}

impl<E: Print> Print for [Param<E>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }
        printer.write_char('(')?;
        for (index, param) in self.iter().enumerate() {
            if index != 0 {
                printer.write_str(", ")?;
            }
            param.print(printer)?;
        }
        printer.write_char(')')
    }
}

// - Arguments

impl<I: Print, V: Print> Print for Arg<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            ArgKind::Exp(exp) => exp.print(printer),
            ArgKind::Def(id) => {
                printer.write_char('$')?;
                id.print(printer)
            }
        }
    }
}

impl<I: Print, V: Print> Print for [Arg<I, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }
        printer.write_char('(')?;
        for (index, arg) in self.iter().enumerate() {
            if index != 0 {
                printer.write_str(", ")?;
            }
            arg.print(printer)?;
        }
        printer.write_char(')')
    }
}

// - Instructions

// Shared control flow parameterized by the tier

/// Prints a tier instruction given the short flag, indent, and step index.
type TierPrinter<Tier> = fn(&mut Printer<'_>, &Tier, bool, usize, usize) -> fmt::Result;

/// Prints one instruction as a numbered step, then its blocks unless `short`.
fn write_instr_with<Tier, E: Print, V: Print>(
    output: &mut Printer<'_>,
    instr: &Instr<Tier, E, V>,
    tier_printer: TierPrinter<Tier>,
    short: bool,
    level: usize,
    index: usize,
) -> fmt::Result {
    let order = format!("{}{index}. ", "  ".repeat(level));
    // The step number is omitted in short form
    let write_order = |output: &mut Printer<'_>| {
        if short { Ok(()) } else { output.write_str(&order) }
    };

    match &instr.node.node {
        // `If (cond), then` and the block, marking a dangling else
        InstrKind::If(IfInstr { exp, iter_exps, block, dangle }) => {
            write_order(output)?;
            output.write_str("If (")?;
            exp.print(output)?;
            output.write_char(')')?;
            iter_exps.as_slice().print(output)?;
            output.write_str(", then")?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, tier_printer, level + 1, 0)?;
                if *dangle {
                    write!(output, "\n\n{order}Else Dangling")?;
                }
            }
            Ok(())
        }
        // `If (rel: args) holds, then` in one of two shapes
        InstrKind::Hold(HoldInstr { id, not_exp, iter_exps, hold_case }) => {
            // The heading reads the same for holds and does-not-holds
            let write_holding = |output: &mut Printer<'_>, negative: bool| {
                output.write_str("If (")?;
                id.print(output)?;
                output.write_str(": ")?;
                not_exp.print_with(output, |exp, output| exp.print(output))?;
                output.write_char(')')?;
                iter_exps.as_slice().print(output)?;
                output.write_char(' ')?;
                output.write_str(if negative { "does not hold" } else { "holds" })?;
                output.write_str(", then")
            };
            match hold_case {
                // Both branches: then-block, `Else,`, else-block
                HoldCase::Both(block_hold, block_not_hold) => {
                    write_order(output)?;
                    write_holding(output, false)?;
                    if !short {
                        output.write_str("\n\n")?;
                        write_block_with(output, block_hold, tier_printer, level + 1, 0)?;
                        write!(output, "\n\n{order}Else,\n\n")?;
                        write_block_with(output, block_not_hold, tier_printer, level + 1, 0)?;
                    }
                    Ok(())
                }
                // One branch: its block, marking a dangling else
                HoldCase::Hold(block, dangle) | HoldCase::NotHold(block, dangle) => {
                    write_order(output)?;
                    write_holding(output, matches!(hold_case, HoldCase::NotHold(..)))?;
                    if !short {
                        output.write_str("\n\n")?;
                        write_block_with(output, block, tier_printer, level + 1, 0)?;
                        if *dangle {
                            write!(output, "\n\n{order}Else Dangling")?;
                        }
                    }
                    Ok(())
                }
            }
        }
        // `Case analysis on` and the numbered arms
        InstrKind::Case(CaseInstr { exp, cases, dangle }) => {
            write_order(output)?;
            output.write_str("Case analysis on ")?;
            exp.print(output)?;
            if !short {
                output.write_str("\n\n")?;
                write_cases_with(output, cases, tier_printer, level + 1)?;
                if *dangle {
                    write!(output, "\n\n{order}Else Dangling")?;
                }
            }
            Ok(())
        }
        // `(Let x be e)` with its iterations
        InstrKind::Let(LetInstr { exp_l, exp_r, iter_instrs }) => {
            write_order(output)?;
            output.write_str("(Let ")?;
            exp_l.print(output)?;
            output.write_str(" be ")?;
            exp_r.print(output)?;
            output.write_char(')')?;
            iter_instrs.as_slice().print(output)
        }
        // `Debug:` and the expression
        InstrKind::Debug(DebugInstr { exp }) => {
            write_order(output)?;
            output.write_str("Debug: ")?;
            exp.print(output)
        }
        // `(Destruct (fields) = e)`
        InstrKind::Destruct(DestructInstr { bindings: fields, exp: exp_r }) => {
            write_order(output)?;
            output.write_str("(Destruct (")?;
            for (index, (_, exp)) in fields.iter().enumerate() {
                if index != 0 {
                    output.write_str(", ")?;
                }
                exp.print(output)?;
            }
            output.write_str(") = ")?;
            exp_r.print(output)?;
            output.write_char(')')
        }
        // `(Let x be e, e has type t)` and the block
        InstrKind::CheckLetSub(CheckLetSubInstr { typ, exp_l, exp_r, block, .. }) => {
            write_order(output)?;
            output.write_str("(Let ")?;
            exp_l.print(output)?;
            output.write_str(" be ")?;
            exp_r.print(output)?;
            output.write_str(", ")?;
            exp_r.print(output)?;
            output.write_str(" has type ")?;
            typ.print(output)?;
            output.write_char(')')?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, tier_printer, level + 1, 0)?;
            }
            Ok(())
        }
        // `(Let x be e, e matches pattern p)` and the block
        InstrKind::CheckLetMatch(CheckLetMatchInstr { pattern, exp_l, exp_r, block }) => {
            write_order(output)?;
            output.write_str("(Let ")?;
            exp_l.print(output)?;
            output.write_str(" be ")?;
            exp_r.print(output)?;
            output.write_str(", ")?;
            exp_r.print(output)?;
            output.write_str(" matches pattern ")?;
            pattern.print(output)?;
            output.write_char(')')?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, tier_printer, level + 1, 0)?;
            }
            Ok(())
        }
        // `(Let x be ! e)`, unwrapping the option, and the block
        InstrKind::OptionGet(OptionGetInstr { exp_l, exp_r, block }) => {
            write_order(output)?;
            output.write_str("(Let ")?;
            exp_l.print(output)?;
            output.write_str(" be ! ")?;
            exp_r.print(output)?;
            output.write_char(')')?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, tier_printer, level + 1, 0)?;
            }
            Ok(())
        }
        // The tier decides how its own instruction prints
        InstrKind::Tier(TierInstr { tier }) => tier_printer(output, tier, short, level, index),
    }
}

// - Group-body tier

impl<E: Print, V: Print> Print for Instr<GroupInstr<E, V>, E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_instr_with(printer, self, write_group_instr_with, false, 0, 0)
    }
}

/// Prints a group-body instruction; a backtrack lists its arms.
fn write_group_instr_with<E: Print, V: Print>(
    output: &mut Printer<'_>,
    tier: &GroupInstr<E, V>,
    short: bool,
    level: usize,
    index: usize,
) -> fmt::Result {
    let order = format!("{}{index}. ", "  ".repeat(level));
    if !short {
        output.write_str(&order)?;
    }

    match tier {
        // A relation without outputs only holds
        GroupInstr::Result(ResultInstr { exps_output, .. }) if exps_output.is_empty() => {
            output.write_str("The relation holds")
        }
        // `Result in:` with the outputs in the notation
        GroupInstr::Result(ResultInstr { rel_signature, exps_output }) => {
            output.write_str("Result in: ")?;
            write_reloutput(output, rel_signature, exps_output)
        }
        // `Return` and the value
        GroupInstr::Return(ReturnInstr { exp }) => {
            output.write_str("Return ")?;
            exp.print(output)
        }
        // `(rel: args)` with its iterations
        GroupInstr::Rule(RuleInstr { id, not_exp, iter_instrs, .. }) => {
            output.write_char('(')?;
            id.print(output)?;
            output.write_str(": ")?;
            not_exp.print_with(output, |exp, output| exp.print(output))?;
            output.write_char(')')?;
            iter_instrs.as_slice().print(output)
        }
        // `Block (n arms)` and each arm's block
        GroupInstr::Backtrack(BacktrackInstr { blocks }) => {
            write!(output, "Block ({} arms)", blocks.len())?;
            if !short {
                let indent = "  ".repeat(level);
                output.write_str("\n\n")?;
                for (arm_idx, arm) in blocks.iter().enumerate() {
                    if arm_idx != 0 {
                        output.write_str("\n\n")?;
                    }
                    write!(output, "{indent}Arm {}:\n\n", arm_idx + 1)?;
                    write_group_block_with(output, arm, level + 1, 0)?;
                }
            }
            Ok(())
        }
    }
}

impl<E: Print, V: Print> Print for GroupBlock<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_group_block_with(printer, self, 0, 0)
    }
}

/// Prints a group-body block.
fn write_group_block_with<E: Print, V: Print>(
    output: &mut Printer<'_>,
    block: &GroupBlock<E, V>,
    level: usize,
    index: usize,
) -> fmt::Result {
    write_block_with(output, block, write_group_instr_with, level, index)
}

// - Dispatch tier

impl<E: Print, V: Print> Print for Instr<DispatchInstr<E, V>, E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_instr_with(printer, self, write_dispatch_instr_with, false, 0, 0)
    }
}

/// Prints a dispatch instruction; a route lists its arms.
fn write_dispatch_instr_with<E: Print, V: Print>(
    output: &mut Printer<'_>,
    tier: &DispatchInstr<E, V>,
    short: bool,
    level: usize,
    index: usize,
) -> fmt::Result {
    let order = format!("{}{index}. ", "  ".repeat(level));
    if !short {
        output.write_str(&order)?;
    }

    match tier {
        // `Group id:` with the inputs in the notation, then the body
        DispatchInstr::Group(RuleGroupInstr {
            id_group, rel_signature, exps_input, block, ..
        }) => {
            output.write_str("Group ")?;
            id_group.print(output)?;
            output.write_str(": ")?;
            write_relinput(output, rel_signature, exps_input)?;
            if !short {
                output.write_str("\n\n")?;
                write_group_block_with(output, block, level + 1, 0)?;
            }
            Ok(())
        }
        // `Block (n arms)` and each arm's block
        DispatchInstr::Route(RouteInstr { blocks }) => {
            write!(output, "Block ({} arms)", blocks.len())?;
            if !short {
                let indent = "  ".repeat(level);
                output.write_str("\n\n")?;
                for (arm_idx, arm) in blocks.iter().enumerate() {
                    if arm_idx != 0 {
                        output.write_str("\n\n")?;
                    }
                    write!(output, "{indent}Arm {}:\n\n", arm_idx + 1)?;
                    write_dispatch_block_with(output, arm, level + 1, 0)?;
                }
            }
            Ok(())
        }
    }
}

impl<E: Print, V: Print> Print for DispatchBlock<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_dispatch_block_with(printer, self, 0, 0)
    }
}

/// Prints a dispatch block.
fn write_dispatch_block_with<E: Print, V: Print>(
    output: &mut Printer<'_>,
    block: &DispatchBlock<E, V>,
    level: usize,
    index: usize,
) -> fmt::Result {
    write_block_with(output, block, write_dispatch_instr_with, level, index)
}

// - Case analysis

impl<E: Print> Print for Guard<E> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Guard::Bool(value) => write!(printer, "{value}"),
            Guard::Cmp(op, _, exp) => {
                printer.write_str("(% ")?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp.print(printer)?;
                printer.write_char(')')
            }
            Guard::Sub(typ, _) => {
                printer.write_str("(% has type ")?;
                typ.print(printer)?;
                printer.write_char(')')
            }
            Guard::Match(pattern) => {
                printer.write_str("(% matches pattern ")?;
                pattern.print(printer)?;
                printer.write_char(')')
            }
            Guard::Mem(exp) => {
                printer.write_str("(% is in ")?;
                exp.print(printer)?;
                printer.write_char(')')
            }
            Guard::CheckLetSub(typ, _, exp) => {
                printer.write_str("(let ")?;
                exp.print(printer)?;
                printer.write_str(" be %, % has type ")?;
                typ.print(printer)?;
                printer.write_char(')')
            }
            Guard::CheckLetMatch(pattern, exp) => {
                printer.write_str("(let ")?;
                exp.print(printer)?;
                printer.write_str(" be %, % matches pattern ")?;
                pattern.print(printer)?;
                printer.write_char(')')
            }
        }
    }
}

/// Prints the arms of a case analysis, numbered from one.
fn write_cases_with<Tier, E: Print, V: Print>(
    output: &mut Printer<'_>,
    cases: &[Case<Tier, E, V>],
    tier_printer: TierPrinter<Tier>,
    level: usize,
) -> fmt::Result {
    for (index, case) in cases.iter().enumerate() {
        if index != 0 {
            output.write_str("\n\n")?;
        }
        write_case_with(output, case, tier_printer, level, index + 1)?;
    }
    Ok(())
}

/// Prints one arm as `Case guard` and its block.
fn write_case_with<Tier, E: Print, V: Print>(
    output: &mut Printer<'_>,
    case: &Case<Tier, E, V>,
    tier_printer: TierPrinter<Tier>,
    level: usize,
    index: usize,
) -> fmt::Result {
    write!(output, "{}{index}. Case ", "  ".repeat(level))?;
    case.guard.print(output)?;
    output.write_str("\n\n")?;
    write_block_with(output, &case.block, tier_printer, level + 1, 0)
}

// - Blocks

/// Prints a block's instructions as consecutive steps.
fn write_block_with<Tier, E: Print, V: Print>(
    output: &mut Printer<'_>,
    block: &Block<Tier, E, V>,
    tier_printer: TierPrinter<Tier>,
    level: usize,
    index: usize,
) -> fmt::Result {
    for (offset, instr) in block.iter().enumerate() {
        if offset != 0 {
            output.write_str("\n\n")?;
        }
        write_instr_with(output, instr, tier_printer, false, level, index + offset + 1)?;
    }
    Ok(())
}

/// Prints the otherwise block as the next step, if present.
fn write_elseblock_opt_with<Tier, E: Print, V: Print>(
    output: &mut Printer<'_>,
    block: &Option<Block<Tier, E, V>>,
    tier_printer: TierPrinter<Tier>,
    level: usize,
    index: usize,
) -> fmt::Result {
    if let Some(block) = block {
        write!(output, "\n\n{}{next}. Otherwise,\n\n", "  ".repeat(level), next = index + 1)?;
        write_block_with(output, block, tier_printer, level + 1, 0)?;
    }
    Ok(())
}

// - Table rows

impl<E: Print, V: Print> Print for TableRow<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("\n  Row : ")?;
        printer.separated(&self.exps_input, ", ")?;
        printer.write_str(" -> ")?;
        self.exp.print(printer)?;
        printer.write_str(":\n\n")?;
        write_group_block_with(printer, &self.block, 2, 0)
    }
}

impl<E: Print, V: Print> Print for [TableRow<E, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, table_row) in self.iter().enumerate() {
            if index != 0 {
                printer.write_char('\n')?;
            }
            table_row.print(printer)?;
        }
        Ok(())
    }
}

// == Type definitions

impl Print for TypDef {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Extern(extern_typ) => {
                printer.write_str("extern syntax ")?;
                extern_typ.id.print(printer)
            }
            Self::Defined(defined_typ) => {
                printer.write_str("syntax ")?;
                defined_typ.id.print(printer)?;
                if !defined_typ.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&defined_typ.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                printer.write_str(" = ")?;
                defined_typ.def_typ.print(printer)
            }
        }
    }
}

// == Relation definitions

impl<E: Print, V: Print> Print for RelDef<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Extern(relation) => {
                printer.write_str("extern relation ")?;
                relation.print(printer)
            }
            Self::Defined(relation) => {
                printer.write_str("relation ")?;
                relation.print(printer)
            }
        }
    }
}

impl<E: Print> Print for ExternRel<E> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)?;
        printer.write_str(": ")?;
        write_relinput(printer, &self.rel_signature, &self.exps_input)
    }
}

impl<E: Print, V: Print> Print for DefinedRel<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)?;
        printer.write_str(": ")?;
        write_relinput(printer, &self.rel_signature, &self.exps_input)?;
        printer.write_str("\n\n")?;
        self.block.print(printer)?;
        write_elseblock_opt_with(
            printer,
            &self.block_else_opt,
            write_dispatch_instr_with,
            0,
            self.block.len(),
        )
    }
}

/// Fills the input expressions into the notation at the hint's positions.
fn write_relinput<E: Print>(
    output: &mut Printer<'_>,
    rel_signature: &RelSignature,
    exps_input: &[E],
) -> fmt::Result {
    let not_typ = &rel_signature.not_typ;
    let idxs_input = rel_signature.input_hint.indices();
    assert_eq!(idxs_input.len(), exps_input.len());
    // Each notation position takes its input, or `%`
    let args = (0..not_typ.node.arity()).map(|index| {
        idxs_input
            .iter()
            .position(|idx_input| idx_input.node == index)
            .map(|position| &exps_input[position])
    });
    let mixfix =
        Mixop::fill(&not_typ.node.to_mixop(), args).expect("relation input arity matches notation");
    mixfix.print_with(output, |exp, output| match exp {
        Some(exp) => exp.print(output),
        None => output.write("%"),
    })
}

/// Fills the output expressions into the notation at the non-input positions.
fn write_reloutput<E: Print>(
    output: &mut Printer<'_>,
    rel_signature: &RelSignature,
    exps_output: &[E],
) -> fmt::Result {
    let not_typ = &rel_signature.not_typ;
    let idxs_input = rel_signature.input_hint.indices();
    // Outputs are the positions the hint leaves
    let outputs = (0..not_typ.node.arity())
        .filter(|index| !idxs_input.iter().any(|idx_input| idx_input.node == *index))
        .collect::<Vec<_>>();
    assert_eq!(outputs.len(), exps_output.len());
    let args = (0..not_typ.node.arity()).map(|index| {
        outputs
            .iter()
            .position(|output| *output == index)
            .map(|position| &exps_output[position])
    });
    let mixfix = Mixop::fill(&not_typ.node.to_mixop(), args)
        .expect("relation output arity matches notation");
    mixfix.print_with(output, |exp, output| match exp {
        Some(exp) => exp.print(output),
        None => output.write("%"),
    })
}

// == Meta-function definitions

impl<E: Print, V: Print> Print for MetaFuncDef<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Extern(func) => {
                printer.write_str("extern def ")?;
                func.print(printer)
            }
            Self::Builtin(func) => {
                printer.write_str("builtin def ")?;
                func.print(printer)
            }
            Self::Table(func) => {
                printer.write_str("tbl def ")?;
                func.print(printer)
            }
            Self::Defined(func) => {
                printer.write_str("def ")?;
                func.print(printer)
            }
        }
    }
}

impl<E: Print> Print for ExternFunc<E> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_char('$')?;
        self.id.print(printer)?;
        if !self.tparams.is_empty() {
            printer.write_char('<')?;
            printer.separated(&self.tparams, ", ")?;
            printer.write_char('>')?;
        }
        self.params.as_slice().print(printer)
    }
}

impl<E: Print> Print for BuiltinFunc<E> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_char('$')?;
        self.id.print(printer)?;
        if !self.tparams.is_empty() {
            printer.write_char('<')?;
            printer.separated(&self.tparams, ", ")?;
            printer.write_char('>')?;
        }
        self.params.as_slice().print(printer)
    }
}

impl<E: Print, V: Print> Print for TableFunc<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_char('$')?;
        self.id.print(printer)?;
        self.params.as_slice().print(printer)?;
        printer.write_str("\n=\n")?;
        self.rows.print(printer)
    }
}

impl<E: Print, V: Print> Print for DefinedFunc<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_char('$')?;
        self.id.print(printer)?;
        if !self.tparams.is_empty() {
            printer.write_char('<')?;
            printer.separated(&self.tparams, ", ")?;
            printer.write_char('>')?;
        }
        self.params.as_slice().print(printer)?;
        printer.write_str("\n\n")?;
        self.block.print(printer)?;
        write_elseblock_opt_with(
            printer,
            &self.block_else_opt,
            write_group_instr_with,
            0,
            self.block.len(),
        )
    }
}

// == Definitions

impl<E: Print, V: Print> Print for Def<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node.node {
            DefKind::Typ(typ_def) => typ_def.print(printer),
            DefKind::Var(var_def) => {
                printer.write_str("var ")?;
                var_def.id.print(printer)?;
                printer.write_str(" : ")?;
                var_def.typ.print(printer)
            }
            DefKind::Rel(rel_def) => rel_def.print(printer),
            DefKind::MetaFunc(meta_func_def) => meta_func_def.print(printer),
        }
    }
}

impl<E: Print, V: Print> Print for [Def<E, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, def) in self.iter().enumerate() {
            if index != 0 {
                printer.write_str("\n\n")?;
            }
            def.print(printer)?;
        }
        Ok(())
    }
}

// == Specifications

impl<E: Print, V: Print> Print for Spec<E, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.as_slice().print(printer)
    }
}
