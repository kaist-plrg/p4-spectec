//! Text rendering for structured-language data
//!
//! Prints SL as a numbered outline, one step per instruction,
//! nested blocks indented under their step,
//! with relation inputs and outputs filled into the notation and `%` elsewhere.
//! `short` prints a step's heading without its blocks.

use std::{
    fmt::{self, Write},
    rc::Rc,
};

use crate::lang::{
    data::notation::tree as notation,
    traits::print::{Print, Printer},
};

use crate::lang::sl::ast::*;

// == Printing

// - Parameters

impl Print for Param {
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

impl Print for [Param] {
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

// - Instructions

impl Print for Instr {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_instr_with(printer, self, false, 0, 0)
    }
}

/// Prints one instruction as a numbered step, then its blocks unless `short`.
fn write_instr_with(
    output: &mut Printer<'_>,
    instr: &Instr,
    short: bool,
    level: usize,
    index: usize,
) -> fmt::Result {
    // The step number is omitted in short form
    let order = format!("{}{index}. ", "  ".repeat(level));
    let mut write_order = || {
        if short { Ok(()) } else { output.write_str(&order) }
    };
    match &instr.node {
        // `If (cond), then` and the block, marking a dangling else
        InstrKind::If(IfInstr { exp, iter_exps, block, dangle }) => {
            write_order()?;
            output.write_str("If (")?;
            exp.print(output)?;
            output.write_char(')')?;
            for exp_iter in iter_exps {
                exp_iter.print(output)?;
            }
            output.write_str(", then")?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, level + 1, 0)?;
                if *dangle {
                    write!(output, "\n\n{order}Else Dangling")?;
                }
            }
            Ok(())
        }
        // `If (rel: args) holds, then` in one of two shapes
        InstrKind::Hold(HoldInstr { id, not_exp, iter_exps, hold_case }) => match hold_case {
            // Both branches: then-block, `Else,`, else-block
            HoldCase::Both(block_hold, block_not_hold) => {
                write_order()?;
                output.write_str("If (")?;
                id.print(output)?;
                output.write_str(": ")?;
                not_exp.print(output)?;
                output.write_char(')')?;
                for exp_iter in iter_exps {
                    exp_iter.print(output)?;
                }
                output.write_str(" holds, then")?;
                if !short {
                    output.write_str("\n\n")?;
                    write_block_with(output, block_hold, level + 1, 0)?;
                    write!(output, "\n\n{order}Else,\n\n")?;
                    write_block_with(output, block_not_hold, level + 1, 0)?;
                }
                Ok(())
            }
            // One branch: its block, marking a dangling else
            HoldCase::Hold(block, dangle) | HoldCase::NotHold(block, dangle) => {
                write_order()?;
                output.write_str("If (")?;
                id.print(output)?;
                output.write_str(": ")?;
                not_exp.print(output)?;
                output.write_char(')')?;
                for exp_iter in iter_exps {
                    exp_iter.print(output)?;
                }
                output.write_char(' ')?;
                output.write_str(if matches!(hold_case, HoldCase::NotHold(..)) {
                    "does not hold"
                } else {
                    "holds"
                })?;
                output.write_str(", then")?;
                if !short {
                    output.write_str("\n\n")?;
                    write_block_with(output, block, level + 1, 0)?;
                    if *dangle {
                        write!(output, "\n\n{order}Else Dangling")?;
                    }
                }
                Ok(())
            }
        },
        // `Case analysis on` and the numbered arms
        InstrKind::Case(CaseInstr { exp, cases, dangle }) => {
            write_order()?;
            output.write_str("Case analysis on ")?;
            exp.print(output)?;
            if !short {
                output.write_str("\n\n")?;
                write_cases_with(output, cases, level + 1)?;
                if *dangle {
                    write!(output, "\n\n{order}Else Dangling")?;
                }
            }
            Ok(())
        }
        // `Group id:` with the inputs in the notation, then the block
        InstrKind::Group(GroupInstr { id, rel_signature, exps, block }) => {
            write_order()?;
            output.write_str("Group ")?;
            id.print(output)?;
            output.write_str(": ")?;
            write_relinput(output, rel_signature, exps)?;
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, level + 1, 0)?;
            }
            Ok(())
        }
        // `(Let x be e)` with its iterations, then the block
        InstrKind::Let(LetInstr { exp_l, exp_r, iter_instrs, block }) => {
            write_order()?;
            output.write_str("(Let ")?;
            exp_l.print(output)?;
            output.write_str(" be ")?;
            exp_r.print(output)?;
            output.write_char(')')?;
            for iter_instr in iter_instrs {
                iter_instr.print(output)?;
            }
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, level + 1, 0)?;
            }
            Ok(())
        }
        // `(rel: args)` with its iterations, then the block
        InstrKind::Rule(RuleInstr { id, not_exp, iter_instrs, block, .. }) => {
            write_order()?;
            output.write_char('(')?;
            id.print(output)?;
            output.write_str(": ")?;
            not_exp.print(output)?;
            output.write_char(')')?;
            for iter_instr in iter_instrs {
                iter_instr.print(output)?;
            }
            if !short {
                output.write_str("\n\n")?;
                write_block_with(output, block, level + 1, 0)?;
            }
            Ok(())
        }
        // A relation without outputs only holds
        InstrKind::Result(ResultInstr { exps, .. }) if exps.is_empty() => {
            write_order()?;
            output.write_str("The relation holds")
        }
        // `Result in:` with the outputs in the notation
        InstrKind::Result(ResultInstr { rel_signature, exps }) => {
            write_order()?;
            output.write_str("Result in: ")?;
            write_reloutput(output, rel_signature, exps)
        }
        // `Return` and the value
        InstrKind::Return(ReturnInstr { exp }) => {
            write_order()?;
            output.write_str("Return ")?;
            exp.print(output)
        }
        // `Debug:` and the expression, then the wrapped instruction
        InstrKind::Debug(DebugInstr { exp, instr: nested }) => {
            write_order()?;
            output.write_str("Debug: ")?;
            exp.print(output)?;
            if !short {
                output.write_str("\n\n")?;
                write_instr_with(output, nested, false, level, index + 1)?;
            }
            Ok(())
        }
    }
}

// - Case analysis

impl Print for Guard {
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
        }
    }
}

/// Prints the arms of a case analysis, numbered from one.
fn write_cases_with(output: &mut Printer<'_>, cases: &[Case], level: usize) -> fmt::Result {
    for (index, case) in cases.iter().enumerate() {
        if index != 0 {
            output.write_str("\n\n")?;
        }
        write_case_with(output, case, level, index + 1)?;
    }
    Ok(())
}

/// Prints one arm as `Case guard` and its block.
fn write_case_with(
    output: &mut Printer<'_>,
    case: &Case,
    level: usize,
    index: usize,
) -> fmt::Result {
    write!(output, "{}{index}. Case ", "  ".repeat(level))?;
    case.guard.print(output)?;
    output.write_str("\n\n")?;
    write_block_with(output, &case.block, level + 1, 0)
}

// - Blocks

impl Print for Block {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_block_with(printer, self, 0, 0)
    }
}

/// Prints a block's instructions as consecutive steps.
fn write_block_with(
    output: &mut Printer<'_>,
    block: &Block,
    level: usize,
    index: usize,
) -> fmt::Result {
    for (offset, instr) in block.iter().enumerate() {
        if offset != 0 {
            output.write_str("\n\n")?;
        }
        write_instr_with(output, instr, false, level, index + offset + 1)?;
    }
    Ok(())
}

/// Prints the otherwise block, if present.
fn write_elseblock_opt_with(
    output: &mut Printer<'_>,
    block: &Option<ElseBlock>,
    level: usize,
    index: usize,
) -> fmt::Result {
    if let Some(block) = block {
        output.write_str("\n\n")?;
        write_elseblock_with(output, block, level, index)?;
    }
    Ok(())
}

/// Prints the otherwise block as the next step, `Otherwise,`.
fn write_elseblock_with(
    output: &mut Printer<'_>,
    block: &ElseBlock,
    level: usize,
    index: usize,
) -> fmt::Result {
    write!(output, "{}{next}. Otherwise,\n\n", "  ".repeat(level), next = index + 1)?;
    write_block_with(output, block, level + 1, 0)
}

// - Table rows

impl Print for TableRow {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("\n  Row : ")?;
        printer.separated(&self.exps_input, ", ")?;
        printer.write_str(" -> ")?;
        self.exp.print(printer)?;
        printer.write_str(":\n\n")?;
        write_block_with(printer, &self.block, 2, 0)
    }
}

impl Print for [TableRow] {
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

impl Print for RelDef {
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

impl Print for ExternRel {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)?;
        printer.write_str(": ")?;
        write_relinput(printer, &self.rel_signature, &self.exps_input)
    }
}

impl Print for DefinedRel {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)?;
        printer.write_str(": ")?;
        write_relinput(printer, &self.rel_signature, &self.exps_input)?;
        printer.write_str("\n\n")?;
        write_block_with(printer, &self.block, 0, 0)?;
        write_elseblock_opt_with(printer, &self.block_else, 0, self.block.len())
    }
}

/// Fills the input expressions into the notation at the hint's positions.
fn write_relinput(
    output: &mut Printer<'_>,
    rel_signature: &RelSignature,
    exps_input: &[Exp],
) -> fmt::Result {
    let not_typ = &rel_signature.not_typ;
    let idxs_input = rel_signature.input_hint.indices();
    assert_eq!(idxs_input.len(), exps_input.len());
    // Each notation position takes its input, or `%`
    let mixfix = notation::Mixfix::fill(Rc::clone(not_typ.node.mixop()), |index| {
        idxs_input
            .iter()
            .position(|idx_input| idx_input.node == index)
            .map(|position| &exps_input[position])
    });
    mixfix.print_with(output, |exp, output| match exp {
        Some(exp) => exp.print(output),
        None => output.write("%"),
    })
}

/// Fills the output expressions into the notation at the non-input positions.
fn write_reloutput(
    output: &mut Printer<'_>,
    rel_signature: &RelSignature,
    exps_output: &[Exp],
) -> fmt::Result {
    let not_typ = &rel_signature.not_typ;
    let idxs_input = rel_signature.input_hint.indices();
    // Outputs are the positions the hint leaves
    let outputs = (0..not_typ.node.arity())
        .filter(|index| !idxs_input.iter().any(|idx_input| idx_input.node == *index))
        .collect::<Vec<_>>();
    assert_eq!(outputs.len(), exps_output.len());
    let mixfix = notation::Mixfix::fill(Rc::clone(not_typ.node.mixop()), |index| {
        outputs
            .iter()
            .position(|output| *output == index)
            .map(|position| &exps_output[position])
    });
    mixfix.print_with(output, |exp, output| match exp {
        Some(exp) => exp.print(output),
        None => output.write("%"),
    })
}

// == Meta-function definitions

impl Print for MetaFuncDef {
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

impl Print for ExternFunc {
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

impl Print for BuiltinFunc {
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

impl Print for TableFunc {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_char('$')?;
        self.id.print(printer)?;
        self.params.as_slice().print(printer)?;
        printer.write_str("\n=\n")?;
        for (index, table_row) in self.table_rows.iter().enumerate() {
            if index != 0 {
                printer.write_char('\n')?;
            }
            table_row.print(printer)?;
        }
        Ok(())
    }
}

impl Print for DefinedFunc {
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
        write_block_with(printer, &self.block, 0, 0)?;
        write_elseblock_opt_with(printer, &self.block_else, 0, self.block.len())
    }
}

// == Definitions

impl Print for Def {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
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

impl Print for [Def] {
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

impl Print for Spec {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.as_slice().print(printer)
    }
}
