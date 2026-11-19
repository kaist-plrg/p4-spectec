//! Text rendering of SL instruction coverage
//!
//! The report starts with the overall ratio and caller-supplied comment lines,
//! then lists each definition with instructions as a numbered outline.
//! Each instruction line starts with `+ ` when hit, `- ` when missed,
//! or two spaces when its definition is ignored; hit lines end with `[hits: N]`.
//!
//! `write_report_to` sends the report to a file or stdout,
//! and `summary_line` formats the one-line result a command prints last.

use std::{
    fmt::{self, Write},
    fs,
    path::Path,
};

use crate::lang::{
    common::source::Span,
    sl::{
        ast::{
            Block, Case, Def, DefKind, Guard, HoldCase, Instr, InstrKind, MetaFuncDef, RelDef, Spec,
        },
        print::{write_instr_heading, write_relinput},
    },
    traits::print::{Print, Printer},
};

use super::{
    error::{self, CoverError},
    instr::Cover,
};

/// Writes the coverage report to `path_cov`, or to stdout without it.
pub fn write_report_to(
    path_cov: Option<&Path>,
    spec: &Spec,
    cover: &Cover,
    comments: &[String],
) -> Result<(), CoverError> {
    let mut text = String::new();
    write_report(&mut text, spec, cover, comments).expect("writing to a String cannot fail");
    match path_cov {
        // Replace the report file
        Some(path) => {
            fs::write(path, text).map_err(|error_io| error::output_write_failed(path, &error_io))
        }
        // Print the report as the command output
        None => {
            print!("{text}");
            Ok(())
        }
    }
}

/// Formats `command: summary; instruction coverage H/T (P%)`.
pub fn summary_line(command: &str, summary: &str, cover: &Cover) -> String {
    let measure = cover.measure();
    format!(
        "{command}: {summary}; instruction coverage {}/{} ({:.2}%)",
        measure.hit,
        measure.total,
        measure.percent()
    )
}

/// Writes the coverage report of `spec`, with `comments` after the ratio line.
pub fn write_report(
    output: &mut dyn Write,
    spec: &Spec,
    cover: &Cover,
    comments: &[String],
) -> fmt::Result {
    let measure = cover.measure();
    writeln!(
        output,
        ";; Instruction coverage: {}/{} ({:.2}%)",
        measure.hit,
        measure.total,
        measure.percent()
    )?;
    for comment in comments {
        writeln!(output, ";; {comment}")?;
    }
    let mut report = Report { output, cover, next: 0 };
    for def in spec {
        report.def(def)?;
    }
    // The walk must visit exactly the numbered instructions
    assert_eq!(report.next, cover.len(), "report visits every numbered instruction once");
    Ok(())
}

/// Report writing state: the output and the next instruction index.
struct Report<'a> {
    output: &'a mut dyn Write,
    cover: &'a Cover,
    next: usize,
}

impl Report<'_> {
    /// Writes one line, separated from the previous line by a blank line.
    fn line(&mut self, text: &str) -> fmt::Result {
        writeln!(self.output, "\n{text}")
    }

    /// Writes a definition's span and heading.
    fn heading(&mut self, span: &Span, heading: &str) -> fmt::Result {
        write!(self.output, "\n;; {span}\n{heading}\n")
    }

    /// Writes a definition with instructions.
    fn def(&mut self, def: &Def) -> fmt::Result {
        match &def.node {
            DefKind::Rel(RelDef::Defined(rel)) => {
                let heading = render(|printer| {
                    printer.write_str("relation ")?;
                    rel.id.print(printer)?;
                    printer.write_str(": ")?;
                    write_relinput(printer, &rel.rel_signature, &rel.exps_input)
                });
                self.heading(&def.span, &heading)?;
                self.block(&rel.block, 0)?;
                self.block_else(rel.block_else.as_ref(), rel.block.instrs.len())
            }
            DefKind::MetaFunc(MetaFuncDef::Defined(func)) => {
                let heading = render(|printer| {
                    printer.write_str("def $")?;
                    func.id.print(printer)?;
                    if !func.tparams.is_empty() {
                        printer.write_char('<')?;
                        printer.separated(&func.tparams, ", ")?;
                        printer.write_char('>')?;
                    }
                    func.params.as_slice().print(printer)
                });
                self.heading(&def.span, &heading)?;
                self.block(&func.block, 0)?;
                self.block_else(func.block_else.as_ref(), func.block.instrs.len())
            }
            DefKind::MetaFunc(MetaFuncDef::Table(func)) => {
                let heading = render(|printer| {
                    printer.write_str("tbl def $")?;
                    func.id.print(printer)?;
                    func.params.as_slice().print(printer)
                });
                self.heading(&def.span, &heading)?;
                for row in &func.table_rows {
                    let heading = render(|printer| {
                        printer.separated(&row.exps_input, ", ")?;
                        printer.write_str(" -> ")?;
                        row.exp.print(printer)
                    });
                    self.line(&format!("    Row : {heading}:"))?;
                    self.block(&row.block, 2)?;
                }
                Ok(())
            }
            DefKind::Rel(RelDef::Extern(_))
            | DefKind::MetaFunc(MetaFuncDef::Extern(_) | MetaFuncDef::Builtin(_))
            | DefKind::Typ(_)
            | DefKind::Var(_) => Ok(()),
        }
    }

    /// Writes a block's instructions as steps numbered from one.
    fn block(&mut self, block: &Block, level: usize) -> fmt::Result {
        for (offset, instr) in block.instrs.iter().enumerate() {
            self.instr(instr, level, offset + 1)?;
        }
        Ok(())
    }

    /// Writes the otherwise block as the step after `index`.
    fn block_else(&mut self, block: Option<&Block>, index: usize) -> fmt::Result {
        if let Some(block) = block {
            self.line(&format!("  {}. Otherwise,", index + 1))?;
            self.block(block, 1)?;
        }
        Ok(())
    }

    /// Writes an instruction's marked step, then its nested instructions.
    fn instr(&mut self, instr: &Instr, level: usize, index: usize) -> fmt::Result {
        let node = self.cover.node(self.next);
        self.next += 1;
        let indent = "  ".repeat(level);
        let marker = match (node.ignored, node.hits) {
            (true, _) => "  ",
            (false, 0) => "- ",
            (false, _) => "+ ",
        };
        let heading = render(|printer| write_instr_heading(printer, instr));
        let hits = if node.hits > 0 { format!("  [hits: {}]", node.hits) } else { String::new() };
        self.line(&format!("{marker}{indent}{index}. {heading}{hits}"))?;
        // Trailing lines align with the step text
        let trailer = format!("  {indent}{index}. ");
        match &instr.node {
            InstrKind::If(instr) => {
                self.block(&instr.block, level + 1)?;
                self.dangle(instr.dangle, &trailer)
            }
            InstrKind::Hold(instr) => match &instr.hold_case {
                HoldCase::Both(block_hold, block_not_hold) => {
                    self.block(block_hold, level + 1)?;
                    self.line(&format!("{trailer}Else,"))?;
                    self.block(block_not_hold, level + 1)
                }
                HoldCase::Hold(block, dangle) | HoldCase::NotHold(block, dangle) => {
                    self.block(block, level + 1)?;
                    self.dangle(*dangle, &trailer)
                }
            },
            InstrKind::Case(instr) => {
                for (offset, case) in instr.cases.iter().enumerate() {
                    self.case(case, level + 1, offset + 1)?;
                }
                self.dangle(instr.dangle, &trailer)
            }
            InstrKind::Group(instr) => self.block(&instr.block, level + 1),
            InstrKind::Let(instr) => self.block(&instr.block, level + 1),
            InstrKind::Rule(instr) => self.block(&instr.block, level + 1),
            InstrKind::Result(_) | InstrKind::Return(_) => Ok(()),
            InstrKind::Debug(instr) => self.instr(&instr.instr, level, index + 1),
        }
    }

    /// Writes one case arm, `Case guard`, and its block.
    fn case(&mut self, case: &Case, level: usize, index: usize) -> fmt::Result {
        let guard = render(|printer| Guard::print(&case.guard, printer));
        self.line(&format!("  {}{index}. Case {guard}", "  ".repeat(level)))?;
        self.block(&case.block, level + 1)
    }

    /// Marks a branch that may fall through.
    fn dangle(&mut self, dangle: bool, trailer: &str) -> fmt::Result {
        if dangle {
            self.line(&format!("{trailer}Else Dangling"))?;
        }
        Ok(())
    }
}

/// Renders printer output into a string.
fn render(f: impl FnOnce(&mut Printer<'_>) -> fmt::Result) -> String {
    let mut text = String::new();
    f(&mut Printer::new(&mut text)).expect("writing to a String cannot fail");
    text
}
