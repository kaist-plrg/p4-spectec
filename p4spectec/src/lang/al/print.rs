//! Text rendering for algorithmic-language data
//!
//! Prints AL in a readable outline:
//! each rule group shows its match (signature, inputs, shared premises)
//! and then its paths, with inputs and outputs filled into the notation
//! and `%` standing for the positions a side does not mention.

use std::fmt::{self, Write};

use crate::lang::{
    common::notation::mixop::Mixop,
    hints::input::InputHint,
    traits::print::{Print, Printer},
};

use super::ast::*;

// == Printing

// - Premises

impl<I: Print, V: Print> Print for Prem<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            PremKind::Rule(RulePrem { id, not_exp, .. }) => {
                id.print(printer)?;
                printer.write_str(": ")?;
                not_exp.print(printer)
            }
            PremKind::If(IfPrem { exp }) => {
                printer.write_str("if ")?;
                exp.print(printer)
            }
            PremKind::IfHold(IfHoldPrem { id, not_exp }) => {
                printer.write_str("if ")?;
                id.print(printer)?;
                printer.write_str(": ")?;
                not_exp.print(printer)?;
                printer.write_str(" holds")
            }
            PremKind::IfNotHold(IfNotHoldPrem { id, not_exp }) => {
                printer.write_str("if ")?;
                id.print(printer)?;
                printer.write_str(": ")?;
                not_exp.print(printer)?;
                printer.write_str(" does not hold")
            }
            PremKind::Let(LetPrem { exp_l, exp_r }) => {
                printer.write_str("let ")?;
                exp_l.print(printer)?;
                printer.write_str(" = ")?;
                exp_r.print(printer)
            }
            PremKind::Iter(IterPrem { prem: prem_inner, prem_iter })
                if matches!(prem_inner.node, PremKind::Iter(_)) =>
            {
                prem_inner.print(printer)?;
                prem_iter.print(printer)
            }
            PremKind::Iter(IterPrem { prem: prem_inner, prem_iter }) => {
                printer.write_char('(')?;
                prem_inner.print(printer)?;
                printer.write_char(')')?;
                prem_iter.print(printer)
            }
            PremKind::Debug(DebugPrem { exp }) => {
                printer.write_str("debug ")?;
                exp.print(printer)
            }
        }
    }
}

impl<I: Print, V: Print> Print for [Prem<I, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_prems_with(printer, 0, self)
    }
}

/// Prints each premise on its own `--` line at the given indent.
fn write_prems_with<I: Print, V: Print>(
    output: &mut Printer<'_>,
    level: usize,
    prems: &[Prem<I, V>],
) -> fmt::Result {
    for prem in prems {
        write!(output, "\n{}-- ", indent(level))?;
        prem.print(output)?;
    }
    Ok(())
}

// - Rules

/// Prints the rule groups separated by blank lines.
fn write_rulegroups<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    rule_groups: &[RuleGroup<I, V>],
) -> fmt::Result {
    for (index, rule_group) in rule_groups.iter().enumerate() {
        if index != 0 {
            output.write_str("\n\n")?;
        }
        write_rulegroup(output, not_typ, input_hint, rule_group)?;
    }
    Ok(())
}

/// Prints one group as `rulegroup id`, its `match`, then its `paths`.
fn write_rulegroup<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    rule_group: &RuleGroup<I, V>,
) -> fmt::Result {
    write!(output, "{}rulegroup ", indent(1))?;
    rule_group.node.id.print(output)?;
    write!(output, "\n\n {}match\n\n", indent(1))?;
    write_rulematch(output, not_typ, input_hint, &rule_group.node.rule_match)?;
    write!(output, "\n\n {}paths\n\n", indent(1))?;
    write_rulepaths(output, not_typ, input_hint, &rule_group.node.rule_paths)
}

/// Prints the otherwise group under an `elsegroup` heading, if present.
fn write_elsegroup_opt<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    else_group: &Option<ElseGroup<I, V>>,
) -> fmt::Result {
    if let Some(else_group) = else_group {
        write!(output, "\n\n{}elsegroup\n\n", indent(1))?;
        write_elsegroup(output, not_typ, input_hint, else_group)?;
    }
    Ok(())
}

/// Prints the otherwise group like a rule group with a single path.
fn write_elsegroup<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    else_group: &ElseGroup<I, V>,
) -> fmt::Result {
    write!(output, "{}rulegroup ", indent(1))?;
    else_group.node.id.print(output)?;
    write!(output, "\n\n {}match\n\n", indent(1))?;
    write_rulematch(output, not_typ, input_hint, &else_group.node.rule_match)?;
    write!(output, "\n\n {}paths\n\n", indent(1))?;
    write_rulepaths(output, not_typ, input_hint, std::slice::from_ref(&else_group.node.rule_path))
}

/// Prints the signature, the input patterns, and the shared premises.
fn write_rulematch<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    rule_match: &RuleMatch<I, V>,
) -> fmt::Result {
    write!(output, "{}(signature) ", indent(2))?;
    write_ruleinput(output, not_typ, input_hint, &rule_match.exps_signature)?;
    output.write_char('\n')?;
    output.write_str(&indent(2))?;
    write_ruleinput(output, not_typ, input_hint, &rule_match.exps_input)?;
    write_prems_with(output, 2, &rule_match.prems)
}

/// Prints the paths separated by blank lines.
fn write_rulepaths<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    rule_paths: &[RulePath<I, V>],
) -> fmt::Result {
    for (index, rule_path) in rule_paths.iter().enumerate() {
        if index != 0 {
            output.write_str("\n\n")?;
        }
        write_rulepath(output, not_typ, input_hint, rule_path)?;
    }
    Ok(())
}

/// Prints one path as `rulepath id`, its premises, and its outputs.
fn write_rulepath<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    rule_path: &RulePath<I, V>,
) -> fmt::Result {
    write!(output, "{}rulepath ", indent(2))?;
    rule_path.id.print(output)?;
    write_prems_with(output, 2, &rule_path.prems)?;
    output.write_char('\n')?;
    output.write_str(&indent(2))?;
    write_ruleoutput(output, not_typ, input_hint, &rule_path.exps_output)
}

/// Fills the input expressions into the notation at the hint's positions.
fn write_ruleinput<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    exps_input: &[Exp<I, V>],
) -> fmt::Result {
    let idxs_input = input_hint.indices();
    assert_eq!(idxs_input.len(), exps_input.len());
    let (_, typs) = not_typ.node.split();
    // Each notation position takes its input, or nothing
    let exps = (0..typs.len())
        .map(|index| {
            idxs_input
                .iter()
                .zip(exps_input)
                .find_map(|(idx_input, exp)| (idx_input.node == index).then_some(exp))
        })
        .collect();
    write_notation(output, not_typ, exps)
}

/// Fills the output expressions into the notation at the non-input positions.
fn write_ruleoutput<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    input_hint: &InputHint,
    exps_output: &[Exp<I, V>],
) -> fmt::Result {
    let idxs_input = input_hint.indices();
    let (_, typs) = not_typ.node.split();
    // Outputs are the positions the hint leaves
    let outputs = (0..typs.len())
        .filter(|index| !idxs_input.iter().any(|idx_input| idx_input.node == *index))
        .collect::<Vec<_>>();
    assert_eq!(outputs.len(), exps_output.len());
    // A relation without outputs only holds
    if exps_output.is_empty() {
        output.write_str("-- the relation holds")
    } else {
        let exps = (0..typs.len())
            .map(|index| {
                outputs
                    .iter()
                    .zip(exps_output)
                    .find_map(|(output, exp)| (*output == index).then_some(exp))
            })
            .collect();
        output.write_str("-- output: ")?;
        write_notation(output, not_typ, exps)
    }
}

// - Clauses

impl<I: Print, V: Print> Print for Clause<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.node.args.print(printer)?;
        printer.write_str(" = ")?;
        self.node.exp.print(printer)?;
        write_prems_with(printer, 1, &self.node.prems)
    }
}

// - Table rows

impl<I: Print, V: Print> Print for TableRow<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write!(printer, "\n{}(signature) ", indent(2))?;
        for (index, exp) in self.node.exps_signature.iter().enumerate() {
            if index != 0 {
                printer.write_str(", ")?;
            }
            exp.print(printer)?;
        }
        printer.write_char('\n')?;
        printer.write_str(&indent(2))?;
        self.node.args.as_slice().print(printer)?;
        printer.write_str(" -> ")?;
        self.node.exp.print(printer)?;
        write_prems_with(printer, 2, &self.node.prems)
    }
}

impl<I: Print, V: Print> Print for [TableRow<I, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, table_row) in self.iter().enumerate() {
            write!(printer, "\n{}row {index} :", indent(1))?;
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

impl<I: Print, V: Print> Print for RelDef<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Extern(extern_rel) => {
                printer.write_str("extern relation ")?;
                extern_rel.id.print(printer)?;
                printer.write_str(": ")?;
                extern_rel.not_typ.print(printer)
            }
            Self::Defined(defined_rel) => {
                printer.write_str("relation ")?;
                defined_rel.id.print(printer)?;
                printer.write_str(": ")?;
                defined_rel.not_typ.print(printer)?;
                printer.write_str("\n\n")?;
                write_rulegroups(
                    printer,
                    &defined_rel.not_typ,
                    &defined_rel.input_hint,
                    &defined_rel.rule_groups,
                )?;
                write_elsegroup_opt(
                    printer,
                    &defined_rel.not_typ,
                    &defined_rel.input_hint,
                    &defined_rel.else_group,
                )
            }
        }
    }
}

// == Meta-function definitions

impl<I: Print, V: Print> Print for MetaFuncDef<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Extern(extern_func) => {
                printer.write_str("extern def $")?;
                extern_func.id.print(printer)?;
                if !extern_func.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&extern_func.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                extern_func.params.as_slice().print(printer)?;
                printer.write_str(" : ")?;
                extern_func.typ.print(printer)
            }
            Self::Builtin(builtin_func) => {
                printer.write_str("builtin def $")?;
                builtin_func.id.print(printer)?;
                if !builtin_func.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&builtin_func.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                builtin_func.params.as_slice().print(printer)?;
                printer.write_str(" : ")?;
                builtin_func.typ.print(printer)
            }
            Self::Table(table_func) => {
                printer.write_str("tbl def $")?;
                table_func.id.print(printer)?;
                table_func.params.as_slice().print(printer)?;
                printer.write_str(" : ")?;
                table_func.typ.print(printer)?;
                printer.write_str(" =")?;
                table_func.table_rows.print(printer)
            }
            Self::Defined(defined_func) => {
                printer.write_str("def $")?;
                defined_func.id.print(printer)?;
                if !defined_func.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&defined_func.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                defined_func.params.as_slice().print(printer)?;
                printer.write_str(" : ")?;
                defined_func.typ.print(printer)?;
                printer.write_str(" =")?;
                for (index, clause) in defined_func.clauses.iter().enumerate() {
                    write!(printer, "\n\n  clause {index} : ")?;
                    clause.print(printer)?;
                }
                if let Some(else_clause) = &defined_func.else_clause {
                    printer.write_str("\n\n  clause -1 : ")?;
                    else_clause.print(printer)?;
                }
                Ok(())
            }
        }
    }
}

// == Definitions

impl<I: Print, V: Print> Print for Def<I, V> {
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

// == Specifications

impl<I: Print, V: Print> Print for Spec<I, V> {
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

// == Helpers

/// Two spaces per level.
fn indent(level: usize) -> String {
    "  ".repeat(level)
}

/// Prints the notation with the given arguments, `%` where one is absent.
fn write_notation<I: Print, V: Print>(
    output: &mut Printer<'_>,
    not_typ: &NotTyp,
    exps: Vec<Option<&Exp<I, V>>>,
) -> fmt::Result {
    let (mixop, typs) = not_typ.node.split();
    assert_eq!(typs.len(), exps.len());
    Mixop::fill(&mixop, exps)
        .expect("notation arguments came from the same split notation")
        .print_with(output, |exp, output| match exp {
            Some(exp) => exp.print(output),
            None => output.write("%"),
        })
}
