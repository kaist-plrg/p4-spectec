//! Text rendering for internal-language data
//!
//! Prints IL in a readable source-like syntax:
//! binary operators are parenthesized, premises print on `--` lines,
//! iterations show their bound (`<-`) and binding (`->`) variables.
//! Values print through `print_value`,
//! with a short form that elides large aggregates.

use std::fmt::{self, Write};

use crate::{
    lang::{
        common::prim::num,
        traits::print::{Print, Printer},
    },
    util::text::escape_text,
};

use super::ast::*;

// == Printing

// - Types

impl Print for Typ {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            TypKind::Bool => printer.write_str("bool"),
            TypKind::Num(num::Typ::Nat) => printer.write_str("nat"),
            TypKind::Num(num::Typ::Int) => printer.write_str("int"),
            TypKind::Text => printer.write_str("text"),
            TypKind::Var(id, targs) => {
                id.print(printer)?;
                if !targs.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(targs, ", ")?;
                    printer.write_char('>')?;
                }
                Ok(())
            }
            TypKind::Tuple(typs) => {
                printer.write_char('(')?;
                printer.separated(typs, ", ")?;
                printer.write_char(')')
            }
            TypKind::Iter(typ, iter) => {
                typ.print(printer)?;
                iter.print(printer)
            }
            TypKind::Func(func_typ) => {
                if !func_typ.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&func_typ.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                printer.write_char('(')?;
                printer.separated(&func_typ.typs_params, ", ")?;
                printer.write_str(") : ")?;
                func_typ.typ_ret.print(printer)
            }
        }
    }
}

// - Notation types

impl Print for NotTyp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.node
            .print_with(printer, |typ, printer| typ.print(printer))
    }
}

// - Defined types

impl Print for DefTyp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            DefTypKind::Plain(typ) => typ.print(printer),
            DefTypKind::Struct(typ_fields) => {
                printer.write_char('{')?;
                printer.separated(typ_fields, ", ")?;
                printer.write_char('}')
            }
            DefTypKind::Variant(typ_cases) => {
                printer.write_str("\n   | ")?;
                printer.separated(typ_cases, "\n   | ")
            }
        }
    }
}

impl Print for TypField {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.atom.print(printer)?;
        printer.write_char(' ')?;
        self.typ.print(printer)
    }
}

impl Print for [TypField] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

impl Print for TypOrigin {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("(from ")?;
        self.node.id.print(printer)?;
        if !self.node.targs.is_empty() {
            printer.write_char('<')?;
            printer.separated(&self.node.targs, ", ")?;
            printer.write_char('>')?;
        }
        printer.write_char(')')
    }
}

impl Print for TypCase {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let TypCase { not_typ, typ_origin, hints } = self;
        not_typ.print(printer)?;
        printer.write_char(' ')?;
        typ_origin.print(printer)?;
        printer.write_char(' ')?;
        hints.print(printer)
    }
}

impl Print for [TypCase] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

// - Values

/// Prints a value in full, resolving handles through the arena.
pub fn print_value(
    arena: &crate::lang::data::value::ValueArena,
    value: &Value,
    printer: &mut Printer<'_>,
) -> fmt::Result {
    write_value_with(arena, printer, value, false, 0)
}

/// Prints a value; `short` elides struct and list contents to a count.
fn write_value_with(
    arena: &crate::lang::data::value::ValueArena,
    output: &mut Printer<'_>,
    value: &Value,
    short: bool,
    level: usize,
) -> fmt::Result {
    match arena.kind(value) {
        ValueKind::Bool(value) => write!(output, "{value}"),
        ValueKind::Num(value) => value.print(output),
        ValueKind::Text(text) => output.write_str(&escape_text(text)),
        // Empty structs stay on one line
        ValueKind::Struct(fields) if fields.is_empty() => output.write_str("{}"),
        // Short form: field count only
        ValueKind::Struct(fields) if short => write!(output, "{{ .../{} }}", fields.len()),
        // One field per line, indented one level deeper
        ValueKind::Struct(fields) => {
            output.write_str("{\n")?;
            for (index, (atom, value)) in fields.iter().enumerate() {
                if index != 0 {
                    output.write_str(";\n")?;
                }
                output.write_str(&indent(level + 1))?;
                atom.print(output)?;
                output.write_char(' ')?;
                write_value_with(arena, output, value, short, level + 1)?;
            }
            output.write_char('\n')?;
            output.write_str(&indent(level))?;
            output.write_char('}')
        }
        // Short form: the case skeleton without arguments
        ValueKind::Case(case) if short => case.to_mixop().print(output),
        ValueKind::Case(case) => write_notval_with(arena, output, case, level),
        ValueKind::Tuple(values) => {
            output.write_char('(')?;
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    output.write_str(", ")?;
                }
                write_value_with(arena, output, value, short, level + 1)?;
            }
            output.write_char(')')
        }
        ValueKind::Opt(Some(value)) => {
            output.write_str("Some(")?;
            write_value_with(arena, output, value, short, level + 1)?;
            output.write_char(')')
        }
        ValueKind::Opt(None) => output.write_str("None"),
        // Empty lists stay on one line
        ValueKind::List(values) if values.is_empty() => output.write_str("[]"),
        // Short form: element count only
        ValueKind::List(values) if short => write!(output, "[ .../{} ]", values.len()),
        // One element per line, indented one level deeper
        ValueKind::List(values) => {
            output.write_str("[\n")?;
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    output.write_str(",\n")?;
                }
                output.write_str(&indent(level + 1))?;
                write_value_with(arena, output, value, short, level + 1)?;
            }
            output.write_char('\n')?;
            output.write_str(&indent(level))?;
            output.write_char(']')
        }
        ValueKind::Func(id) => {
            output.write_char('$')?;
            output.write_str(&id.node)
        }
        ValueKind::Extern(_) => output.write_str("extern"),
    }
}

/// Prints a variant value with its arguments filled into the skeleton.
fn write_notval_with(
    arena: &crate::lang::data::value::ValueArena,
    output: &mut Printer<'_>,
    not_val: &ValueCase,
    level: usize,
) -> fmt::Result {
    not_val.print_with(output, |value, output| {
        write_value_with(arena, output, value, false, level + 1)
    })
}

// - Expressions

impl<I: Print, V: Print> Print for Exp<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
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
                exp.print(printer)?;
                printer.write_str(" as ")?;
                typ.print(printer)
            }
            ExpKind::Sub(exp, typ, _) => {
                exp.print(printer)?;
                printer.write_str(" <: ")?;
                typ.print(printer)
            }
            ExpKind::Match(exp, pattern) => {
                exp.print(printer)?;
                printer.write_str(" matches ")?;
                pattern.print(printer)
            }
            ExpKind::Tuple(exps) => {
                printer.write_char('(')?;
                printer.separated(exps, ", ")?;
                printer.write_char(')')
            }
            ExpKind::Case(not_exp) => not_exp.print(printer),
            ExpKind::Str(fields) => {
                printer.write_char('{')?;
                for (index, ExpField { atom, exp }) in fields.iter().enumerate() {
                    if index != 0 {
                        printer.write_str(", ")?;
                    }
                    atom.print(printer)?;
                    printer.write_char(' ')?;
                    exp.print(printer)?;
                }
                printer.write_char('}')
            }
            ExpKind::Opt(exp) => {
                printer.write_str("?(")?;
                if let Some(exp) = exp {
                    exp.print(printer)?;
                }
                printer.write_char(')')
            }
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
                printer.write_str(" <- ")?;
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
                args.print(printer)
            }
            ExpKind::Iter(exp, exp_iter) => {
                exp.print(printer)?;
                exp_iter.print(printer)
            }
        }
    }
}

impl<I: Print, V: Print> Print for [Exp<I, V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

impl<I: Print, V: Print> Print for NotExp<I, V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.print_with(printer, |exp, printer| exp.print(printer))
    }
}

impl<V: Print> Print for ExpIter<V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.iter.print(printer)?;
        printer.write_char('{')?;
        for (index, var) in self.vars.iter().enumerate() {
            if index != 0 {
                printer.write_str(", ")?;
            }
            var.print(printer)?;
            printer.write_str(" <- ")?;
            var.print(printer)?;
            self.iter.print(printer)?;
        }
        printer.write_char('}')
    }
}

impl<V: Print> Print for [ExpIter<V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for exp_iter in self {
            exp_iter.print(printer)?;
        }
        Ok(())
    }
}

// - Patterns

impl Print for Pattern {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Pattern::Case(mixop) => mixop.print(printer),
            Pattern::List(ListPattern::Cons) => printer.write_str("_ :: _"),
            Pattern::List(ListPattern::Fixed(length)) => write!(printer, "[ _/{length} ]"),
            Pattern::List(ListPattern::Nil) => printer.write_str("[]"),
            Pattern::Opt(OptPattern::Some) => printer.write_str("(_)"),
            Pattern::Opt(OptPattern::None) => printer.write_str("()"),
        }
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

impl Print for Param {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            ParamKind::Exp(typ) => typ.print(printer),
            ParamKind::Def(id, tparams, params, typ) => {
                printer.write_char('$')?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.print(printer)?;
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

// - Hints

impl Print for Hint {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write!(printer, " hint({} ", self.id.node)?;
        self.exp.print(printer)?;
        printer.write_char(')')
    }
}

impl Print for [Hint] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for hint in self {
            hint.print(printer)?;
        }
        Ok(())
    }
}

// - Premises

impl Print for Prem {
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
            // Nested iterations stack without parentheses
            PremKind::Iter(IterPrem { prem: prem_inner, prem_iter })
                if matches!(prem_inner.node, PremKind::Iter(_)) =>
            {
                prem_inner.print(printer)?;
                prem_iter.print(printer)
            }
            // Any other premise is parenthesized under its iteration
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

impl Print for [Prem] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        write_prems_with(printer, 0, self)
    }
}

/// Prints each premise on its own `--` line at the given indent.
fn write_prems_with(output: &mut Printer<'_>, level: usize, prems: &[Prem]) -> fmt::Result {
    for prem in prems {
        write!(output, "\n{}-- ", indent(level))?;
        prem.print(output)?;
    }
    Ok(())
}

/// Prints an iteration as `*{x <- x*, y -> y*}`: bound, then binding variables.
pub(crate) fn print_prem_iter<B: Print>(
    iter: &Iter,
    vars_bound: &[B],
    vars_bind: &[B],
    printer: &mut Printer<'_>,
) -> fmt::Result {
    iter.print(printer)?;
    printer.write_char('{')?;
    // Bound variables point in, binding variables point out
    let vars = vars_bound
        .iter()
        .map(|var| (var, "<-"))
        .chain(vars_bind.iter().map(|var| (var, "->")));
    for (idx, (var, arrow)) in vars.enumerate() {
        if idx != 0 {
            printer.write_str(", ")?;
        }
        var.print(printer)?;
        write!(printer, " {arrow} ")?;
        var.print(printer)?;
        iter.print(printer)?;
    }
    printer.write_char('}')
}

impl<V: Print> Print for PremIter<V> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        print_prem_iter(&self.iter, &self.vars_bound, &self.vars_bind, printer)
    }
}

impl<V: Print> Print for [PremIter<V>] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for prem_iter in self {
            prem_iter.print(printer)?;
        }
        Ok(())
    }
}

// - Rules

impl Print for Rule {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("rule ")?;
        self.node.id.print(printer)?;
        printer.write_str(": ")?;
        self.node.not_exp.print(printer)?;
        write_prems_with(printer, 2, &self.node.prems)
    }
}

impl Print for [Rule] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for rule in self {
            printer.write_str("\n\n  ")?;
            rule.print(printer)?;
        }
        Ok(())
    }
}

impl Print for RuleGroup {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("  rulegroup ")?;
        self.node.id.print(printer)?;
        for rule in &self.node.rules {
            printer.write_str("\n\n    ")?;
            rule.print(printer)?;
        }
        Ok(())
    }
}

impl Print for [RuleGroup] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, rule_group) in self.iter().enumerate() {
            if index != 0 {
                printer.write_str("\n\n")?;
            }
            rule_group.print(printer)?;
        }
        Ok(())
    }
}

impl Print for ElseGroup {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("  rulegroup ")?;
        self.node.id.print(printer)?;
        printer.write_str("\n\n    ")?;
        self.node.rule.print(printer)
    }
}

impl Print for Option<ElseGroup> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        if let Some(else_group) = self {
            printer.write_str("\n\n  elsegroup\n\n")?;
            else_group.print(printer)?;
        }
        Ok(())
    }
}

// - Clauses

impl Print for Clause {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.node.args.print(printer)?;
        printer.write_str(" = ")?;
        self.node.exp.print(printer)?;
        write_prems_with(printer, 1, &self.node.prems)
    }
}

// - Table rows

impl Print for TableRow {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("\n    ")?;
        self.node.args.print(printer)?;
        printer.write_str(" -> ")?;
        self.node.exp.print(printer)
    }
}

impl Print for [TableRow] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, table_row) in self.iter().enumerate() {
            write!(printer, "\n  row {index} :")?;
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
                defined_rel.rule_groups.print(printer)?;
                defined_rel.else_group.print(printer)
            }
        }
    }
}

// == Meta-function definitions

impl Print for MetaFuncDef {
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
                extern_func.params.print(printer)?;
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
                builtin_func.params.print(printer)?;
                printer.write_str(" : ")?;
                builtin_func.typ.print(printer)
            }
            Self::Table(table_func) => {
                printer.write_str("tbl def $")?;
                table_func.id.print(printer)?;
                table_func.params.print(printer)?;
                printer.write_str(" : ")?;
                table_func.typ.print(printer)?;
                printer.write_str(" =")?;
                table_func.rows.print(printer)
            }
            Self::Defined(defined_func) => {
                printer.write_str("def $")?;
                defined_func.id.print(printer)?;
                if !defined_func.tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(&defined_func.tparams, ", ")?;
                    printer.write_char('>')?;
                }
                defined_func.params.print(printer)?;
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

// == Helpers

/// Two spaces per level.
fn indent(level: usize) -> String {
    "  ".repeat(level)
}
