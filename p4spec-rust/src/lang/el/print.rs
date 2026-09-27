//! Text rendering for elaboration-language data
//!
//! Prints EL back in its source syntax,
//! so a parsed and printed specification reads like the original.
//! Premises print on their own `--` lines under a rule.

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
        match self {
            Typ::Plain(plain_typ) => plain_typ.print(printer),
            Typ::Notation(not_typ) => not_typ.print(printer),
        }
    }
}

impl Print for [Typ] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

// - Plain types

impl Print for PlainTyp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            PlainTypKind::Bool => printer.write_str("bool"),
            PlainTypKind::Num(num::Typ::Nat) => printer.write_str("nat"),
            PlainTypKind::Num(num::Typ::Int) => printer.write_str("int"),
            PlainTypKind::Text => printer.write_str("text"),
            PlainTypKind::Var(id, targs) => {
                id.print(printer)?;
                if !targs.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(targs, ", ")?;
                    printer.write_char('>')?;
                }
                Ok(())
            }
            PlainTypKind::Paren(plain_typ) => {
                printer.write_char('(')?;
                plain_typ.print(printer)?;
                printer.write_char(')')
            }
            PlainTypKind::Tuple(plain_typs) => {
                printer.write_char('(')?;
                printer.separated(plain_typs, ", ")?;
                printer.write_char(')')
            }
            PlainTypKind::Iter(plain_typ, iter) => {
                plain_typ.print(printer)?;
                iter.print(printer)
            }
        }
    }
}

// - Notation types

impl Print for NotTyp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            NotTypKind::Atom(atom) => atom.print(printer),
            NotTypKind::Seq(typs) => printer.separated(typs, " "),
            NotTypKind::Infix(typ_l, atom, typ_r) => {
                typ_l.print(printer)?;
                printer.write_char(' ')?;
                atom.print(printer)?;
                printer.write_char(' ')?;
                typ_r.print(printer)
            }
            NotTypKind::Brack(atom_l, typ, atom_r) => {
                printer.write_char('`')?;
                atom_l.print(printer)?;
                typ.print(printer)?;
                atom_r.print(printer)
            }
        }
    }
}

impl Print for [NotTyp] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

// - Defined types

impl Print for DefTyp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            DefTypKind::Plain(plain_typ) => plain_typ.print(printer),
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

impl Print for TypCase {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.typ.print(printer)
    }
}

impl Print for [TypCase] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

// - Operators

impl Print for UnOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Bool(op) => op.print(printer),
            Self::Num(op) => op.print(printer),
        }
    }
}

impl Print for BinOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Bool(op) => op.print(printer),
            Self::Num(op) => op.print(printer),
        }
    }
}

impl Print for CmpOp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Bool(op) => op.print(printer),
            Self::Num(op) => op.print(printer),
        }
    }
}

// - Expressions

impl Print for Exp {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            ExpKind::Bool(value) => write!(printer, "{value}"),
            ExpKind::Num(NumOp::Dec, num::Number::Nat(num)) => write!(printer, "{num}"),
            ExpKind::Num(NumOp::Hex, num::Number::Nat(num)) => {
                write!(printer, "0x{}", num.as_bigint().to_str_radix(16).to_uppercase())
            }
            ExpKind::Num(_, num) => num.print(printer),
            ExpKind::Text(text) => write!(printer, "\"{}\"", escape_text(text)),
            ExpKind::Id(id) => printer.write_str(&id.node),
            ExpKind::Un(op, exp) => {
                op.print(printer)?;
                exp.print(printer)
            }
            ExpKind::Bin(exp_l, op, exp_r) => {
                exp_l.print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.print(printer)
            }
            ExpKind::Cmp(exp_l, op, exp_r) => {
                exp_l.print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.print(printer)
            }
            ExpKind::Arith(exp) => {
                printer.write_str("$(")?;
                exp.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Eps => printer.write_str("eps"),
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
            ExpKind::Len(exp) => {
                printer.write_char('|')?;
                exp.print(printer)?;
                printer.write_char('|')
            }
            ExpKind::Mem(exp_l, exp_r) => {
                exp_l.print(printer)?;
                printer.write_str(" <- ")?;
                exp_r.print(printer)
            }
            ExpKind::Str(fields) => {
                printer.write_char('{')?;
                for (index, (atom, exp)) in fields.iter().enumerate() {
                    if index != 0 {
                        printer.write_str(", ")?;
                    }
                    atom.print(printer)?;
                    printer.write_char(' ')?;
                    exp.print(printer)?;
                }
                printer.write_char('}')
            }
            ExpKind::Dot(exp, atom) => {
                exp.print(printer)?;
                printer.write_char('.')?;
                atom.print(printer)
            }
            ExpKind::Upd(exp_base, path, exp_field) => {
                exp_base.print(printer)?;
                printer.write_char('[')?;
                path.print(printer)?;
                printer.write_str(" = ")?;
                exp_field.print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Paren(exp) => {
                printer.write_char('(')?;
                exp.print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Tuple(exps) => {
                printer.write_char('(')?;
                printer.separated(exps, ", ")?;
                printer.write_char(')')
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
            ExpKind::Iter(exp, iter) => {
                exp.print(printer)?;
                iter.print(printer)
            }
            ExpKind::Sub(exp, plain_typ) => {
                exp.print(printer)?;
                printer.write_str(" <:")?;
                printer.write_char(' ')?;
                plain_typ.print(printer)
            }
            ExpKind::Atom(atom) => atom.print(printer),
            ExpKind::Seq(exps) => printer.separated(exps, " "),
            ExpKind::Infix(exp_l, atom, exp_r) => {
                exp_l.print(printer)?;
                printer.write_char(' ')?;
                atom.print(printer)?;
                printer.write_char(' ')?;
                exp_r.print(printer)
            }
            ExpKind::Brack(atom_l, exp, atom_r) => {
                printer.write_char('`')?;
                atom_l.print(printer)?;
                exp.print(printer)?;
                atom_r.print(printer)
            }
            ExpKind::Hole(Hole::Num(num)) => write!(printer, "%{num}"),
            ExpKind::Hole(Hole::Next) => printer.write_char('%'),
            ExpKind::Hole(Hole::Rest) => printer.write_str("%%"),
            ExpKind::Hole(Hole::None) => printer.write_str("!%"),
            ExpKind::Fuse(exp_l, _, exp_r) => {
                exp_l.print(printer)?;
                printer.write_char('#')?;
                exp_r.print(printer)
            }
            ExpKind::Unparen(exp) => {
                printer.write_str("##")?;
                exp.print(printer)
            }
            ExpKind::Latex(text) => write!(printer, "latex({})", escape_text(text)),
        }
    }
}

impl Print for [Exp] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.separated(self, ", ")
    }
}

// - Paths

impl Print for Path {
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
            ParamKind::Exp(plain_typ) => plain_typ.print(printer),
            ParamKind::Def(id, tparams, params, plain_typ) => {
                printer.write_char('$')?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
        }
    }
}

impl Print for [Param] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        if self.is_empty() {
            Ok(())
        } else {
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
}

// - Arguments

impl Print for Arg {
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

impl Print for [Arg] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        if self.is_empty() {
            Ok(())
        } else {
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
}

// - Premises

impl Print for Prem {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            PremKind::Var(VarPrem { id, plain_typ }) => {
                id.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            PremKind::Rule(RulePrem { id, exp }) => {
                id.print(printer)?;
                printer.write_str(": ")?;
                exp.print(printer)
            }
            PremKind::RuleNot(RuleNotPrem { id, exp }) => {
                id.print(printer)?;
                printer.write_str(":/ ")?;
                exp.print(printer)
            }
            PremKind::If(IfPrem { exp }) => {
                printer.write_str("if ")?;
                exp.print(printer)
            }
            PremKind::Else => printer.write_str("otherwise"),
            // Nested iterations stack without parentheses
            PremKind::Iter(IterPrem { prem: inner, iter })
                if matches!(inner.node, PremKind::Iter(_)) =>
            {
                inner.print(printer)?;
                iter.print(printer)
            }
            // Any other premise is parenthesized under its iteration
            PremKind::Iter(IterPrem { prem: inner, iter }) => {
                printer.write_char('(')?;
                inner.print(printer)?;
                printer.write_char(')')?;
                iter.print(printer)
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
        for prem in self {
            printer.write_str("\n -- ")?;
            prem.print(printer)?;
        }
        Ok(())
    }
}

// - Rules

impl Print for Rule {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write_str("rule ")?;
        self.node.id_rel.print(printer)?;
        if !self.node.id_rule.node.is_empty() {
            printer.write_char('/')?;
            self.node.id_rule.print(printer)?;
        }
        printer.write_str(":\n  ")?;
        self.node.exp.print(printer)?;
        self.node.prems.print(printer)
    }
}

impl Print for [Rule] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, rule) in self.iter().enumerate() {
            if index != 0 {
                printer.write_char('\n')?;
            }
            rule.print(printer)?;
        }
        Ok(())
    }
}

// - Table rows

impl Print for TableRow {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.node.exp_pattern.print(printer)?;
        printer.write_str(" => ")?;
        self.node.exp_body.print(printer)
    }
}

impl Print for [TableRow] {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for (index, row) in self.iter().enumerate() {
            if index != 0 {
                printer.write_str("\n  | ")?;
            }
            row.print(printer)?;
        }
        Ok(())
    }
}

// == Definitions

impl Print for Def {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match &self.node {
            DefKind::ExternSyntax(ExternSyntaxDef { id, .. }) => {
                printer.write_str("extern syntax ")?;
                id.print(printer)
            }
            DefKind::Syntax(SyntaxDef { entries }) => {
                printer.write_str("syntax ")?;
                for (index, SyntaxDefEntry { id, tparams }) in entries.iter().enumerate() {
                    if index != 0 {
                        printer.write_str(", ")?;
                    }
                    id.print(printer)?;
                    if !tparams.is_empty() {
                        printer.write_char('<')?;
                        printer.separated(tparams, ", ")?;
                        printer.write_char('>')?;
                    }
                }
                Ok(())
            }
            DefKind::Typ(TypDef { id, tparams, def_typ, .. }) => {
                printer.write_str("syntax ")?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                printer.write_str(" = ")?;
                def_typ.print(printer)
            }
            DefKind::Var(VarDef { id, plain_typ, .. }) => {
                printer.write_str("var ")?;
                id.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            DefKind::ExternRel(ExternRelDef { id, not_typ, .. }) => {
                printer.write_str("extern relation ")?;
                id.print(printer)?;
                printer.write_str(": ")?;
                not_typ.print(printer)
            }
            DefKind::Rel(RelDef { id, not_typ, .. }) => {
                printer.write_str("relation ")?;
                id.print(printer)?;
                printer.write_str(": ")?;
                not_typ.print(printer)
            }
            DefKind::RuleGroup(RuleGroupDef { relid, groupid, rules }) => {
                printer.write_str("rulegroup ")?;
                relid.print(printer)?;
                if !groupid.node.is_empty() {
                    printer.write_char('/')?;
                    groupid.print(printer)?;
                }
                printer.write_str(":\n  ")?;
                for (index, rule) in rules.iter().enumerate() {
                    if index != 0 {
                        printer.write_str("\n  ")?;
                    }
                    rule.print(printer)?;
                }
                Ok(())
            }
            DefKind::ExternDec(ExternDecDef { id, tparams, params, plain_typ, .. }) => {
                printer.write_str("extern dec $")?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            DefKind::BuiltinDec(BuiltinDecDef { id, tparams, params, plain_typ, .. }) => {
                printer.write_str("builtin dec $")?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            DefKind::TableDec(TableDecDef { id, params, plain_typ, .. }) => {
                printer.write_str("tbl dec $")?;
                id.print(printer)?;
                params.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            DefKind::FuncDec(FuncDecDef { id, tparams, params, plain_typ, .. }) => {
                printer.write_str("dec $")?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                params.print(printer)?;
                printer.write_str(" : ")?;
                plain_typ.print(printer)
            }
            DefKind::TableDef(TableDef { id, rows }) => {
                printer.write_str("tbl def $")?;
                id.print(printer)?;
                printer.write_str(" =\n  ")?;
                for (index, row) in rows.iter().enumerate() {
                    if index != 0 {
                        printer.write_str("\n  | ")?;
                    }
                    row.node.exp_pattern.print(printer)?;
                    printer.write_str(" => ")?;
                    row.node.exp_body.print(printer)?;
                }
                Ok(())
            }
            DefKind::FuncDef(FuncDef { id, tparams, args, exp, prems }) => {
                printer.write_str("def $")?;
                id.print(printer)?;
                if !tparams.is_empty() {
                    printer.write_char('<')?;
                    printer.separated(tparams, ", ")?;
                    printer.write_char('>')?;
                }
                args.print(printer)?;
                printer.write_str(" = ")?;
                exp.print(printer)?;
                prems.print(printer)
            }
            DefKind::Sep => printer.write_str("\n\n"),
        }
    }
}

// == Specifications

impl Print for Spec {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        for def in self {
            def.print(printer)?;
            printer.write_char('\n')?;
        }
        Ok(())
    }
}
