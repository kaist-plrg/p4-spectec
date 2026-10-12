//! Borrowed prepared syntax rendered through the Print trait
//!
//! The `view` methods pair prepared syntax with its notation arena.
//! `Print::print` resolves mixops while traversing the syntax;
//! `Print::to_string` supplies the common string-rendering entry point.

use std::fmt::{self, Write};

use crate::util::text::escape_text;

use crate::lang::{
    data::notation::MixopArena,
    traits::print::{Print, Printer},
};

use crate::lang::il::prepared::*;

// == Borrowed views

/// Borrows a prepared expression and its notation arena for printing.
#[derive(Clone, Copy, Debug)]
pub struct ExpRef<'a> {
    /// The arena that owns the notation handles.
    arena_mixop: &'a MixopArena,
    /// The prepared expression to render.
    exp: &'a Exp,
}

/// Borrows prepared expressions for comma-separated printing.
#[derive(Clone, Copy, Debug)]
pub struct ExpsRef<'a> {
    arena_mixop: &'a MixopArena,
    exps: &'a [Exp],
}

/// Borrows a prepared path and its notation arena for printing.
#[derive(Clone, Copy, Debug)]
pub struct PathRef<'a> {
    arena_mixop: &'a MixopArena,
    path: &'a Path,
}

/// Borrows a prepared argument and its notation arena for printing.
#[derive(Clone, Copy, Debug)]
pub struct ArgRef<'a> {
    /// The arena that owns the notation handles.
    arena_mixop: &'a MixopArena,
    /// The prepared argument to render.
    arg: &'a Arg,
}

/// Borrows prepared arguments, printing parentheses only for nonempty lists.
#[derive(Clone, Copy, Debug)]
pub struct ArgsRef<'a> {
    arena_mixop: &'a MixopArena,
    args: &'a [Arg],
}

/// Borrows a prepared pattern and its notation arena for printing.
#[derive(Clone, Copy, Debug)]
pub struct PatternRef<'a> {
    /// The arena that owns the notation handles.
    arena_mixop: &'a MixopArena,
    /// The prepared pattern to render.
    pattern: &'a Pattern,
}

impl Exp {
    /// Borrows this expression with its notation arena for printing.
    pub fn view<'a>(&'a self, arena_mixop: &'a MixopArena) -> ExpRef<'a> {
        ExpRef { arena_mixop, exp: self }
    }
}

impl<'a> ExpsRef<'a> {
    /// Borrows the slice with its notation arena for printing.
    pub fn new(arena_mixop: &'a MixopArena, exps: &'a [Exp]) -> Self {
        Self { arena_mixop, exps }
    }
}

impl Path {
    /// Borrows this path with its notation arena for printing.
    pub fn view<'a>(&'a self, arena_mixop: &'a MixopArena) -> PathRef<'a> {
        PathRef { arena_mixop, path: self }
    }
}

impl Arg {
    /// Borrows this argument with its notation arena for printing.
    pub fn view<'a>(&'a self, arena_mixop: &'a MixopArena) -> ArgRef<'a> {
        ArgRef { arena_mixop, arg: self }
    }
}

impl<'a> ArgsRef<'a> {
    /// Borrows the slice with its notation arena for printing.
    pub fn new(arena_mixop: &'a MixopArena, args: &'a [Arg]) -> Self {
        Self { arena_mixop, args }
    }
}

impl Pattern {
    /// Borrows this pattern with its notation arena for printing.
    pub fn view<'a>(&'a self, arena_mixop: &'a MixopArena) -> PatternRef<'a> {
        PatternRef { arena_mixop, pattern: self }
    }
}

// == Printing

// - Expressions

impl Print for ExpRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let exp = self.exp;
        match &exp.node {
            ExpKind::Bool(value) => write!(printer, "{value}"),
            ExpKind::Num(value) => value.print(printer),
            ExpKind::Text(text) => write!(printer, "\"{}\"", escape_text(text)),
            ExpKind::Id(id) => id.print(printer),
            ExpKind::Un(op, _, exp) => {
                op.print(printer)?;
                exp.view(arena_mixop).print(printer)
            }
            ExpKind::Bin(op, _, exp_l, exp_r) => {
                printer.write_char('(')?;
                exp_l.view(arena_mixop).print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.view(arena_mixop).print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Cmp(op, _, exp_l, exp_r) => {
                printer.write_char('(')?;
                exp_l.view(arena_mixop).print(printer)?;
                printer.write_char(' ')?;
                op.print(printer)?;
                printer.write_char(' ')?;
                exp_r.view(arena_mixop).print(printer)?;
                printer.write_char(')')
            }
            ExpKind::UpCast(typ, exp) | ExpKind::DownCast(typ, exp) => {
                exp.view(arena_mixop).print(printer)?;
                printer.write_str(" as ")?;
                typ.print(printer)
            }
            ExpKind::Sub(exp, typ, _) => {
                exp.view(arena_mixop).print(printer)?;
                printer.write_str(" <: ")?;
                typ.print(printer)
            }
            ExpKind::Match(exp, pattern) => {
                exp.view(arena_mixop).print(printer)?;
                printer.write_str(" matches ")?;
                pattern.view(arena_mixop).print(printer)
            }
            ExpKind::Tuple(exps) => {
                printer.write_char('(')?;
                ExpsRef::new(arena_mixop, exps).print(printer)?;
                printer.write_char(')')
            }
            ExpKind::Case(not_exp) => {
                not_exp.print_with(arena_mixop, printer, |exp| exp.view(arena_mixop))
            }
            ExpKind::Str(exp_fields) => {
                printer.write_char('{')?;
                for (idx, ExpField { atom, exp }) in exp_fields.iter().enumerate() {
                    if idx != 0 {
                        printer.write_str(", ")?;
                    }
                    atom.print(printer)?;
                    printer.write_char(' ')?;
                    exp.view(arena_mixop).print(printer)?;
                }
                printer.write_char('}')
            }
            ExpKind::Opt(exp) => {
                printer.write_str("?(")?;
                if let Some(exp) = exp {
                    exp.view(arena_mixop).print(printer)?;
                }
                printer.write_char(')')
            }
            ExpKind::List(exps) => {
                printer.write_char('[')?;
                ExpsRef::new(arena_mixop, exps).print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Cons(exp_head, exp_tail) => {
                exp_head.view(arena_mixop).print(printer)?;
                printer.write_str(" :: ")?;
                exp_tail.view(arena_mixop).print(printer)
            }
            ExpKind::Cat(exp_l, exp_r) => {
                exp_l.view(arena_mixop).print(printer)?;
                printer.write_str(" ++ ")?;
                exp_r.view(arena_mixop).print(printer)
            }
            ExpKind::Mem(exp_elem, exp_set) => {
                exp_elem.view(arena_mixop).print(printer)?;
                printer.write_str(" <- ")?;
                exp_set.view(arena_mixop).print(printer)
            }
            ExpKind::Len(exp) => {
                printer.write_char('|')?;
                exp.view(arena_mixop).print(printer)?;
                printer.write_char('|')
            }
            ExpKind::Dot(exp, atom) => {
                exp.view(arena_mixop).print(printer)?;
                printer.write_char('.')?;
                atom.print(printer)
            }
            ExpKind::Idx(exp_base, exp_idx) => {
                exp_base.view(arena_mixop).print(printer)?;
                printer.write_char('[')?;
                exp_idx.view(arena_mixop).print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                exp_base.view(arena_mixop).print(printer)?;
                printer.write_char('[')?;
                exp_idx.view(arena_mixop).print(printer)?;
                printer.write_str(" : ")?;
                exp_len.view(arena_mixop).print(printer)?;
                printer.write_char(']')
            }
            ExpKind::Upd(exp_base, path, exp_field) => {
                exp_base.view(arena_mixop).print(printer)?;
                printer.write_char('[')?;
                path.view(arena_mixop).print(printer)?;
                printer.write_str(" = ")?;
                exp_field.view(arena_mixop).print(printer)?;
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
                ArgsRef::new(arena_mixop, args).print(printer)
            }
            ExpKind::Iter(exp, exp_iter) => {
                exp.view(arena_mixop).print(printer)?;
                exp_iter.print(printer)
            }
        }
    }
}

impl Print for ExpsRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let exps = self.exps;
        for (idx, exp) in exps.iter().enumerate() {
            if idx != 0 {
                printer.write(", ")?;
            }
            exp.view(arena_mixop).print(printer)?;
        }
        Ok(())
    }
}

// - Paths

impl Print for PathRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let path = self.path;
        match &path.node {
            PathKind::Root => Ok(()),
            PathKind::Idx(path, exp_idx) => {
                path.view(arena_mixop).print(printer)?;
                printer.write_char('[')?;
                exp_idx.view(arena_mixop).print(printer)?;
                printer.write_char(']')
            }
            PathKind::Slice(path, exp_idx, exp_len) => {
                path.view(arena_mixop).print(printer)?;
                printer.write_char('[')?;
                exp_idx.view(arena_mixop).print(printer)?;
                printer.write_str(" : ")?;
                exp_len.view(arena_mixop).print(printer)?;
                printer.write_char(']')
            }
            PathKind::Dot(path, atom) if matches!(path.node, PathKind::Root) => atom.print(printer),
            PathKind::Dot(path, atom) => {
                path.view(arena_mixop).print(printer)?;
                printer.write_char('.')?;
                atom.print(printer)
            }
        }
    }
}

// - Arguments

impl Print for ArgRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let arg = self.arg;
        match &arg.node {
            ArgKind::Exp(exp) => exp.view(arena_mixop).print(printer),
            ArgKind::Def(id) => {
                printer.write_char('$')?;
                id.print(printer)
            }
        }
    }
}

impl Print for ArgsRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let args = self.args;
        if args.is_empty() {
            return Ok(());
        }
        printer.write("(")?;
        for (idx, arg) in args.iter().enumerate() {
            if idx != 0 {
                printer.write(", ")?;
            }
            arg.view(arena_mixop).print(printer)?;
        }
        printer.write(")")
    }
}

// - Patterns

impl Print for PatternRef<'_> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        let arena_mixop = self.arena_mixop;
        let pattern = self.pattern;
        match pattern {
            Pattern::Case(mixop) => mixop.print_with(arena_mixop, printer, |_| "%"),
            Pattern::List(ListPattern::Cons) => printer.write_str("_ :: _"),
            Pattern::List(ListPattern::Fixed(length)) => write!(printer, "[ _/{length} ]"),
            Pattern::List(ListPattern::Nil) => printer.write_str("[]"),
            Pattern::Opt(OptPattern::Some) => printer.write_str("(_)"),
            Pattern::Opt(OptPattern::None) => printer.write_str("()"),
        }
    }
}
