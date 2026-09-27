//! AsciiDoc rendering for prose-language definitions
//!
//! ```text
//! def $i_if(n, m) = n
//!   -- if $(n < m)
//! -> xref:i_if[$i_if(n, m)]
//!
//!    . Check that ``n`` is less than ``m``.
//!    . Return ``n``.
//!
//! extern relation Oracle: nat ~> nat
//! -> xref:Oracle[Oracle: ``nat`` ``+~>+`` ``%``]
//! ```

use crate::{
    lang::{
        common::{
            Iter,
            notation::{atom::Atom, mixfix::Mixfix},
            prim::{
                bool::{BinOp as BoolBinOp, CmpOp as BoolCmpOp, UnOp as BoolUnOp},
                num::CmpOp as NumCmpOp,
            },
        },
        el,
        hints::{alter, input},
        il::ast::{ListPattern, OptPattern, Pattern},
        pl::{
            annot::Hints,
            ast::{self as pl, ExpKind},
            rule_group,
        },
        sl,
        traits::{has_call::HasCall, print::Print},
    },
    util::text::escape_text,
};

use super::{
    doc::{
        doc::{Block, Code, ItemKind, Link, Prose, Subject, Table},
        serialize,
    },
    fallthrough::{self, Anchors, Context},
};

// == Render utils

/// The largest visible width kept inline by the prose renderer.
const ADOC_WIDTH_SHORT: usize = 30;

// - Lists
//
//   [x]         -> ``x``
//   [x, y]      -> ``x`` and ``y``
//   [x, y, z]   -> ``x``, ``y``, and ``z``

impl Prose {
    /// Joins owned prose with an Oxford comma without cloning its trees.
    fn of_list(proses: Vec<Prose>) -> Prose {
        let num_proses = proses.len();
        // The final separator depends on whether the list has two or more items
        Prose::join_with(
            |idx| {
                let separator = if num_proses == 2 {
                    " and "
                } else if idx + 1 == num_proses {
                    ", and "
                } else {
                    ", "
                };
                Prose::text(separator)
            },
            proses,
        )
    }
}

// - Scripts
//
//   adoc_subscript("max")   -> ~max~
//   adoc_superscript("?")   -> ^?^

/// Formats a subscript.
fn adoc_subscript(text: &str) -> String {
    format!("~{text}~")
}

/// Formats a superscript.
fn adoc_superscript(text: &str) -> String {
    format!("^{text}^")
}

// - Line joins
//
//   reindent_lines(1, "x\ny")   -> x
//                                   ** y
//   unindent_lines("x\ny")      -> xy

/// Starts continuation lines as unordered list entries.
fn reindent_lines(level: usize, text: &str) -> String {
    let bullet = serialize::adoc_unordered_bullet(level);
    text.replace('\n', &format!("\n{bullet}"))
}

/// Joins lines without inserting a separator.
fn unindent_lines(text: &str) -> String {
    text.replace('\n', "")
}

// == Alternation
//
//   hint(prose_in %0 "mod" %1) on $modulo(n_a, n_b)   -> ``n~a~`` mod ``n~b~``

struct AlterRenderer<'a, Item> {
    base_text: &'a dyn Fn(&str) -> String,
    render_item: &'a dyn Fn(&Item) -> Prose,
}

impl<Item> alter::Renderer<Item> for AlterRenderer<'_, Item> {
    type Output = Prose;

    fn empty(&self) -> Self::Output {
        Prose::Empty
    }

    fn text(&self, text_hint: &str) -> Option<Self::Output> {
        (!text_hint.is_empty()).then(|| Prose::text((self.base_text)(text_hint)))
    }

    fn atom(&self, atom: &pl::Atom) -> Self::Output {
        let text_atom = string_of_atom(atom);
        Prose::code(Code::token(text_atom))
    }

    fn join(&self, proses: Vec<Self::Output>) -> Self::Output {
        Prose::join(" ", proses)
    }

    fn fuse(&self, output_l: Self::Output, output_r: Self::Output) -> Self::Output {
        Prose::seq([output_l, output_r])
    }

    fn other(&self, exp: &el::ast::Exp) -> Self::Output {
        Prose::text(Print::to_string(exp))
    }

    fn item(&self, item: &Item) -> Self::Output {
        (self.render_item)(item)
    }
}

/// Applies an alteration hint to prose items.
fn alternate<Item>(
    hint: &alter::AlterationHint,
    base_text: &dyn Fn(&str) -> String,
    render_item: &dyn Fn(&Item) -> Prose,
    items: &[Item],
    caps: bool,
) -> Prose {
    let renderer_alter = AlterRenderer { base_text, render_item };
    let prose_alternated =
        alter::alternate(hint, items, &renderer_alter).expect("prosify validates alteration hints");
    if caps { prose_alternated.capitalize_first() } else { prose_alternated }
}

// == Mixfix
//
//   INT n     -> ``+INT+`` ``n``
//   n* ~> %   -> ``n^{asterisk}^`` ``+~>+`` ``%``

impl Code {
    /// Renders a mixfix tree with caller-rendered arguments.
    fn of_mixfix<T>(mixfix: &Mixfix<T>, render_arg: &dyn Fn(&T) -> Code) -> Code {
        match mixfix {
            Mixfix::Arg(arg) => render_arg(arg),
            Mixfix::Atom(atom) => {
                let text_atom = string_of_atom(atom);
                Code::token(text_atom)
            }
            Mixfix::Brack(atom_l, mixfix_inner, atom_r) => {
                let text_l = string_of_atom(atom_l);
                let code_inner = Code::of_mixfix(mixfix_inner, render_arg);
                let text_r = string_of_atom(atom_r);
                Code::seq([
                    Code::token(text_l),
                    Code::token(" "),
                    code_inner,
                    Code::token(" "),
                    Code::token(text_r),
                ])
            }
            Mixfix::Infix(mixfix_l, atom, mixfix_r) => {
                let code_l = Code::of_mixfix(mixfix_l, render_arg);
                let text_atom = string_of_atom(atom);
                let code_r = Code::of_mixfix(mixfix_r, render_arg);
                Code::seq([
                    code_l,
                    Code::token(" "),
                    Code::token(text_atom),
                    Code::token(" "),
                    code_r,
                ])
            }
            Mixfix::Seq(mixfixes) => {
                let codes = mixfixes
                    .iter()
                    .map(|mixfix| Code::of_mixfix(mixfix, render_arg));
                Code::join(" ", codes)
            }
        }
    }
}

// == Identifiers
//
//   n       -> ``n``
//   n_max   -> ``n~max~``
//   _       -> ``++_++``
//   $f      -> $f

fn string_of_defid(id: &pl::Id) -> String {
    format!("${}", id.node)
}

impl Code {
    /// Renders an identifier with its suffix as a subscript.
    fn of_id(id: &pl::Id) -> Code {
        // Preserve the anonymous identifier literally
        if id.node.starts_with('_') {
            return Code::token("++_++");
        }

        // Render the base before any underscore suffix
        let mut parts = id.node.split('_');
        let base = parts.next().unwrap_or_default();
        let subscript = parts.collect::<Vec<_>>().join("_");
        if subscript.is_empty() {
            Code::token(base)
        } else {
            let text_subscript = adoc_subscript(&subscript);
            Code::token(format!("{base}{text_subscript}"))
        }
    }
}

// == Atoms
//
//   LEFT     -> +LEFT+
//   _EMPTY   -> {nbsp}~EMPTY~
//   '++'     -> {apos}{plus}{plus}{apos}
//   ->       -> +->+

/// Escapes an atom for an AsciiDoc code span.
fn string_of_atom(atom: &pl::Atom) -> String {
    match &atom.node {
        Atom::Tag(id) => {
            let text_subscript = adoc_subscript(id);
            format!("{{nbsp}}{text_subscript}")
        }
        atom_kind => {
            let text_atom = Print::to_string(atom_kind);
            if text_atom.contains('+') {
                text_atom.replace('+', "{plus}").replace('\'', "{apos}")
            } else {
                format!("+{text_atom}+")
            }
        }
    }
}

// == Iterators
//
//   *   -> ^{asterisk}^, list
//   ?   -> ^?^, option

impl Code {
    fn of_iter(iter: Iter) -> Code {
        let text_iter = match iter {
            Iter::List => "{asterisk}",
            Iter::Opt => "?",
        };
        let text_superscript = adoc_superscript(text_iter);
        Code::token(text_superscript)
    }
}

fn string_of_iter(iter: Iter) -> &'static str {
    match iter {
        Iter::List => "list",
        Iter::Opt => "option",
    }
}

// == Variables
//
//   n*        -> ``n^{asterisk}^``
//   n in n*   -> ``n`` in ``n^{asterisk}^``

impl Code {
    fn of_var(var: &pl::Var) -> Code {
        let code_id = Code::of_id(&var.id);
        let codes_iter = var.iters.iter().map(|iter| Code::of_iter(*iter));
        Code::seq(std::iter::once(code_id).chain(codes_iter))
    }
}

impl Prose {
    fn of_in_itervar(iter: Iter, var: &pl::Var) -> Prose {
        let code_var = Code::of_var(var);
        let code_iterated = Code::seq([Code::of_var(var), Code::of_iter(iter)]);
        Prose::seq([Prose::code(code_var), Prose::text(" in "), Prose::code(code_iterated)])
    }

    fn of_in_itervars(iter: Iter, vars: &[pl::Var]) -> Prose {
        let proses = vars
            .iter()
            .map(|var| Prose::of_in_itervar(iter, var))
            .collect();
        Prose::of_list(proses)
    }

    fn of_out_itervars(iter: Iter, vars: &[&pl::Var]) -> Prose {
        let proses = vars
            .iter()
            .filter(|var| !var.id.node.starts_with('_'))
            .map(|var| {
                let code_iterated = Code::seq([Code::of_var(var), Code::of_iter(iter)]);
                Prose::code(code_iterated)
            })
            .collect();
        Prose::of_list(proses)
    }
}

// == Types
//
//   datum   -> ``datum``

impl Code {
    fn of_typ(typ: &pl::Typ) -> Code {
        Code::token(Print::to_string(typ))
    }
}

// == Operators
//
//   /\   -> and
//   =>   -> implies
//   <    -> is less than
//   =    -> is equal to

fn string_of_binop(op: pl::BinOp) -> String {
    match op {
        pl::BinOp::Bool(BoolBinOp::And) => "and".to_owned(),
        pl::BinOp::Bool(BoolBinOp::Or) => "or".to_owned(),
        pl::BinOp::Bool(BoolBinOp::Impl) => "implies".to_owned(),
        pl::BinOp::Bool(BoolBinOp::Equiv) => "is equivalent to".to_owned(),
        _ => Print::to_string(&op),
    }
}

fn string_of_cmpop(op: pl::CmpOp) -> &'static str {
    match op {
        pl::CmpOp::Bool(BoolCmpOp::Eq) => "is equal to",
        pl::CmpOp::Bool(BoolCmpOp::Ne) => "is not equal to",
        pl::CmpOp::Num(NumCmpOp::Lt) => "is less than",
        pl::CmpOp::Num(NumCmpOp::Gt) => "is greater than",
        pl::CmpOp::Num(NumCmpOp::Le) => "is less than or equal to",
        pl::CmpOp::Num(NumCmpOp::Ge) => "is greater than or equal to",
    }
}

// == Expressions as code

impl Code {
    // - Expression
    //
    //   $e_num(n)   -> xref:e_num[``$e_num(n)``]
    //   n*[m]       -> ``n^{asterisk}^[m]``

    /// Renders an expression in compact code form.
    fn of_exp(exp: &pl::Exp) -> Code {
        match &exp.node.node {
            ExpKind::Bool(value) => Code::of_bool_exp(*value),
            ExpKind::Num(num) => Code::of_num_exp(num),
            ExpKind::Text(text_value) => Code::of_text_exp(text_value),
            ExpKind::Id(id) => Code::of_var_exp(id),
            ExpKind::Un(op, _, exp_inner) => Code::of_un_exp(op, exp_inner),
            ExpKind::Bin(op, _, exp_l, exp_r) => Code::of_bin_exp(op, exp_l, exp_r),
            ExpKind::Cmp(op, _, exp_l, exp_r) => Code::of_cmp_exp(op, exp_l, exp_r),
            ExpKind::UpCast(_, exp_inner) => Code::of_upcast_exp(exp_inner),
            ExpKind::DownCast(_, exp_inner) => Code::of_downcast_exp(exp_inner),
            ExpKind::Sub(exp_inner, typ, _) => Code::of_sub_exp(exp_inner, typ),
            ExpKind::Match(exp_inner, pattern) => Code::of_match_exp(exp_inner, pattern),
            ExpKind::Tuple(exps) => Code::of_tuple_exp(exps),
            ExpKind::Case(not_exp) => Code::of_case_exp(not_exp),
            ExpKind::Str(fields) => Code::of_str_exp(fields),
            ExpKind::Opt(exp_opt) => Code::of_opt_exp(exp_opt.as_deref()),
            ExpKind::List(exps) => Code::of_list_exp(exps),
            ExpKind::Cons(exp_head, exp_tail) => Code::of_cons_exp(exp_head, exp_tail),
            ExpKind::Cat(exp_l, exp_r) => Code::of_cat_exp(exp_l, exp_r),
            ExpKind::Mem(exp_elem, exp_set) => Code::of_mem_exp(exp_elem, exp_set),
            ExpKind::Len(exp_inner) => Code::of_len_exp(exp_inner),
            ExpKind::Dot(exp_base, atom) => Code::of_dot_exp(exp_base, atom),
            ExpKind::Idx(exp_base, exp_idx) => Code::of_idx_exp(exp_base, exp_idx),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                Code::of_slice_exp(exp_base, exp_idx, exp_len)
            }
            ExpKind::Upd(exp_base, path, exp_field) => Code::of_upd_exp(exp_base, path, exp_field),
            ExpKind::Call(id, targs, args) => Code::of_call_exp(id, targs, args),
            ExpKind::Iter(exp_inner, iter_exp) => Code::of_iter_exp(exp_inner, iter_exp),
        }
    }

    fn of_exps(exps: &[pl::Exp], separator: &str) -> Code {
        Code::join(separator, exps.iter().map(Code::of_exp))
    }

    // - Boolean expressions
    //
    //   true   -> ``true``

    fn of_bool_exp(value: bool) -> Code {
        Code::token(value.to_string())
    }

    // - Numeric expressions
    //
    //   42   -> ``42``

    fn of_num_exp(num: &pl::Num) -> Code {
        Code::token(Print::to_string(num))
    }

    // - Text expressions
    //
    //   "a\nb"   -> ``"a\nb"``

    fn of_text_exp(text_value: &str) -> Code {
        let text_escaped = escape_text(text_value);
        Code::token(format!("\"{text_escaped}\""))
    }

    // - Variable expressions
    //
    //   n_max   -> ``n~max~``

    fn of_var_exp(id: &pl::Id) -> Code {
        Code::of_id(id)
    }

    // - Unary expressions
    //
    //   ~b   -> ``~b``

    fn of_un_exp(op: &pl::UnOp, exp_inner: &pl::Exp) -> Code {
        let code_inner = Code::of_exp(exp_inner);
        Code::seq([Code::token(Print::to_string(op)), code_inner])
    }

    // - Binary expressions
    //
    //   b /\ c     -> ``b`` ``/\`` ``c``
    //   $(n + m)   -> ``n`` ``{plus}`` ``m``

    fn of_bin_exp(op: &pl::BinOp, exp_l: &pl::Exp, exp_r: &pl::Exp) -> Code {
        let code_l = Code::of_exp(exp_l);
        let text_op = Print::to_string(op).replace('+', "{plus}");
        let code_r = Code::of_exp(exp_r);
        Code::seq([code_l, Code::token(format!(" {text_op} ")), code_r])
    }

    // - Comparison expressions
    //
    //   $(n < m)   -> ``n`` ``<`` ``m``

    fn of_cmp_exp(op: &pl::CmpOp, exp_l: &pl::Exp, exp_r: &pl::Exp) -> Code {
        let code_l = Code::of_exp(exp_l);
        let text_op = Print::to_string(op);
        let code_r = Code::of_exp(exp_r);
        Code::seq([code_l, Code::token(format!(" {text_op} ")), code_r])
    }

    // - Upcast expressions
    //
    //   UpCast(int, n)   -> ``n``

    fn of_upcast_exp(exp_inner: &pl::Exp) -> Code {
        Code::of_exp(exp_inner)
    }

    // - Downcast expressions
    //
    //   DownCast(nat, i)   -> ``i``

    fn of_downcast_exp(exp_inner: &pl::Exp) -> Code {
        Code::of_exp(exp_inner)
    }

    // - Subtype checks
    //
    //   v <: datum   -> ``v`` ``has`` ``type`` ``datum``

    fn of_sub_exp(exp_inner: &pl::Exp, typ: &pl::Typ) -> Code {
        let code_inner = Code::of_exp(exp_inner);
        let code_typ = Code::of_typ(typ);
        Code::seq([code_inner, Code::token(" has type "), code_typ])
    }

    // - Pattern checks
    //
    //   Match(b*, [])      -> ``b^{asterisk}^`` ``is`` ``an`` ``empty`` ``list``
    //   Match(k, _EMPTY)   -> ``k`` ``is`` ``{nbsp}~EMPTY~``

    fn of_match_exp(exp: &pl::Exp, pattern: &pl::Pattern) -> Code {
        let code_scrut = Code::of_exp(exp);
        match pattern {
            Pattern::Case(mixop) if mixop.arity() == 0 => {
                let code_pattern = Code::of_pattern(pattern);
                Code::seq([code_scrut, Code::token(" is "), code_pattern])
            }
            Pattern::List(ListPattern::Nil) => {
                Code::seq([code_scrut, Code::token(" is an empty list")])
            }
            Pattern::List(ListPattern::Cons) => {
                Code::seq([code_scrut, Code::token(" is a non-empty list")])
            }
            Pattern::List(ListPattern::Fixed(num_elems)) => {
                Code::seq([code_scrut, Code::token(format!(" is a list of length {num_elems}"))])
            }
            Pattern::Opt(OptPattern::None) => Code::seq([code_scrut, Code::token(" is none")]),
            Pattern::Opt(OptPattern::Some) => Code::seq([code_scrut, Code::token(" is defined")]),
            Pattern::Case(_) => {
                let code_pattern = Code::of_pattern(pattern);
                Code::seq([code_scrut, Code::token(" matches pattern "), code_pattern])
            }
        }
    }

    // - Tuple expressions
    //
    //   (n, m)   -> ``(`` ``n,`` ``m`` ``)``

    fn of_tuple_exp(exps: &[pl::Exp]) -> Code {
        let code_exps = Code::of_exps(exps, ", ");
        Code::seq([Code::token("( "), code_exps, Code::token(" )")])
    }

    // - Case expressions
    //
    //   INT n   -> ``+INT+`` ``n``

    fn of_case_exp(not_exp: &pl::NotExp) -> Code {
        Code::of_mixfix(not_exp, &Code::of_exp)
    }

    // - Struct expressions
    //
    //   {LEFT n, RIGHT m}   -> ``+{++LEFT+`` ``n,`` ``+RIGHT+`` ``m+}+``

    fn of_str_exp(fields: &[(pl::Atom, pl::Exp)]) -> Code {
        let codes_field = fields.iter().map(|(atom, exp_field)| {
            let text_atom = string_of_atom(atom);
            let code_field = Code::of_exp(exp_field);
            Code::seq([Code::token(text_atom), Code::token(" "), code_field])
        });
        let code_fields = Code::join(", ", codes_field);
        Code::seq([Code::token("+{+"), code_fields, Code::token("+}+")])
    }

    // - Option expressions
    //
    //   eps   -> ``·``
    //   n     -> ``n``

    fn of_opt_exp(exp_opt: Option<&pl::Exp>) -> Code {
        match exp_opt {
            None => Code::token("·"),
            Some(exp_inner) => Code::of_exp(exp_inner),
        }
    }

    // - List expressions
    //
    //   []       -> ``·``
    //   [n]      -> ``n``
    //   [n, m]   -> ``+[+`` ``n,`` ``m`` ``+]+``

    fn of_list_exp(exps: &[pl::Exp]) -> Code {
        match exps {
            [] => Code::token("·"),
            [exp] => Code::of_exp(exp),
            _ => {
                let code_exps = Code::of_exps(exps, ", ");
                Code::seq([Code::token("+[+ "), code_exps, Code::token(" +]+")])
            }
        }
    }

    // - Cons expressions
    //
    //   n :: n'*   -> ``n`` ``{two-colons}`` ``n'^{asterisk}^``

    fn of_cons_exp(exp_head: &pl::Exp, exp_tail: &pl::Exp) -> Code {
        let code_head = Code::of_exp(exp_head);
        let code_tail = Code::of_exp(exp_tail);
        Code::seq([code_head, Code::token(" {two-colons} "), code_tail])
    }

    // - Concatenation expressions
    //
    //   n* ++ m*   -> ``n^{asterisk}^`` ``{pp}`` ``m^{asterisk}^``

    fn of_cat_exp(exp_l: &pl::Exp, exp_r: &pl::Exp) -> Code {
        let code_l = Code::of_exp(exp_l);
        let code_r = Code::of_exp(exp_r);
        Code::seq([code_l, Code::token(" {pp} "), code_r])
    }

    // - Membership expressions
    //
    //   n <- m*   -> ``n`` ``is`` ``in`` ``m^{asterisk}^``

    fn of_mem_exp(exp_elem: &pl::Exp, exp_set: &pl::Exp) -> Code {
        let code_elem = Code::of_exp(exp_elem);
        let code_set = Code::of_exp(exp_set);
        Code::seq([code_elem, Code::token(" is in "), code_set])
    }

    // - Length expressions
    //
    //   |n*|   -> ``the`` ``length`` ``of`` ``n^{asterisk}^``

    fn of_len_exp(exp_inner: &pl::Exp) -> Code {
        let code_inner = Code::of_exp(exp_inner);
        Code::seq([Code::token("the length of "), code_inner])
    }

    // - Field-access expressions
    //
    //   p.LEFT   -> ``p.+LEFT+``

    fn of_dot_exp(exp_base: &pl::Exp, atom: &pl::Atom) -> Code {
        let code_base = Code::of_exp(exp_base);
        let text_atom = string_of_atom(atom);
        Code::seq([code_base, Code::token("."), Code::token(text_atom)])
    }

    // - Index expressions
    //
    //   n*[m]   -> ``n^{asterisk}^[m]``

    fn of_idx_exp(exp_base: &pl::Exp, exp_idx: &pl::Exp) -> Code {
        let code_base = Code::of_exp(exp_base);
        let code_idx = Code::of_exp(exp_idx);
        Code::seq([code_base, Code::token("["), code_idx, Code::token("]")])
    }

    // - Slice expressions
    //
    //   n*[m : n']   -> ``n^{asterisk}^[m`` ``:`` ``n']``

    fn of_slice_exp(exp_base: &pl::Exp, exp_idx: &pl::Exp, exp_len: &pl::Exp) -> Code {
        let code_base = Code::of_exp(exp_base);
        let code_idx = Code::of_exp(exp_idx);
        let code_len = Code::of_exp(exp_len);
        Code::seq([
            code_base,
            Code::token("["),
            code_idx,
            Code::token(" : "),
            code_len,
            Code::token("]"),
        ])
    }

    // - Update expressions
    //
    //   p[.LEFT = n]   -> ``p[+LEFT+`` ``=`` ``n]``

    fn of_upd_exp(exp_base: &pl::Exp, path: &pl::Path, exp_field: &pl::Exp) -> Code {
        let code_base = Code::of_exp(exp_base);
        let code_path = Code::of_path(path);
        let code_field = Code::of_exp(exp_field);
        Code::seq([
            code_base,
            Code::token("["),
            code_path,
            Code::token(" = "),
            code_field,
            Code::token("]"),
        ])
    }

    // - Function calls
    //
    //   $e_num(n)   -> xref:e_num[``$e_num(n)``]

    fn of_call_exp(id: &pl::Id, targs: &[pl::Targ], args: &[pl::Arg]) -> Code {
        let text_id = string_of_defid(id);
        let text_targs = string_of_targs(targs);
        let code_args = Code::of_args(args);
        let code_call = Code::seq([Code::token(text_id), Code::token(text_targs), code_args]);
        let link = Link::Subject(Subject::Function(id.node.clone()));
        Code::link(link, code_call)
    }

    // - Iterated expressions
    //
    //   $e_num(n)*   -> xref:e_num[``$e_num(n)``]``^{asterisk}^``

    fn of_iter_exp(exp_inner: &pl::Exp, iter_exp: &pl::ExpIter) -> Code {
        // Iterations without variables render as their body
        if iter_exp.vars.is_empty() {
            return Code::of_exp(exp_inner);
        }

        let code_inner = Code::of_exp(exp_inner);
        let code_iter = Code::of_iter(iter_exp.iter);
        // Parenthesize compound bodies whose code contains spaces
        let needs_parens = !matches!(exp_inner.node.node, ExpKind::Id(_) | ExpKind::Tuple(_))
            && serialize::ser_code(&|_| None, &code_inner).contains(' ');
        if needs_parens {
            Code::seq([Code::token("( "), code_inner, Code::token(" )"), code_iter])
        } else {
            Code::seq([code_inner, code_iter])
        }
    }
}

// == Expressions as prose

impl Prose {
    // - Expression
    //
    //   b /\ c   -> ``b`` and ``c``
    //   n = m    -> ``n`` is equal to ``m``

    /// Renders an expression in readable prose form.
    fn of_exp(exp: &pl::Exp) -> Prose {
        match &exp.node.node {
            ExpKind::Bool(value) => Prose::of_bool_exp(*value),
            ExpKind::Num(num) => Prose::of_num_exp(num),
            ExpKind::Text(text_value) => Prose::of_text_exp(text_value),
            ExpKind::Id(id) => Prose::of_var_exp(id),
            ExpKind::Un(op, _, exp_inner) => Prose::of_un_exp(op, exp_inner),
            ExpKind::Bin(op, _, exp_l, exp_r) => Prose::of_bin_exp(op, exp_l, exp_r),
            ExpKind::Cmp(op, _, exp_l, exp_r) => Prose::of_cmp_exp(op, exp_l, exp_r),
            ExpKind::UpCast(_, exp_inner) => Prose::of_upcast_exp(exp_inner),
            ExpKind::DownCast(_, exp_inner) => Prose::of_downcast_exp(exp_inner),
            ExpKind::Sub(exp_inner, typ, _) => Prose::of_sub_exp(exp_inner, typ),
            ExpKind::Match(exp_inner, pattern) => Prose::of_match_exp(exp_inner, pattern),
            ExpKind::Tuple(exps) => Prose::of_tuple_exp(exps),
            ExpKind::Case(not_exp) => Prose::of_case_exp(exp, not_exp),
            ExpKind::Str(fields) => Prose::of_str_exp(fields),
            ExpKind::Opt(exp_opt) => Prose::of_opt_exp(exp_opt.as_deref()),
            ExpKind::List(exps) => Prose::of_list_exp(exps),
            ExpKind::Cons(exp_head, exp_tail) => Prose::of_cons_exp(exp_head, exp_tail),
            ExpKind::Cat(exp_l, exp_r) => Prose::of_cat_exp(exp_l, exp_r),
            ExpKind::Mem(exp_elem, exp_set) => Prose::of_mem_exp(exp_elem, exp_set),
            ExpKind::Len(exp_inner) => Prose::of_len_exp(exp_inner),
            ExpKind::Dot(exp_base, atom) => Prose::of_dot_exp(exp_base, atom),
            ExpKind::Idx(exp_base, exp_idx) => Prose::of_idx_exp(exp_base, exp_idx),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                Prose::of_slice_exp(exp_base, exp_idx, exp_len)
            }
            ExpKind::Upd(exp_base, path, exp_field) => Prose::of_upd_exp(exp_base, path, exp_field),
            ExpKind::Call(id, targs, args) => Prose::of_call_exp(exp, id, targs, args),
            ExpKind::Iter(exp_inner, iter_exp) => Prose::of_iter_exp(exp_inner, iter_exp),
        }
    }

    fn of_exps(exps: &[pl::Exp]) -> Prose {
        let proses = exps.iter().map(Prose::of_exp).collect();
        Prose::of_list(proses)
    }

    // - Boolean expressions
    //
    //   true   -> ``true``

    fn of_bool_exp(value: bool) -> Prose {
        Prose::code(Code::of_bool_exp(value))
    }

    // - Numeric expressions
    //
    //   42   -> ``42``

    fn of_num_exp(num: &pl::Num) -> Prose {
        Prose::code(Code::of_num_exp(num))
    }

    // - Text expressions
    //
    //   "a\nb"   -> ``"a\nb"``

    fn of_text_exp(text_value: &str) -> Prose {
        Prose::code(Code::of_text_exp(text_value))
    }

    // - Variable expressions
    //
    //   n_max   -> ``n~max~``

    fn of_var_exp(id: &pl::Id) -> Prose {
        Prose::code(Code::of_var_exp(id))
    }

    // - Negated checks
    //
    //   Match(direction_h, _EMPTY)   -> ``direction~h~`` does not match pattern ``{nbsp}~EMPTY~``
    //   nameIR_h <- nameIR'*         -> ``nameIR~h~`` is not in ``nameIR'^{asterisk}^``

    /// Describes the readable negation of a partial check when available.
    fn of_negated_exp(exp: &pl::Exp) -> Option<Prose> {
        match &exp.node.node {
            ExpKind::Match(exp_elem, pattern) => {
                let prose_elem = Prose::of_exp(exp_elem);
                let code_pattern = Code::of_pattern(pattern);
                let proses = [
                    prose_elem,
                    Prose::text(" does not match pattern "),
                    Prose::code(code_pattern),
                ];
                Some(Prose::seq(proses))
            }
            ExpKind::Sub(exp_elem, typ, _) => {
                let code_elem = Code::of_exp(exp_elem);
                let code_typ = Code::of_typ(typ);
                let proses = [
                    Prose::code(code_elem),
                    Prose::text(" does not have type "),
                    Prose::code(code_typ),
                ];
                Some(Prose::seq(proses))
            }
            ExpKind::Mem(exp_elem, exp_set) => {
                let code_elem = Code::of_exp(exp_elem);
                let code_set = Code::of_exp(exp_set);
                let proses =
                    [Prose::code(code_elem), Prose::text(" is not in "), Prose::code(code_set)];
                Some(Prose::seq(proses))
            }
            ExpKind::Call(id, _, args) => {
                // Unhinted calls fall back to negated code
                let Some(hint) = &exp.hints.prose_false else {
                    let code_exp = Code::of_exp(exp);
                    return Some(Prose::code(Code::seq([Code::token("~"), code_exp])));
                };
                let prose_call = alternate(
                    hint,
                    &|text_body| reindent_lines(0, text_body),
                    &Prose::of_arg,
                    args,
                    false,
                );
                let link = Link::Subject(Subject::Function(id.node.clone()));
                Some(Prose::link(link, prose_call))
            }
            _ => None,
        }
    }

    // - Unary expressions
    //
    //   ~(nameIR_h <- nameIR'*)   -> ``nameIR~h~`` is not in ``nameIR'^{asterisk}^``
    //   ~b                        -> ``~b``

    fn of_un_exp(op: &pl::UnOp, exp_inner: &pl::Exp) -> Prose {
        // Boolean negation prefers the readable negated check
        if let pl::UnOp::Bool(BoolUnOp::Not) = op
            && let Some(prose_negated) = Prose::of_negated_exp(exp_inner)
        {
            return prose_negated;
        }

        Prose::code(Code::of_un_exp(op, exp_inner))
    }

    // - Binary expressions
    //
    //   b /\ c     -> ``b`` and ``c``
    //   b => c     -> if ``b``, then ``c``
    //   $(n + m)   -> ``n`` ``{plus}`` ``m``

    fn of_bin_exp(op: &pl::BinOp, exp_l: &pl::Exp, exp_r: &pl::Exp) -> Prose {
        match op {
            pl::BinOp::Bool(BoolBinOp::Impl) => {
                let prose_l = Prose::of_exp(exp_l);
                let prose_r = Prose::of_exp(exp_r);
                Prose::seq([Prose::text("if "), prose_l, Prose::text(", then "), prose_r])
            }
            pl::BinOp::Bool(_) => {
                let prose_l = Prose::of_exp(exp_l);
                let text_op = string_of_binop(*op);
                let prose_r = Prose::of_exp(exp_r);
                Prose::seq([prose_l, Prose::text(format!(" {text_op} ")), prose_r])
            }
            _ => Prose::code(Code::of_bin_exp(op, exp_l, exp_r)),
        }
    }

    // - Comparison expressions
    //
    //   $(n < m)   -> ``n`` is less than ``m``

    fn of_cmp_exp(op: &pl::CmpOp, exp_l: &pl::Exp, exp_r: &pl::Exp) -> Prose {
        let prose_l = Prose::of_exp(exp_l);
        let text_op = string_of_cmpop(*op);
        let prose_r = Prose::of_exp(exp_r);
        Prose::seq([prose_l, Prose::text(format!(" {text_op} ")), prose_r])
    }

    // - Upcast expressions
    //
    //   UpCast(int, n)   -> ``n``

    fn of_upcast_exp(exp_inner: &pl::Exp) -> Prose {
        Prose::code(Code::of_exp(exp_inner))
    }

    // - Downcast expressions
    //
    //   DownCast(nat, i)   -> ``i``

    fn of_downcast_exp(exp_inner: &pl::Exp) -> Prose {
        Prose::code(Code::of_exp(exp_inner))
    }

    // - Subtype checks
    //
    //   v <: datum   -> ``v`` has type ``datum``

    fn of_sub_exp(exp_inner: &pl::Exp, typ: &pl::Typ) -> Prose {
        let code_inner = Code::of_exp(exp_inner);
        let code_typ = Code::of_typ(typ);
        Prose::seq([Prose::code(code_inner), Prose::text(" has type "), Prose::code(code_typ)])
    }

    // - Pattern checks
    //
    //   Match(b*, [])         -> ``b^{asterisk}^`` is an empty list
    //   Match(typeIR?, (_))   -> ``typeIR^?^`` is defined

    /// Describes a pattern check using its specialized list and option wording.
    fn of_match_exp(exp: &pl::Exp, pattern: &pl::Pattern) -> Prose {
        let prose_scrut = Prose::of_exp(exp);
        match pattern {
            Pattern::Case(mixop) if mixop.arity() == 0 => {
                let code_pattern = Code::of_pattern(pattern);
                Prose::seq([prose_scrut, Prose::text(" is "), Prose::code(code_pattern)])
            }
            Pattern::List(ListPattern::Nil) => {
                Prose::seq([prose_scrut, Prose::text(" is an empty list")])
            }
            Pattern::List(ListPattern::Cons) => {
                Prose::seq([prose_scrut, Prose::text(" is a non-empty list")])
            }
            Pattern::List(ListPattern::Fixed(num_elems)) => {
                Prose::seq([prose_scrut, Prose::text(format!(" is a list of length {num_elems}"))])
            }
            Pattern::Opt(OptPattern::None) => Prose::seq([prose_scrut, Prose::text(" is none")]),
            Pattern::Opt(OptPattern::Some) => Prose::seq([prose_scrut, Prose::text(" is defined")]),
            Pattern::Case(_) => {
                let code_pattern = Code::of_pattern(pattern);
                Prose::seq([
                    prose_scrut,
                    Prose::text(" matches pattern "),
                    Prose::code(code_pattern),
                ])
            }
        }
    }

    // - Tuple expressions
    //
    //   (n, m)   -> ( ``n``, ``m`` )

    fn of_tuple_exp(exps: &[pl::Exp]) -> Prose {
        let prose_exps = Prose::join(", ", exps.iter().map(Prose::of_exp));
        Prose::seq([Prose::text("( "), prose_exps, Prose::text(" )")])
    }

    // - Case expressions
    //
    //   INT n   -> ``+INT+`` ``n``

    fn of_case_exp(exp: &pl::Exp, not_exp: &pl::NotExp) -> Prose {
        // Hinted variant values link their prose to the type definition
        if let (Some(hint), pl::TypKind::Var(id_typ, _)) = (&exp.hints.prose, &exp.node.note) {
            let exps = not_exp.args();
            let prose_case = alternate(
                hint,
                &|text_body| reindent_lines(0, text_body),
                &|&exp| Prose::of_exp(exp),
                &exps,
                false,
            );
            return Prose::link(Link::Direct(id_typ.node.clone()), prose_case);
        }

        Prose::code(Code::of_case_exp(not_exp))
    }

    // - Struct expressions
    //
    //   {LEFT n, RIGHT m}   -> +{++LEFT+ ``n``, +RIGHT+ ``m``+}+

    fn of_str_exp(fields: &[(pl::Atom, pl::Exp)]) -> Prose {
        let proses_field = fields.iter().map(|(atom, exp_field)| {
            let text_atom = string_of_atom(atom);
            let prose_field = Prose::of_exp(exp_field);
            Prose::seq([Prose::text(text_atom), Prose::text(" "), prose_field])
        });
        let prose_fields = Prose::join(", ", proses_field);
        Prose::seq([Prose::text("+{+"), prose_fields, Prose::text("+}+")])
    }

    // - Option expressions
    //
    //   eps   -> ``·``
    //   n     -> ``n``

    fn of_opt_exp(exp_opt: Option<&pl::Exp>) -> Prose {
        match exp_opt {
            None => Prose::code(Code::of_opt_exp(None)),
            Some(exp_inner) => Prose::of_exp(exp_inner),
        }
    }

    // - List expressions
    //
    //   [n, m]   -> ``+[+`` ``n,`` ``m`` ``+]+``

    fn of_list_exp(exps: &[pl::Exp]) -> Prose {
        Prose::code(Code::of_list_exp(exps))
    }

    // - Cons expressions
    //
    //   n :: n'*   -> ``n`` ``{two-colons}`` ``n'^{asterisk}^``

    fn of_cons_exp(exp_head: &pl::Exp, exp_tail: &pl::Exp) -> Prose {
        Prose::code(Code::of_cons_exp(exp_head, exp_tail))
    }

    // - Concatenation expressions
    //
    //   n* ++ m*   -> ``n^{asterisk}^`` concatenated with ``m^{asterisk}^``

    fn of_cat_exp(exp_l: &pl::Exp, exp_r: &pl::Exp) -> Prose {
        let prose_l = Prose::of_exp(exp_l);
        let prose_r = Prose::of_exp(exp_r);
        Prose::seq([prose_l, Prose::text(" concatenated with "), prose_r])
    }

    // - Membership expressions
    //
    //   n <- m*   -> ``n`` is in ``m^{asterisk}^``

    fn of_mem_exp(exp_elem: &pl::Exp, exp_set: &pl::Exp) -> Prose {
        let prose_elem = Prose::of_exp(exp_elem);
        let prose_set = Prose::of_exp(exp_set);
        Prose::seq([prose_elem, Prose::text(" is in "), prose_set])
    }

    // - Length expressions
    //
    //   |n*|   -> the length of ``n^{asterisk}^``

    fn of_len_exp(exp_inner: &pl::Exp) -> Prose {
        let prose_inner = Prose::of_exp(exp_inner);
        Prose::seq([Prose::text("the length of "), prose_inner])
    }

    // - Field-access expressions
    //
    //   p.LEFT   -> ``p.+LEFT+``

    fn of_dot_exp(exp_base: &pl::Exp, atom: &pl::Atom) -> Prose {
        Prose::code(Code::of_dot_exp(exp_base, atom))
    }

    // - Index expressions
    //
    //   n*[m]   -> ``n^{asterisk}^[m]``

    fn of_idx_exp(exp_base: &pl::Exp, exp_idx: &pl::Exp) -> Prose {
        Prose::code(Code::of_idx_exp(exp_base, exp_idx))
    }

    // - Slice expressions
    //
    //   n*[m : n']   -> ``n^{asterisk}^[m`` ``:`` ``n']``

    fn of_slice_exp(exp_base: &pl::Exp, exp_idx: &pl::Exp, exp_len: &pl::Exp) -> Prose {
        Prose::code(Code::of_slice_exp(exp_base, exp_idx, exp_len))
    }

    // - Update expressions
    //
    //   p[.LEFT = n]   -> ``p`` with ``+LEFT+`` set to ``n``

    fn of_upd_exp(exp_base: &pl::Exp, path: &pl::Path, exp_field: &pl::Exp) -> Prose {
        let code_base = Code::of_exp(exp_base);
        let code_path = Code::of_path(path);
        let code_field = Code::of_exp(exp_field);
        Prose::seq([
            Prose::code(code_base),
            Prose::text(" with "),
            Prose::code(code_path),
            Prose::text(" set to "),
            Prose::code(code_field),
        ])
    }

    // - Function calls
    //
    //   $modulo(n, 42), hinted %0 "mod" %1   -> xref:modulo[``n`` mod ``42``]
    //   $e_num(n), unhinted                  -> xref:e_num[``$e_num(n)``]

    fn of_call_exp(exp: &pl::Exp, id: &pl::Id, targs: &[pl::Targ], args: &[pl::Arg]) -> Prose {
        // Unhinted calls keep their code form
        let Some(hint) = exp
            .hints
            .prose_in
            .as_ref()
            .or(exp.hints.prose_true.as_ref())
        else {
            return Prose::code(Code::of_call_exp(id, targs, args));
        };
        let prose_call =
            alternate(hint, &|text_body| reindent_lines(0, text_body), &Prose::of_arg, args, false);
        let link = Link::Subject(Subject::Function(id.node.clone()));
        Prose::link(link, prose_call)
    }

    // - Iterated expressions
    //
    //   $e_num(n)*   -> xref:e_num[``$e_num(n)``]``^{asterisk}^``

    fn of_iter_exp(exp_inner: &pl::Exp, iter_exp: &pl::ExpIter) -> Prose {
        // Iterations without variables render as their body
        if iter_exp.vars.is_empty() {
            return Prose::of_exp(exp_inner);
        }

        Prose::code(Code::of_iter_exp(exp_inner, iter_exp))
    }
}

// == Patterns
//
//   Nil        -> ``[]``
//   Cons       -> ``_`` ``::`` ``_``
//   Fixed(2)   -> ``[`` ``_/2`` ``]``
//   Some       -> ``(_)``
//   None       -> ``()``

impl Code {
    /// Renders a pattern in its prose-backend notation.
    fn of_pattern(pattern: &pl::Pattern) -> Code {
        match pattern {
            Pattern::Case(mixop) => Code::of_mixfix(mixop, &|()| Code::token("%")),
            Pattern::List(ListPattern::Cons) => Code::token("_ :: _"),
            Pattern::List(ListPattern::Fixed(num_elems)) => {
                Code::token(format!("[ _/{num_elems} ]"))
            }
            Pattern::List(ListPattern::Nil) => Code::token("[]"),
            Pattern::Opt(OptPattern::Some) => Code::token("(_)"),
            Pattern::Opt(OptPattern::None) => Code::token("()"),
        }
    }
}

// == Paths

impl Code {
    // - Path
    //
    //   p[.LEFT = n]           -> ``p`` with ``+LEFT+`` set to ``n``
    //   t[ [n_len] = t_new ]   -> ``t`` with ``[n~len~]`` set to ``t~new~``

    /// Renders an update path.
    fn of_path(path: &pl::Path) -> Code {
        match &path.node {
            pl::PathKind::Root => Code::of_root_path(),
            pl::PathKind::Idx(path_base, exp_idx) => Code::of_idx_path(path_base, exp_idx),
            pl::PathKind::Slice(path_base, exp_idx, exp_len) => {
                Code::of_slice_path(path_base, exp_idx, exp_len)
            }
            pl::PathKind::Dot(path_base, atom) => Code::of_dot_path(path_base, atom),
        }
    }

    // - Root paths
    //
    //   (root)   -> (empty)

    fn of_root_path() -> Code {
        Code::Empty
    }

    // - Index paths
    //
    //   t[ [n_len] = t_new ]   -> ``t`` with ``[n~len~]`` set to ``t~new~``

    fn of_idx_path(path_base: &pl::Path, exp_idx: &pl::Exp) -> Code {
        let code_base = Code::of_path(path_base);
        let code_idx = Code::of_exp(exp_idx);
        Code::seq([code_base, Code::token("["), code_idx, Code::token("]")])
    }

    // - Slice paths
    //
    //   p[[i : j] = n]   -> ``p`` with ``[i`` ``:`` ``j]`` set to ``n``

    fn of_slice_path(path_base: &pl::Path, exp_idx: &pl::Exp, exp_len: &pl::Exp) -> Code {
        let code_base = Code::of_path(path_base);
        let code_idx = Code::of_exp(exp_idx);
        let code_len = Code::of_exp(exp_len);
        Code::seq([
            code_base,
            Code::token("["),
            code_idx,
            Code::token(" : "),
            code_len,
            Code::token("]"),
        ])
    }

    // - Field paths
    //
    //   p[.LEFT = n]   -> ``p`` with ``+LEFT+`` set to ``n``

    fn of_dot_path(path_base: &pl::Path, atom: &pl::Atom) -> Code {
        let text_atom = string_of_atom(atom);
        // The first field in a path has no leading dot
        if matches!(path_base.node, pl::PathKind::Root) {
            return Code::token(text_atom);
        }

        let code_base = Code::of_path(path_base);
        Code::seq([code_base, Code::token("."), Code::token(text_atom)])
    }
}

// == Parameters
//
//   $i_if(n, m), unhinted                    -> xref:i_if[$i_if(n, m)]
//   table $is_defaultable_typeIR(typeIR'')   -> | (``typeIR''``) | Result

impl Code {
    fn of_param(param: &pl::Param) -> Code {
        match &param.node {
            pl::ParamKind::Exp(_, exp) => Code::of_exp(exp),
            pl::ParamKind::Def(id, _, _, _) => Code::token(string_of_defid(id)),
        }
    }

    fn of_params(params: &[pl::Param]) -> Code {
        if params.is_empty() {
            return Code::Empty;
        }

        let code_params = Code::join(", ", params.iter().map(Code::of_param));
        Code::seq([Code::token("("), code_params, Code::token(")")])
    }
}

impl Prose {
    fn of_param(param: &pl::Param) -> Prose {
        match &param.node {
            pl::ParamKind::Exp(_, exp) => Prose::of_exp(exp),
            pl::ParamKind::Def(id, _, _, _) => Prose::code(Code::token(string_of_defid(id))),
        }
    }

    fn of_params(params: &[pl::Param]) -> Prose {
        if params.is_empty() {
            return Prose::Empty;
        }

        let prose_params = Prose::join(", ", params.iter().map(Prose::of_param));
        Prose::seq([Prose::text("("), prose_params, Prose::text(")")])
    }
}

// == Type arguments
//
//   <nat, text>   -> <nat, text>
//   (none)        -> (empty)

fn string_of_targs(targs: &[pl::Targ]) -> String {
    if targs.is_empty() {
        return String::new();
    }

    let texts_targ: Vec<String> = targs.iter().map(Print::to_string).collect();
    let text_targs = texts_targ.join(", ");
    format!("<{text_targs}>")
}

// == Arguments
//
//   $e_num(n)   -> xref:e_num[``$e_num(n)``]

impl Code {
    fn of_arg(arg: &pl::Arg) -> Code {
        match &arg.node {
            pl::ArgKind::Exp(exp) => Code::of_exp(exp),
            pl::ArgKind::Def(id) => Code::token(string_of_defid(id)),
        }
    }

    /// Renders a nonempty argument list with parentheses.
    fn of_args(args: &[pl::Arg]) -> Code {
        if args.is_empty() {
            return Code::Empty;
        }

        let code_args = Code::join(", ", args.iter().map(Code::of_arg));
        Code::seq([Code::token("("), code_args, Code::token(")")])
    }
}

impl Prose {
    fn of_arg(arg: &pl::Arg) -> Prose {
        match &arg.node {
            pl::ArgKind::Exp(exp) => Prose::of_exp(exp),
            pl::ArgKind::Def(id) => Prose::code(Code::token(string_of_defid(id))),
        }
    }
}

// == Case analysis

impl Prose {
    // - Guard
    //
    //   t'* matches []   -> ``t'^{asterisk}^`` matches pattern ``[]``
    //   let t_h be t'*   -> let ``t~h~`` be ``t'^{asterisk}^``

    /// Describes a case guard against its scrutinee.
    fn of_guard(exp_scrut: &pl::Exp, guard: &pl::Guard) -> Prose {
        match guard {
            pl::Guard::Bool(true) => Prose::of_true_guard(exp_scrut),
            pl::Guard::Bool(false) => Prose::of_false_guard(exp_scrut),
            pl::Guard::Cmp(op, _, exp) => Prose::of_cmp_guard(exp_scrut, op, exp),
            pl::Guard::Sub(typ, _) => Prose::of_sub_guard(exp_scrut, typ),
            pl::Guard::Match(pattern) => Prose::of_match_guard(exp_scrut, pattern),
            pl::Guard::Mem(exp) => Prose::of_mem_guard(exp_scrut, exp),
            pl::Guard::CheckLetSub(_, _, exp_target) | pl::Guard::CheckLetMatch(_, exp_target) => {
                Prose::of_check_let_guard(exp_scrut, exp_target)
            }
        }
    }

    // - True guards
    //
    //   b, guarded by true   -> ``b``

    fn of_true_guard(exp_scrut: &pl::Exp) -> Prose {
        Prose::of_exp(exp_scrut)
    }

    // - False guards
    //
    //   n <- m*, guarded by false   -> ``n`` is not in ``m^{asterisk}^``

    fn of_false_guard(exp_scrut: &pl::Exp) -> Prose {
        // Checks without readable negation fall back to negated code
        Prose::of_negated_exp(exp_scrut).unwrap_or_else(|| {
            let code_scrut = Code::of_exp(exp_scrut);
            Prose::code(Code::seq([Code::token("~"), code_scrut]))
        })
    }

    // - Comparison guards
    //
    //   b, guarded by = true   -> ``b`` is equal to ``true``

    fn of_cmp_guard(exp_scrut: &pl::Exp, op: &pl::CmpOp, exp: &pl::Exp) -> Prose {
        let prose_scrut = Prose::of_exp(exp_scrut);
        let text_op = string_of_cmpop(*op);
        let prose_exp = Prose::of_exp(exp);
        Prose::seq([prose_scrut, Prose::text(format!(" {text_op} ")), prose_exp])
    }

    // - Subtype guards
    //
    //   v, guarded by <: datum   -> ``v`` has type ``datum``

    fn of_sub_guard(exp_scrut: &pl::Exp, typ: &pl::Typ) -> Prose {
        let code_scrut = Code::of_exp(exp_scrut);
        let code_typ = Code::of_typ(typ);
        Prose::seq([Prose::code(code_scrut), Prose::text(" has type "), Prose::code(code_typ)])
    }

    // - Pattern guards
    //
    //   t'*, guarded by []   -> ``t'^{asterisk}^`` matches pattern ``[]``

    fn of_match_guard(exp_scrut: &pl::Exp, pattern: &pl::Pattern) -> Prose {
        let prose_scrut = Prose::of_exp(exp_scrut);
        let code_pattern = Code::of_pattern(pattern);
        Prose::seq([prose_scrut, Prose::text(" matches pattern "), Prose::code(code_pattern)])
    }

    // - Membership guards
    //
    //   n, guarded by <- m*   -> ``n`` is in ``m^{asterisk}^``

    fn of_mem_guard(exp_scrut: &pl::Exp, exp: &pl::Exp) -> Prose {
        let prose_scrut = Prose::of_exp(exp_scrut);
        let prose_exp = Prose::of_exp(exp);
        Prose::seq([prose_scrut, Prose::text(" is in "), prose_exp])
    }

    // - Binding guards
    //
    //   t'*, bound to t_h   -> let ``t~h~`` be ``t'^{asterisk}^``

    fn of_check_let_guard(exp_scrut: &pl::Exp, exp_target: &pl::Exp) -> Prose {
        let code_target = Code::of_exp(exp_target);
        let prose_scrut = Prose::of_exp(exp_scrut);
        Prose::seq([
            Prose::text("let "),
            Prose::code(code_target),
            Prose::text(" be "),
            prose_scrut,
        ])
    }
}

// == Rendering context
//
//   two render_rulegroup calls on one Renderer   -> arm anchors bk-Rel-1-arm-1, then bk-Rel-2-arm-1
//   one call on a fresh Renderer                 -> arm anchor bk-Rel-1-arm-1

/// Renders a document's definitions and fragments with shared arm counters.
///
/// Reuse one renderer when composing rule groups, dispatch, and otherwise
/// fragments into a document. Create a new renderer for each new document.
pub struct Renderer<'a> {
    anchors: Anchors,
    anchor: &'a dyn Fn(&Subject) -> Option<String>,
}

/// A tier instruction ready to fold inline or nest below its enclosing head.
enum Rendered {
    /// Appends prose directly to the optional enclosing heading.
    Inline(Prose),
    /// Appends a dispatch target and capitalizes the composed heading.
    InlineGoto(Prose),
    /// Keeps a block below the heading instead of folding it into prose.
    Nested(Block),
}

/// Renders one tier payload while preserving the shared renderer state.
type RenderTier<'a, Tier> =
    fn(&mut Renderer<'a>, usize, &Context, bool, &pl::Instr<Tier>, &Tier) -> Rendered;

impl<'a> Renderer<'a> {
    /// Starts a document with fresh arm counters and a subject resolver.
    pub fn new(anchor: &'a dyn Fn(&Subject) -> Option<String>) -> Self {
        Self { anchors: Anchors::default(), anchor }
    }
}

// == Instructions

impl<'a> Renderer<'a> {
    // - Tier results
    //
    //   Inline under ". If ``b`` is equal to ``true``:"
    //   -> . If ``b`` is equal to ``true``: return ``X~t~``.
    //
    //   InlineGoto under ". If ``nat'^{asterisk}^`` matches pattern ``[]``:"
    //   -> ... matches pattern ``[]``: goto xref:Even-nil[nil]
    //
    //   Nested under ". Else:"
    //   -> . Else:
    //       .. Let ``t~h~`` ``{two-colons}`` ``t~t~^{asterisk}^`` be ``t^{asterisk}^``.

    /// Composes a tier result with the enclosing instruction head.
    fn compose(block_head: Option<Block>, singleton: bool, rendered: Rendered) -> Block {
        match rendered {
            Rendered::Inline(prose_tail) => {
                let block_tail = Block::inline(prose_tail);
                match block_head {
                    Some(block_head) => Block::concat([block_head, block_tail]),
                    None => block_tail,
                }
            }
            Rendered::InlineGoto(prose_tail) => {
                let block_tail = Block::inline(prose_tail);
                let block = match block_head {
                    Some(block_head) => Block::concat([block_head, block_tail]),
                    None => block_tail,
                };
                block.capitalize_first()
            }
            Rendered::Nested(block) if !singleton => block,
            Rendered::Nested(block) => match block_head {
                Some(block_head) => Block::seq([block_head, block]),
                None => Block::concat([Block::raw("\n"), Block::seq([block])]),
            },
        }
    }

    // - Instruction
    //
    //   -- if $(n < m)   -> . Check that ``n`` is less than ``m``.
    //   -- debug n       -> . (debug: ``n``)

    /// Renders one shared or tier-specific instruction.
    fn render_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
    ) -> Block {
        match &instr.node.node {
            pl::InstrKind::If(if_instr) => {
                self.render_if_instr(level, ctx, render_tier, instr, if_instr)
            }
            pl::InstrKind::Hold(hold_instr) => {
                self.render_hold_instr(level, ctx, render_tier, instr, hold_instr)
            }
            pl::InstrKind::Case(case_instr) => {
                self.render_case_instr(level, ctx, render_tier, instr, case_instr)
            }
            pl::InstrKind::Let(let_instr) => Self::render_let_instr(level, ctx, instr, let_instr),
            pl::InstrKind::Debug(debug_instr) => {
                Self::render_debug_instr(level, ctx, instr, debug_instr)
            }
            pl::InstrKind::Destruct(destruct_instr) => {
                Self::render_destruct_instr(level, ctx, instr, destruct_instr)
            }
            pl::InstrKind::CheckLetSub(check_instr) => {
                self.render_check_let_sub_instr(level, ctx, render_tier, instr, check_instr)
            }
            pl::InstrKind::CheckLetMatch(check_instr) => {
                self.render_check_let_match_instr(level, ctx, render_tier, instr, check_instr)
            }
            pl::InstrKind::OptionGet(option_instr) => {
                self.render_option_get_instr(level, ctx, render_tier, instr, option_instr)
            }
            pl::InstrKind::Tier(tier_instr) => {
                self.render_tier_instr(level, ctx, render_tier, instr, tier_instr)
            }
        }
    }

    // - Instruction sequences
    //
    //   $ite<X>(true, X_t, X_f) = X_t   -> . If ``b`` is equal to ``true``: return ``X~t~``.

    /// Renders instructions under an optional heading.
    fn render_instrs<Tier>(
        &mut self,
        level: usize,
        block_head: Option<Block>,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instrs: &[pl::Instr<Tier>],
    ) -> Block {
        // Fold a lone tier instruction into the enclosing heading
        if let [instr] = instrs
            && let pl::InstrKind::Tier(tier_instr) = &instr.node.node
        {
            let rendered = render_tier(self, level, ctx, true, instr, &tier_instr.tier);
            return Self::compose(block_head, true, rendered);
        }

        // Render general blocks as a sequence below the optional heading
        let blocks_rendered: Vec<Block> = instrs
            .iter()
            .map(|instr| self.render_instr(level, ctx, render_tier, instr))
            .collect();
        match block_head {
            Some(block_head) => Block::seq(std::iter::once(block_head).chain(blocks_rendered)),
            None => Block::concat([Block::raw("\n"), Block::seq(blocks_rendered)]),
        }
    }

    /// Appends continuation instructions below a check heading at the same level.
    fn render_continuation<Tier>(
        &mut self,
        level: usize,
        block_head: Block,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instrs: &[pl::Instr<Tier>],
    ) -> Block {
        // Empty continuations leave the heading alone
        if instrs.is_empty() {
            return block_head;
        }

        let blocks_rendered = instrs
            .iter()
            .map(|instr| self.render_instr(level, ctx, render_tier, instr));
        Block::seq(std::iter::once(block_head).chain(blocks_rendered))
    }

    // - Otherwise blocks
    //
    //   def $starts_with(t, t_prefix) = false
    //     -- otherwise
    //   -> . +++<span id="starts_with-else"></span>+++Otherwise: return ``false``.

    /// Serializes an otherwise block with its optional anchor.
    fn render_elseblock<Tier>(
        &mut self,
        anchor_else: Option<&str>,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        block_opt: Option<&[pl::Instr<Tier>]>,
    ) -> String {
        // Omit absent and empty otherwise blocks
        let Some(block) = block_opt.filter(|block| !block.is_empty()) else {
            return String::new();
        };
        // Prefix the visible heading with its optional destination anchor
        let text_anchor = anchor_else
            .map(|anchor| format!("+++<span id=\"{anchor}\"></span>+++"))
            .unwrap_or_default();
        let block_body = self.render_instrs(1, None, ctx, render_tier, block);
        let text_body = serialize::ser_block(self.anchor, &block_body);
        let text_bullet = serialize::adoc_ordered_bullet(0);
        format!("\n\n{text_bullet}{text_anchor}Otherwise:{text_body}")
    }
}

impl Prose {
    // - Iteration suffixes
    //
    //   -- if typeIR? matches (_), iterated over typeIR?*
    //   -> Check that ``typeIR^?^`` is defined, for all ``typeIR^?^`` in ``typeIR^?^^{asterisk}^``.

    fn of_iterexp_suffix(iter_exps: &[pl::ExpIter]) -> Prose {
        // Collect every expression iterator binding in source order
        let proses: Vec<Prose> = iter_exps
            .iter()
            .flat_map(|iter_exp| {
                let iter = iter_exp.iter;
                iter_exp
                    .vars
                    .iter()
                    .map(move |var| Prose::of_in_itervar(iter, var))
            })
            .collect();
        // Omit the quantifier when no variables are bound
        if proses.is_empty() {
            return Prose::Empty;
        }

        let prose_vars = Prose::of_list(proses);
        Prose::seq([Prose::text(", for all "), prose_vars])
    }

    fn of_iterinstr_suffix(iter_instrs: &[pl::InstrIter]) -> Prose {
        // Collect every instruction iterator binding in source order
        let proses: Vec<Prose> = iter_instrs
            .iter()
            .flat_map(|iter_instr| {
                let iter = iter_instr.iter;
                iter_instr
                    .vars_bound
                    .iter()
                    .map(move |var| Prose::of_in_itervar(iter, var))
            })
            .collect();
        // Omit the quantifier when no variables are bound
        if proses.is_empty() {
            return Prose::Empty;
        }

        let prose_vars = Prose::of_list(proses);
        Prose::seq([Prose::text(", for each "), prose_vars])
    }
}

impl<'a> Renderer<'a> {
    // - Iteration blocks
    //
    //   -- (if m = $(n + 1))*   -> . For each ``n`` in ``n^{asterisk}^``:
    //                              +
    //                              --
    //                               ** Let ``m`` be ``n`` ``{plus}`` ``1``.
    //                              --
    //                              +
    //                              Let ``m^{asterisk}^`` be the resulting list.

    /// Wraps a body in nested iteration blocks and binds visible outputs.
    fn render_iterinstrs(
        level: usize,
        prose_fallthrough: Prose,
        iter_instrs: &[pl::InstrIter],
        render_body: &dyn Fn(usize) -> Block,
    ) -> Block {
        Self::render_iterinstr_levels(level, true, &prose_fallthrough, iter_instrs, render_body)
    }

    /// Opens one iteration block per level, attaching the fallthrough to the outermost one.
    fn render_iterinstr_levels(
        level: usize,
        outermost: bool,
        prose_fallthrough: &Prose,
        iter_instrs: &[pl::InstrIter],
        render_body: &dyn Fn(usize) -> Block,
    ) -> Block {
        // Finish recursion at the caller-supplied body
        let Some((iter_instr, iter_instrs_tail)) = iter_instrs.split_last() else {
            return render_body(level);
        };
        // Keep only outputs that appear in the rendered binding
        let vars_output: Vec<&pl::Var> = iter_instr
            .vars_bind
            .iter()
            .filter(|var| !var.id.node.starts_with('_'))
            .collect();
        // Render inner iteration levels before their enclosing open block
        let block_inner = Self::render_iterinstr_levels(
            level + 1,
            false,
            prose_fallthrough,
            iter_instrs_tail,
            render_body,
        );
        let prose_vars_bound = Prose::of_in_itervars(iter_instr.iter, &iter_instr.vars_bound);
        let prose_head = Prose::seq([Prose::text("For each "), prose_vars_bound, Prose::text(":")]);
        // Bind visible outputs after closing the iteration body
        let block_body = if vars_output.is_empty() {
            Block::concat([Block::raw("+\n--\n"), block_inner, Block::raw("\n--\n")])
        } else {
            let prose_vars_output = Prose::of_out_itervars(iter_instr.iter, &vars_output);
            let text_noun = string_of_iter(iter_instr.iter);
            let text_suffix = if vars_output.len() > 1 { "s" } else { "" };
            let prose_label = if outermost { prose_fallthrough.clone() } else { Prose::Empty };
            let prose_binding = Prose::seq([
                Prose::text("Let "),
                prose_vars_output,
                Prose::text(format!(" be the resulting {text_noun}{text_suffix}.")),
                prose_label,
            ]);
            Block::concat([
                Block::raw("+\n--\n"),
                block_inner,
                Block::raw("\n--\n+\n"),
                Block::inline(prose_binding),
            ])
        };
        Block::item(level, ItemKind::Ordered(None), prose_head, block_body)
    }

    // - If instructions
    //
    //   -- if $(n < m)   -> . Check that ``n`` is less than ``m``.

    /// Renders a conditional check and its continuation.
    fn render_if_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        if_instr: &pl::IfInstr<Tier>,
    ) -> Block {
        // Build the check heading with its failure continuation
        let prose_cond = Prose::of_exp(&if_instr.exp);
        let prose_suffix = Prose::of_iterexp_suffix(&if_instr.iter_exps);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head = Prose::seq([
            Prose::text("Check that "),
            prose_cond,
            prose_suffix,
            Prose::text("."),
            prose_fallthrough,
        ]);
        let block_head = Block::item_ordered(level, prose_head);
        self.render_continuation(level, block_head, ctx, render_tier, &if_instr.block)
    }

    // - Hold instructions
    //
    //   rule Even/cons: |- n_a :: n_b :: n*
    //     -- Even: |- n*
    //   -> . If xref:Even[``n^{asterisk}^`` has even length]:+++<sub class="bk-mark">[FAIL]</sub>+++ then, the relation holds.

    /// Builds the heading of a positive or negative relation holding branch.
    fn render_hold_head<Tier>(
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<Tier>,
        hold_instr: &pl::HoldInstr<Tier>,
        hold: bool,
    ) -> Block {
        let hint_opt = if hold { &instr.hints.prose_true } else { &instr.hints.prose_false };
        let link = Link::Subject(Subject::Relation(hold_instr.id.node.clone()));
        let prose_cond = match hint_opt {
            // Hinted relations describe the branch condition in prose
            Some(hint) => {
                let exps = hold_instr.not_exp.args();
                let prose_hint = alternate(
                    hint,
                    &|text_body| reindent_lines(0, text_body),
                    &|&exp| Prose::of_exp(exp),
                    &exps,
                    false,
                );
                Prose::link(link, prose_hint)
            }
            // Unhinted relations show their notation followed by the verdict
            None => {
                let code_not = Code::of_mixfix(&hold_instr.not_exp, &Code::of_exp);
                let prose_rel = Prose::link(link, Prose::code(code_not));
                let text_verdict = if hold { " holds" } else { " does not hold" };
                Prose::seq([prose_rel, Prose::text(text_verdict)])
            }
        };
        let prose_suffix = Prose::of_iterexp_suffix(&hold_instr.iter_exps);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head = Prose::seq([
            Prose::text("If "),
            prose_cond,
            prose_suffix,
            Prose::text(":"),
            prose_fallthrough,
        ]);
        Block::item_ordered(level, prose_head)
    }

    /// Renders positive, negative, or two-sided relation holding branches.
    fn render_hold_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        hold_instr: &pl::HoldInstr<Tier>,
    ) -> Block {
        // Render the selected one-sided or two-sided branch structure
        match &hold_instr.hold_case {
            pl::HoldCase::Hold(block, _) => {
                let block_head = Self::render_hold_head(level, ctx, instr, hold_instr, true);
                self.render_instrs(level + 1, Some(block_head), ctx, render_tier, block)
            }
            pl::HoldCase::NotHold(block, _) => {
                let block_head = Self::render_hold_head(level, ctx, instr, hold_instr, false);
                self.render_instrs(level + 1, Some(block_head), ctx, render_tier, block)
            }
            pl::HoldCase::Both(block_hold, block_not_hold) => {
                let block_head_hold = Self::render_hold_head(level, ctx, instr, hold_instr, true);
                let block_branch_hold = self.render_instrs(
                    level + 1,
                    Some(block_head_hold),
                    ctx,
                    render_tier,
                    block_hold,
                );
                let block_head_else = Block::item_ordered(level, Prose::text("Else:"));
                let block_branch_else = self.render_instrs(
                    level + 1,
                    Some(block_head_else),
                    ctx,
                    render_tier,
                    block_not_hold,
                );
                Block::seq([block_branch_hold, block_branch_else])
            }
        }
    }

    // - Case instructions
    //
    //   def $join_text(eps, t_sep) = ""
    //   def $join_text([ t_h ], t_sep) = t_h
    //   -> . If ``t'^{asterisk}^`` matches pattern ``[]``: return ``""``.
    //      . Else if let ``t~h~`` be ``t'^{asterisk}^``: return ``t~h~``.

    /// Renders a check or an if/else-if/else case ladder.
    fn render_case_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        case_instr: &pl::CaseInstr<Tier>,
    ) -> Block {
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // Render a single arm as a check without an if ladder
        if let [case] = case_instr.cases.as_slice() {
            let prose_guard = Prose::of_guard(&case_instr.exp, &case.guard);
            let prose_head = Prose::seq([
                Prose::text("Check that "),
                prose_guard,
                Prose::text("."),
                prose_fallthrough,
            ]);
            let block_head = Block::item_ordered(level, prose_head);
            return self.render_continuation(level, block_head, ctx, render_tier, &case.block);
        }

        let num_cases = case_instr.cases.len();
        let mut blocks_case = Vec::with_capacity(num_cases);
        for (idx, case) in case_instr.cases.iter().enumerate() {
            // Turn the final total arm into an otherwise branch
            if idx + 1 == num_cases && !case_instr.dangle {
                let block_else = Block::item_ordered(level, Prose::text("Else:"));
                let is_binding =
                    matches!(case.guard, pl::Guard::CheckLetSub(..) | pl::Guard::CheckLetMatch(..));
                let block_case = if is_binding {
                    // A binding guard becomes the first step of the otherwise branch
                    let prose_guard =
                        Prose::of_guard(&case_instr.exp, &case.guard).capitalize_first();
                    let prose_bind = Prose::seq([prose_guard, Prose::text(".")]);
                    let block_bind = Block::item_ordered(level + 1, prose_bind);
                    let blocks_rendered = case
                        .block
                        .iter()
                        .map(|instr| self.render_instr(level + 1, ctx, render_tier, instr));
                    Block::seq([block_else, block_bind].into_iter().chain(blocks_rendered))
                } else {
                    self.render_instrs(level + 1, Some(block_else), ctx, render_tier, &case.block)
                };
                blocks_case.push(block_case);
                continue;
            }

            // Attach fallthrough only where evaluating the condition can fail
            let can_fail = case.guard.has_call() || (idx == 0 && case_instr.exp.has_call());
            let prose_label = if can_fail { prose_fallthrough.clone() } else { Prose::Empty };
            let keyword = if idx == 0 { "If " } else { "Else if " };
            // Nest the selected case body below its condition
            let prose_guard = Prose::of_guard(&case_instr.exp, &case.guard);
            let prose_head =
                Prose::seq([Prose::text(keyword), prose_guard, Prose::text(":"), prose_label]);
            let block_head = Block::item_ordered(level, prose_head);
            let block_case =
                self.render_instrs(level + 1, Some(block_head), ctx, render_tier, &case.block);
            blocks_case.push(block_case);
        }
        Block::seq(blocks_case)
    }
}

impl Prose {
    // - Group dispatch links
    //
    //   Even/nil   -> goto xref:Even-nil[nil]

    fn of_group_dispatch(id_rel: &pl::Id, id_group: &pl::Id) -> Prose {
        let anchor_group = fallthrough::anchor_of_group(&id_rel.node, &id_group.node);
        let prose_group =
            Prose::link(Link::Direct(anchor_group), Prose::text(id_group.node.clone()));
        Prose::seq([Prose::text("goto "), prose_group])
    }
}

impl<'a> Renderer<'a> {
    // - Group dispatch instructions
    //
    //   lone Even/nil under a check   -> ... goto xref:Even-nil[nil]
    //   nested Sign/zero              -> . Goto xref:Sign-zero[zero]

    fn render_group_instr_dispatch(
        level: usize,
        singleton: bool,
        group_instr: &pl::RuleGroupInstr,
    ) -> Rendered {
        let prose_dispatch = Prose::of_group_dispatch(&group_instr.id_rel, &group_instr.id_group);
        // A lone dispatch folds onto its heading
        if singleton {
            return Rendered::InlineGoto(Prose::seq([Prose::text(" "), prose_dispatch]));
        }

        Rendered::Nested(Block::item_ordered(level, prose_dispatch.capitalize_first()))
    }

    // - Let instructions
    //
    //   -- if m = $(n + 1)            -> . Let ``m`` be ``n`` ``{plus}`` ``1``.
    //   -- if {LEFT n, RIGHT m} = p   -> . Let ``+{++LEFT+`` ``n,`` ``+RIGHT+`` ``m+}+`` be ``p``.

    /// Renders a binding and any instruction iterations around it.
    fn render_let_instr<Tier>(
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<Tier>,
        let_instr: &pl::LetInstr,
    ) -> Block {
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // Detect outputs that require explicit iteration blocks
        let has_output = let_instr
            .iter_instrs
            .iter()
            .flat_map(|iter_instr| &iter_instr.vars_bind)
            .any(|var| !var.id.node.starts_with('_'));
        // Keep output-free bindings inline with their iterator suffix
        if !has_output {
            let code_l = Code::of_exp(&let_instr.exp_l);
            let prose_r = Prose::of_exp(&let_instr.exp_r);
            let prose_suffix = Prose::of_iterinstr_suffix(&let_instr.iter_instrs);
            let prose_head = Prose::seq([
                Prose::text("Let "),
                Prose::code(code_l),
                Prose::text(" be "),
                prose_r,
                prose_suffix,
                Prose::text("."),
                prose_fallthrough,
            ]);
            return Block::item_ordered(level, prose_head);
        }

        // Nest output-producing bindings under their iteration scopes
        Self::render_iterinstrs(level, prose_fallthrough, &let_instr.iter_instrs, &|level| {
            let code_l = Code::of_exp(&let_instr.exp_l);
            let prose_r = Prose::of_exp(&let_instr.exp_r);
            let prose_head = Prose::seq([
                Prose::text("Let "),
                Prose::code(code_l),
                Prose::text(" be "),
                prose_r,
                Prose::text("."),
            ]);
            Block::item_unordered(level, prose_head)
        })
    }

    // - Rule instructions
    //
    //   -- Double: n_t* ~> m*   -> . Let xref:Double[``n~t~^{asterisk}^`` ``+~>+`` ``m^{asterisk}^``].

    /// Renders a relation application and its bound outputs.
    fn render_rule_instr(
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<pl::GroupInstr>,
        rule_instr: &pl::RuleInstr,
    ) -> Block {
        // Split the notation into input and output expressions
        let exps = rule_instr.not_exp.args();
        let (exps_input, exps_output) =
            input::split(&rule_instr.input_hint, exps).expect("validated rule input hint");
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // Detect outputs collected by an enclosing iteration
        let has_output = rule_instr
            .iter_instrs
            .iter()
            .flat_map(|iter_instr| &iter_instr.vars_bind)
            .any(|var| !var.id.node.starts_with('_'));
        // Apply paired relation hints when both sides are available
        let link = Link::Subject(Subject::Relation(rule_instr.id.node.clone()));
        let prose_rule = if let (Some(hint_input), Some(hint_output)) =
            (&instr.hints.prose_in, &instr.hints.prose_out)
        {
            let prose_output = alternate(
                hint_output,
                &unindent_lines,
                &|&exp| Prose::of_exp(exp),
                &exps_output,
                false,
            );
            let text_output = serialize::ser_prose_in_link(&prose_output);
            let prose_input = alternate(
                hint_input,
                &unindent_lines,
                &|&exp| Prose::of_exp(exp),
                &exps_input,
                false,
            );
            Prose::seq([
                Prose::text("Let "),
                Prose::text(text_output),
                Prose::text(" be the result of "),
                Prose::link(link, prose_input),
            ])
        } else {
            let code_not = Code::of_mixfix(&rule_instr.not_exp, &Code::of_exp);
            Prose::seq([Prose::text("Let "), Prose::link(link, Prose::code(code_not))])
        };
        // Wrap bindings that produce iterated outputs in open blocks
        if !has_output {
            let prose_suffix = Prose::of_iterinstr_suffix(&rule_instr.iter_instrs);
            let prose_head =
                Prose::seq([prose_rule, prose_suffix, Prose::text("."), prose_fallthrough]);
            return Block::item_ordered(level, prose_head);
        }

        Self::render_iterinstrs(level, prose_fallthrough, &rule_instr.iter_instrs, &|level| {
            let prose_head = Prose::seq([prose_rule.clone(), Prose::text(".")]);
            Block::item_unordered(level, prose_head)
        })
    }
}

impl Prose {
    // - Results
    //
    //   n* ~> true   -> the result is ``true``.
    //   |- eps       -> then, the relation holds.

    /// Describes a relation result according to its output shape and hints.
    fn of_result(hints: &Hints, signature: &pl::RelSignature, exps: &[pl::Exp]) -> Prose {
        let typs = signature.not_typ.node.args();
        let is_conditional = input::is_conditional(&signature.input_hint, &typs)
            .expect("validated relation input hint");
        if is_conditional {
            Prose::text("then, the relation holds.")
        } else if let Some(hint) = &hints.prose_out {
            let prose_output = alternate(
                hint,
                &|text_body| reindent_lines(0, text_body),
                &Prose::of_exp,
                exps,
                false,
            );
            Prose::seq([Prose::text("the result is "), prose_output, Prose::text(".")])
        } else if exps.is_empty() {
            Prose::text("the relation holds.")
        } else {
            let prose_exps = Prose::of_exps(exps);
            Prose::seq([Prose::text("the result is "), prose_exps, Prose::text(".")])
        }
    }
}

impl<'a> Renderer<'a> {
    // - Result instructions
    //
    //   rule Even/nil: |- eps
    //   -> xref:Even[``·`` has even length]:
    //       then, the relation holds.
    //
    //   rule Double/cons: n_h :: n_t* ~> n_h :: n_h :: m*
    //   -> . The result is ``n~h~`` ``{two-colons}`` ``n~h~`` ``{two-colons}`` ``m^{asterisk}^``.

    fn render_result_instr(
        level: usize,
        ctx: &Context,
        singleton: bool,
        instr: &pl::Instr<pl::GroupInstr>,
        result_instr: &pl::ResultInstr,
    ) -> Rendered {
        let prose_result =
            Prose::of_result(&instr.hints, &result_instr.rel_signature, &result_instr.exps_output);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // A short lone result folds onto its heading
        if singleton && prose_result.width() <= ADOC_WIDTH_SHORT {
            let prose_tail = Prose::seq([Prose::text(" "), prose_result, prose_fallthrough]);
            return Rendered::Inline(prose_tail);
        }

        let prose_head = Prose::seq([prose_result.capitalize_first(), prose_fallthrough]);
        Rendered::Nested(Block::item_ordered(level, prose_head))
    }

    // - Return instructions
    //
    //   def $e_bool(n) = true                 -> xref:e_bool[$e_bool(n)]
    //
    //                                             return ``true``.
    //   def $i_if(n, m) = n  -- if $(n < m)   -> . Return ``n``.

    fn render_return_instr(
        level: usize,
        ctx: &Context,
        singleton: bool,
        instr: &pl::Instr<pl::GroupInstr>,
        return_instr: &pl::ReturnInstr,
    ) -> Rendered {
        let prose_exp = Prose::of_exp(&return_instr.exp);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // A short lone return folds onto its heading
        if singleton && prose_exp.width() <= ADOC_WIDTH_SHORT {
            let prose_tail = Prose::seq([
                Prose::text(" return "),
                prose_exp,
                Prose::text("."),
                prose_fallthrough,
            ]);
            return Rendered::Inline(prose_tail);
        }

        let prose_head =
            Prose::seq([Prose::text("Return "), prose_exp, Prose::text("."), prose_fallthrough]);
        Rendered::Nested(Block::item_ordered(level, prose_head))
    }

    // - Debug instructions
    //
    //   -- debug n   -> . (debug: ``n``)

    fn render_debug_instr<Tier>(
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<Tier>,
        debug_instr: &pl::DebugInstr,
    ) -> Block {
        let prose_exp = Prose::of_exp(&debug_instr.exp);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head =
            Prose::seq([Prose::text("(debug: "), prose_exp, Prose::text(")"), prose_fallthrough]);
        Block::item_ordered(level, prose_head)
    }

    // - Destruct instructions
    //
    //   one named projection   -> . Let ``expressionIR`` be the expression of ``typedExpressionIR``.

    /// Renders named destructuring projections.
    fn render_destruct_instr<Tier>(
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<Tier>,
        destruct_instr: &pl::DestructInstr,
    ) -> Block {
        // Discard unnamed projections from the prose binding
        let projections: Vec<(&String, &pl::Exp)> = destruct_instr
            .bindings
            .iter()
            .filter_map(|(name, exp)| name.as_ref().map(|name| (name, exp)))
            .collect();
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        // Use dedicated singular prose for one named projection
        if let [(name, exp_target)] = projections.as_slice() {
            let prose_target = Prose::of_exp(exp_target);
            let prose_source = Prose::of_exp(&destruct_instr.exp);
            let prose_head = Prose::seq([
                Prose::text("Let "),
                prose_target,
                Prose::text(format!(" be the {name} of ")),
                prose_source,
                Prose::text("."),
                prose_fallthrough,
            ]);
            return Block::item_ordered(level, prose_head);
        }

        // Pair multiple targets with their projection names
        let proses_target = projections
            .iter()
            .map(|(_, exp)| Prose::of_exp(exp))
            .collect();
        let prose_targets = Prose::of_list(proses_target);
        let proses_name = projections
            .iter()
            .map(|(name, _)| Prose::text(format!("the {name}")))
            .collect();
        let prose_names = Prose::of_list(proses_name);
        let prose_source = Prose::of_exp(&destruct_instr.exp);
        let prose_head = Prose::seq([
            Prose::text("Let "),
            prose_targets,
            Prose::text(" be "),
            prose_names,
            Prose::text(" of "),
            prose_source,
            Prose::text("."),
            prose_fallthrough,
        ]);
        Block::item_ordered(level, prose_head)
    }

    // - Check-let instructions
    //
    //   def $join_text(t_h1 :: t_h2 :: t_t*, t_sep) = ...
    //   -> .. Let!~type~ ``t~h2~`` ``{two-colons}`` ``t~t~^{asterisk}^`` be ``t^{asterisk}^``.+++<sub class="bk-mark">[FAIL]</sub>+++

    /// Renders a partial subtype binding.
    fn render_check_let_sub_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        check_instr: &pl::CheckLetSubInstr<Tier>,
    ) -> Block {
        let pl::CheckLetSubInstr { exp_l, exp_r, block, .. } = check_instr;
        // Build the partial binding heading with its failure continuation
        let code_l = Code::of_exp(exp_l);
        let prose_r = Prose::of_exp(exp_r);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head = Prose::seq([
            Prose::text("Let!~type~ "),
            Prose::code(code_l),
            Prose::text(" be "),
            prose_r,
            Prose::text("."),
            prose_fallthrough,
        ]);
        let block_head = Block::item_ordered(level, prose_head);
        self.render_continuation(level, block_head, ctx, render_tier, block)
    }

    /// Renders a partial pattern binding.
    fn render_check_let_match_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        check_instr: &pl::CheckLetMatchInstr<Tier>,
    ) -> Block {
        let pl::CheckLetMatchInstr { exp_l, exp_r, block, .. } = check_instr;
        // Build the partial binding heading with its failure continuation
        let code_l = Code::of_exp(exp_l);
        let prose_r = Prose::of_exp(exp_r);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head = Prose::seq([
            Prose::text("Let!~type~ "),
            Prose::code(code_l),
            Prose::text(" be "),
            prose_r,
            Prose::text("."),
            prose_fallthrough,
        ]);
        let block_head = Block::item_ordered(level, prose_head);
        self.render_continuation(level, block_head, ctx, render_tier, block)
    }

    // - Option-get instructions
    //
    //   -- if n_result = $modulo(n, 42)
    //   -> . Let ``n~result~`` be xref:option_get[*!*] xref:modulo[``n`` mod ``42``].+++<sub class="bk-mark">[FAIL]</sub>+++

    /// Renders a forced option binding.
    fn render_option_get_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        option_instr: &pl::OptionGetInstr<Tier>,
    ) -> Block {
        // Build the forced binding heading with its failure continuation
        let code_l = Code::of_exp(&option_instr.exp_l);
        let link_get = Link::Direct("option_get".to_owned());
        let prose_get = Prose::link(link_get, Prose::text("*!*"));
        let prose_r = Prose::of_exp(&option_instr.exp_r);
        let prose_fallthrough = Prose::of_fallthrough_link(ctx, instr);
        let prose_head = Prose::seq([
            Prose::text("Let "),
            Prose::code(code_l),
            Prose::text(" be "),
            prose_get,
            Prose::text(" "),
            prose_r,
            Prose::text("."),
            prose_fallthrough,
        ]);
        let block_head = Block::item_ordered(level, prose_head);
        self.render_continuation(level, block_head, ctx, render_tier, &option_instr.block)
    }

    // - Tier instructions
    //
    //   return n   -> . Return ``n``.

    fn render_tier_instr<Tier>(
        &mut self,
        level: usize,
        ctx: &Context,
        render_tier: RenderTier<'a, Tier>,
        instr: &pl::Instr<Tier>,
        tier_instr: &pl::TierInstr<Tier>,
    ) -> Block {
        let rendered = render_tier(self, level, ctx, false, instr, &tier_instr.tier);
        Self::compose(None, false, rendered)
    }
}

// == Relations

impl<'a> Renderer<'a> {
    // - Synthesized outputs
    //
    //   SL Iter(Id(m), *)   -> PL Iter(Id(m), *) with the same note and span

    /// Lifts a synthesized SL output expression into an unhinted PL expression.
    fn lift_synthesized_exp(exp_sl: &sl::ast::Exp) -> pl::Exp {
        let exp_kind = match &exp_sl.node {
            sl::ast::ExpKind::Id(id) => ExpKind::Id(id.clone()),
            sl::ast::ExpKind::Iter(exp_inner_sl, iter_exp) => {
                let exp_inner = Self::lift_synthesized_exp(exp_inner_sl);
                ExpKind::Iter(Box::new(exp_inner), iter_exp.clone())
            }
            _ => panic!("relation title outputs are synthesized variables"),
        };
        crate::annotated_note_phrase! {
            node: exp_kind,
            note: exp_sl.note.as_ref().clone(),
            span: exp_sl.span.clone(),
        }
    }
}

impl Prose {
    // - Relation notation titles
    //
    //   Double: nat* ~> nat*, input %0   -> ``nat^{asterisk}^`` ``+~>+`` ``%``

    /// Fills relation inputs and leaves output positions as percent holes.
    fn of_rel_title_math(signature: &pl::RelSignature, exps: &[pl::Exp]) -> Prose {
        let mixop = signature.not_typ.node.to_mixop();
        let num_outputs = mixop.arity() - exps.len();
        let codes_input: Vec<Code> = exps.iter().map(Code::of_exp).collect();
        let codes_output: Vec<Code> = (0..num_outputs).map(|_| Code::token("%")).collect();
        let codes_args = input::combine(&signature.input_hint, codes_input, codes_output)
            .expect("validated relation input hint");
        let not_exp =
            pl::Mixop::fill(&mixop, codes_args).expect("relation title fills its notation");
        let code_not = Code::of_mixfix(&not_exp, &Clone::clone);
        Prose::code(code_not)
    }
}

impl<'a> Renderer<'a> {
    // - Relation titles
    //
    //   relation Even: |- nat*, hinted prose_true
    //   -> xref:Even[Even]:
    //
    //      * ``nat'^{asterisk}^`` has even length
    //
    //   relation Double: nat* ~> nat*, unhinted
    //   -> xref:Double[Double: ``nat^{asterisk}^`` ``+~>+`` ``%``]

    /// Builds a relation title from input, output, truth, or notation prose.
    fn render_rel_title_block(
        hints: &Hints,
        id_rel: &pl::Id,
        signature: &pl::RelSignature,
        exps: &[pl::Exp],
    ) -> Block {
        // Prefer synthesized inputs when prosification changed title bindings
        let exps_synthesized = hints.prose_input_exps.as_ref().map(|exps| {
            exps.iter()
                .map(Self::lift_synthesized_exp)
                .collect::<Vec<_>>()
        });
        let exps_input = exps_synthesized.as_deref().unwrap_or(exps);
        // Build the shared linked heading before selecting the title form
        let link = Link::Subject(Subject::Relation(id_rel.node.clone()));
        let prose_name = Prose::link(link.clone(), Prose::text(id_rel.node.clone()));
        let prose_header = Prose::seq([prose_name, Prose::text(":")]);
        let block_header = Block::concat([Block::inline(prose_header), Block::raw("\n\n")]);
        // Select paired, input-only, truth, or notation prose
        match (&hints.prose_in, &hints.prose_out, &hints.prose_output_exps, &hints.prose_true) {
            // Reject incomplete paired output hints
            (Some(_), Some(_), None, _) => panic!("prose_out title requires synthesized outputs"),
            (Some(hint_input), Some(hint_output), Some(exps_output_sl), _) => {
                // Render paired input and synthesized output prose
                let exps_output: Vec<pl::Exp> = exps_output_sl
                    .iter()
                    .map(Self::lift_synthesized_exp)
                    .collect();
                let prose_input = alternate(
                    hint_input,
                    &|text_body| reindent_lines(1, text_body),
                    &Prose::of_exp,
                    exps_input,
                    true,
                );
                let prose_output = alternate(
                    hint_output,
                    &|text_body| reindent_lines(1, text_body),
                    &Prose::of_exp,
                    &exps_output,
                    false,
                );
                let prose_result = Prose::seq([Prose::text("The result is "), prose_output]);
                Block::concat([
                    block_header,
                    Block::item_unordered(0, prose_input),
                    Block::raw(":\n"),
                    Block::item_unordered(0, prose_result),
                    Block::raw("."),
                ])
            }
            // Render input prose without a synthesized result
            (Some(hint_input), _, _, _) => {
                let prose_input = alternate(
                    hint_input,
                    &|text_body| reindent_lines(1, text_body),
                    &Prose::of_exp,
                    exps_input,
                    true,
                );
                Block::concat([
                    block_header,
                    Block::item_unordered(0, prose_input),
                    Block::raw("."),
                ])
            }
            // Render a direct truth description
            (_, _, _, Some(hint_true)) => {
                let prose_true = alternate(
                    hint_true,
                    &|text_body| reindent_lines(0, text_body),
                    &Prose::of_exp,
                    exps,
                    true,
                );
                Block::concat([block_header, Block::item_unordered(0, prose_true)])
            }
            // Fall back to the filled relation notation
            _ => {
                let prose_math = Prose::of_rel_title_math(signature, exps);
                let prose_title =
                    Prose::seq([Prose::text(format!("{}: ", id_rel.node)), prose_math]);
                Block::inline(Prose::link(link, prose_title))
            }
        }
    }
}

// == External relations

impl<'a> Renderer<'a> {
    // - External relation definition
    //
    //   extern relation Oracle: nat ~> nat   -> xref:Oracle[Oracle: ``nat`` ``+~>+`` ``%``]

    fn render_extern_rel_def(hints: &Hints, rel: &pl::ExternRel) -> Block {
        Self::render_rel_title_block(hints, &rel.id, &rel.rel_signature, &rel.exps_input)
    }
}

// == Tier renderers

impl<'a> Renderer<'a> {
    // - Backtracking arms
    //
    //   def $modulo(n_a, n_b) = $(n_a \ n_b)
    //     -- if n_b =/= 0
    //   def $modulo(n_a, n_b) = eps
    //     -- if n_b = 0
    //   -> . +++<span class="bk-arm-anchor" id="bk-modulo-1-arm-1"></span>+++Try:
    //       .. Check that ``n~b~`` is not equal to ``0``.
    //       .. Return ``n~a~`` ``\`` ``n~b~``.
    //      . +++<span class="bk-arm-anchor" id="bk-modulo-1-arm-2"></span>+++Then, try:
    //       .. Check that ``n~b~`` is equal to ``0``.
    //       .. Return ``·``.

    /// Renders backtracking arms with derived next-arm targets.
    fn render_block_arms<Arm>(
        &mut self,
        level: usize,
        ctx: &Context,
        arms: &[Arm],
        render_arm: &dyn Fn(&mut Renderer<'a>, &Context, &Arm) -> Block,
    ) -> Block {
        // Allocate one shared namespace for every arm target
        let anchor_block = self.anchors.fresh_block(&ctx.namespace);
        let num_arms = arms.len();
        let mut blocks_arm = Vec::with_capacity(num_arms);
        for (idx, arm) in arms.iter().enumerate() {
            // Point each arm at its successor or the enclosing destination
            let anchor_next_opt = if idx + 1 < num_arms {
                Some(fallthrough::anchor_of_arm(&anchor_block, idx + 1))
            } else {
                ctx.next.clone()
            };
            let ctx_arm = Context { namespace: ctx.namespace.clone(), next: anchor_next_opt };
            // Anchor the arm before rendering its body with the derived context
            let anchor_arm = fallthrough::anchor_of_arm(&anchor_block, idx);
            let text_head = if idx == 0 { "Try:" } else { "Then, try:" };
            let block_body = render_arm(self, &ctx_arm, arm);
            let block_arm = Block::item(
                level,
                ItemKind::Ordered(Some(anchor_arm)),
                Prose::text(text_head),
                block_body,
            );
            blocks_arm.push(block_arm);
        }
        Block::seq(blocks_arm)
    }

    // - Group-body tier
    //
    //   return n           -> . Return ``n``.
    //   then n* ~> false   -> the result is ``false``.

    /// Renders a group-tier instruction.
    fn render_instr_group(
        &mut self,
        level: usize,
        ctx: &Context,
        singleton: bool,
        instr: &pl::Instr<pl::GroupInstr>,
        tier: &pl::GroupInstr,
    ) -> Rendered {
        match tier {
            pl::GroupInstr::Return(return_instr) => {
                Self::render_return_instr(level, ctx, singleton, instr, return_instr)
            }
            pl::GroupInstr::Result(result_instr) => {
                Self::render_result_instr(level, ctx, singleton, instr, result_instr)
            }
            pl::GroupInstr::Rule(rule_instr) => {
                Rendered::Nested(Self::render_rule_instr(level, ctx, instr, rule_instr))
            }
            pl::GroupInstr::Backtrack(backtrack_instr) => {
                self.render_backtrack_instr(level, ctx, backtrack_instr)
            }
        }
    }

    // - Backtrack instructions
    //
    //   two $modulo clauses
    //   -> . +++<span class="bk-arm-anchor" id="bk-modulo-1-arm-1"></span>+++Try:
    //       .. ...
    //      . +++<span class="bk-arm-anchor" id="bk-modulo-1-arm-2"></span>+++Then, try:
    //       .. ...

    fn render_backtrack_instr(
        &mut self,
        level: usize,
        ctx: &Context,
        backtrack_instr: &pl::BacktrackInstr,
    ) -> Rendered {
        let level_body = level + 1;
        let block_arms = self.render_block_arms(
            level,
            ctx,
            &backtrack_instr.blocks,
            &|renderer, ctx_arm, arm| {
                let blocks_rendered = arm.iter().map(|instr| {
                    renderer.render_instr(level_body, ctx_arm, Self::render_instr_group, instr)
                });
                Block::seq(blocks_rendered)
            },
        );
        Rendered::Nested(block_arms)
    }

    // - Dispatch tier
    //
    //   Even dispatch, first group
    //   -> . If ``nat'^{asterisk}^`` matches pattern ``[]``: goto xref:Even-nil[nil]
    //
    //   Sign dispatch, last group
    //   -> . Goto xref:Sign-zero[zero]

    /// Renders a dispatch-tier instruction.
    fn render_instr_dispatch(
        &mut self,
        level: usize,
        ctx: &Context,
        singleton: bool,
        _instr: &pl::Instr<pl::DispatchInstr>,
        tier: &pl::DispatchInstr,
    ) -> Rendered {
        match tier {
            pl::DispatchInstr::Group(group_instr) => {
                Self::render_group_instr_dispatch(level, singleton, group_instr)
            }
            pl::DispatchInstr::Route(route_instr) => {
                self.render_route_instr(level, ctx, route_instr)
            }
        }
    }

    // - Route instructions
    //
    //   Parity/even and Parity/odd
    //   -> . +++<span class="bk-arm-anchor" id="bk-Parity-1-arm-1"></span>+++Try:
    //       .. Goto xref:Parity-even[even]
    //      . +++<span class="bk-arm-anchor" id="bk-Parity-1-arm-2"></span>+++Then, try:
    //       .. Goto xref:Parity-odd[odd]

    fn render_route_instr(
        &mut self,
        level: usize,
        ctx: &Context,
        route_instr: &pl::RouteInstr,
    ) -> Rendered {
        let level_body = level + 1;
        let block_arms =
            self.render_block_arms(level, ctx, &route_instr.blocks, &|renderer, ctx_arm, arm| {
                let blocks_rendered = arm.iter().map(|instr| {
                    renderer.render_instr(level_body, ctx_arm, Self::render_instr_dispatch, instr)
                });
                Block::seq(blocks_rendered)
            });
        Rendered::Nested(block_arms)
    }

    // - Inline dispatch tier
    //
    //   rule Sign/other: i ~> "nonzero"  -- otherwise
    //   -> .. xref:Sign[``i`` ``+~>+`` ``%``]: the result is ``"nonzero"``.

    /// Renders an otherwise dispatch group with its body inline.
    fn render_instr_dispatch_inline(
        &mut self,
        level: usize,
        ctx: &Context,
        singleton: bool,
        instr: &pl::Instr<pl::DispatchInstr>,
        tier: &pl::DispatchInstr,
    ) -> Rendered {
        match tier {
            pl::DispatchInstr::Group(group_instr) => {
                self.render_group_instr_inline(level, ctx, instr, group_instr)
            }
            pl::DispatchInstr::Route(_) => {
                self.render_instr_dispatch(level, ctx, singleton, instr, tier)
            }
        }
    }

    // - Inline group instructions
    //
    //   rule Sign/other: i ~> "nonzero"  -- otherwise
    //   -> .. xref:Sign[``i`` ``+~>+`` ``%``]: the result is ``"nonzero"``.

    fn render_group_instr_inline(
        &mut self,
        level: usize,
        ctx: &Context,
        instr: &pl::Instr<pl::DispatchInstr>,
        group_instr: &pl::RuleGroupInstr,
    ) -> Rendered {
        // Select hinted prose or filled relation notation for the title
        let hint_opt = instr
            .hints
            .prose_in
            .as_ref()
            .or(instr.hints.prose_true.as_ref());
        let prose_body = match hint_opt {
            Some(hint) => alternate(
                hint,
                &|text_body| reindent_lines(0, text_body),
                &Prose::of_exp,
                &group_instr.exps_input,
                true,
            ),
            None => Prose::of_rel_title_math(&group_instr.rel_signature, &group_instr.exps_input),
        };
        let link = Link::Subject(Subject::Relation(group_instr.id_rel.node.clone()));
        let prose_title = Prose::link(link, prose_body);
        // Render the group body below its linked title
        let block_head = Block::item_ordered(level, Prose::seq([prose_title, Prose::text(":")]));
        let block_group = self.render_instrs(
            level + 1,
            Some(block_head),
            ctx,
            Self::render_instr_group,
            &group_instr.block,
        );
        Rendered::Nested(block_group)
    }
}

// == Defined relations

impl<'a> Renderer<'a> {
    // - Rule group fragments
    //
    //   rule Even/nil: |- eps   -> xref:Even[``·`` has even length]:
    //                               then, the relation holds.

    /// Renders a rule group, reserving arm anchors within this document.
    pub fn render_rulegroup(
        &mut self,
        hints: &Hints,
        id_rel: &pl::Id,
        signature: &pl::RelSignature,
        exps: &[pl::Exp],
        block: &pl::GroupBlock,
    ) -> String {
        // Select hinted prose or filled relation notation for the title
        let hint_opt = hints.prose_in.as_ref().or(hints.prose_true.as_ref());
        let prose_body = match hint_opt {
            Some(hint) => alternate(
                hint,
                &|text_body| reindent_lines(0, text_body),
                &Prose::of_exp,
                exps,
                true,
            ),
            None => Prose::of_rel_title_math(signature, exps),
        };
        let link = Link::Subject(Subject::Relation(id_rel.node.clone()));
        let prose_title = Prose::link(link, prose_body);
        // Render the body with counters shared by the enclosing document
        let ctx = Context::new(&id_rel.node);
        let block_body = self.render_instrs(0, None, &ctx, Self::render_instr_group, block);
        // Serialize the linked title and body as one fragment
        let text_title = serialize::ser_prose(self.anchor, &prose_title);
        let text_body = serialize::ser_block(self.anchor, &block_body);
        format!("{text_title}:\n{text_body}")
    }

    // - Otherwise fragments
    //
    //   rule Sign/other: i ~> "nonzero"  -- otherwise
    //   -> . +++<span id="Sign-else"></span>+++Otherwise:
    //       .. xref:Sign[``i`` ``+~>+`` ``%``]: the result is ``"nonzero"``.

    /// Renders an otherwise fragment with counters shared by other fragments.
    pub fn render_rulegroup_else(&mut self, id_rel: &pl::Id, block: &pl::DispatchBlock) -> String {
        let ctx = Context::new(&id_rel.node);
        let anchor_else = fallthrough::anchor_of_else(&id_rel.node);
        let text_else = self.render_elseblock(
            Some(&anchor_else),
            &ctx,
            Self::render_instr_dispatch_inline,
            Some(block),
        );
        text_else.trim().to_owned()
    }

    // - Dispatch fragments
    //
    //   relation Sign   -> Sign dispatch:
    //
    //                      . Check that ``i`` is equal to ``0``.
    //                      . Goto xref:Sign-zero[zero]

    /// Renders relation dispatch with counters shared by other fragments.
    pub fn render_defined_rel_def_dispatch(&mut self, rel: &pl::DefinedRel) -> String {
        let ctx = Context::new(&rel.id.node);
        let block_dispatch =
            self.render_instrs(0, None, &ctx, Self::render_instr_dispatch, &rel.block);
        let text_dispatch = serialize::ser_block(self.anchor, &block_dispatch);
        format!("{} dispatch:\n{text_dispatch}", rel.id.node)
    }

    // - Defined relation definition
    //
    //   relation Sign: int ~> text
    //   rule Sign/zero: 0 ~> "zero"
    //   rule Sign/other: i ~> "nonzero"
    //     -- otherwise
    //   -> xref:Sign[Sign: ``i`` ``+~>+`` ``%``]
    //
    //      xref:Sign[``0`` ``+~>+`` ``%``]:
    //       the result is ``"zero"``.
    //
    //      . +++<span id="Sign-else"></span>+++Otherwise:
    //       .. xref:Sign[``i`` ``+~>+`` ``%``]: the result is ``"nonzero"``.
    //
    //      Sign dispatch:
    //
    //      . Check that ``i`` is equal to ``0``.
    //      . Goto xref:Sign-zero[zero]

    /// Builds a complete relation block with source-compatible counter order.
    fn render_defined_rel_def(&mut self, hints: &Hints, rel: &pl::DefinedRel) -> Block {
        // Reserve an otherwise anchor only for a visible block
        let has_else = rel
            .block_else_opt
            .as_ref()
            .is_some_and(|block| !block.is_empty());
        let anchor_else = has_else.then(|| fallthrough::anchor_of_else(&rel.id.node));
        // Allocate counter-bearing fragments in OCaml right-to-left evaluation order
        let text_dispatch = self.render_defined_rel_def_dispatch(rel);
        let ctx = Context::new(&rel.id.node);
        let text_else = self.render_elseblock(
            anchor_else.as_deref(),
            &ctx,
            Self::render_instr_dispatch_inline,
            rel.block_else_opt.as_deref(),
        );
        // Extract each rule group from the dispatch tree in document order
        let rule_groups = rule_group::collect_rule_groups(&rel.block);
        let mut texts_group = Vec::with_capacity(rule_groups.len());
        for rule_group in rule_groups {
            let text_group = self.render_rulegroup(
                rule_group.hints,
                rule_group.id_rel,
                rule_group.rel_signature,
                rule_group.exps_input,
                rule_group.block,
            );
            texts_group.push(text_group);
        }
        let text_groups = texts_group.join("\n\n");
        // Assemble fragments in their displayed order
        let block_title =
            Self::render_rel_title_block(hints, &rel.id, &rel.rel_signature, &rel.exps_input);
        Block::concat([
            block_title,
            Block::raw("\n\n"),
            Block::raw(text_groups),
            Block::raw(text_else),
            Block::raw(format!("\n\n{text_dispatch}")),
        ])
    }
}

// == Function definitions

impl<'a> Renderer<'a> {
    // - Function headers
    //
    //   dec $modulo(nat, nat) : nat?, hinted %0 "mod" %1   -> xref:modulo[``n~a~`` mod ``n~b~``]
    //   dec $i_if(nat, nat) : nat, unhinted                -> xref:i_if[$i_if(n, m)]

    /// Builds the linked inline header used before a body or table.
    fn render_func_header_block(
        hints: &Hints,
        id_func: &pl::Id,
        tparams: &[pl::TParam],
        params: &[pl::Param],
    ) -> Block {
        let hint_opt = hints.prose_in.as_ref().or(hints.prose_true.as_ref());
        // Keep nested links visible to the final serializer
        let prose_body = match hint_opt {
            Some(hint) => alternate(
                hint,
                &|text_body| reindent_lines(0, text_body),
                &Prose::of_param,
                params,
                true,
            ),
            // Preserve plain signature text without pre-serializing its code
            None => {
                let text_id = string_of_defid(id_func);
                let text_tparams = if tparams.is_empty() {
                    String::new()
                } else {
                    let texts_tparam: Vec<String> = tparams.iter().map(Print::to_string).collect();
                    let text_tparams = texts_tparam.join(", ");
                    format!("<{text_tparams}>")
                };
                let code_params = Code::of_params(params);
                let code_signature =
                    Code::seq([Code::token(text_id), Code::token(text_tparams), code_params]);
                Prose::PlainCode(code_signature)
            }
        };
        let link = Link::Subject(Subject::Function(id_func.node.clone()));
        Block::inline(Prose::link(link, prose_body))
    }

    // - External function definition
    //
    //   extern $check(x), hinted "checking" %0   -> xref:check[Checking value ``x``]

    fn render_extern_func_def(hints: &Hints, func: &pl::ExternFunc) -> Block {
        Self::render_func_header_block(hints, &func.id, &func.tparams, &func.params)
    }

    // - Builtin function definition
    //
    //   builtin dec $sum_nat(nat*) : nat   -> xref:sum_nat[Sum``+`(+`` ``nat^{asterisk}^`` ``+`)+``]

    fn render_builtin_func_def(hints: &Hints, func: &pl::BuiltinFunc) -> Block {
        Self::render_func_header_block(hints, &func.id, &func.tparams, &func.params)
    }

    // - Table function definition
    //
    //   tbl def $is_defaultable_typeIR =
    //     | BOOL => true
    //     | ERROR => true
    //   -> xref:is_defaultable_typeIR[``typeIR''`` can be default-initialized]:
    //      [cols="2", options="header"]
    //      |===
    //      | (``typeIR''``) | Result
    //
    //      | +BOOL+ | true
    //      | +ERROR+ | true
    //      ...

    /// Builds a table function with argument and result columns.
    fn render_table_func_def(hints: &Hints, func: &pl::TableFunc) -> Block {
        // Retain code links until the enclosing document resolves its anchors
        let rows_table = func
            .rows
            .iter()
            .map(|row| vec![Code::of_exps(&row.exps_input, ", "), Code::of_exp(&row.exp)])
            .collect();
        // Assemble the linked header and table with one result column
        let block_header = Self::render_func_header_block(hints, &func.id, &[], &func.params);
        let prose_params = Prose::of_params(&func.params);
        let block_table = Block::Table(Table {
            header: vec![prose_params, Prose::text("Result")],
            rows: rows_table,
        });
        Block::concat([block_header, Block::raw(":\n"), block_table])
    }

    // - Defined function definition
    //
    //   def $c_sign(0) = "zero"
    //   def $c_sign(i) = "pos"
    //     -- if $(i > 0)
    //   def $c_sign(i) = "neg"
    //     -- otherwise
    //   -> xref:c_sign[$c_sign(i)]
    //
    //      . +++<span class="bk-arm-anchor" id="bk-c_sign-1-arm-1"></span>+++Try:
    //       .. Check that ``i`` is equal to ``0``.
    //       .. Return ``"zero"``.
    //      . +++<span class="bk-arm-anchor" id="bk-c_sign-1-arm-2"></span>+++Then, try:
    //       .. Check that ``i`` is greater than ``0``.
    //       .. Return ``"pos"``.
    //
    //      . +++<span id="c_sign-else"></span>+++Otherwise: return ``"neg"``.

    /// Builds a function body and its optional otherwise clause.
    fn render_defined_func_def(&mut self, hints: &Hints, func: &pl::DefinedFunc) -> Block {
        // Reserve an otherwise anchor only for a visible block
        let has_else = func
            .block_else_opt
            .as_ref()
            .is_some_and(|block| !block.is_empty());
        let ctx = Context::new(&func.id.node);
        // Choose the compact boolean or general body form
        let block_body;
        let anchor_else;
        match func.block.as_slice() {
            [instr]
                if let pl::InstrKind::Tier(tier_instr) = &instr.node.node
                    && let pl::GroupInstr::Return(return_instr) = &tier_instr.tier
                    && matches!(return_instr.exp.node.node, ExpKind::Bool(_)) =>
            {
                // Render a lone boolean return as inline prose
                let code_exp = Code::of_exp(&return_instr.exp);
                let prose_tail =
                    Prose::seq([Prose::text(" return "), Prose::code(code_exp), Prose::text(".")]);
                block_body = Block::inline(prose_tail);
                anchor_else = None;
            }
            _ => {
                // Render the general instruction sequence with an optional target
                let blocks_rendered: Vec<Block> = func
                    .block
                    .iter()
                    .map(|instr| self.render_instr(0, &ctx, Self::render_instr_group, instr))
                    .collect();
                block_body = Block::seq(blocks_rendered);
                anchor_else = has_else.then(|| fallthrough::anchor_of_else(&func.id.node));
            }
        }
        // Append the otherwise clause after the selected body form
        let block_header =
            Self::render_func_header_block(hints, &func.id, &func.tparams, &func.params);
        let text_else = self.render_elseblock(
            anchor_else.as_deref(),
            &ctx,
            Self::render_instr_group,
            func.block_else_opt.as_deref(),
        );
        Block::concat([block_header, Block::raw("\n\n"), block_body, Block::raw(text_else)])
    }
}

// == Definitions

impl<'a> Renderer<'a> {
    // - Definition
    //
    //   syntax rec = {LEFT nat, RIGHT nat}   -> None
    //   extern relation Oracle: nat ~> nat   -> Some("xref:Oracle[Oracle: ``nat`` ``+~>+`` ``%``]")

    /// Renders a definition with counters shared by other document fragments.
    pub fn render_def(&mut self, def: &pl::Def) -> Option<String> {
        let block = match &def.node.node {
            pl::DefKind::Typ(_) | pl::DefKind::Var(_) => return None,
            pl::DefKind::Rel(pl::RelDef::Extern(rel)) => {
                Self::render_extern_rel_def(&def.hints, rel)
            }
            pl::DefKind::Rel(pl::RelDef::Defined(rel)) => {
                self.render_defined_rel_def(&def.hints, rel)
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Extern(func)) => {
                Self::render_extern_func_def(&def.hints, func)
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Builtin(func)) => {
                Self::render_builtin_func_def(&def.hints, func)
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(func)) => {
                Self::render_table_func_def(&def.hints, func)
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(func)) => {
                self.render_defined_func_def(&def.hints, func)
            }
        };
        Some(serialize::ser_block(self.anchor, &block))
    }

    /// Renders definitions in order with this document's anchors and counters.
    pub fn render_defs(&mut self, defs: &[pl::Def]) -> String {
        let texts_def: Vec<String> = defs.iter().filter_map(|def| self.render_def(def)).collect();
        texts_def.join("\n\n")
    }
}

// == Entry point
//
//   render_spec([Oracle, Sign])   -> the two definitions joined by a blank line

/// Renders one definition, omitting type and variable declarations.
pub fn render_def(anchor: &dyn Fn(&Subject) -> Option<String>, def: &pl::Def) -> Option<String> {
    Renderer::new(anchor).render_def(def)
}

/// Renders a complete prose specification with definition-name anchors.
pub fn render_spec(spec: &pl::Spec) -> String {
    Renderer::new(&serialize::subject_name).render_defs(spec)
}
