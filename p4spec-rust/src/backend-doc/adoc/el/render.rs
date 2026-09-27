//! AsciiDoc source text for elaboration-language definitions
//!
//! ```text
//! var x : nat                       -> var x : nat
//! relation Eval: nat '++' nat       -> relation Eval:
//!                                        nat '++' nat
//! syntax value = _EMPTY | '+' nat   -> value
//!                                          : /* empty */ | + nat
//!                                          ;
//! ```

use crate::{
    lang::{
        common::{Iter, notation::atom::Atom as AtomKind, prim::num},
        el::ast::*,
        traits::print::Print,
    },
    util::text::escape_text,
};

use super::doc::doc::Doc;

// == Documents
//
//   '+' in AtomMode::Source    -> '+'
//   '+' in AtomMode::Display   -> +

/// Column limit for layout groups.
const WIDTH: usize = 80;

/// Selects atom spellings for executable syntax or displayed notation.
#[derive(Clone, Copy)]
enum AtomMode {
    /// Preserves source delimiters, such as quotes around operators.
    Source,
    /// Shows notation glyphs and renders the EMPTY tag as an empty comment.
    Display,
}

// == Iterators
//
//   Opt    -> ?
//   List   -> *

impl Doc {
    fn of_iter(iter: Iter) -> Doc {
        let text = match iter {
            Iter::Opt => "?",
            Iter::List => "*",
        };
        Doc::text(text)
    }
}

// == Identifiers
//
//   Doc::of_varid(x)            -> x
//   Doc::of_defid(f)            -> $f
//   Doc::of_rule_suffix(step)   -> /step
//   Doc::of_rule_suffix("")     -> (empty)

impl Doc {
    fn of_varid(id_var: &Id) -> Doc {
        Doc::text(id_var.node.clone())
    }

    fn of_typid(id_typ: &Id) -> Doc {
        Doc::text(id_typ.node.clone())
    }

    fn of_relid(id_rel: &Id) -> Doc {
        Doc::text(id_rel.node.clone())
    }

    fn of_defid(id_def: &Id) -> Doc {
        Doc::text(format!("${}", id_def.node))
    }

    fn of_tparam(tparam: &TParam) -> Doc {
        Doc::text(tparam.node.clone())
    }

    fn of_rule_suffix(id_suffix: &Id) -> Doc {
        if id_suffix.node.is_empty() {
            Doc::Empty
        } else {
            Doc::text(format!("/{}", id_suffix.node))
        }
    }
}

// == Atoms
//
//   '+' in Source       -> '+'
//   '+' in Display      -> +
//   _EMPTY in Display   -> /* empty */
//   _Some in Display    -> _Some
//   `{ in Display       -> {

fn string_of_display_atom(atom: &AtomKind) -> String {
    match atom {
        AtomKind::Keyword(id) => id.clone(),
        AtomKind::Tag(id) if id == "EMPTY" => "/* empty */".to_owned(),
        AtomKind::Tag(id) => format!("_{id}"),
        AtomKind::Operator(op) => op.clone(),
        AtomKind::Sub => "<:".to_owned(),
        AtomKind::Sup => ":>".to_owned(),
        AtomKind::Turnstile => "|-".to_owned(),
        AtomKind::Tilesturn => "-|".to_owned(),
        AtomKind::Arrow => "->".to_owned(),
        AtomKind::ArrowSub => "->_".to_owned(),
        AtomKind::DoubleArrowSub => "=>_".to_owned(),
        AtomKind::DoubleArrowLong => "==>".to_owned(),
        AtomKind::SqArrow => "~>".to_owned(),
        AtomKind::SqArrowStar => "~>*".to_owned(),
        AtomKind::Dot => ".".to_owned(),
        AtomKind::Dot2 => "..".to_owned(),
        AtomKind::Dot3 => "...".to_owned(),
        AtomKind::Semicolon => ";".to_owned(),
        AtomKind::Colon => ":".to_owned(),
        AtomKind::ColonEq => ":=".to_owned(),
        AtomKind::Tilde2 => "~~".to_owned(),
        AtomKind::Backslash => "\\".to_owned(),
        AtomKind::LAngle => "<".to_owned(),
        AtomKind::RAngle => ">".to_owned(),
        AtomKind::LParen => "(".to_owned(),
        AtomKind::RParen => ")".to_owned(),
        AtomKind::LBrack => "[".to_owned(),
        AtomKind::RBrack => "]".to_owned(),
        AtomKind::LBrace => "{".to_owned(),
        AtomKind::RBrace => "}".to_owned(),
    }
}

fn string_of_atom(atom_mode: AtomMode, atom: &Atom) -> String {
    match atom_mode {
        AtomMode::Source => Print::to_string(&atom.node),
        AtomMode::Display => string_of_display_atom(&atom.node),
    }
}

impl Doc {
    fn of_atom(atom_mode: AtomMode, atom: &Atom) -> Doc {
        let text = string_of_atom(atom_mode, atom);
        Doc::text(text)
    }
}

// == Lists

impl Doc {
    // - Comma lists
    //
    //   [x, y]              -> [x, y]
    //   []                  -> []
    //   [x, y] at width 4   -> [
    //                            x,
    //                            y]

    fn of_comma_list<T>(
        indent: usize,
        open: &str,
        close: &str,
        doc_of_item: impl Fn(&T) -> Doc,
        items: &[T],
    ) -> Doc {
        // Empty lists retain their delimiters without a break
        if items.is_empty() {
            return Doc::text(format!("{open}{close}"));
        }

        let separator = Doc::concat([Doc::text(","), Doc::break_(" ")]);
        let doc_items = Doc::join(separator, items.iter().map(doc_of_item));
        let doc_body = Doc::nest(indent, Doc::concat([Doc::break_(""), doc_items]));
        Doc::group(Doc::concat([Doc::text(open), doc_body, Doc::text(close)]))
    }

    // - Optional comma lists
    //
    //   list<nat>   -> list<nat>
    //   list        -> list

    fn of_optional_comma_list<T>(
        indent: usize,
        open: &str,
        close: &str,
        doc_of_item: impl Fn(&T) -> Doc,
        items: &[T],
    ) -> Doc {
        if items.is_empty() {
            Doc::Empty
        } else {
            Doc::of_comma_list(indent, open, close, doc_of_item, items)
        }
    }
}

// == Operators

impl Doc {
    // - Brackets
    //
    //   `( x `)              -> `( x `)
    //   `( x `) at width 3   -> `(
    //                             x
    //                           `)

    fn of_bracket(atom_mode: AtomMode, doc_body: Doc, atom_l: &Atom, atom_r: &Atom) -> Doc {
        let doc_l = Doc::of_atom(atom_mode, atom_l);
        let doc_r = Doc::of_atom(atom_mode, atom_r);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(" "), doc_body]));
        Doc::group(Doc::concat([doc_l, doc_body, Doc::break_(" "), doc_r]))
    }

    // - Infix operators
    //
    //   xs ++ ys              -> xs ++ ys
    //   xs ++ ys at width 4   -> xs
    //                                ++ ys

    fn of_infix(doc_l: Doc, op: impl Into<String>, doc_r: Doc) -> Doc {
        let doc_op = Doc::text(op);
        let doc_tail = Doc::concat([Doc::break_(" "), doc_op, Doc::text(" "), doc_r]);
        Doc::group(Doc::concat([doc_l, Doc::nest(4, doc_tail)]))
    }

    // - Operator spellings
    //
    //   ~b        -> ~
    //   b /\ c    -> /\
    //   x =/= y   -> =/=

    fn of_unop(op: &UnOp) -> Doc {
        let text = match op {
            UnOp::Bool(op) => Print::to_string(op),
            UnOp::Num(op) => Print::to_string(op),
        };
        Doc::text(text)
    }
}

fn string_of_binop(op: &BinOp) -> String {
    match op {
        BinOp::Bool(op) => Print::to_string(op),
        BinOp::Num(op) => Print::to_string(op),
    }
}

fn string_of_cmpop(op: &CmpOp) -> String {
    match op {
        CmpOp::Bool(op) => Print::to_string(op),
        CmpOp::Num(op) => Print::to_string(op),
    }
}

// == Types

impl Doc {
    // - Type
    //
    //   nat*         -> nat*
    //   p |- e : t   -> p |- e : t

    fn of_typ(atom_mode: AtomMode, typ: &Typ) -> Doc {
        match typ {
            Typ::Plain(plain_typ) => Doc::of_plaintyp(atom_mode, plain_typ),
            Typ::Notation(not_typ) => Doc::of_nottyp(atom_mode, not_typ),
        }
    }

    // - Plain types
    //
    //   list<nat>     -> list<nat>
    //   (nat, text)   -> (nat, text)

    fn of_plaintyp(atom_mode: AtomMode, plain_typ: &PlainTyp) -> Doc {
        match &plain_typ.node {
            PlainTypKind::Bool => Doc::of_bool_typ(),
            PlainTypKind::Num(num_typ) => Doc::of_num_typ(num_typ),
            PlainTypKind::Text => Doc::of_text_typ(),
            PlainTypKind::Var(id_typ, targs) => Doc::of_var_typ(atom_mode, id_typ, targs),
            PlainTypKind::Paren(plain_typ) => Doc::of_paren_typ(atom_mode, plain_typ),
            PlainTypKind::Tuple(plain_typs) => Doc::of_tuple_typ(atom_mode, plain_typs),
            PlainTypKind::Iter(plain_typ, iter) => Doc::of_iter_typ(atom_mode, plain_typ, *iter),
        }
    }

    // - Boolean types
    //
    //   bool   -> bool

    fn of_bool_typ() -> Doc {
        Doc::text("bool")
    }

    // - Number types
    //
    //   nat   -> nat
    //   int   -> int

    fn of_num_typ(num_typ: &num::Typ) -> Doc {
        Doc::text(Print::to_string(num_typ))
    }

    // - Text types
    //
    //   text   -> text

    fn of_text_typ() -> Doc {
        Doc::text("text")
    }

    // - Type applications
    //
    //   list<nat>                    -> list<nat>
    //   list<nat, text> at width 4   -> list<
    //                                     nat,
    //                                     text>

    fn of_var_typ(atom_mode: AtomMode, id_typ: &Id, targs: &[Targ]) -> Doc {
        let doc_id = Doc::of_typid(id_typ);
        let doc_targs = Doc::of_targs(atom_mode, targs);
        Doc::concat([doc_id, doc_targs])
    }

    // - Parenthesized types
    //
    //   (nat)   -> (nat)

    fn of_paren_typ(atom_mode: AtomMode, plain_typ: &PlainTyp) -> Doc {
        let plain_typs = std::slice::from_ref(plain_typ);
        Doc::of_comma_list(
            2,
            "(",
            ")",
            |plain_typ| Doc::of_plaintyp(atom_mode, plain_typ),
            plain_typs,
        )
    }

    // - Tuple types
    //
    //   (nat, text)              -> (nat, text)
    //   (nat, text) at width 4   -> (
    //                                 nat,
    //                                 text)

    fn of_tuple_typ(atom_mode: AtomMode, plain_typs: &[PlainTyp]) -> Doc {
        Doc::of_comma_list(
            2,
            "(",
            ")",
            |plain_typ| Doc::of_plaintyp(atom_mode, plain_typ),
            plain_typs,
        )
    }

    // - Iterated types
    //
    //   nat*   -> nat*
    //   nat?   -> nat?

    fn of_iter_typ(atom_mode: AtomMode, plain_typ: &PlainTyp, iter: Iter) -> Doc {
        let doc_base = Doc::of_plaintyp(atom_mode, plain_typ);
        let doc_iter = Doc::of_iter(iter);
        Doc::concat([doc_base, doc_iter])
    }

    // - Notation types
    //
    //   nat '++' nat   -> nat '++' nat
    //   p |- e : t     -> p |- e : t
    //   `{ nat `}      -> `{ nat `}

    fn of_nottyp(atom_mode: AtomMode, not_typ: &NotTyp) -> Doc {
        match &not_typ.node {
            NotTypKind::Atom(atom) => Doc::of_atom(atom_mode, atom),
            NotTypKind::Seq(typs) => Doc::of_seq_typ(atom_mode, typs),
            NotTypKind::Infix(typ_l, atom, typ_r) => {
                Doc::of_infix_typ(atom_mode, typ_l, atom, typ_r)
            }
            NotTypKind::Brack(atom_l, typ, atom_r) => {
                Doc::of_brack_typ(atom_mode, atom_l, typ, atom_r)
            }
        }
    }

    // - Sequence types
    //
    //   _Some nat   -> _Some nat

    fn of_seq_typ(atom_mode: AtomMode, typs: &[Typ]) -> Doc {
        Doc::flow(typs.iter().map(|typ| Doc::of_typ(atom_mode, typ)))
    }

    // - Infix types
    //
    //   p |- e                   -> p |- e
    //   nat -> text in Display   -> nat -> text

    fn of_infix_typ(atom_mode: AtomMode, typ_l: &Typ, atom: &Atom, typ_r: &Typ) -> Doc {
        let doc_l = Doc::of_typ(atom_mode, typ_l);
        let op = string_of_atom(atom_mode, atom);
        let doc_r = Doc::of_typ(atom_mode, typ_r);
        Doc::of_infix(doc_l, op, doc_r)
    }

    // - Bracketed types
    //
    //   `{ nat `}              -> `{ nat `}
    //   `{ nat `} in Display   -> { nat }

    fn of_brack_typ(atom_mode: AtomMode, atom_l: &Atom, typ: &Typ, atom_r: &Atom) -> Doc {
        let doc_body = Doc::of_typ(atom_mode, typ);
        Doc::of_bracket(atom_mode, doc_body, atom_l, atom_r)
    }

    // - Type arguments
    //
    //   <nat>    -> <nat>
    //   (none)   -> (empty)

    fn of_targ(atom_mode: AtomMode, targ: &Targ) -> Doc {
        Doc::of_plaintyp(atom_mode, targ)
    }

    fn of_targs(atom_mode: AtomMode, targs: &[Targ]) -> Doc {
        Doc::of_optional_comma_list(2, "<", ">", |targ| Doc::of_targ(atom_mode, targ), targs)
    }

    // - Struct fields and variant cases
    //
    //   VALUE nat     -> VALUE nat
    //   | _Some nat   -> _Some nat

    fn of_typfield(atom_mode: AtomMode, typ_field: &TypField) -> Doc {
        let doc_atom = Doc::of_atom(atom_mode, &typ_field.atom);
        let doc_typ = Doc::of_plaintyp(atom_mode, &typ_field.typ);
        Doc::group(Doc::concat([doc_atom, Doc::text(" "), doc_typ]))
    }

    fn of_typcase(atom_mode: AtomMode, typ_case: &TypCase) -> Doc {
        let doc_typ = Doc::of_typ(atom_mode, &typ_case.typ);
        Doc::nest(4, doc_typ)
    }

    // - Definition types
    //
    //   syntax t<A> = list<A>    -> t<A> = list<A>
    //   syntax t = {VALUE nat}   -> t = {
    //                                 VALUE nat
    //                               }
    //   syntax t = nat | text    -> t
    //                                   : nat | text
    //                                   ;

    fn of_deftyp(atom_mode: AtomMode, def_typ: &DefTyp) -> Doc {
        match &def_typ.node {
            DefTypKind::Plain(plain_typ) => Doc::of_alias_typ(atom_mode, plain_typ),
            DefTypKind::Struct(typ_fields) => Doc::of_struct_typ(atom_mode, typ_fields),
            DefTypKind::Variant(typ_cases) => Doc::of_variant_typ(atom_mode, typ_cases),
        }
    }

    // - Alias types
    //
    //   syntax t<A> = list<A>   -> t<A> = list<A>

    fn of_alias_typ(atom_mode: AtomMode, plain_typ: &PlainTyp) -> Doc {
        let doc_typ = Doc::of_plaintyp(atom_mode, plain_typ);
        Doc::concat([Doc::text(" = "), doc_typ])
    }

    // - Struct types
    //
    //   syntax t = {VALUE nat, NEXT text}   -> t = {
    //                                            VALUE nat,
    //                                            NEXT text
    //                                          }

    fn of_struct_typ(atom_mode: AtomMode, typ_fields: &[TypField]) -> Doc {
        let separator = Doc::concat([Doc::text(","), Doc::Line]);
        let docs_field = typ_fields
            .iter()
            .map(|typ_field| Doc::of_typfield(atom_mode, typ_field));
        let doc_fields = Doc::join(separator, docs_field);
        let doc_body = Doc::nest(2, Doc::concat([Doc::Line, doc_fields]));
        Doc::concat([Doc::text(" = {"), doc_body, Doc::Line, Doc::text("}")])
    }

    // - Variant types
    //
    //   syntax t = nat | text         -> t
    //                                        : nat | text
    //                                        ;
    //   syntax t = _EMPTY | '+' nat   -> t
    //                                        : /* empty */ | + nat
    //                                        ;

    fn of_variant_typ(atom_mode: AtomMode, typ_cases: &[TypCase]) -> Doc {
        let mut docs_case = typ_cases
            .iter()
            .map(|typ_case| Doc::of_typcase(atom_mode, typ_case));
        let mut docs = vec![Doc::Line, Doc::text(": ")];
        docs.extend(docs_case.next());
        for doc_case in docs_case {
            let doc_alternative = Doc::concat([Doc::break_(" "), Doc::text("| "), doc_case]);
            docs.push(Doc::group(doc_alternative));
        }
        docs.extend([Doc::Line, Doc::text(";")]);
        Doc::nest(4, Doc::concat(docs))
    }
}

// == Expressions

impl Doc {
    // - Expression
    //
    //   $f<nat>(x)   -> $f<nat>(x)
    //   xs[i : n]    -> xs[i : n]

    fn of_exp(atom_mode: AtomMode, exp: &Exp) -> Doc {
        match &exp.node {
            ExpKind::Bool(value) => Doc::of_bool_exp(*value),
            ExpKind::Num(op, num) => Doc::of_num_exp(*op, num),
            ExpKind::Text(text_value) => Doc::of_text_exp(text_value),
            ExpKind::Id(id_var) => Doc::of_varid(id_var),
            ExpKind::Un(op, exp_inner) => Doc::of_un_exp(atom_mode, op, exp_inner),
            ExpKind::Bin(exp_l, op, exp_r) => Doc::of_bin_exp(atom_mode, exp_l, op, exp_r),
            ExpKind::Cmp(exp_l, op, exp_r) => Doc::of_cmp_exp(atom_mode, exp_l, op, exp_r),
            ExpKind::Arith(exp_inner) => Doc::of_arith_exp(atom_mode, exp_inner),
            ExpKind::Eps => Doc::of_eps_exp(),
            ExpKind::List(exps) => Doc::of_list_exp(atom_mode, exps),
            ExpKind::Cons(exp_l, exp_r) => Doc::of_cons_exp(atom_mode, exp_l, exp_r),
            ExpKind::Cat(exp_l, exp_r) => Doc::of_cat_exp(atom_mode, exp_l, exp_r),
            ExpKind::Idx(exp_base, exp_idx) => Doc::of_idx_exp(atom_mode, exp_base, exp_idx),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                Doc::of_slice_exp(atom_mode, exp_base, exp_idx, exp_len)
            }
            ExpKind::Len(exp_inner) => Doc::of_len_exp(atom_mode, exp_inner),
            ExpKind::Mem(exp_l, exp_r) => Doc::of_mem_exp(atom_mode, exp_l, exp_r),
            ExpKind::Str(fields) => Doc::of_str_exp(atom_mode, fields),
            ExpKind::Dot(exp_base, atom) => Doc::of_dot_exp(atom_mode, exp_base, atom),
            ExpKind::Upd(exp_base, path, exp_field) => {
                Doc::of_upd_exp(atom_mode, exp_base, path, exp_field)
            }
            ExpKind::Paren(exp_inner) => Doc::of_paren_exp(atom_mode, exp_inner),
            ExpKind::Tuple(exps) => Doc::of_tuple_exp(atom_mode, exps),
            ExpKind::Call(id_def, targs, args) => Doc::of_call_exp(atom_mode, id_def, targs, args),
            ExpKind::Iter(exp_inner, iter) => Doc::of_iter_exp(atom_mode, exp_inner, *iter),
            ExpKind::Sub(exp_inner, plain_typ) => Doc::of_sub_exp(atom_mode, exp_inner, plain_typ),
            ExpKind::Atom(atom) => Doc::of_atom(atom_mode, atom),
            ExpKind::Seq(exps) => Doc::of_seq_exp(atom_mode, exps),
            ExpKind::Infix(exp_l, atom, exp_r) => Doc::of_infix_exp(atom_mode, exp_l, atom, exp_r),
            ExpKind::Brack(atom_l, exp_inner, atom_r) => {
                Doc::of_brack_exp(atom_mode, atom_l, exp_inner, atom_r)
            }
            ExpKind::Hole(hole) => Doc::of_hole_exp(hole),
            ExpKind::Fuse(exp_l, _, exp_r) => Doc::of_fuse_exp(atom_mode, exp_l, exp_r),
            ExpKind::Unparen(exp_inner) => Doc::of_unparen_exp(atom_mode, exp_inner),
            ExpKind::Latex(text_value) => Doc::of_latex_exp(text_value),
        }
    }

    // - Boolean expressions
    //
    //   true   -> true

    fn of_bool_exp(value: bool) -> Doc {
        Doc::text(value.to_string())
    }

    // - Numeric expressions
    //
    //   255    -> 255
    //   0xFF   -> 0xFF
    //   -3     -> -3

    fn of_num_exp(op: NumOp, num: &Num) -> Doc {
        match (op, num) {
            (NumOp::Dec, num::Number::Nat(nat)) => Doc::text(nat.to_string()),
            (NumOp::Hex, num::Number::Nat(nat)) => {
                let digits = nat.as_bigint().to_str_radix(16).to_uppercase();
                Doc::text(format!("0x{digits}"))
            }
            (_, num) => Doc::text(Print::to_string(num)),
        }
    }

    // - Text expressions
    //
    //   "a\nb"   -> "a\nb"

    fn of_text_exp(text_value: &str) -> Doc {
        let text_escaped = escape_text(text_value);
        Doc::text(format!("\"{text_escaped}\""))
    }

    // - Unary expressions
    //
    //   ~b      -> ~b
    //   $(-x)   -> $(-x)

    fn of_un_exp(atom_mode: AtomMode, op: &UnOp, exp_inner: &Exp) -> Doc {
        let doc_op = Doc::of_unop(op);
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        Doc::concat([doc_op, doc_inner])
    }

    // - Binary expressions
    //
    //   b /\ c              -> b /\ c
    //   b /\ c at width 4   -> b
    //                              /\ c

    fn of_bin_exp(atom_mode: AtomMode, exp_l: &Exp, op: &BinOp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, string_of_binop(op), doc_r)
    }

    // - Comparison expressions
    //
    //   x = y     -> x = y
    //   x =/= y   -> x =/= y

    fn of_cmp_exp(atom_mode: AtomMode, exp_l: &Exp, op: &CmpOp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, string_of_cmpop(op), doc_r)
    }

    // - Arithmetic expressions
    //
    //   $(x + 1)              -> $(x + 1)
    //   $(x + 1) at width 4   -> $(
    //                              x
    //                                  + 1)

    fn of_arith_exp(atom_mode: AtomMode, exp_inner: &Exp) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(""), doc_inner]));
        Doc::group(Doc::concat([Doc::text("$("), doc_body, Doc::text(")")]))
    }

    // - Empty-sequence expressions
    //
    //   eps   -> eps

    fn of_eps_exp() -> Doc {
        Doc::text("eps")
    }

    // - List expressions
    //
    //   [x, y]   -> [x, y]
    //   []       -> []

    fn of_list_exp(atom_mode: AtomMode, exps: &[Exp]) -> Doc {
        Doc::of_comma_list(2, "[", "]", |exp| Doc::of_exp(atom_mode, exp), exps)
    }

    // - Cons expressions
    //
    //   x :: xs   -> x :: xs

    fn of_cons_exp(atom_mode: AtomMode, exp_l: &Exp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, "::", doc_r)
    }

    // - Concatenation expressions
    //
    //   xs ++ ys   -> xs ++ ys

    fn of_cat_exp(atom_mode: AtomMode, exp_l: &Exp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, "++", doc_r)
    }

    // - Index expressions
    //
    //   xs[i]              -> xs[i]
    //   xs[i] at width 3   -> xs[
    //                           i]

    fn of_idx_exp(atom_mode: AtomMode, exp_base: &Exp, exp_idx: &Exp) -> Doc {
        let doc_base = Doc::of_exp(atom_mode, exp_base);
        let doc_idx = Doc::of_exp(atom_mode, exp_idx);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(""), doc_idx]));
        let doc_suffix = Doc::group(Doc::concat([Doc::text("["), doc_body, Doc::text("]")]));
        Doc::concat([doc_base, doc_suffix])
    }

    // - Slice expressions
    //
    //   xs[i : n]              -> xs[i : n]
    //   xs[i : n] at width 4   -> xs[
    //                               i
    //                                   : n]

    fn of_slice_exp(atom_mode: AtomMode, exp_base: &Exp, exp_idx: &Exp, exp_len: &Exp) -> Doc {
        let doc_base = Doc::of_exp(atom_mode, exp_base);
        let doc_idx = Doc::of_exp(atom_mode, exp_idx);
        let doc_len = Doc::of_exp(atom_mode, exp_len);
        let doc_range = Doc::of_infix(doc_idx, ":", doc_len);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(""), doc_range]));
        let doc_suffix = Doc::group(Doc::concat([Doc::text("["), doc_body, Doc::text("]")]));
        Doc::concat([doc_base, doc_suffix])
    }

    // - Length expressions
    //
    //   |xs|   -> |xs|

    fn of_len_exp(atom_mode: AtomMode, exp_inner: &Exp) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        Doc::concat([Doc::text("|"), doc_inner, Doc::text("|")])
    }

    // - Membership expressions
    //
    //   x <- xs   -> x <- xs

    fn of_mem_exp(atom_mode: AtomMode, exp_l: &Exp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, "<-", doc_r)
    }

    // - Struct expressions
    //
    //   {LEFT x, RIGHT y}              -> {LEFT x, RIGHT y}
    //   {}                             -> {}
    //   {LEFT x, RIGHT y} at width 8   -> {
    //                                       LEFT x,
    //                                       RIGHT y
    //                                     }

    fn of_str_exp(atom_mode: AtomMode, fields: &[(Atom, Exp)]) -> Doc {
        // Empty structs retain adjacent braces
        if fields.is_empty() {
            return Doc::text("{}");
        }

        let separator = Doc::concat([Doc::text(","), Doc::break_(" ")]);
        let docs_field = fields.iter().map(|(atom, exp_field)| {
            let doc_atom = Doc::of_atom(atom_mode, atom);
            let doc_field = Doc::of_exp(atom_mode, exp_field);
            Doc::concat([doc_atom, Doc::text(" "), doc_field])
        });
        let doc_fields = Doc::join(separator, docs_field);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(""), doc_fields]));
        Doc::group(Doc::concat([Doc::text("{"), doc_body, Doc::break_(""), Doc::text("}")]))
    }

    // - Field-access expressions
    //
    //   p.LEFT   -> p.LEFT

    fn of_dot_exp(atom_mode: AtomMode, exp_base: &Exp, atom: &Atom) -> Doc {
        let doc_base = Doc::of_exp(atom_mode, exp_base);
        let doc_atom = Doc::of_atom(atom_mode, atom);
        Doc::concat([doc_base, Doc::text("."), doc_atom])
    }

    // - Update expressions
    //
    //   p[.LEFT = x]   -> p[LEFT = x]
    //   p[.A[i] = x]   -> p[A[i] = x]

    fn of_upd_exp(atom_mode: AtomMode, exp_base: &Exp, path: &Path, exp_field: &Exp) -> Doc {
        let doc_base = Doc::of_exp(atom_mode, exp_base);
        let doc_path = Doc::of_path(atom_mode, path);
        let doc_field = Doc::of_exp(atom_mode, exp_field);
        let doc_body =
            Doc::nest(2, Doc::concat([Doc::break_(""), doc_path, Doc::text(" = "), doc_field]));
        let doc_suffix = Doc::group(Doc::concat([Doc::text("["), doc_body, Doc::text("]")]));
        Doc::concat([doc_base, doc_suffix])
    }

    // - Parenthesized expressions
    //
    //   (x)   -> (x)

    fn of_paren_exp(atom_mode: AtomMode, exp_inner: &Exp) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        let doc_body = Doc::nest(2, Doc::concat([Doc::break_(""), doc_inner]));
        Doc::group(Doc::concat([Doc::text("("), doc_body, Doc::text(")")]))
    }

    // - Tuple expressions
    //
    //   (x, y)   -> (x, y)

    fn of_tuple_exp(atom_mode: AtomMode, exps: &[Exp]) -> Doc {
        Doc::of_comma_list(2, "(", ")", |exp| Doc::of_exp(atom_mode, exp), exps)
    }

    // - Function calls
    //
    //   $f<nat>(x)            -> $f<nat>(x)
    //   $f()                  -> $f()
    //   $f(x, y) at width 4   -> $f(
    //                                x,
    //                                y)

    fn of_call_exp(atom_mode: AtomMode, id_def: &Id, targs: &[Targ], args: &[Arg]) -> Doc {
        let doc_id = Doc::of_defid(id_def);
        let doc_targs = Doc::of_targs(atom_mode, targs);
        let doc_args = Doc::of_args(atom_mode, args);
        Doc::group(Doc::concat([doc_id, doc_targs, doc_args]))
    }

    // - Iterated expressions
    //
    //   x*   -> x*
    //   x?   -> x?

    fn of_iter_exp(atom_mode: AtomMode, exp_inner: &Exp, iter: Iter) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        let doc_iter = Doc::of_iter(iter);
        Doc::concat([doc_inner, doc_iter])
    }

    // - Subtype expressions
    //
    //   x <: nat   -> x <: nat

    fn of_sub_exp(atom_mode: AtomMode, exp_inner: &Exp, plain_typ: &PlainTyp) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        let doc_typ = Doc::of_plaintyp(atom_mode, plain_typ);
        Doc::of_infix(doc_inner, "<:", doc_typ)
    }

    // - Expression sequences
    //
    //   A x y              -> A x y
    //   A x y at width 3   -> A x
    //                         y

    fn of_seq_exp(atom_mode: AtomMode, exps: &[Exp]) -> Doc {
        Doc::flow(exps.iter().map(|exp| Doc::of_exp(atom_mode, exp)))
    }

    // - Infix expressions
    //
    //   x |- y   -> x |- y
    //   x -> y   -> x -> y

    fn of_infix_exp(atom_mode: AtomMode, exp_l: &Exp, atom: &Atom, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let op = string_of_atom(atom_mode, atom);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::of_infix(doc_l, op, doc_r)
    }

    // - Bracketed expressions
    //
    //   `( x `)   -> `( x `)

    fn of_brack_exp(atom_mode: AtomMode, atom_l: &Atom, exp_inner: &Exp, atom_r: &Atom) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        Doc::of_bracket(atom_mode, doc_inner, atom_l, atom_r)
    }

    // - Holes
    //
    //   %    -> %
    //   %2   -> %2
    //   %%   -> %%
    //   !%   -> !%

    fn of_hole_exp(hole: &Hole) -> Doc {
        match hole {
            Hole::Num(num) => Doc::text(format!("%{num}")),
            Hole::Next => Doc::text("%"),
            Hole::Rest => Doc::text("%%"),
            Hole::None => Doc::text("!%"),
        }
    }

    // - Fused expressions
    //
    //   x#y   -> x#y

    fn of_fuse_exp(atom_mode: AtomMode, exp_l: &Exp, exp_r: &Exp) -> Doc {
        let doc_l = Doc::of_exp(atom_mode, exp_l);
        let doc_r = Doc::of_exp(atom_mode, exp_r);
        Doc::concat([doc_l, Doc::text("#"), doc_r])
    }

    // - Unparenthesized expressions
    //
    //   ##x   -> ##x

    fn of_unparen_exp(atom_mode: AtomMode, exp_inner: &Exp) -> Doc {
        let doc_inner = Doc::of_exp(atom_mode, exp_inner);
        Doc::concat([Doc::text("##"), doc_inner])
    }

    // - LaTeX expressions
    //
    //   Latex("a\nb")   -> latex("a\nb")

    fn of_latex_exp(text_value: &str) -> Doc {
        let text_escaped = escape_text(text_value);
        Doc::text(format!("latex(\"{text_escaped}\")"))
    }
}

// == Paths

impl Doc {
    // - Path
    //
    //   p[.A.B = x]        -> A.B
    //   p[.A[i : n] = x]   -> A[i : n]

    fn of_path(atom_mode: AtomMode, path: &Path) -> Doc {
        match &path.node {
            PathKind::Root => Doc::of_root_path(),
            PathKind::Idx(path_base, exp_idx) => Doc::of_idx_path(atom_mode, path_base, exp_idx),
            PathKind::Slice(path_base, exp_idx, exp_len) => {
                Doc::of_slice_path(atom_mode, path_base, exp_idx, exp_len)
            }
            PathKind::Dot(path_base, atom) => Doc::of_dot_path(atom_mode, path_base, atom),
        }
    }

    // - Root paths
    //
    //   (root)       -> (empty)
    //   p[[i] = x]   -> p[[i] = x]

    fn of_root_path() -> Doc {
        Doc::Empty
    }

    // - Index paths
    //
    //   p[.A[i] = x]   -> A[i]

    fn of_idx_path(atom_mode: AtomMode, path_base: &Path, exp_idx: &Exp) -> Doc {
        let doc_base = Doc::of_path(atom_mode, path_base);
        let doc_idx = Doc::of_exp(atom_mode, exp_idx);
        Doc::concat([doc_base, Doc::text("["), doc_idx, Doc::text("]")])
    }

    // - Slice paths
    //
    //   p[.A[i : n] = x]   -> A[i : n]

    fn of_slice_path(atom_mode: AtomMode, path_base: &Path, exp_idx: &Exp, exp_len: &Exp) -> Doc {
        let doc_base = Doc::of_path(atom_mode, path_base);
        let doc_idx = Doc::of_exp(atom_mode, exp_idx);
        let doc_len = Doc::of_exp(atom_mode, exp_len);
        Doc::concat([doc_base, Doc::text("["), doc_idx, Doc::text(" : "), doc_len, Doc::text("]")])
    }

    // - Field paths
    //
    //   p[.LEFT = x]   -> LEFT
    //   p[.A.B = x]    -> A.B

    fn of_dot_path(atom_mode: AtomMode, path_base: &Path, atom: &Atom) -> Doc {
        // The first field in an update path has no leading dot
        if matches!(path_base.node, PathKind::Root) {
            return Doc::of_atom(atom_mode, atom);
        }

        let doc_base = Doc::of_path(atom_mode, path_base);
        let doc_atom = Doc::of_atom(atom_mode, atom);
        Doc::concat([doc_base, Doc::text("."), doc_atom])
    }
}

// == Arguments

impl Doc {
    // - Argument
    //
    //   x        -> x
    //   def $g   -> def $g

    fn of_arg(atom_mode: AtomMode, arg: &Arg) -> Doc {
        match &arg.node {
            ArgKind::Exp(exp) => Doc::of_exp(atom_mode, exp),
            ArgKind::Def(id_def) => Doc::of_def_arg(id_def),
        }
    }

    // - Function arguments
    //
    //   def $g   -> def $g

    fn of_def_arg(id_def: &Id) -> Doc {
        let doc_id = Doc::of_defid(id_def);
        Doc::concat([Doc::text("def "), doc_id])
    }

    // - Argument lists
    //
    //   (x, y)              -> (x, y)
    //   ()                  -> ()
    //   (x, y) at width 4   -> (
    //                              x,
    //                              y)

    fn of_args(atom_mode: AtomMode, args: &[Arg]) -> Doc {
        Doc::of_comma_list(4, "(", ")", |arg| Doc::of_arg(atom_mode, arg), args)
    }
}

// == Parameters

impl Doc {
    // - Parameter
    //
    //   nat                -> nat
    //   def $f<T>(T) : T   -> def $f<T>(T) : T

    fn of_param(atom_mode: AtomMode, param: &Param) -> Doc {
        match &param.node {
            ParamKind::Exp(plain_typ) => Doc::of_plaintyp(atom_mode, plain_typ),
            ParamKind::Def(id_def, tparams, params, plain_typ) => {
                Doc::of_def_param(atom_mode, id_def, tparams, params, plain_typ)
            }
        }
    }

    // - Function parameters
    //
    //   def $f<T>(T) : T   -> def $f<T>(T) : T

    fn of_def_param(
        atom_mode: AtomMode,
        id_def: &Id,
        tparams: &[TParam],
        params: &[Param],
        plain_typ: &PlainTyp,
    ) -> Doc {
        let doc_id = Doc::of_defid(id_def);
        let doc_tparams = Doc::of_tparams(tparams);
        let doc_params = Doc::of_params(atom_mode, params);
        let doc_typ = Doc::of_plaintyp(atom_mode, plain_typ);
        Doc::group(Doc::concat([
            Doc::text("def "),
            doc_id,
            doc_tparams,
            doc_params,
            Doc::text(" : "),
            doc_typ,
        ]))
    }

    // - Parameter lists
    //
    //   (nat, text)   -> (nat, text)
    //   (none)        -> (empty)

    fn of_params(atom_mode: AtomMode, params: &[Param]) -> Doc {
        Doc::of_optional_comma_list(4, "(", ")", |param| Doc::of_param(atom_mode, param), params)
    }
}

// == Type parameters
//
//   <T, U>   -> <T, U>
//   (none)   -> (empty)

impl Doc {
    fn of_tparams(tparams: &[TParam]) -> Doc {
        Doc::of_optional_comma_list(2, "<", ">", Doc::of_tparam, tparams)
    }
}

// == Premises

impl Doc {
    // - Relation premise layout
    //
    //   Eval: x              -> Eval: x
    //   Eval: x at width 4   -> Eval:
    //                               x

    fn of_rel_prem(atom_mode: AtomMode, marker: &str, id_rel: &Id, exp: &Exp) -> Doc {
        let doc_id = Doc::of_relid(id_rel);
        let doc_exp = Doc::of_exp(atom_mode, exp);
        let doc_body = Doc::nest(4, Doc::concat([Doc::break_(" "), doc_exp]));
        Doc::group(Doc::concat([doc_id, Doc::text(marker), doc_body]))
    }

    // - Premise
    //
    //   if true   -> if true
    //   (if b)*   -> (if b)*

    fn of_prem(atom_mode: AtomMode, prem: &Prem) -> Doc {
        match &prem.node {
            PremKind::Var(prem) => Doc::of_var_prem(atom_mode, prem),
            PremKind::Rule(prem) => Doc::of_rule_prem(atom_mode, prem),
            PremKind::RuleNot(prem) => Doc::of_rule_not_prem(atom_mode, prem),
            PremKind::If(prem) => Doc::of_if_prem(atom_mode, prem),
            PremKind::Else => Doc::of_else_prem(),
            PremKind::Iter(prem) => Doc::of_iter_prem(atom_mode, prem),
            PremKind::Debug(prem) => Doc::of_debug_prem(atom_mode, prem),
        }
    }

    // - Variable premises
    //
    //   var x : nat   -> x : nat

    fn of_var_prem(atom_mode: AtomMode, prem: &VarPrem) -> Doc {
        let VarPrem { id, plain_typ } = prem;
        let doc_id = Doc::of_varid(id);
        let doc_typ = Doc::of_plaintyp(atom_mode, plain_typ);
        Doc::concat([doc_id, Doc::text(" : "), doc_typ])
    }

    // - Relation premises
    //
    //   Eval: x   -> Eval: x

    fn of_rule_prem(atom_mode: AtomMode, prem: &RulePrem) -> Doc {
        let RulePrem { id, exp } = prem;
        Doc::of_rel_prem(atom_mode, ":", id, exp)
    }

    // - Negated relation premises
    //
    //   Eval:/ x   -> Eval:/ x

    fn of_rule_not_prem(atom_mode: AtomMode, prem: &RuleNotPrem) -> Doc {
        let RuleNotPrem { id, exp } = prem;
        Doc::of_rel_prem(atom_mode, ":/", id, exp)
    }

    // - Condition premises
    //
    //   if true   -> if true

    fn of_if_prem(atom_mode: AtomMode, prem: &IfPrem) -> Doc {
        let IfPrem { exp } = prem;
        let doc_exp = Doc::of_exp(atom_mode, exp);
        Doc::concat([Doc::text("if "), doc_exp])
    }

    // - Otherwise premises
    //
    //   otherwise   -> otherwise

    fn of_else_prem() -> Doc {
        Doc::text("otherwise")
    }

    // - Iterated premises
    //
    //   (if b)*      -> (if b)*
    //   ((if b)*)?   -> (if b)*?

    fn of_iter_prem(atom_mode: AtomMode, prem: &IterPrem) -> Doc {
        let IterPrem { prem: prem_inner, iter } = prem;
        let doc_inner = Doc::of_prem(atom_mode, prem_inner);
        let doc_iter = Doc::of_iter(*iter);
        // Nested iterations share the innermost pair of parentheses
        if matches!(prem_inner.node, PremKind::Iter(_)) {
            return Doc::concat([doc_inner, doc_iter]);
        }

        Doc::concat([Doc::text("("), doc_inner, Doc::text(")"), doc_iter])
    }

    // - Debug premises
    //
    //   debug x   -> debug x

    fn of_debug_prem(atom_mode: AtomMode, prem: &DebugPrem) -> Doc {
        let DebugPrem { exp } = prem;
        let doc_exp = Doc::of_exp(atom_mode, exp);
        Doc::concat([Doc::text("debug "), doc_exp])
    }

    // - Premise lists
    //
    //   rule R: x -- if true -- otherwise   -> rule R:
    //                                            x
    //                                            -- if true
    //                                            -- otherwise

    fn of_prems(atom_mode: AtomMode, prems: &[Prem]) -> Doc {
        Doc::concat(prems.iter().map(|prem| {
            let doc_prem = Doc::of_prem(atom_mode, prem);
            Doc::concat([Doc::Line, Doc::text("-- "), doc_prem])
        }))
    }
}

// == Syntax definitions

impl Doc {
    // - External syntax definition
    //
    //   extern syntax t   -> extern syntax t

    fn of_extern_syntax_def(def: &ExternSyntaxDef) -> Doc {
        let ExternSyntaxDef { id, .. } = def;
        let doc_id = Doc::of_typid(id);
        Doc::concat([Doc::text("extern syntax "), doc_id])
    }

    // - Syntax definition
    //
    //   syntax t, u<A>   -> syntax t, u<A>

    fn of_syntax_def(def: &SyntaxDef) -> Doc {
        let SyntaxDef { entries } = def;
        let separator = Doc::concat([Doc::text(","), Doc::break_(" ")]);
        let docs_entry = entries.iter().map(|entry| {
            let doc_id = Doc::of_typid(&entry.id);
            let doc_tparams = Doc::of_tparams(&entry.tparams);
            Doc::concat([doc_id, doc_tparams])
        });
        let doc_entries = Doc::group(Doc::join(separator, docs_entry));
        Doc::concat([Doc::text("syntax "), doc_entries])
    }
}

// == Type definitions
//
//   syntax t<A> = list<A>   -> t<A> = list<A>
//   syntax t = nat | text   -> t
//                                  : nat | text
//                                  ;

impl Doc {
    fn of_typ_def(def: &TypDef) -> Doc {
        let TypDef { id, tparams, def_typ, .. } = def;
        let doc_id = Doc::of_typid(id);
        let doc_tparams = Doc::of_tparams(tparams);
        let doc_body = Doc::of_deftyp(AtomMode::Display, def_typ);
        Doc::concat([doc_id, doc_tparams, doc_body])
    }
}

// == Meta-variable definitions
//
//   var x : nat              -> var x : nat
//   var x : nat at width 8   -> var x
//                                 : nat

impl Doc {
    fn of_var_def(def: &VarDef) -> Doc {
        let VarDef { id, plain_typ, .. } = def;
        let doc_id = Doc::of_varid(id);
        let doc_typ = Doc::of_plaintyp(AtomMode::Source, plain_typ);
        let doc_annotation =
            Doc::nest(2, Doc::concat([Doc::break_(" "), Doc::text(": "), doc_typ]));
        Doc::group(Doc::concat([Doc::text("var "), doc_id, doc_annotation]))
    }
}

// == Relation definitions

impl Doc {
    // - External relation definition
    //
    //   extern relation R: p |- e   -> extern relation R:
    //                                    p |- e

    fn of_extern_rel_def(def: &ExternRelDef) -> Doc {
        let ExternRelDef { id, not_typ, .. } = def;
        let doc_id = Doc::of_relid(id);
        let doc_typ = Doc::of_nottyp(AtomMode::Source, not_typ);
        let doc_signature = Doc::nest(2, Doc::concat([Doc::Line, doc_typ]));
        Doc::concat([Doc::text("extern relation "), doc_id, Doc::text(":"), doc_signature])
    }

    // - Relation definition
    //
    //   relation R: p |- e : t   -> relation R:
    //                                 p |- e : t

    fn of_rel_def(def: &RelDef) -> Doc {
        let RelDef { id, not_typ, .. } = def;
        let doc_id = Doc::of_relid(id);
        let doc_typ = Doc::of_nottyp(AtomMode::Source, not_typ);
        let doc_signature = Doc::nest(2, Doc::concat([Doc::Line, doc_typ]));
        Doc::concat([Doc::text("relation "), doc_id, Doc::text(":"), doc_signature])
    }

    // - Rule
    //
    //   rule R/step: x -- if true   -> rule R/step:
    //                                    x
    //                                    -- if true

    fn of_rule(atom_mode: AtomMode, rule: &Rule) -> Doc {
        let doc_id = Doc::of_relid(&rule.node.id_rel);
        let doc_suffix = Doc::of_rule_suffix(&rule.node.id_rule);
        let doc_name = Doc::nest(2, Doc::concat([Doc::break_(" "), doc_id, doc_suffix]));
        let doc_heading = Doc::group(Doc::concat([Doc::text("rule"), doc_name, Doc::text(":")]));
        let doc_exp = Doc::of_exp(atom_mode, &rule.node.exp);
        let doc_prems = Doc::of_prems(atom_mode, &rule.node.prems);
        let doc_body = Doc::nest(2, Doc::concat([Doc::Line, doc_exp, doc_prems]));
        Doc::concat([doc_heading, doc_body])
    }

    // - Rule group definition
    //
    //   rulegroup R/g { rule R/a: x rule R/b: y }   -> rulegroup R/g {
    //
    //                                                    rule R/a:
    //                                                      x
    //
    //                                                    rule R/b:
    //                                                      y
    //
    //                                                  }
    //   rulegroup R/g { rule R/a: x }               -> rule R/a:
    //                                                    x

    fn of_rule_group_def(def: &RuleGroupDef) -> Doc {
        let RuleGroupDef { relid, groupid, rules } = def;
        // A singleton group uses the rule heading directly
        if let [rule] = rules.as_slice() {
            return Doc::of_rule(AtomMode::Source, rule);
        }

        let doc_id = Doc::of_relid(relid);
        let doc_suffix = Doc::of_rule_suffix(groupid);
        let mut docs = vec![Doc::text("rulegroup "), doc_id, doc_suffix, Doc::text(" {")];
        for rule in rules {
            let doc_rule = Doc::of_rule(AtomMode::Source, rule);
            let doc_body = Doc::nest(2, Doc::concat([Doc::Line, doc_rule]));
            docs.extend([Doc::Line, doc_body]);
        }
        docs.extend([Doc::Line, Doc::Line, Doc::text("}")]);
        Doc::concat(docs)
    }
}

// == Meta-function definitions

impl Doc {
    // - Function signature
    //
    //   dec $f<T>(T) : T   -> dec $f<T>(T) : T
    //   dec $f : nat       -> dec $f : nat

    fn of_func_dec(
        prefix: &str,
        id_def: &Id,
        tparams: &[TParam],
        params: &[Param],
        plain_typ: &PlainTyp,
    ) -> Doc {
        let doc_id = Doc::of_defid(id_def);
        let doc_tparams = Doc::of_tparams(tparams);
        let doc_params = Doc::of_params(AtomMode::Source, params);
        let doc_typ = Doc::of_plaintyp(AtomMode::Source, plain_typ);
        let doc_result = Doc::nest(2, Doc::concat([Doc::break_(" "), Doc::text(": "), doc_typ]));
        Doc::group(Doc::concat([Doc::text(prefix), doc_id, doc_tparams, doc_params, doc_result]))
    }

    // - External function declaration
    //
    //   extern dec $f(nat) : bool   -> extern dec $f(nat) : bool

    fn of_extern_dec_def(def: &ExternDecDef) -> Doc {
        let ExternDecDef { id, tparams, params, plain_typ, .. } = def;
        Doc::of_func_dec("extern dec ", id, tparams, params, plain_typ)
    }

    // - Builtin function declaration
    //
    //   builtin dec $f(nat) : bool   -> builtin dec $f(nat) : bool

    fn of_builtin_dec_def(def: &BuiltinDecDef) -> Doc {
        let BuiltinDecDef { id, tparams, params, plain_typ, .. } = def;
        Doc::of_func_dec("builtin dec ", id, tparams, params, plain_typ)
    }

    // - Table function declaration
    //
    //   tbl dec $f(nat) : bool   -> tbl dec $f(nat) : bool

    fn of_table_dec_def(def: &TableDecDef) -> Doc {
        let TableDecDef { id, params, plain_typ, .. } = def;
        Doc::of_func_dec("tbl dec ", id, &[], params, plain_typ)
    }

    // - Function declaration
    //
    //   dec $f<T>(T) : T   -> dec $f<T>(T) : T

    fn of_func_dec_def(def: &FuncDecDef) -> Doc {
        let FuncDecDef { id, tparams, params, plain_typ, .. } = def;
        Doc::of_func_dec("dec ", id, tparams, params, plain_typ)
    }

    // - Table row
    //
    //   | x => x              -> | x => x
    //   | x => x at width 6   -> | x
    //                                => x

    fn of_tablerow(atom_mode: AtomMode, row: &TableRow) -> Doc {
        let doc_pattern = Doc::of_exp(atom_mode, &row.node.exp_pattern);
        let doc_body = Doc::of_exp(atom_mode, &row.node.exp_body);
        let doc_result = Doc::nest(4, Doc::concat([Doc::break_(" "), Doc::text("=> "), doc_body]));
        Doc::group(Doc::concat([Doc::text("| "), doc_pattern, doc_result]))
    }

    // - Table function definition
    //
    //   tbl def $f = | 0 => true | n => false   -> tbl def $f =
    //                                                | 0 => true
    //                                                | n => false

    fn of_table_def(def: &TableDef) -> Doc {
        let TableDef { id, rows } = def;
        let doc_id = Doc::of_defid(id);
        let docs_row = rows.iter().map(|row| {
            let doc_row = Doc::of_tablerow(AtomMode::Source, row);
            Doc::concat([Doc::Line, doc_row])
        });
        let doc_rows = Doc::nest(2, Doc::concat(docs_row));
        Doc::concat([Doc::text("tbl def "), doc_id, Doc::text(" ="), doc_rows])
    }

    // - Function definition
    //
    //   def $f(x) = x -- if true   -> def $f(x) = x
    //                                   -- if true
    //   def $f = 3                 -> def $f() = 3

    fn of_func_def(def: &FuncDef) -> Doc {
        let FuncDef { id, tparams, args, exp, prems } = def;
        let doc_id = Doc::of_defid(id);
        let doc_tparams = Doc::of_tparams(tparams);
        let doc_args = Doc::of_args(AtomMode::Source, args);
        let doc_exp = Doc::of_exp(AtomMode::Source, exp);
        let doc_result = Doc::nest(2, Doc::concat([Doc::break_(" "), Doc::text("= "), doc_exp]));
        let doc_clause =
            Doc::group(Doc::concat([Doc::text("def "), doc_id, doc_tparams, doc_args, doc_result]));
        let doc_prems = Doc::of_prems(AtomMode::Source, prems);
        Doc::concat([doc_clause, Doc::nest(2, doc_prems)])
    }
}

// == Definitions

impl Doc {
    // - Definition
    //
    //   var x : nat    -> var x : nat
    //   dec $f : nat   -> dec $f : nat

    fn of_def(def: &Def) -> Doc {
        match &def.node {
            DefKind::ExternSyntax(def) => Doc::of_extern_syntax_def(def),
            DefKind::Syntax(def) => Doc::of_syntax_def(def),
            DefKind::Typ(def) => Doc::of_typ_def(def),
            DefKind::Var(def) => Doc::of_var_def(def),
            DefKind::ExternRel(def) => Doc::of_extern_rel_def(def),
            DefKind::Rel(def) => Doc::of_rel_def(def),
            DefKind::RuleGroup(def) => Doc::of_rule_group_def(def),
            DefKind::ExternDec(def) => Doc::of_extern_dec_def(def),
            DefKind::BuiltinDec(def) => Doc::of_builtin_dec_def(def),
            DefKind::TableDec(def) => Doc::of_table_dec_def(def),
            DefKind::FuncDec(def) => Doc::of_func_dec_def(def),
            DefKind::TableDef(def) => Doc::of_table_def(def),
            DefKind::FuncDef(def) => Doc::of_func_def(def),
            DefKind::Sep => Doc::of_sep_def(),
        }
    }

    // - Definition separators
    //
    //   Sep   -> "\n\n"

    fn of_sep_def() -> Doc {
        Doc::concat([Doc::Line, Doc::Line])
    }
}

// == Entry point
//
//   var x : nat   -> var x : nat

/// Renders one elaboration-language definition without AsciiDoc block delimiters.
pub fn render_def(def: &Def) -> String {
    Doc::of_def(def).render(WIDTH)
}
