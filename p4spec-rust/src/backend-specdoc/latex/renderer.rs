//! EL syntax translated to semantic TeX documents
//!
//! `Doc::of_*` builds a `Doc`; `ExpTerm::of_*` also keeps the outermost category,
//! so a nested operand knows whether it needs parentheses:
//!
//! ```text
//! ExpTerm::of_exp(Bin(a, Add, b))                -> ExpTerm { tex: a + b, category: Additive }
//! ExpTerm::of_exp(Bin(Bin(a, Add, b), Mul, c))   -> ExpTerm { tex: \left(a + b\right) \cdot c, .. }
//! Doc::of_exp(exp)                               -> ExpTerm::of_exp(exp).tex
//! Doc::of_defs([FuncDef f, FuncDef f, Sep])      -> Gathered([Line(aligned clauses of f), Gap])
//! ```
//!
//! Rule and function layouts resolve at width 80 before serialization.

use num_traits::Signed;

use crate::lang::{
    common::{
        Iter,
        notation::atom::Atom as AtomKind,
        prim::{bool, num},
    },
    el::ast::*,
};

use super::{
    error::{Error, Result},
    precedence::{self, Category, Prec, Side},
    render::Anchors,
    tex::{
        doc::{Alignment, Block, Delimiter, Doc, GridRow, Soft, Style, Symbol, Target},
        layout, link, width,
    },
};

/// Line width for resolved rule and function layouts.
const WIDTH_LAYOUT: usize = 80;
/// Continuation indentation before a broken infix operator.
const INDENT_INFIX: usize = 4;

// == Helpers

impl Doc {
    // - Breakable infix
    //
    //   Doc::of_breakable_infix(x, +, y)
    //   -> LayoutGroup(Concat([x, Nest(4, [SoftBreak(SoftSpace), +, Space, y])]))

    /// Offers an indented break before an operator only when all terms are visible.
    fn of_breakable_infix(tex_l: Doc, tex_op: Doc, tex_r: Doc) -> Doc {
        // Missing operands must not leave an empty indented continuation
        if tex_l.is_empty() || tex_op.is_empty() || tex_r.is_empty() {
            return Doc::concat_spaced(vec![tex_l, tex_op, tex_r]);
        }
        // Nest only the continuation so the first line retains its original width
        let tex_break = Doc::SoftBreak(Soft::SoftSpace);
        let tex_continuation = Doc::concat(vec![tex_break, tex_op, Doc::Space, tex_r]);
        let tex_continuation = Doc::nest(INDENT_INFIX, tex_continuation);
        let tex_infix = Doc::concat(vec![tex_l, tex_continuation]);
        Doc::layout_group(tex_infix)
    }

    // - Index suffixes
    //
    //   [i]       -> \left[\mathsf{i}\right]
    //   [i : n]   -> \left[\mathsf{i} : \mathsf{n}\right]

    /// Renders an index suffix, `[i]`.
    fn of_idx_suffix(exp_idx: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_idx = Doc::of_exp(exp_idx, anchors)?;
        let tex_suffix = Doc::delimited(Delimiter::Bracket, tex_idx);
        Ok(tex_suffix)
    }

    /// Renders a slice suffix, `[i : n]`.
    fn of_slice_suffix(exp_idx: &Exp, exp_len: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_idx = Doc::of_exp(exp_idx, anchors)?;
        let tex_len = Doc::of_exp(exp_len, anchors)?;
        let tex_body = Doc::concat_spaced(vec![tex_idx, Doc::Fixed(Symbol::Colon), tex_len]);
        let tex_suffix = Doc::delimited(Delimiter::Bracket, tex_body);
        Ok(tex_suffix)
    }
}

// - Links
//
//   $g(x), with anchor g   -> \href{#g}{\mathrm{g}}\left(\mathsf{x}\right)

/// Resolves the anchor of a function reference.
fn anchor_of_func(anchors: Option<&Anchors<'_>>, id: &Id) -> Option<String> {
    let anchors = anchors?;
    (anchors.func)(&id.node)
}

/// Resolves the anchor of a relation reference.
fn anchor_of_rel(anchors: Option<&Anchors<'_>>, id: &Id) -> Option<String> {
    let anchors = anchors?;
    (anchors.rel)(&id.node)
}

impl Doc {
    /// Links a reference when its anchor resolves.
    fn of_link(anchor: Option<&str>, tex_ref: Doc) -> Result<Doc> {
        let Some(anchor) = anchor else {
            return Ok(tex_ref);
        };
        let target = Target::of_string(anchor)?;
        let tex_linked = link::link_unowned_doc(&target, tex_ref);
        Ok(tex_linked)
    }
}

// == Lexical rendering

impl Doc {
    // - Identifiers
    //
    //   t_sep   -> {\mathsf{t}}_{\mathsf{sep}}
    //   _x      -> \mathsf{\_}
    //   $g      -> \mathrm{g}

    /// Renders underscore-prefixed variables as `_` and `TC_0` as a subscript.
    fn of_varid(id: &Id) -> Doc {
        // Underscore-prefixed identifiers all denote an anonymous variable
        if id.node.starts_with('_') {
            return Doc::Styled(Style::Mathsf, "_".to_owned());
        }
        // Keep every suffix after the first underscore in one subscript
        let Some((text_base, text_sub)) = id.node.split_once('_') else {
            return Doc::Styled(Style::Mathsf, id.node.clone());
        };
        let tex_base = Doc::Styled(Style::Mathsf, text_base.to_owned());
        let tex_sub = Doc::Styled(Style::Mathsf, text_sub.to_owned());
        Doc::sub(tex_base, tex_sub)
    }

    fn of_typid(id: &Id) -> Doc {
        Doc::Styled(Style::Mathsf, id.node.clone())
    }

    fn of_defid(id: &Id) -> Doc {
        Doc::Styled(Style::Mathrm, id.node.clone())
    }

    // - Numbers
    //
    //   42     -> 42
    //   0xFF   -> \mathtt{0xff}

    /// Keeps signed integers visibly distinct from natural literals.
    fn of_number(op: NumOp, num: &Num) -> Doc {
        let int = num::to_int(num);
        let tex_sign = match num {
            num::Number::Nat(_) => Doc::Empty,
            num::Number::Int(_) if int.is_negative() => Doc::Fixed(Symbol::Minus),
            num::Number::Int(_) => Doc::Fixed(Symbol::Plus),
        };
        let int_abs = int.abs();
        let tex_abs = match op {
            NumOp::Dec => Doc::Decimal(int_abs),
            NumOp::Hex => Doc::Hexadecimal(int_abs),
        };
        Doc::concat(vec![tex_sign, tex_abs])
    }

    // - Atoms
    //
    //   IF    -> \mathsf{IF}
    //   <:    -> \mathrel{<:}
    //   |-    -> \mathrel{\vdash}
    //   ->_   -> \to

    /// Classifies two adjacent symbols as one relation, as in `<:`.
    fn of_rel_symbols(symbol_l: Symbol, symbol_r: Symbol) -> Doc {
        let tex_symbols = Doc::concat(vec![Doc::Fixed(symbol_l), Doc::Fixed(symbol_r)]);
        Doc::mathrel(tex_symbols)
    }

    /// Renders a notation atom with its fixed TeX spelling.
    fn of_atom(atom: &Atom) -> Doc {
        use AtomKind as A;
        use Symbol as S;
        match &atom.node {
            A::Keyword(text) => Doc::Styled(Style::Mathsf, text.clone()),
            A::Tag(text) => {
                let tex_tag = Doc::Styled(Style::Mathsf, text.clone());
                Doc::sub(Doc::ThinSpace, tex_tag)
            }
            A::Operator(text) => {
                let tex_op = Doc::Styled(Style::Mathtt, text.clone());
                Doc::mathbin(tex_op)
            }
            A::Sub => Doc::of_rel_symbols(S::Less, S::Colon),
            A::Sup => Doc::of_rel_symbols(S::Colon, S::Greater),
            A::Turnstile => Doc::mathrel(Doc::Fixed(S::Turnstile)),
            A::Tilesturn => Doc::mathrel(Doc::Fixed(S::Tilesturn)),
            A::Arrow | A::ArrowSub => Doc::Fixed(S::To),
            A::DoubleArrowSub => Doc::Fixed(S::Rightarrow),
            A::DoubleArrowLong => Doc::Fixed(S::Longrightarrow),
            A::SqArrow => Doc::Fixed(S::Hookrightarrow),
            A::SqArrowStar => Doc::sup(Doc::Fixed(S::Hookrightarrow), Doc::Fixed(S::Ast)),
            A::Dot => Doc::group(Doc::Fixed(S::Dot)),
            A::Dot2 => Doc::Fixed(S::Dot2),
            A::Dot3 => Doc::Fixed(S::Ellipsis),
            A::Semicolon => Doc::Fixed(S::Semicolon),
            A::Colon => Doc::Fixed(S::Colon),
            A::ColonEq => Doc::of_rel_symbols(S::Colon, S::Equal),
            A::Tilde2 => Doc::Fixed(S::Sim),
            A::Backslash => Doc::Fixed(S::Setminus),
            A::LAngle => Doc::Fixed(S::Less),
            A::RAngle => Doc::Fixed(S::Greater),
            A::LParen => Doc::Fixed(S::LeftParen),
            A::RParen => Doc::Fixed(S::RightParen),
            A::LBrack => Doc::Fixed(S::LeftBracket),
            A::RBrack => Doc::Fixed(S::RightBracket),
            A::LBrace => Doc::Fixed(S::LeftBrace),
            A::RBrace => Doc::Fixed(S::RightBrace),
        }
    }

    // - Brackets
    //
    //   `[ x `]   -> \left[\mathsf{x}\right]

    /// Pairs recognized bracket atoms and otherwise retains their literal notation.
    fn of_bracket(atom_l: &Atom, tex_body: Doc, atom_r: &Atom) -> Doc {
        let delimiter = match (&atom_l.node, &atom_r.node) {
            (AtomKind::LParen, AtomKind::RParen) => Delimiter::Paren,
            (AtomKind::LBrack, AtomKind::RBrack) => Delimiter::Bracket,
            (AtomKind::LBrace, AtomKind::RBrace) => Delimiter::Brace,
            (AtomKind::LAngle, AtomKind::RAngle) => Delimiter::Angle,
            // Unpaired atoms keep their literal spelling
            _ => {
                let tex_l = Doc::of_atom(atom_l);
                let tex_r = Doc::of_atom(atom_r);
                return Doc::concat(vec![tex_l, tex_body, tex_r]);
            }
        };
        Doc::delimited(delimiter, tex_body)
    }

    // - Iterations
    //
    //   *   -> \ast
    //   ?   -> ?

    fn of_iter(iter: Iter) -> Doc {
        let symbol = match iter {
            Iter::Opt => Symbol::Question,
            Iter::List => Symbol::Ast,
        };
        Doc::Fixed(symbol)
    }
}

// == Types

impl Doc {
    // - Type
    //
    //   B nat   -> \mathsf{B}\,\mathbb{N}

    fn of_typ(typ: &Typ) -> Doc {
        match typ {
            Typ::Plain(plain_typ) => Doc::of_plaintyp(plain_typ),
            Typ::Notation(not_typ) => Doc::of_nottyp(not_typ),
        }
    }

    fn of_typs(typs: &[Typ]) -> Doc {
        let texs = typs.iter().map(Doc::of_typ).collect();
        Doc::concat_juxtaposed(texs)
    }

    // - Plain types
    //
    //   nat           -> \mathbb{N}
    //   list<nat>     -> \mathsf{list}\left\langle\mathbb{N}\right\rangle
    //   (nat, bool)   -> \left(\mathbb{N}, \mathbb{B}\right)
    //   T*            -> {\mathsf{T}}^{\ast}

    fn of_plaintyp(plain_typ: &PlainTyp) -> Doc {
        match &plain_typ.node {
            PlainTypKind::Bool => Doc::of_bool_typ(),
            PlainTypKind::Num(num_typ) => Doc::of_num_typ(*num_typ),
            PlainTypKind::Text => Doc::of_text_typ(),
            PlainTypKind::Var(id, targs) => Doc::of_var_typ(id, targs),
            PlainTypKind::Paren(plain_typ) => Doc::of_paren_typ(plain_typ),
            PlainTypKind::Tuple(plain_typs) => Doc::of_tuple_typ(plain_typs),
            PlainTypKind::Iter(plain_typ, iter) => Doc::of_iter_typ(plain_typ, *iter),
        }
    }

    // - Boolean types
    //
    //   bool   -> \mathbb{B}

    fn of_bool_typ() -> Doc {
        Doc::Styled(Style::Mathbb, "B".to_owned())
    }

    // - Number types
    //
    //   nat   -> \mathbb{N}
    //   int   -> \mathbb{Z}

    fn of_num_typ(num_typ: num::Typ) -> Doc {
        let text = match num_typ {
            num::Typ::Nat => "N",
            num::Typ::Int => "Z",
        };
        Doc::Styled(Style::Mathbb, text.to_owned())
    }

    // - Text types
    //
    //   text   -> \mathbb{T}

    fn of_text_typ() -> Doc {
        Doc::Styled(Style::Mathbb, "T".to_owned())
    }

    // - Type applications
    //
    //   list<nat>   -> \mathsf{list}\left\langle\mathbb{N}\right\rangle

    fn of_var_typ(id: &Id, targs: &[Targ]) -> Doc {
        let tex_name = Doc::of_typid(id);
        let tex_targs = Doc::of_targs(targs);
        Doc::concat(vec![tex_name, tex_targs])
    }

    // - Parenthesized types
    //
    //   (nat)   -> \left(\mathbb{N}\right)

    fn of_paren_typ(plain_typ: &PlainTyp) -> Doc {
        let tex_typ = Doc::of_plaintyp(plain_typ);
        Doc::parenthesized(tex_typ)
    }

    // - Tuple types
    //
    //   (nat, bool)   -> \left(\mathbb{N}, \mathbb{B}\right)

    fn of_tuple_typ(plain_typs: &[PlainTyp]) -> Doc {
        let texs = plain_typs.iter().map(Doc::of_plaintyp).collect();
        let tex_typs = Doc::layout_group_soft_comma_separated(texs);
        Doc::parenthesized(tex_typs)
    }

    // - Iterated types
    //
    //   T*   -> {\mathsf{T}}^{\ast}

    fn of_iter_typ(plain_typ: &PlainTyp, iter: Iter) -> Doc {
        let tex_typ = Doc::of_plaintyp(plain_typ);
        let tex_iter = Doc::of_iter(iter);
        Doc::sup(tex_typ, tex_iter)
    }

    // - Notation types
    //
    //   p |- e : nat       -> \mathsf{p} \mathrel{\vdash} \mathsf{e} : \mathbb{N}
    //   nat ->_ nat bool   -> \mathbb{N} {\to}_{\mathbb{N}} \mathbb{B}

    fn of_nottyp(not_typ: &NotTyp) -> Doc {
        match &not_typ.node {
            NotTypKind::Atom(atom) => Doc::of_atom(atom),
            NotTypKind::Seq(typs) => Doc::of_typs(typs),
            NotTypKind::Infix(typ_l, atom, typ_r) => Doc::of_infix_typ(typ_l, atom, typ_r),
            NotTypKind::Brack(atom_l, typ, atom_r) => Doc::of_brack_typ(atom_l, typ, atom_r),
        }
    }

    // - Bracketed types
    //
    //   `[ nat `]   -> \left[\mathbb{N}\right]

    fn of_brack_typ(atom_l: &Atom, typ: &Typ, atom_r: &Atom) -> Doc {
        let tex_body = Doc::of_typ(typ);
        Doc::of_bracket(atom_l, tex_body, atom_r)
    }

    // - Infix types
    //
    //   p |- e             -> \mathsf{p} \mathrel{\vdash} \mathsf{e}
    //   nat ->_ nat bool   -> \mathbb{N} {\to}_{\mathbb{N}} \mathbb{B}

    /// Consumes the first right-hand type as an arrow subscript when present.
    fn of_infix_typ(typ_l: &Typ, atom: &Atom, typ_r: &Typ) -> Doc {
        let tex_l = Doc::of_typ(typ_l);
        let tex_op = Doc::of_atom(atom);

        // Plain infix notation retains its complete right operand
        if !matches!(atom.node, AtomKind::ArrowSub | AtomKind::DoubleArrowSub) {
            let tex_r = Doc::of_typ(typ_r);
            return Doc::concat_spaced(vec![tex_l, tex_op, tex_r]);
        }

        // Subscripted arrows consume one type from a sequence or the whole operand
        let (tex_sub, tex_r) = match typ_r {
            // A sequence tail remains to the right of the arrow
            Typ::Notation(NotTyp { node: NotTypKind::Seq(typs), .. }) => {
                let Some((typ_sub, typs)) = typs.split_first() else {
                    return Doc::concat_spaced(vec![tex_l, tex_op]);
                };
                let tex_sub = Doc::of_typ(typ_sub);
                let tex_r = Doc::of_typs(typs);
                (tex_sub, tex_r)
            }
            // A single operand supplies only the subscript
            typ_sub => {
                let tex_sub = Doc::of_typ(typ_sub);
                (tex_sub, Doc::Empty)
            }
        };
        let tex_op = Doc::sub(tex_op, tex_sub);
        Doc::concat_spaced(vec![tex_l, tex_op, tex_r])
    }

    // - Type heads
    //
    //   list<T>   -> \mathsf{list}\left\langle\mathsf{T}\right\rangle

    /// Renders a type name with its type parameters.
    fn of_typ_head(id: &Id, tparams: &[TParam]) -> Doc {
        let tex_name = Doc::of_typid(id);
        let tex_tparams = Doc::of_tparams(tparams);
        Doc::concat(vec![tex_name, tex_tparams])
    }

    // - Type arguments
    //
    //   <nat>   -> \left\langle\mathbb{N}\right\rangle

    fn of_targs(targs: &[Targ]) -> Doc {
        if targs.is_empty() {
            return Doc::Empty;
        }
        let texs = targs.iter().map(Doc::of_plaintyp).collect();
        let tex_targs = Doc::layout_group_soft_comma_separated(texs);
        Doc::delimited(Delimiter::Angle, tex_targs)
    }

    // - Definition types
    //
    //   {A nat, B bool}   -> \left\{\mathsf{A} \mathbb{N}, \mathsf{B} \mathbb{B}\right\}

    fn of_deftyp(def_typ: &DefTyp) -> Doc {
        match &def_typ.node {
            DefTypKind::Plain(plain_typ) => Doc::of_plaintyp(plain_typ),
            DefTypKind::Struct(typ_fields) => Doc::of_struct_typ(typ_fields),
            DefTypKind::Variant(typ_cases) => Doc::of_variant_typ(typ_cases),
        }
    }

    // - Struct types
    //
    //   {A nat, B bool}   -> \left\{\mathsf{A} \mathbb{N}, \mathsf{B} \mathbb{B}\right\}

    fn of_typ_field(typ_field: &TypField) -> Doc {
        let tex_atom = Doc::of_atom(&typ_field.atom);
        let tex_typ = Doc::of_plaintyp(&typ_field.typ);
        Doc::concat_spaced(vec![tex_atom, tex_typ])
    }

    fn of_struct_typ(typ_fields: &[TypField]) -> Doc {
        let texs = typ_fields.iter().map(Doc::of_typ_field).collect();
        let tex_fields = Doc::layout_group_soft_comma_separated(texs);
        Doc::delimited(Delimiter::Brace, tex_fields)
    }

    // - Variant types
    //
    //   | A | B nat   -> Gathered([Line(\mathsf{A}), Line(\mathsf{B}\,\mathbb{N})])

    fn of_variant_typ(typ_cases: &[TypCase]) -> Doc {
        let blocks = typ_cases
            .iter()
            .map(|typ_case| {
                let tex_case = Doc::of_typ(&typ_case.typ);
                Block::Line(tex_case)
            })
            .collect();
        Doc::gathered(blocks)
    }
}

// == Operators
//
//   ~    -> \neg
//   *    -> \cdot
//   ^    -> BinopTerm::Exponent
//   <=   -> \le

impl Doc {
    fn of_unop(op: UnOp) -> Doc {
        let symbol = match op {
            UnOp::Bool(bool::UnOp::Not) => Symbol::Neg,
            UnOp::Num(num::UnOp::Plus) => Symbol::Plus,
            UnOp::Num(num::UnOp::Minus) => Symbol::Minus,
        };
        Doc::Fixed(symbol)
    }
}

/// Spelling of a binary operator, or its superscript rendering.
enum BinopTerm {
    /// An infix operator symbol.
    Infix(Doc),
    /// Exponentiation, rendered as a superscript.
    Exponent,
}

impl BinopTerm {
    /// Selects the infix symbol or exponent rendering of a binary operator.
    fn of_binop(op: BinOp) -> BinopTerm {
        let symbol = match op {
            BinOp::Bool(bool::BinOp::And) => Symbol::Land,
            BinOp::Bool(bool::BinOp::Or) => Symbol::Lor,
            BinOp::Bool(bool::BinOp::Impl) => Symbol::Rightarrow,
            BinOp::Bool(bool::BinOp::Equiv) => Symbol::Leftrightarrow,
            BinOp::Num(num::BinOp::Add) => Symbol::Plus,
            BinOp::Num(num::BinOp::Sub) => Symbol::Minus,
            BinOp::Num(num::BinOp::Mul) => Symbol::Cdot,
            BinOp::Num(num::BinOp::Div) => Symbol::Slash,
            BinOp::Num(num::BinOp::Mod) => Symbol::Bmod,
            BinOp::Num(num::BinOp::Pow) => return BinopTerm::Exponent,
        };
        let tex_op = Doc::Fixed(symbol);
        BinopTerm::Infix(tex_op)
    }
}

impl Doc {
    fn of_cmpop(op: CmpOp) -> Doc {
        let symbol = match op {
            CmpOp::Bool(bool::CmpOp::Eq) => Symbol::Equal,
            CmpOp::Bool(bool::CmpOp::Ne) => Symbol::NotEqual,
            CmpOp::Num(num::CmpOp::Lt) => Symbol::Less,
            CmpOp::Num(num::CmpOp::Gt) => Symbol::Greater,
            CmpOp::Num(num::CmpOp::Le) => Symbol::LessEqual,
            CmpOp::Num(num::CmpOp::Ge) => Symbol::GreaterEqual,
        };
        Doc::Fixed(symbol)
    }
}

// == Expressions

// - Expression terms
//
//   ExpTerm::atomic(\mathsf{x})   -> ExpTerm { tex: \mathsf{x}, category: Atomic }

/// A rendered expression with the category needed to preserve operand grouping.
struct ExpTerm {
    tex: Doc,
    category: Category,
}

impl ExpTerm {
    fn new(tex: Doc, category: Category) -> Self {
        Self { tex, category }
    }

    fn atomic(tex: Doc) -> Self {
        Self::new(tex, Category::Atomic)
    }
}

impl Doc {
    // - Expression
    //
    //   Doc::of_nested_exp(of_binop(Mul), Left, ExpTerm { tex: x + y, category: Additive })
    //   -> \left(x + y\right)

    fn of_exp(exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let term = ExpTerm::of_exp(exp, anchors)?;
        Ok(term.tex)
    }

    fn of_exps(exps: &[Exp], anchors: Option<&Anchors<'_>>) -> Result<Vec<Doc>> {
        exps.iter().map(|exp| Doc::of_exp(exp, anchors)).collect()
    }

    /// Parenthesizes a weaker operand or an equal-precedence associativity conflict.
    fn of_nested_exp(prec_parent: Prec, side: Side, term: ExpTerm) -> Doc {
        if precedence::needs_parentheses(prec_parent, side, term.category) {
            Doc::parenthesized(term.tex)
        } else {
            term.tex
        }
    }
}

impl ExpTerm {
    /// Renders an expression while retaining the category of its outermost operator.
    fn of_exp(exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        match &exp.node {
            ExpKind::Bool(value) => Ok(ExpTerm::of_bool_exp(*value)),
            ExpKind::Num(op, num) => Ok(ExpTerm::of_num_exp(*op, num)),
            ExpKind::Text(text) => Ok(ExpTerm::of_text_exp(text)),
            ExpKind::Id(id) => Ok(ExpTerm::of_var_exp(id)),
            ExpKind::Un(op, exp) => ExpTerm::of_un_exp(*op, exp, anchors),
            ExpKind::Bin(exp_l, op, exp_r) => ExpTerm::of_bin_exp(exp_l, *op, exp_r, anchors),
            ExpKind::Cmp(exp_l, op, exp_r) => ExpTerm::of_cmp_exp(exp_l, *op, exp_r, anchors),
            ExpKind::Arith(exp) => ExpTerm::of_exp(exp, anchors),
            ExpKind::Eps => Ok(ExpTerm::of_eps_exp()),
            ExpKind::List(exps) => ExpTerm::of_list_exp(exps, anchors),
            ExpKind::Cons(exp_l, exp_r) => ExpTerm::of_cons_exp(exp_l, exp_r, anchors),
            ExpKind::Cat(exp_l, exp_r) => ExpTerm::of_cat_exp(exp_l, exp_r, anchors),
            ExpKind::Idx(exp_base, exp_idx) => ExpTerm::of_idx_exp(exp_base, exp_idx, anchors),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                ExpTerm::of_slice_exp(exp_base, exp_idx, exp_len, anchors)
            }
            ExpKind::Len(exp) => ExpTerm::of_len_exp(exp, anchors),
            ExpKind::Mem(exp_l, exp_r) => ExpTerm::of_mem_exp(exp_l, exp_r, anchors),
            ExpKind::Str(exp_fields) => ExpTerm::of_str_exp(exp_fields, anchors),
            ExpKind::Dot(exp_base, atom) => ExpTerm::of_dot_exp(exp_base, atom, anchors),
            ExpKind::Upd(exp_base, path, exp_field) => {
                ExpTerm::of_upd_exp(exp_base, path, exp_field, anchors)
            }
            ExpKind::Paren(exp) => ExpTerm::of_paren_exp(exp, anchors),
            ExpKind::Tuple(exps) => ExpTerm::of_tuple_exp(exps, anchors),
            ExpKind::Call(id, targs, args) => ExpTerm::of_call_exp(id, targs, args, anchors),
            ExpKind::Iter(exp, iter) => ExpTerm::of_iter_exp(exp, *iter, anchors),
            ExpKind::Sub(exp, plain_typ) => ExpTerm::of_sub_exp(exp, plain_typ, anchors),
            ExpKind::Atom(atom) => Ok(ExpTerm::of_atom_exp(atom)),
            ExpKind::Seq(exps) => ExpTerm::of_seq_exp(exps, anchors),
            ExpKind::Infix(exp_l, atom, exp_r) => {
                ExpTerm::of_infix_exp(exp_l, atom, exp_r, anchors)
            }
            ExpKind::Brack(atom_l, exp, atom_r) => {
                ExpTerm::of_brack_exp(atom_l, exp, atom_r, anchors)
            }
            ExpKind::Hole(_) => Err(Error::Hole(exp.span.clone())),
            ExpKind::Fuse(..) => Err(Error::Fuse(exp.span.clone())),
            ExpKind::Unparen(_) => Err(Error::Unparen(exp.span.clone())),
            ExpKind::Latex(_) => Err(Error::RawLatex(exp.span.clone())),
        }
    }

    // - Binary expression layout
    //
    //   x + y   -> Doc::of_breakable_infix(\mathsf{x}, +, \mathsf{y})

    /// Preserves operand grouping and adds a break before the infix operator.
    fn of_binary_exp(
        prec: Prec,
        tex_op: Doc,
        exp_l: &Exp,
        exp_r: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let term_l = ExpTerm::of_exp(exp_l, anchors)?;
        let term_r = ExpTerm::of_exp(exp_r, anchors)?;
        let tex_l = Doc::of_nested_exp(prec, Side::Left, term_l);
        let tex_r = Doc::of_nested_exp(prec, Side::Right, term_r);
        let tex = Doc::of_breakable_infix(tex_l, tex_op, tex_r);
        Ok(ExpTerm::new(tex, prec.category))
    }

    // - Postfix expression layout
    //
    //   xs[i]   -> \mathsf{xs}\left[\mathsf{i}\right]

    /// Parenthesizes a postfix base before attaching its rendered suffix.
    fn of_postfix_exp(
        exp_base: &Exp,
        tex_suffix: Doc,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let term_base = ExpTerm::of_exp(exp_base, anchors)?;
        let tex_base = Doc::of_nested_exp(precedence::POSTFIX, Side::Left, term_base);
        let tex = Doc::concat(vec![tex_base, tex_suffix]);
        Ok(ExpTerm::new(tex, Category::Postfix))
    }

    // - Boolean expressions
    //
    //   true   -> \mathsf{true}

    fn of_bool_exp(value: bool) -> ExpTerm {
        let tex = Doc::Styled(Style::Mathsf, value.to_string());
        ExpTerm::atomic(tex)
    }

    // - Numeric expressions
    //
    //   0xFF   -> \mathtt{0xff}

    fn of_num_exp(op: NumOp, num: &Num) -> ExpTerm {
        let tex = Doc::of_number(op, num);
        ExpTerm::atomic(tex)
    }

    // - Text expressions
    //
    //   "ok"   -> \texttt{"ok"}

    fn of_text_exp(text: &str) -> ExpTerm {
        let tex = Doc::Styled(Style::Texttt, format!("\"{text}\""));
        ExpTerm::atomic(tex)
    }

    // - Variable expressions
    //
    //   t_sep   -> {\mathsf{t}}_{\mathsf{sep}}

    fn of_var_exp(id: &Id) -> ExpTerm {
        let tex = Doc::of_varid(id);
        ExpTerm::atomic(tex)
    }

    // - Unary expressions
    //
    //   ~b   -> \neg \mathsf{b}

    fn of_un_exp(op: UnOp, exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let term = ExpTerm::of_exp(exp, anchors)?;
        let tex_op = Doc::of_unop(op);
        let tex_exp = Doc::of_nested_exp(precedence::UNARY, Side::Right, term);
        let tex = Doc::concat_spaced(vec![tex_op, tex_exp]);
        Ok(ExpTerm::new(tex, Category::Unary))
    }

    // - Binary expressions
    //
    //   x + y      -> \mathsf{x} + \mathsf{y}
    //   $(x ^ y)   -> {\mathsf{x}}^{\mathsf{y}}

    fn of_bin_exp(
        exp_l: &Exp,
        op: BinOp,
        exp_r: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        match BinopTerm::of_binop(op) {
            BinopTerm::Infix(tex_op) => {
                let prec = precedence::of_binop(op);
                ExpTerm::of_binary_exp(prec, tex_op, exp_l, exp_r, anchors)
            }
            BinopTerm::Exponent => ExpTerm::of_pow_exp(op, exp_l, exp_r, anchors),
        }
    }

    /// Renders exponentiation as a superscript on the base.
    fn of_pow_exp(
        op: BinOp,
        exp_l: &Exp,
        exp_r: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let prec = precedence::of_binop(op);
        let term_l = ExpTerm::of_exp(exp_l, anchors)?;
        let term_r = ExpTerm::of_exp(exp_r, anchors)?;
        let tex_l = Doc::of_nested_exp(prec, Side::Left, term_l);
        let tex_r = Doc::of_nested_exp(prec, Side::Right, term_r);
        let tex = Doc::sup(tex_l, tex_r);
        Ok(ExpTerm::new(tex, Category::Power))
    }

    // - Comparison expressions
    //
    //   $(x <= y)   -> \mathsf{x} \le \mathsf{y}

    fn of_cmp_exp(
        exp_l: &Exp,
        op: CmpOp,
        exp_r: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let prec = precedence::of_cmpop(op);
        let tex_op = Doc::of_cmpop(op);
        ExpTerm::of_binary_exp(prec, tex_op, exp_l, exp_r, anchors)
    }

    // - Empty-sequence expressions
    //
    //   eps   -> \epsilon

    fn of_eps_exp() -> ExpTerm {
        let tex = Doc::Fixed(Symbol::Epsilon);
        ExpTerm::atomic(tex)
    }

    // - List expressions
    //
    //   [x, y]   -> \left[\mathsf{x}, \mathsf{y}\right]

    fn of_list_exp(exps: &[Exp], anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let texs = Doc::of_exps(exps, anchors)?;
        let tex_elems = Doc::layout_group_soft_comma_separated(texs);
        let tex = Doc::delimited(Delimiter::Bracket, tex_elems);
        Ok(ExpTerm::atomic(tex))
    }

    // - Cons expressions
    //
    //   x :: xs   -> \mathsf{x} \mathbin{::} \mathsf{xs}

    fn of_cons_exp(exp_l: &Exp, exp_r: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_op = Doc::mathbin(Doc::Fixed(Symbol::DoubleColon));
        ExpTerm::of_binary_exp(precedence::CONS, tex_op, exp_l, exp_r, anchors)
    }

    // - Concatenation expressions
    //
    //   xs ++ ys   -> \mathsf{xs} \mathbin{+\!\!+} \mathsf{ys}

    fn of_cat_exp(exp_l: &Exp, exp_r: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_op = Doc::mathbin(Doc::Fixed(Symbol::Cat));
        ExpTerm::of_binary_exp(precedence::CAT, tex_op, exp_l, exp_r, anchors)
    }

    // - Index expressions
    //
    //   xs[i]   -> \mathsf{xs}\left[\mathsf{i}\right]

    fn of_idx_exp(exp_base: &Exp, exp_idx: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_suffix = Doc::of_idx_suffix(exp_idx, anchors)?;
        ExpTerm::of_postfix_exp(exp_base, tex_suffix, anchors)
    }

    // - Slice expressions
    //
    //   xs[i : n]   -> \mathsf{xs}\left[\mathsf{i} : \mathsf{n}\right]

    fn of_slice_exp(
        exp_base: &Exp,
        exp_idx: &Exp,
        exp_len: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let tex_suffix = Doc::of_slice_suffix(exp_idx, exp_len, anchors)?;
        ExpTerm::of_postfix_exp(exp_base, tex_suffix, anchors)
    }

    // - Length expressions
    //
    //   |xs|   -> \left|\mathsf{xs}\right|

    fn of_len_exp(exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_exp = Doc::of_exp(exp, anchors)?;
        let tex = Doc::delimited(Delimiter::Bar, tex_exp);
        Ok(ExpTerm::new(tex, Category::Unary))
    }

    // - Membership expressions
    //
    //   x <- xs   -> \mathsf{x} \in \mathsf{xs}

    fn of_mem_exp(exp_l: &Exp, exp_r: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_op = Doc::Fixed(Symbol::In);
        ExpTerm::of_binary_exp(precedence::COMPARISON, tex_op, exp_l, exp_r, anchors)
    }
}

impl Doc {
    // - Record expressions
    //
    //   {A x, B y}   -> \left\{\mathsf{A} \mathsf{x}, \mathsf{B} \mathsf{y}\right\}

    fn of_exp_field(atom: &Atom, exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_atom = Doc::of_atom(atom);
        let tex_exp = Doc::of_exp(exp, anchors)?;
        let tex_field = Doc::concat_spaced(vec![tex_atom, tex_exp]);
        Ok(tex_field)
    }
}

impl ExpTerm {
    fn of_str_exp(exp_fields: &[(Atom, Exp)], anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let texs = exp_fields
            .iter()
            .map(|(atom, exp)| Doc::of_exp_field(atom, exp, anchors))
            .collect::<Result<Vec<_>>>()?;
        let tex_fields = Doc::layout_group_soft_comma_separated(texs);
        let tex = Doc::delimited(Delimiter::Brace, tex_fields);
        Ok(ExpTerm::atomic(tex))
    }

    // - Field-access expressions
    //
    //   x.A   -> {\mathsf{x}}_{\mathsf{A}}

    /// Attaches a visible field as a subscript of its base.
    fn of_dot_exp(exp_base: &Exp, atom: &Atom, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_field = Doc::of_atom(atom);
        // An invisible field preserves the preceding path without a dot
        if tex_field.is_empty() {
            return ExpTerm::of_exp(exp_base, anchors);
        }
        let term_base = ExpTerm::of_exp(exp_base, anchors)?;
        let tex_base = Doc::of_nested_exp(precedence::POSTFIX, Side::Left, term_base);
        let tex = Doc::sub(tex_base, tex_field);
        Ok(ExpTerm::new(tex, Category::Postfix))
    }

    // - Update expressions
    //
    //   x[.A = y]   -> \mathsf{x}\left[\mathsf{A} = \mathsf{y}\right]

    fn of_upd_exp(
        exp_base: &Exp,
        path: &Path,
        exp_field: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let tex_path = Doc::of_path(path, anchors)?;
        let tex_field = Doc::of_exp(exp_field, anchors)?;
        let tex_body = Doc::concat_spaced(vec![tex_path, Doc::Fixed(Symbol::Equal), tex_field]);
        let tex_suffix = Doc::delimited(Delimiter::Bracket, tex_body);
        ExpTerm::of_postfix_exp(exp_base, tex_suffix, anchors)
    }

    // - Parenthesized expressions
    //
    //   (x)   -> \left(\mathsf{x}\right)

    fn of_paren_exp(exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let tex_exp = Doc::of_exp(exp, anchors)?;
        let tex = Doc::parenthesized(tex_exp);
        Ok(ExpTerm::atomic(tex))
    }

    // - Tuple expressions
    //
    //   (x, y)   -> \left(\mathsf{x}, \mathsf{y}\right)

    fn of_tuple_exp(exps: &[Exp], anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let texs = Doc::of_exps(exps, anchors)?;
        let tex_elems = Doc::layout_group_soft_comma_separated(texs);
        let tex = Doc::parenthesized(tex_elems);
        Ok(ExpTerm::atomic(tex))
    }

    // - Function calls
    //
    //   $g(x, y), with anchor g   -> \href{#g}{\mathrm{g}}\left(\mathsf{x}, \mathsf{y}\right)

    /// Links the function name when its anchor resolves.
    fn of_call_exp(
        id: &Id,
        targs: &[Targ],
        args: &[Arg],
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let anchor = anchor_of_func(anchors, id);
        let tex_name = Doc::of_defid(id);
        let tex_name = Doc::of_link(anchor.as_deref(), tex_name)?;
        let tex_targs = Doc::of_targs(targs);
        let tex_args = Doc::of_args(args, anchors)?;
        let tex = Doc::concat(vec![tex_name, tex_targs, tex_args]);
        Ok(ExpTerm::atomic(tex))
    }

    // - Iterated expressions
    //
    //   x*   -> {\mathsf{x}}^{\ast}
    //   x?   -> {\mathsf{x}}^{?}

    fn of_iter_exp(exp: &Exp, iter: Iter, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let term = ExpTerm::of_exp(exp, anchors)?;
        let tex_base = Doc::of_nested_exp(precedence::POSTFIX, Side::Left, term);
        let tex_iter = Doc::of_iter(iter);
        let tex = Doc::sup(tex_base, tex_iter);
        Ok(ExpTerm::new(tex, Category::Postfix))
    }

    // - Subtype expressions
    //
    //   x <: nat   -> \mathsf{x} \mathrel{<:} \mathbb{N}

    fn of_sub_exp(
        exp: &Exp,
        plain_typ: &PlainTyp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let term = ExpTerm::of_exp(exp, anchors)?;
        let tex_l = Doc::of_nested_exp(precedence::SUBTYPE, Side::Left, term);
        let tex_op = Doc::of_rel_symbols(Symbol::Less, Symbol::Colon);
        let tex_r = Doc::of_plaintyp(plain_typ);
        let tex = Doc::of_breakable_infix(tex_l, tex_op, tex_r);
        Ok(ExpTerm::new(tex, Category::Colon))
    }

    // - Atom expressions
    //
    //   IF   -> \mathsf{IF}

    fn of_atom_exp(atom: &Atom) -> ExpTerm {
        let tex = Doc::of_atom(atom);
        ExpTerm::atomic(tex)
    }

    // - Expression sequences
    //
    //   A x   -> \mathsf{A}\,\mathsf{x}

    /// Packs notation terms with thin spaces and right-operand parenthesization.
    fn of_seq_exp(exps: &[Exp], anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        let texs = exps
            .iter()
            .map(|exp| {
                let term = ExpTerm::of_exp(exp, anchors)?;
                let tex = Doc::of_nested_exp(precedence::SEQUENCE, Side::Right, term);
                Ok(tex)
            })
            .collect::<Result<Vec<_>>>()?;
        let tex = Doc::fill(0, Doc::ThinSpace, texs);
        Ok(ExpTerm::new(tex, Category::Sequence))
    }
}

// - Infix expressions
//
//   p |- e      -> \mathsf{p} \mathrel{\vdash} \mathsf{e}
//   x ->_ n y   -> \mathsf{x} {\to}_{\mathsf{n}} \mathsf{y}

impl ExpTerm {
    /// Keeps the right operand that a subscripted arrow leaves after its subscript.
    fn of_arrow_operand(exp_r: &Exp, anchors: Option<&Anchors<'_>>) -> Result<ExpTerm> {
        match &exp_r.node {
            // Preserve the sequence category even when its tail is empty
            ExpKind::Seq(exps) => match exps.split_first() {
                Some((_, exps_tail)) => ExpTerm::of_seq_exp(exps_tail, anchors),
                None => Ok(ExpTerm::atomic(Doc::Empty)),
            },
            // A singleton right operand becomes only a subscript
            _ => Ok(ExpTerm::atomic(Doc::Empty)),
        }
    }
}

impl Doc {
    /// Moves the first right-hand term of a subscripted arrow into its subscript.
    fn of_arrow_subscript(tex_op: Doc, exp_r: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let exp_sub = match &exp_r.node {
            ExpKind::Seq(exps) => {
                // An empty sequence leaves the arrow without a subscript
                let Some((exp_sub, _)) = exps.split_first() else {
                    return Ok(tex_op);
                };
                exp_sub
            }
            _ => exp_r,
        };
        let tex_sub = Doc::of_exp(exp_sub, anchors)?;
        Ok(Doc::sub(tex_op, tex_sub))
    }
}

impl ExpTerm {
    /// Renders notation infix, with arrow subscripts split from the right operand.
    fn of_infix_exp(
        exp_l: &Exp,
        atom: &Atom,
        exp_r: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let prec = precedence::of_infix(&atom.node);
        let term_l = ExpTerm::of_exp(exp_l, anchors)?;
        let tex_atom = Doc::of_atom(atom);
        let is_subscripted = matches!(atom.node, AtomKind::ArrowSub | AtomKind::DoubleArrowSub);
        // Subscripted arrows consume the first right-hand term
        let term_r = if is_subscripted {
            ExpTerm::of_arrow_operand(exp_r, anchors)?
        } else {
            ExpTerm::of_exp(exp_r, anchors)?
        };
        let tex_op = if is_subscripted {
            Doc::of_arrow_subscript(tex_atom, exp_r, anchors)?
        } else {
            tex_atom
        };
        // Apply the original operator precedence to the visible operands
        let tex_l = Doc::of_nested_exp(prec, Side::Left, term_l);
        let tex_r = Doc::of_nested_exp(prec, Side::Right, term_r);
        let tex = Doc::of_breakable_infix(tex_l, tex_op, tex_r);
        Ok(ExpTerm::new(tex, prec.category))
    }

    // - Bracketed expressions
    //
    //   `[ x `]   -> \left[\mathsf{x}\right]

    fn of_brack_exp(
        atom_l: &Atom,
        exp: &Exp,
        atom_r: &Atom,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<ExpTerm> {
        let tex_body = Doc::of_exp(exp, anchors)?;
        let tex = Doc::of_bracket(atom_l, tex_body, atom_r);
        Ok(ExpTerm::atomic(tex))
    }
}

// == Paths

impl Doc {
    // - Path
    //
    //   x[.A[i] = y]   -> \mathsf{x}\left[\mathsf{A}\left[\mathsf{i}\right] = \mathsf{y}\right]

    /// Renders update paths, omitting the dot before a root field.
    fn of_path(path: &Path, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        match &path.node {
            PathKind::Root => Ok(Doc::of_root_path()),
            PathKind::Idx(path, exp_idx) => Doc::of_idx_path(path, exp_idx, anchors),
            PathKind::Slice(path, exp_idx, exp_len) => {
                Doc::of_slice_path(path, exp_idx, exp_len, anchors)
            }
            PathKind::Dot(path, atom) => Doc::of_dot_path(path, atom, anchors),
        }
    }

    // - Root paths
    //
    //   Root   -> Empty

    fn of_root_path() -> Doc {
        Doc::Empty
    }

    // - Index paths
    //
    //   .A[i]   -> \mathsf{A}\left[\mathsf{i}\right]

    fn of_idx_path(path: &Path, exp_idx: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_path = Doc::of_path(path, anchors)?;
        let tex_suffix = Doc::of_idx_suffix(exp_idx, anchors)?;
        let tex = Doc::concat(vec![tex_path, tex_suffix]);
        Ok(tex)
    }

    // - Slice paths
    //
    //   .A[i : n]   -> \mathsf{A}\left[\mathsf{i} : \mathsf{n}\right]

    fn of_slice_path(
        path: &Path,
        exp_idx: &Exp,
        exp_len: &Exp,
        anchors: Option<&Anchors<'_>>,
    ) -> Result<Doc> {
        let tex_path = Doc::of_path(path, anchors)?;
        let tex_suffix = Doc::of_slice_suffix(exp_idx, exp_len, anchors)?;
        let tex = Doc::concat(vec![tex_path, tex_suffix]);
        Ok(tex)
    }

    // - Field paths
    //
    //   .A     -> \mathsf{A}
    //   .A.B   -> \mathsf{A}.\mathsf{B}

    fn of_dot_path(path: &Path, atom: &Atom, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_field = Doc::of_atom(atom);
        // A root field has no preceding path to separate
        if matches!(path.node, PathKind::Root) {
            return Ok(tex_field);
        }
        let tex_path = Doc::of_path(path, anchors)?;
        // An invisible field preserves the preceding path without a dot
        if tex_field.is_empty() {
            return Ok(tex_path);
        }
        let tex = Doc::concat(vec![tex_path, Doc::Fixed(Symbol::Dot), tex_field]);
        Ok(tex)
    }
}

// == Arguments and parameters

impl Doc {
    // - Type parameters
    //
    //   <T, U>   -> \left\langle\mathsf{T}, \mathsf{U}\right\rangle

    fn of_tparams(tparams: &[TParam]) -> Doc {
        if tparams.is_empty() {
            return Doc::Empty;
        }
        let texs = tparams.iter().map(Doc::of_typid).collect();
        let tex_tparams = Doc::layout_group_soft_comma_separated(texs);
        Doc::delimited(Delimiter::Angle, tex_tparams)
    }

    // - Arguments
    //
    //   (x, y)   -> \left(\mathsf{x}, \mathsf{y}\right)

    fn of_arg(arg: &Arg, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        match &arg.node {
            ArgKind::Exp(exp) => Doc::of_exp(exp, anchors),
            ArgKind::Def(id) => Ok(Doc::of_defid(id)),
        }
    }

    fn of_args(args: &[Arg], anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let texs = args
            .iter()
            .map(|arg| Doc::of_arg(arg, anchors))
            .collect::<Result<Vec<_>>>()?;
        let tex_args = Doc::layout_group_soft_comma_separated(texs);
        let tex = Doc::parenthesized(tex_args);
        Ok(tex)
    }

    // - Parameters
    //
    //   (nat, def $h(nat) : nat)   -> \left(\mathbb{N}, \mathrm{h}\left(\mathbb{N}\right) : \mathbb{N}\right)

    fn of_param(param: &Param) -> Doc {
        match &param.node {
            ParamKind::Exp(plain_typ) => Doc::of_plaintyp(plain_typ),
            ParamKind::Def(id, tparams, params, plain_typ) => {
                Doc::of_func_signature(id, tparams, params, plain_typ, None)
            }
        }
    }

    fn of_params(params: &[Param]) -> Doc {
        let texs = params.iter().map(Doc::of_param).collect();
        let tex_params = Doc::layout_group_soft_comma_separated(texs);
        Doc::parenthesized(tex_params)
    }
}

// == Premises

impl Doc {
    // - Premise
    //
    //   -- if x = y      -> \mathsf{x} = \mathsf{y}
    //   -- var z : nat   -> \mathsf{z} : \mathbb{N}
    //   -- otherwise     -> \mathrm{otherwise}

    /// Renders one premise; relation references link to their anchors.
    fn of_prem(prem: &Prem, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        match &prem.node {
            PremKind::Var(VarPrem { id, plain_typ }) => Ok(Doc::of_var_prem(id, plain_typ)),
            PremKind::Rule(RulePrem { id, exp }) => Doc::of_rule_prem(id, exp, anchors),
            PremKind::RuleNot(RuleNotPrem { id, exp }) => Doc::of_rule_not_prem(id, exp, anchors),
            PremKind::If(IfPrem { exp }) => Doc::of_exp(exp, anchors),
            PremKind::Else => Ok(Doc::of_else_prem()),
            PremKind::Iter(IterPrem { prem, iter }) => Doc::of_iter_prem(prem, *iter, anchors),
            PremKind::Debug(DebugPrem { exp }) => Doc::of_debug_prem(exp, anchors),
        }
    }

    /// Renders premises, dropping those with no visible content.
    fn of_prems(prems: &[Prem], anchors: Option<&Anchors<'_>>) -> Result<Vec<Doc>> {
        let texs = prems
            .iter()
            .map(|prem| Doc::of_prem(prem, anchors))
            .collect::<Result<Vec<_>>>()?;
        let texs = texs.into_iter().filter(|tex| !tex.is_empty()).collect();
        Ok(texs)
    }

    // - Variable premises
    //
    //   -- var z : nat   -> \mathsf{z} : \mathbb{N}

    fn of_var_prem(id: &Id, plain_typ: &PlainTyp) -> Doc {
        let tex_var = Doc::of_varid(id);
        let tex_typ = Doc::of_plaintyp(plain_typ);
        Doc::concat_spaced(vec![tex_var, Doc::Fixed(Symbol::Colon), tex_typ])
    }

    // - Relation premises
    //
    //   -- Sub: x <: y, with anchor Sub   -> \href{#Sub}{\mathsf{x} \mathrel{<:} \mathsf{y}}

    fn of_rule_prem(id: &Id, exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let anchor = anchor_of_rel(anchors, id);
        let tex_exp = Doc::of_exp(exp, anchors)?;
        Doc::of_link(anchor.as_deref(), tex_exp)
    }

    // - Negated relation premises
    //
    //   -- Eval:/ p |- e : t, with anchor Eval
    //   -> \neg \href{#Eval}{\left(\mathsf{p} \mathrel{\vdash} \mathsf{e} : \mathsf{t}\right)}

    fn of_rule_not_prem(id: &Id, exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let anchor = anchor_of_rel(anchors, id);
        let term = ExpTerm::of_exp(exp, anchors)?;
        let tex_exp = Doc::of_nested_exp(precedence::UNARY, Side::Right, term);
        let tex_exp = Doc::of_link(anchor.as_deref(), tex_exp)?;
        let tex = Doc::concat_spaced(vec![Doc::Fixed(Symbol::Neg), tex_exp]);
        Ok(tex)
    }

    // - Otherwise premises
    //
    //   -- otherwise   -> \mathrm{otherwise}

    fn of_else_prem() -> Doc {
        Doc::Styled(Style::Mathrm, "otherwise".to_owned())
    }

    // - Iterated premises
    //
    //   -- (if x)*   -> {\left(\mathsf{x}\right)}^{\ast}

    /// Parenthesizes a premise before its iteration unless it is already iterated.
    fn of_iter_prem(prem: &Prem, iter: Iter, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_prem = Doc::of_prem(prem, anchors)?;
        let tex_base = if matches!(prem.node, PremKind::Iter(_)) {
            tex_prem
        } else {
            Doc::parenthesized(tex_prem)
        };
        let tex_iter = Doc::of_iter(iter);
        let tex = Doc::sup(tex_base, tex_iter);
        Ok(tex)
    }

    // - Debug premises
    //
    //   -- debug x   -> \mathrm{debug} \mathsf{x}

    fn of_debug_prem(exp: &Exp, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let tex_debug = Doc::Styled(Style::Mathrm, "debug".to_owned());
        let tex_exp = Doc::of_exp(exp, anchors)?;
        let tex = Doc::concat_spaced(vec![tex_debug, tex_exp]);
        Ok(tex)
    }
}

// == Definition annotations
//
//   Doc::of_annotated(\mathsf{t}, "syntax")   -> \mathsf{t} \quad \text{syntax}

impl Doc {
    /// Appends a definition-kind label after a quad.
    fn of_annotated(tex_body: Doc, annotation: &str) -> Doc {
        let tex_annotation = Doc::Styled(Style::Text, annotation.to_owned());
        Doc::concat_spaced(vec![tex_body, Doc::Quad, tex_annotation])
    }
}

// == Syntax definitions

impl Doc {
    // - External syntax definition
    //
    //   extern syntax t   -> \mathsf{t} \quad \text{external syntax}

    fn of_extern_syntax_def(id: &Id) -> Doc {
        let tex_name = Doc::of_typid(id);
        Doc::of_annotated(tex_name, "external syntax")
    }

    // - Syntax definition
    //
    //   syntax t, u   -> \mathsf{t}, \mathsf{u} \quad \text{syntax}

    /// Lists declared type names, or the empty set for an empty declaration.
    fn of_syntax_def(entries: &[SyntaxDefEntry]) -> Doc {
        let tex_entries = if entries.is_empty() {
            Doc::Fixed(Symbol::EmptySet)
        } else {
            let texs = entries
                .iter()
                .map(|entry| Doc::of_typ_head(&entry.id, &entry.tparams))
                .collect();
            Doc::concat_comma_separated(texs)
        };
        Doc::of_annotated(tex_entries, "syntax")
    }
}

// == Type definitions

impl Doc {
    // - Type definition
    //
    //   syntax t = nat   -> \mathsf{t} \mathrel{::=} \mathbb{N}
    //
    //   syntax t = | A | B nat
    //   -> \begin{aligned}
    //      \mathsf{t} & \mathrel{::=} & \mathsf{A} \\
    //       & \mathrel{|} & \mathsf{B}\,\mathbb{N}
    //      \end{aligned}

    /// Aligns variant alternatives beneath their production operator.
    fn of_typ_def(id: &Id, tparams: &[TParam], def_typ: &DefTyp) -> Doc {
        let tex_l = Doc::of_typ_head(id, tparams);
        let tex_production = Doc::mathrel(Doc::Fixed(Symbol::Production));

        // Non-variant definitions keep their body on the production line
        let DefTypKind::Variant(typ_cases) = &def_typ.node else {
            let tex_r = Doc::of_deftyp(def_typ);
            return Doc::concat_spaced(vec![tex_l, tex_production, tex_r]);
        };

        // An empty variant denotes the empty set
        let Some((typ_case, typ_cases)) = typ_cases.split_first() else {
            return Doc::concat_spaced(vec![tex_l, tex_production, Doc::Fixed(Symbol::EmptySet)]);
        };

        // Continue each alternative in the operator and body columns
        let tex_case = Doc::of_typ(&typ_case.typ);
        let mut rows = vec![vec![tex_l, tex_production, tex_case]];
        for typ_case in typ_cases {
            let tex_alternative = Doc::mathrel(Doc::Fixed(Symbol::VerticalBar));
            let tex_case = Doc::of_typ(&typ_case.typ);
            rows.push(vec![Doc::Empty, tex_alternative, tex_case]);
        }
        Doc::Aligned(rows)
    }
}

// == Meta-variable definitions
//
//   var x : nat   -> \mathsf{x} : \mathbb{N}

impl Doc {
    fn of_var_def(id: &Id, plain_typ: &PlainTyp) -> Doc {
        let tex_var = Doc::of_varid(id);
        let tex_typ = Doc::of_plaintyp(plain_typ);
        Doc::concat_spaced(vec![tex_var, Doc::Fixed(Symbol::Colon), tex_typ])
    }
}

// == Relation definitions

impl Doc {
    // - Relation signature
    //
    //   relation Eval: p |- e : t          -> \mathrm{Eval} : \mathsf{p} \mathrel{\vdash} \mathsf{e} : \mathsf{t}
    //   extern relation Eval: p |- e : t   -> ... : \mathsf{t} \quad \text{external}

    fn of_rel_signature(id: &Id, not_typ: &NotTyp, annotation: Option<&str>) -> Doc {
        let tex_name = Doc::of_defid(id);
        let tex_typ = Doc::of_nottyp(not_typ);
        let tex_signature = Doc::concat_spaced(vec![tex_name, Doc::Fixed(Symbol::Colon), tex_typ]);
        match annotation {
            None => tex_signature,
            Some(annotation) => Doc::of_annotated(tex_signature, annotation),
        }
    }

    // - External relation definition
    //
    //   extern relation Eval: p |- e : t   -> ... : \mathsf{t} \quad \text{external}

    fn of_extern_rel_def(id: &Id, not_typ: &NotTyp) -> Doc {
        Doc::of_rel_signature(id, not_typ, Some("external"))
    }

    // - Relation definition
    //
    //   relation Eval: p |- e : t   -> \mathrm{Eval} : \mathsf{p} \mathrel{\vdash} \mathsf{e} : \mathsf{t}

    fn of_rel_def(id: &Id, not_typ: &NotTyp) -> Doc {
        Doc::of_rel_signature(id, not_typ, None)
    }
}

// - Rule
//
//   rule Eval/ok: p |- e : t -- if x = y -- Sub: x <: y
//   -> LeftStack([Badge("Eval-ok"), Displaystyle(Fraction(Numbered([x = y, x <: y]), p |- e : t))])

/// Joins a relation and rule identifier, as in `Eval-ok`.
fn text_of_rule_id(id_rel: &Id, id_rule: &Id) -> String {
    if id_rule.node.is_empty() {
        id_rel.node.clone()
    } else {
        format!("{}-{}", id_rel.node, id_rule.node)
    }
}

impl Doc {
    /// Places a rule label above its inference fraction and numbers multiple premises.
    fn of_rule(rule: &Rule, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let RuleKind { id_rel, id_rule, exp, prems } = &rule.node;

        // A single premise needs no numbered gutter
        let mut texs_prem = Doc::of_prems(prems, anchors)?;
        let tex_numerator =
            if texs_prem.len() == 1 { texs_prem.remove(0) } else { Doc::numbered(texs_prem) };

        // Resolve the inference fraction at the layout width
        let tex_conclusion = Doc::of_exp(exp, anchors)?;
        let tex_fraction = Doc::fraction(tex_numerator, tex_conclusion);
        let tex_fraction = Doc::displaystyle(tex_fraction);
        let tex_fraction = layout::resolve(WIDTH_LAYOUT, &tex_fraction)?;

        // Keep the full rule identifier in the badge above the inference rule
        let text_badge = text_of_rule_id(id_rel, id_rule);
        let tex_badge = Doc::badge(text_badge);
        let tex = Doc::left_stack(vec![tex_badge, tex_fraction]);
        Ok(tex)
    }

    // - Rule group definition
    //
    //   []                 -> \mathrm{Eval} : \varnothing \quad \text{rules}
    //   [rule]             -> the rule alone
    //   [rule_a, rule_b]   -> Gathered([Line(rule_a), Gap, Line(rule_b)])

    /// Separates multiple inference rules while retaining empty-group annotations.
    fn of_rulegroup(
        id_rel: &Id,
        id_group: &Id,
        rules: &[Rule],
        anchors: Option<&Anchors<'_>>,
    ) -> Result<Doc> {
        match rules {
            // An empty rule group remains visible as a named empty set
            [] => {
                let text_name = text_of_rule_id(id_rel, id_group);
                let tex_name = Doc::Styled(Style::Mathrm, text_name);
                let tex_empty = Doc::concat_spaced(vec![
                    tex_name,
                    Doc::Fixed(Symbol::Colon),
                    Doc::Fixed(Symbol::EmptySet),
                ]);
                Ok(Doc::of_annotated(tex_empty, "rules"))
            }
            // A single rule has no surrounding gathered document
            [rule] => Doc::of_rule(rule, anchors),
            // Multiple rules receive one gap between their blocks
            rules => {
                let mut blocks = Vec::new();
                for rule in rules {
                    if !blocks.is_empty() {
                        blocks.push(Block::Gap);
                    }
                    let tex_rule = Doc::of_rule(rule, anchors)?;
                    blocks.push(Block::Line(tex_rule));
                }
                Ok(Doc::gathered(blocks))
            }
        }
    }
}

// == Meta-function definitions

impl Doc {
    // - Function signature
    //
    //   dec $g<T>(nat) : T         -> \mathrm{g}\left\langle\mathsf{T}\right\rangle\left(\mathbb{N}\right) : \mathsf{T}
    //   extern dec $g(nat) : nat   -> \mathrm{g}\left(\mathbb{N}\right) : \mathbb{N} \quad \text{external}

    fn of_func_signature(
        id: &Id,
        tparams: &[TParam],
        params: &[Param],
        plain_typ: &PlainTyp,
        annotation: Option<&str>,
    ) -> Doc {
        let tex_name = Doc::of_defid(id);
        let tex_tparams = Doc::of_tparams(tparams);
        let tex_params = Doc::of_params(params);
        let tex_head = Doc::concat(vec![tex_name, tex_tparams, tex_params]);
        let tex_result = Doc::of_plaintyp(plain_typ);
        let tex_signature =
            Doc::concat_spaced(vec![tex_head, Doc::Fixed(Symbol::Colon), tex_result]);
        match annotation {
            None => tex_signature,
            Some(annotation) => Doc::of_annotated(tex_signature, annotation),
        }
    }

    // - External function declaration
    //
    //   extern dec $g(nat) : nat   -> \mathrm{g}\left(\mathbb{N}\right) : \mathbb{N} \quad \text{external}

    fn of_extern_dec_def(
        id: &Id,
        tparams: &[TParam],
        params: &[Param],
        plain_typ: &PlainTyp,
    ) -> Doc {
        Doc::of_func_signature(id, tparams, params, plain_typ, Some("external"))
    }

    // - Builtin function declaration
    //
    //   builtin dec $g(nat) : nat   -> \mathrm{g}\left(\mathbb{N}\right) : \mathbb{N} \quad \text{builtin}

    fn of_builtin_dec_def(
        id: &Id,
        tparams: &[TParam],
        params: &[Param],
        plain_typ: &PlainTyp,
    ) -> Doc {
        Doc::of_func_signature(id, tparams, params, plain_typ, Some("builtin"))
    }

    // - Table function declaration
    //
    //   tbl dec $g(nat) : nat   -> \mathrm{g}\left(\mathbb{N}\right) : \mathbb{N} \quad \text{table}

    fn of_table_dec_def(id: &Id, params: &[Param], plain_typ: &PlainTyp) -> Doc {
        Doc::of_func_signature(id, &[], params, plain_typ, Some("table"))
    }

    // - Function declaration
    //
    //   dec $g(nat) : nat   -> \mathrm{g}\left(\mathbb{N}\right) : \mathbb{N}

    fn of_func_dec_def(id: &Id, tparams: &[TParam], params: &[Param], plain_typ: &PlainTyp) -> Doc {
        Doc::of_func_signature(id, tparams, params, plain_typ, None)
    }

    // - Table function definition
    //
    //   tbl def $g = | x => y | z => w
    //   -> \begin{aligned}
    //      \mathrm{g}\left(\mathsf{x}\right) & \mapsto & \mathsf{y} \\
    //      \mathrm{g}\left(\mathsf{z}\right) & \mapsto & \mathsf{w}
    //      \end{aligned}

    /// Renders one table row as a parenthesized pattern mapped to its result.
    fn of_table_row(id: &Id, row: &TableRow, anchors: Option<&Anchors<'_>>) -> Result<Vec<Doc>> {
        let tex_pattern = Doc::of_exp(&row.node.exp_pattern, anchors)?;
        let tex_pattern = Doc::parenthesized(tex_pattern);
        let tex_name = Doc::of_defid(id);
        let tex_l = Doc::concat(vec![tex_name, tex_pattern]);
        let tex_r = Doc::of_exp(&row.node.exp_body, anchors)?;
        Ok(vec![tex_l, Doc::Fixed(Symbol::Mapsto), tex_r])
    }

    /// Renders table rows in aligned mapsto rows.
    fn of_table(id: &Id, rows: &[TableRow], anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        // Empty tables retain their name and declaration category
        if rows.is_empty() {
            let tex_name = Doc::of_defid(id);
            let tex_empty = Doc::concat_spaced(vec![
                tex_name,
                Doc::Fixed(Symbol::Colon),
                Doc::Fixed(Symbol::EmptySet),
            ]);
            return Ok(Doc::of_annotated(tex_empty, "table"));
        }
        let rows_aligned = rows
            .iter()
            .map(|row| Doc::of_table_row(id, row, anchors))
            .collect::<Result<Vec<_>>>()?;
        Ok(Doc::Aligned(rows_aligned))
    }
}

// - Defined function definition
//
//   def $g(x) = y -- if x = y
//   -> \mathrm{g}\left(\mathsf{x}\right) & = & \mathsf{y} \quad \text{if}\,\mathsf{x} = \mathsf{y}
//
//   A clause wider than 80 columns moves its condition to a spanning row below.

/// Layout of one function clause.
enum LayoutFunc {
    /// Equation with its condition inline, as one row.
    Single(Vec<Doc>),
    /// Equation row followed by its condition on a spanning row.
    Multi(Vec<Doc>, Doc),
}

impl LayoutFunc {
    /// Moves a condition below its equation only when the inline form exceeds width 80.
    fn of_func_def(def: &FuncDef, anchors: Option<&Anchors<'_>>) -> Result<LayoutFunc> {
        let tex_name = Doc::of_defid(&def.id);
        let tex_tparams = Doc::of_tparams(&def.tparams);
        let tex_args = Doc::of_args(&def.args, anchors)?;
        let tex_l = Doc::concat(vec![tex_name, tex_tparams, tex_args]);
        let tex_body = Doc::of_exp(&def.exp, anchors)?;
        let texs_prem = Doc::of_prems(&def.prems, anchors)?;

        // Measure the full inline equation before choosing a continuation layout
        let tex_prem = match texs_prem.as_slice() {
            [] => Doc::Empty,
            [tex] => tex.clone(),
            texs => Doc::numbered(texs.to_vec()),
        };
        let tex_r = if texs_prem.is_empty() {
            tex_body.clone()
        } else {
            let tex_if = Doc::Styled(Style::Text, "if".to_owned());
            let tex_condition = Doc::concat_juxtaposed(vec![tex_if, tex_prem.clone()]);
            Doc::concat_spaced(vec![tex_body.clone(), Doc::Quad, tex_condition])
        };
        let docs_inline = vec![tex_l.clone(), Doc::Fixed(Symbol::Equal), tex_r];
        let tex_inline = Doc::Aligned(vec![docs_inline.clone()]);
        if width::flat(&tex_inline) <= WIDTH_LAYOUT {
            return Ok(LayoutFunc::Single(docs_inline));
        }

        // Resolve the two sides before placing a condition on its own spanning row
        let tex_l = layout::resolve(WIDTH_LAYOUT, &tex_l)?;
        let tex_body = layout::resolve(WIDTH_LAYOUT, &tex_body)?;
        let docs = vec![tex_l, Doc::Fixed(Symbol::Equal), tex_body];
        if texs_prem.is_empty() {
            return Ok(LayoutFunc::Single(docs));
        }

        // Reserve the prefix width when breaking the premise block
        let tex_if = Doc::Styled(Style::Text, "if".to_owned());
        let tex_prefix = Doc::concat(vec![Doc::Quad, tex_if, Doc::ThinSpace]);
        let width_prem = WIDTH_LAYOUT - width::flat(&tex_prefix);
        let tex_prem = layout::resolve(width_prem, &tex_prem)?;
        let tex_condition = Doc::concat(vec![tex_prefix, tex_prem]);
        let tex_condition = Doc::layout_group(tex_condition);
        Ok(LayoutFunc::Multi(docs, tex_condition))
    }
}

impl GridRow {
    fn of_layout_func(layout_func: LayoutFunc) -> Vec<GridRow> {
        match layout_func {
            LayoutFunc::Single(docs) => vec![GridRow::Cells(docs)],
            LayoutFunc::Multi(docs, doc) => {
                vec![GridRow::Cells(docs), GridRow::Spanning(doc)]
            }
        }
    }
}

impl Doc {
    /// Aligns clauses together, with wide conditions spanning beneath each clause.
    fn of_layout_funcs(defs_func: &[&FuncDef], anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        let layouts_func = defs_func
            .iter()
            .map(|def_func| LayoutFunc::of_func_def(def_func, anchors))
            .collect::<Result<Vec<_>>>()?;
        let has_condition_below = layouts_func
            .iter()
            .any(|layout_func| matches!(layout_func, LayoutFunc::Multi(..)));

        // Separate consecutive clauses by one gap
        let mut rows = Vec::new();
        for layout_func in layouts_func {
            if !rows.is_empty() {
                rows.push(GridRow::Gap);
            }
            let rows_func = GridRow::of_layout_func(layout_func);
            rows.extend(rows_func);
        }

        // Multiple clauses use a grid; a compact single equation uses aligned
        let tex = if defs_func.len() > 1 {
            let alignments = vec![Alignment::Left, Alignment::Center, Alignment::Left];
            Doc::grid(alignments, rows)?
        } else if has_condition_below {
            let alignments = vec![Alignment::Right, Alignment::Center, Alignment::Left];
            Doc::grid(alignments, rows)?
        } else {
            let rows = rows
                .into_iter()
                .filter_map(|row| match row {
                    GridRow::Cells(docs) => Some(docs),
                    _ => None,
                })
                .collect();
            Doc::Aligned(rows)
        };
        layout::resolve(WIDTH_LAYOUT, &tex)
    }

    /// Renders one defined function clause as its own aligned layout.
    fn of_func_def(def_func: &FuncDef, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        Doc::of_layout_funcs(&[def_func], anchors)
    }
}

// == Definitions

impl Doc {
    // - Definition
    //
    //   var x : nat   -> \mathsf{x} : \mathbb{N}
    //   Sep           -> Empty

    /// Renders one definition, ignoring presentation hints in canonical output.
    pub(super) fn of_def(def: &Def, anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        match &def.node {
            DefKind::ExternSyntax(ExternSyntaxDef { id, .. }) => Ok(Doc::of_extern_syntax_def(id)),
            DefKind::Syntax(SyntaxDef { entries }) => Ok(Doc::of_syntax_def(entries)),
            DefKind::Typ(TypDef { id, tparams, def_typ, .. }) => {
                Ok(Doc::of_typ_def(id, tparams, def_typ))
            }
            DefKind::Var(VarDef { id, plain_typ, .. }) => Ok(Doc::of_var_def(id, plain_typ)),
            DefKind::ExternRel(ExternRelDef { id, not_typ, .. }) => {
                Ok(Doc::of_extern_rel_def(id, not_typ))
            }
            DefKind::Rel(RelDef { id, not_typ, .. }) => Ok(Doc::of_rel_def(id, not_typ)),
            DefKind::RuleGroup(RuleGroupDef { relid, groupid, rules }) => {
                Doc::of_rulegroup(relid, groupid, rules, anchors)
            }
            DefKind::ExternDec(ExternDecDef { id, tparams, params, plain_typ, .. }) => {
                Ok(Doc::of_extern_dec_def(id, tparams, params, plain_typ))
            }
            DefKind::BuiltinDec(BuiltinDecDef { id, tparams, params, plain_typ, .. }) => {
                Ok(Doc::of_builtin_dec_def(id, tparams, params, plain_typ))
            }
            DefKind::TableDec(TableDecDef { id, params, plain_typ, .. }) => {
                Ok(Doc::of_table_dec_def(id, params, plain_typ))
            }
            DefKind::FuncDec(FuncDecDef { id, tparams, params, plain_typ, .. }) => {
                Ok(Doc::of_func_dec_def(id, tparams, params, plain_typ))
            }
            DefKind::TableDef(TableDef { id, rows }) => Doc::of_table(id, rows, anchors),
            DefKind::FuncDef(def_func) => Doc::of_func_def(def_func, anchors),
            DefKind::Sep => Ok(Doc::of_sep_def()),
        }
    }

    // - Definition separators
    //
    //   Sep   -> Empty

    fn of_sep_def() -> Doc {
        Doc::Empty
    }
}

// == Entry point
//
//   [def $g(0) = 1, def $g(n) = n, var x : nat]
//   -> \begin{gathered}
//      \begin{array}{lcl}
//      \mathrm{g}\left(0\right) & = & 1 \\[1ex]
//      \mathrm{g}\left(\mathsf{n}\right) & = & \mathsf{n}
//      \end{array} \\
//      \mathsf{x} : \mathbb{N}
//      \end{gathered}

impl Block {
    /// Renders one group of adjacent definitions as a gathered block.
    fn of_defgroup(defs_group: &[Def], anchors: Option<&Anchors<'_>>) -> Result<Block> {
        let def_head = &defs_group[0];
        match &def_head.node {
            // Separators interrupt clause grouping even if they render no text
            DefKind::Sep => Ok(Block::Gap),
            // Share alignment across consecutive clauses with the same identifier
            DefKind::FuncDef(_) => {
                let defs_func: Vec<_> = defs_group
                    .iter()
                    .filter_map(|def| match &def.node {
                        DefKind::FuncDef(def_func) => Some(def_func),
                        _ => None,
                    })
                    .collect();
                let tex_funcs = Doc::of_layout_funcs(&defs_func, anchors)?;
                Ok(Block::Line(tex_funcs))
            }
            // Every other definition keeps its source position in the document
            _ => {
                let tex_def = Doc::of_def(def_head, anchors)?;
                Ok(Block::Line(tex_def))
            }
        }
    }
}

impl Doc {
    /// Groups only adjacent clauses of the same function, preserving separators.
    pub(super) fn of_defs(defs: &[Def], anchors: Option<&Anchors<'_>>) -> Result<Doc> {
        // Clauses of the same function stay in one group
        let blocks = defs
            .chunk_by(|def_a, def_b| match (&def_a.node, &def_b.node) {
                (DefKind::FuncDef(def_func_a), DefKind::FuncDef(def_func_b)) => {
                    def_func_a.id.node == def_func_b.id.node
                }
                _ => false,
            })
            .map(|defs_group| Block::of_defgroup(defs_group, anchors))
            .collect::<Result<Vec<_>>>()?;
        // Empty and separator-only specifications produce no gathered wrapper
        if blocks.iter().all(|block| matches!(block, Block::Gap)) {
            return Ok(Doc::Empty);
        }
        Ok(Doc::gathered(blocks))
    }
}
