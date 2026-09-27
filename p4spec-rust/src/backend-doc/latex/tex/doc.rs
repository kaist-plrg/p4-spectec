//! Semantic TeX document model and its normalizing constructors
//!
//! A `Doc` keeps mathematical and layout intent until layout and serialization.
//! Constructors drop emptiness and redundant structure while building:
//!
//! ```text
//! Doc::concat([a, Empty, Concat([b, c])])              -> Concat([a, b, c])
//! Doc::link(target, Empty)                             -> Empty
//! Doc::fill(2, separator, [x])                         -> x
//! Doc::grid(columns, [Gap, Cells(x), Gap, Gap, Gap])   -> Grid(columns, [Cells(x)])
//! Doc::gathered([Gap, Line(x), Gap, Gap, Line(y)])     -> Gathered([Line(x), Gap, Line(y)])
//! ```

use crate::backend_doc::latex::error::{Error, Result};
use num_bigint::BigInt;

// == Document model
//
//   Delimited(Paren, Styled(Mathsf, "x"))               -> \left(\mathsf{x}\right)
//   LayoutGroup(Concat([x, SoftBreak(SoftSpace), y]))   -> x y on one line, or x and y on two

/// Selects a font and its escaping context.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Style {
    /// Italic math text, `\mathit{...}`.
    #[allow(dead_code)]
    Mathit,
    /// Upright math text, `\mathrm{...}`.
    Mathrm,
    /// Sans-serif math text, `\mathsf{...}`.
    Mathsf,
    /// Blackboard-bold math text, `\mathbb{...}`.
    Mathbb,
    /// Monospace math text, `\mathtt{...}`.
    Mathtt,
    /// Text-mode content, `\text{...}`, with math-only glyphs split out.
    Text,
    /// Monospace text, `\texttt{...}`, with math-only glyphs split out.
    Texttt,
}

/// Selects a balanced delimiter pair.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Delimiter {
    /// Parentheses, `\left(...\right)`.
    Paren,
    /// Square brackets, `\left[...\right]`.
    Bracket,
    /// Braces, `\left\{...\right\}`.
    Brace,
    /// Angle brackets, `\left\langle...\right\rangle`.
    Angle,
    /// Vertical bars, `\left|...\right|`.
    Bar,
}

/// Aligns one grid column.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Alignment {
    /// Left-aligns the column.
    Left,
    /// Centers the column.
    Center,
    /// Right-aligns the column.
    Right,
}

/// Specifies the flat representation of a break opportunity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Soft {
    /// Emits nothing in flat mode and starts a new line in broken mode.
    #[allow(dead_code)]
    SoftCut,
    /// Emits one space in flat mode and starts a new line in broken mode.
    SoftSpace,
}

/// Names a fixed mathematical atom.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Symbol {
    /// Equality, `=`.
    Equal,
    /// Inequality, `\ne`.
    NotEqual,
    /// Less-than relation, `<`.
    Less,
    /// Greater-than relation, `>`.
    Greater,
    /// Less-than-or-equal relation, `\le`.
    LessEqual,
    /// Greater-than-or-equal relation, `\ge`.
    GreaterEqual,
    /// Addition or positive sign, `+`.
    Plus,
    /// Subtraction or negative sign, `-`.
    Minus,
    /// Optional iteration marker, `?`.
    Question,
    /// List iteration marker, `\ast`.
    Ast,
    /// Division slash, `/`.
    Slash,
    /// Comma separator, `,`.
    Comma,
    /// Semicolon separator, `;`.
    Semicolon,
    /// Colon separator, `:`.
    Colon,
    /// List construction, `::`.
    DoubleColon,
    /// Concatenation, `+\!\!+`.
    Cat,
    /// Grammar production, `::=`.
    Production,
    /// Grammar alternative separator, `|`.
    VerticalBar,
    /// Single dot, `.`.
    Dot,
    /// Two literal dots, `..`.
    Dot2,
    /// Ellipsis, `\ldots`.
    Ellipsis,
    /// Empty sequence, `\epsilon`.
    Epsilon,
    /// Membership, `\in`.
    In,
    /// Logical negation, `\neg`.
    Neg,
    /// Conjunction, `\land`.
    Land,
    /// Disjunction, `\lor`.
    Lor,
    /// Implication arrow, `\Rightarrow`.
    Rightarrow,
    /// Equivalence arrow, `\Leftrightarrow`.
    Leftrightarrow,
    /// Multiplication dot, `\cdot`.
    Cdot,
    /// Numeric remainder, `\bmod`.
    Bmod,
    /// Right-facing turnstile, `\vdash`.
    Turnstile,
    /// Left-facing turnstile, `\dashv`.
    Tilesturn,
    /// Single arrow, `\to`.
    To,
    /// Long double arrow, `\Longrightarrow`.
    Longrightarrow,
    /// Hooked arrow, `\hookrightarrow`.
    Hookrightarrow,
    /// Table mapping arrow, `\mapsto`.
    Mapsto,
    /// Similarity relation, `\sim`.
    Sim,
    /// Set difference, `\setminus`.
    Setminus,
    /// Empty set, `\varnothing`.
    EmptySet,
    /// Literal opening parenthesis, without automatic sizing.
    LeftParen,
    /// Literal closing parenthesis, without automatic sizing.
    RightParen,
    /// Literal opening square bracket, without automatic sizing.
    LeftBracket,
    /// Literal closing square bracket, without automatic sizing.
    RightBracket,
    /// Literal opening brace, without automatic sizing.
    LeftBrace,
    /// Literal closing brace, without automatic sizing.
    RightBrace,
}

/// Identifies a local HTML anchor validated by `Target::of_string`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Target(pub(super) String);

/// Retains mathematical and layout intent until interpretation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Doc {
    // Atomic documents
    /// No content and no width.
    Empty,
    /// Text escaped in the selected font and math or text context.
    Styled(Style, String),
    /// A boxed, shaded rule label containing small monospace text.
    Badge(String),
    /// A decimal integer in math mode.
    Decimal(BigInt),
    /// A hexadecimal integer with a `0x` prefix inside `\mathtt{...}`.
    Hexadecimal(BigInt),
    /// A mathematical atom with a predefined TeX spelling.
    Fixed(Symbol),
    /// A literal space, measured as one column.
    Space,
    /// A thin math space, `\,`, measured as one column.
    ThinSpace,
    /// A wide math space, `\quad`, measured as two columns.
    Quad,
    // Sequential composition
    /// Documents concatenated without implicit spacing.
    Concat(Vec<Doc>),
    // TeX classification
    /// An explicit TeX group, `{...}`, retained even when empty.
    Group(Box<Doc>),
    /// Content classified as a binary operator, `\mathbin{...}`.
    Mathbin(Box<Doc>),
    /// Content classified as a relation, `\mathrel{...}`.
    Mathrel(Box<Doc>),
    /// Content forced to display-style math, `{\displaystyle ...}`.
    Displaystyle(Box<Doc>),
    // Delimiters and attachments
    /// A delimiter pair sized to its enclosed document.
    Delimited(Delimiter, Box<Doc>),
    /// A base and subscript, in that order: `{base}_{sub}`.
    Sub(Box<Doc>, Box<Doc>),
    /// A base and superscript, in that order: `{base}^{sup}`.
    Sup(Box<Doc>, Box<Doc>),
    /// A base, subscript, and superscript: `{base}_{sub}^{sup}`.
    #[allow(dead_code)]
    Subsup(Box<Doc>, Box<Doc>, Box<Doc>),
    /// A numerator and denominator: `\frac{num}{den}`.
    Fraction(Box<Doc>, Box<Doc>),
    // Navigation
    /// A local anchor and its visible document: `\href{#target}{doc}`.
    Link(Target, Box<Doc>),
    // Width-sensitive layout
    /// A break opportunity whose flat spelling is selected by `Soft`.
    SoftBreak(Soft),
    /// Content whose soft breaks are selected together by available width.
    /// Nested layout groups choose their own mode; no TeX braces are added.
    LayoutGroup(Box<Doc>),
    /// Additional continuation indentation and its document, in that order.
    /// The first line keeps its current column.
    Nest(usize, Box<Doc>),
    /// Continuation indentation, separator, and greedily packed documents.
    /// A line break replaces the separator; `Doc::fill` keeps only nonempty items.
    Fill(usize, Box<Doc>, Vec<Doc>),
    // Multi-row layout
    /// Equation rows in `aligned`, with shared column widths.
    Aligned(Vec<Vec<Doc>>),
    /// Explicit column alignments and cell, spanning, or gap rows.
    Grid(Vec<Alignment>, Vec<GridRow>),
    /// Rows in `aligned`, each following an empty alignment cell.
    #[allow(dead_code)]
    Stacked(Vec<Doc>),
    /// Left-aligned rows, also used for resolved line breaks.
    LeftStack(Vec<Doc>),
    /// Premises with numeric labels; continuation rows have no label.
    Numbered(Vec<Doc>),
    /// Centered blocks with ordinary or enlarged vertical separation.
    Gathered(Vec<Block>),
}

/// Supplies cells, a spanning document, or a vertical grid gap.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum GridRow {
    /// One document per declared grid column.
    Cells(Vec<Doc>),
    /// Content spanning the grid, laid out with the full line-width budget.
    Spanning(Doc),
    /// Extra space after a content row; `grid` trims and coalesces gaps.
    Gap,
}

/// Supplies a gathered line or a vertical gap.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Block {
    /// One centered document in a gathered environment.
    Line(Doc),
    /// Extra space after a line; `gathered` trims and coalesces gaps.
    Gap,
}

// == Inspection

impl Doc {
    // - Emptiness
    //
    //   Concat([Empty, Nest(2, Empty)]).is_empty()   -> true
    //   Group(Empty).is_empty()                      -> false

    /// Tests semantic emptiness without discarding explicit TeX groups.
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Doc::Empty => true,
            Doc::Styled(..)
            | Doc::Badge(_)
            | Doc::Decimal(_)
            | Doc::Hexadecimal(_)
            | Doc::Fixed(_)
            | Doc::Space
            | Doc::ThinSpace
            | Doc::Quad
            | Doc::Group(_)
            | Doc::Mathbin(_)
            | Doc::Mathrel(_)
            | Doc::Delimited(..)
            | Doc::Sub(..)
            | Doc::Sup(..)
            | Doc::Subsup(..)
            | Doc::Fraction(..)
            | Doc::Link(..)
            | Doc::SoftBreak(_)
            | Doc::Aligned(_)
            | Doc::Grid(..)
            | Doc::Gathered(_) => false,
            Doc::Displaystyle(doc) | Doc::LayoutGroup(doc) | Doc::Nest(_, doc) => doc.is_empty(),
            Doc::Concat(docs)
            | Doc::Stacked(docs)
            | Doc::LeftStack(docs)
            | Doc::Numbered(docs)
            | Doc::Fill(_, _, docs) => docs.iter().all(Doc::is_empty),
        }
    }

    // - Children
    //
    //   Fill(_, s, [x, y]).children()         -> [s, x, y]
    //   Sub(x, i).map_children(f)             -> Sub(f(x), f(i))
    //   Fill(_, s, [x]).map_children(f)       -> Fill(_, s, [f(x)])

    /// Lists direct children, with a fill separator before its items.
    pub(crate) fn children(&self) -> Vec<&Doc> {
        match self {
            Doc::Empty
            | Doc::Styled(..)
            | Doc::Badge(_)
            | Doc::Decimal(_)
            | Doc::Hexadecimal(_)
            | Doc::Fixed(_)
            | Doc::Space
            | Doc::ThinSpace
            | Doc::Quad
            | Doc::SoftBreak(_) => Vec::new(),
            Doc::Group(doc)
            | Doc::Mathbin(doc)
            | Doc::Mathrel(doc)
            | Doc::Displaystyle(doc)
            | Doc::Delimited(_, doc)
            | Doc::Link(_, doc)
            | Doc::LayoutGroup(doc)
            | Doc::Nest(_, doc) => vec![doc],
            Doc::Sub(doc_base, doc_sub) => vec![doc_base, doc_sub],
            Doc::Sup(doc_base, doc_sup) => vec![doc_base, doc_sup],
            Doc::Subsup(doc_base, doc_sub, doc_sup) => vec![doc_base, doc_sub, doc_sup],
            Doc::Fraction(doc_num, doc_den) => vec![doc_num, doc_den],
            Doc::Fill(_, separator, docs) => std::iter::once(&**separator).chain(docs).collect(),
            Doc::Concat(docs) | Doc::Stacked(docs) | Doc::LeftStack(docs) | Doc::Numbered(docs) => {
                docs.iter().collect()
            }
            Doc::Aligned(rows) => rows.iter().flatten().collect(),
            Doc::Grid(_, rows) => rows
                .iter()
                .flat_map(|row| match row {
                    GridRow::Cells(docs) => docs.iter().collect(),
                    GridRow::Spanning(doc) => vec![doc],
                    GridRow::Gap => Vec::new(),
                })
                .collect(),
            Doc::Gathered(blocks) => blocks
                .iter()
                .filter_map(|block| match block {
                    Block::Line(doc) => Some(doc),
                    Block::Gap => None,
                })
                .collect(),
        }
    }

    /// Maps one boxed child, reusing its allocation.
    fn map_box(mut doc: Box<Doc>, map: &mut impl FnMut(Doc) -> Doc) -> Box<Doc> {
        let doc_inner = std::mem::replace(&mut *doc, Doc::Empty);
        *doc = map(doc_inner);
        doc
    }

    /// Rebuilds a document with each child mapped, leaving fill separators untouched.
    pub(crate) fn map_children(self, mut map: impl FnMut(Doc) -> Doc) -> Doc {
        match self {
            Doc::Empty
            | Doc::Styled(..)
            | Doc::Badge(_)
            | Doc::Decimal(_)
            | Doc::Hexadecimal(_)
            | Doc::Fixed(_)
            | Doc::Space
            | Doc::ThinSpace
            | Doc::Quad
            | Doc::SoftBreak(_) => self,
            Doc::Concat(docs) => {
                let docs = docs.into_iter().map(map).collect();
                Doc::Concat(docs)
            }
            Doc::Group(doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Group(doc)
            }
            Doc::Mathbin(doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Mathbin(doc)
            }
            Doc::Mathrel(doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Mathrel(doc)
            }
            Doc::Displaystyle(doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Displaystyle(doc)
            }
            Doc::Delimited(delimiter, doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Delimited(delimiter, doc)
            }
            Doc::Sub(doc_base, doc_sub) => {
                let doc_base = Doc::map_box(doc_base, &mut map);
                let doc_sub = Doc::map_box(doc_sub, &mut map);
                Doc::Sub(doc_base, doc_sub)
            }
            Doc::Sup(doc_base, doc_sup) => {
                let doc_base = Doc::map_box(doc_base, &mut map);
                let doc_sup = Doc::map_box(doc_sup, &mut map);
                Doc::Sup(doc_base, doc_sup)
            }
            Doc::Subsup(doc_base, doc_sub, doc_sup) => {
                let doc_base = Doc::map_box(doc_base, &mut map);
                let doc_sub = Doc::map_box(doc_sub, &mut map);
                let doc_sup = Doc::map_box(doc_sup, &mut map);
                Doc::Subsup(doc_base, doc_sub, doc_sup)
            }
            Doc::Fraction(doc_num, doc_den) => {
                let doc_num = Doc::map_box(doc_num, &mut map);
                let doc_den = Doc::map_box(doc_den, &mut map);
                Doc::Fraction(doc_num, doc_den)
            }
            Doc::Link(target, doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Link(target, doc)
            }
            Doc::LayoutGroup(doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::LayoutGroup(doc)
            }
            Doc::Nest(indent, doc) => {
                let doc = Doc::map_box(doc, &mut map);
                Doc::Nest(indent, doc)
            }
            Doc::Fill(indent, separator, docs) => {
                let docs = docs.into_iter().map(map).collect();
                Doc::Fill(indent, separator, docs)
            }
            Doc::Aligned(rows) => {
                let rows = rows
                    .into_iter()
                    .map(|docs| docs.into_iter().map(&mut map).collect())
                    .collect();
                Doc::Aligned(rows)
            }
            Doc::Grid(alignments, rows) => {
                let rows = rows
                    .into_iter()
                    .map(|row| match row {
                        GridRow::Cells(docs) => {
                            let docs = docs.into_iter().map(&mut map).collect();
                            GridRow::Cells(docs)
                        }
                        GridRow::Spanning(doc) => {
                            let doc = map(doc);
                            GridRow::Spanning(doc)
                        }
                        GridRow::Gap => GridRow::Gap,
                    })
                    .collect();
                Doc::Grid(alignments, rows)
            }
            Doc::Stacked(docs) => {
                let docs = docs.into_iter().map(map).collect();
                Doc::Stacked(docs)
            }
            Doc::LeftStack(docs) => {
                let docs = docs.into_iter().map(map).collect();
                Doc::LeftStack(docs)
            }
            Doc::Numbered(docs) => {
                let docs = docs.into_iter().map(map).collect();
                Doc::Numbered(docs)
            }
            Doc::Gathered(blocks) => {
                let blocks = blocks
                    .into_iter()
                    .map(|block| match block {
                        Block::Line(doc) => {
                            let doc = map(doc);
                            Block::Line(doc)
                        }
                        Block::Gap => Block::Gap,
                    })
                    .collect();
                Doc::Gathered(blocks)
            }
        }
    }
}

// == Constructors

impl Doc {
    // - Atomic documents
    //
    //   Doc::badge("")    -> Empty
    //   Doc::badge("R")   -> Badge("R")

    /// Omits a badge whose label is empty.
    pub(crate) fn badge(text: String) -> Doc {
        if text.is_empty() { Doc::Empty } else { Doc::Badge(text) }
    }

    // - Sequential composition
    //
    //   Doc::concat([a, Empty, Concat([b, c])])   -> Concat([a, b, c])
    //   Doc::concat_spaced([x, Empty, y])         -> Concat([x, Space, y])

    /// Flattens concatenation and removes empty atomic documents.
    pub(crate) fn concat(docs: Vec<Doc>) -> Doc {
        let mut docs_flat = Vec::new();
        let mut docs_pending = docs;
        docs_pending.reverse();
        // Expand nested sequences in their original order
        while let Some(doc) = docs_pending.pop() {
            match doc {
                // Discard only atomic emptiness
                Doc::Empty => {}
                // Put nested children before the remaining siblings
                Doc::Concat(docs) => docs_pending.extend(docs.into_iter().rev()),
                // Preserve wrappers even when their content is empty
                doc => docs_flat.push(doc),
            }
        }
        match docs_flat.len() {
            0 => Doc::Empty,
            1 => docs_flat.pop().unwrap(),
            _ => Doc::Concat(docs_flat),
        }
    }

    /// Inserts separators only between semantically nonempty documents.
    pub(crate) fn concat_intersperse(separator: Doc, docs: Vec<Doc>) -> Doc {
        let mut docs_separated = Vec::new();
        // Skip emptiness before deciding whether a separator is needed
        for doc in docs.into_iter().filter(|doc| !doc.is_empty()) {
            if !docs_separated.is_empty() {
                docs_separated.push(separator.clone());
            }
            docs_separated.push(doc);
        }
        Doc::concat(docs_separated)
    }

    /// Separates nonempty documents by spaces.
    pub(crate) fn concat_spaced(docs: Vec<Doc>) -> Doc {
        Doc::concat_intersperse(Doc::Space, docs)
    }

    /// Separates nonempty documents by commas and spaces.
    pub(crate) fn concat_comma_separated(docs: Vec<Doc>) -> Doc {
        let separator = Doc::concat(vec![Doc::Fixed(Symbol::Comma), Doc::Space]);
        Doc::concat_intersperse(separator, docs)
    }

    /// Separates nonempty documents by thin spaces.
    pub(crate) fn concat_juxtaposed(docs: Vec<Doc>) -> Doc {
        Doc::concat_intersperse(Doc::ThinSpace, docs)
    }

    // - TeX classification
    //
    //   Doc::mathrel(x)             -> Mathrel(x)
    //   Doc::displaystyle(Empty)    -> Empty

    pub(crate) fn group(doc: Doc) -> Doc {
        Doc::Group(Box::new(doc))
    }

    pub(crate) fn mathbin(doc: Doc) -> Doc {
        Doc::Mathbin(Box::new(doc))
    }

    pub(crate) fn mathrel(doc: Doc) -> Doc {
        Doc::Mathrel(Box::new(doc))
    }

    /// Omits display style around an empty document.
    pub(crate) fn displaystyle(doc: Doc) -> Doc {
        if doc.is_empty() { Doc::Empty } else { Doc::Displaystyle(Box::new(doc)) }
    }

    // - Delimiters and attachments
    //
    //   Doc::parenthesized(x)   -> Delimited(Paren, x)
    //   Doc::sub(x, i)          -> Sub(x, i)

    pub(crate) fn delimited(delimiter: Delimiter, doc: Doc) -> Doc {
        Doc::Delimited(delimiter, Box::new(doc))
    }

    pub(crate) fn parenthesized(doc: Doc) -> Doc {
        Doc::delimited(Delimiter::Paren, doc)
    }

    pub(crate) fn sub(doc_base: Doc, doc_sub: Doc) -> Doc {
        Doc::Sub(Box::new(doc_base), Box::new(doc_sub))
    }

    pub(crate) fn sup(doc_base: Doc, doc_sup: Doc) -> Doc {
        Doc::Sup(Box::new(doc_base), Box::new(doc_sup))
    }

    pub(crate) fn fraction(doc_num: Doc, doc_den: Doc) -> Doc {
        Doc::Fraction(Box::new(doc_num), Box::new(doc_den))
    }

    // - Navigation
    //
    //   Doc::link(target, Empty)   -> Empty

    /// Omits a link around an empty document.
    pub(crate) fn link(target: Target, doc: Doc) -> Doc {
        if doc.is_empty() { Doc::Empty } else { Doc::Link(target, Box::new(doc)) }
    }

    // - Width-sensitive layout
    //
    //   Doc::nest(0, x)                                -> x
    //   Doc::fill(2, s, [x, Empty])                    -> x
    //   Doc::fill_line(s, [x, y, z])                   -> x, s, y, s, z
    //   Doc::layout_group_soft_comma_separated([x, y])
    //   -> LayoutGroup(Concat([x, Fixed(Comma), SoftBreak(SoftSpace), y]))

    /// Omits a layout group around an empty document.
    pub(crate) fn layout_group(doc: Doc) -> Doc {
        if doc.is_empty() { Doc::Empty } else { Doc::LayoutGroup(Box::new(doc)) }
    }

    /// Groups a comma-separated list with breakable spaces.
    pub(crate) fn layout_group_soft_comma_separated(docs: Vec<Doc>) -> Doc {
        let separator =
            Doc::concat(vec![Doc::Fixed(Symbol::Comma), Doc::SoftBreak(Soft::SoftSpace)]);
        let doc = Doc::concat_intersperse(separator, docs);
        Doc::layout_group(doc)
    }

    /// Omits ineffective continuation indentation.
    pub(crate) fn nest(indent: usize, doc: Doc) -> Doc {
        if indent == 0 || doc.is_empty() { doc } else { Doc::Nest(indent, Box::new(doc)) }
    }

    /// Retains a fill only when multiple nonempty documents need packing.
    pub(crate) fn fill(indent: usize, separator: Doc, docs: Vec<Doc>) -> Doc {
        let mut docs: Vec<_> = docs.into_iter().filter(|doc| !doc.is_empty()).collect();
        match docs.len() {
            0 => Doc::Empty,
            1 => docs.pop().unwrap(),
            _ => Doc::Fill(indent, Box::new(separator), docs),
        }
    }

    /// Lists fill items with the separator between each pair, as on one line.
    ///
    /// Items need no emptiness filter: `Doc::fill` already dropped empty ones.
    pub(super) fn fill_line<'a>(
        separator: &'a Doc,
        docs: &'a [Doc],
    ) -> impl Iterator<Item = &'a Doc> {
        docs.iter().enumerate().flat_map(move |(idx, doc)| {
            let separator = (idx != 0).then_some(separator);
            separator.into_iter().chain(std::iter::once(doc))
        })
    }

    // - Multi-row layout
    //
    //   Doc::grid(columns, [Gap, Cells(x), Gap, Gap, Gap])   -> Grid(columns, [Cells(x)])
    //   Doc::gathered([Gap, Line(x), Gap, Gap, Line(y)])     -> Gathered([Line(x), Gap, Line(y)])
    //   Doc::left_stack([x, Empty])                          -> x

    /// Removes blank rows and outer gaps, and coalesces interior gaps.
    ///
    /// ```text
    /// [Gap, x, Gap, blank, Gap, y, Gap]   -> [x, Gap, y]
    /// ```
    fn normalize_rows<T>(
        rows: Vec<T>,
        is_gap: impl Fn(&T) -> bool,
        is_blank: impl Fn(&T) -> bool,
    ) -> Vec<T> {
        let mut rows_normalized = Vec::new();
        let mut gap_pending = None;
        // Defer each gap until a subsequent content row uses it
        for row in rows {
            // A leading gap has no preceding row
            if is_gap(&row) {
                if !rows_normalized.is_empty() {
                    gap_pending = Some(row);
                }
                continue;
            }
            // A blank row keeps the pending gap for the next content row
            if is_blank(&row) {
                continue;
            }
            // Emit at most one gap before this row
            if let Some(gap) = gap_pending.take() {
                rows_normalized.push(gap);
            }
            rows_normalized.push(row);
        }
        rows_normalized
    }

    /// Validates grid arity, then removes blank rows and redundant gaps.
    pub(crate) fn grid(alignments: Vec<Alignment>, rows: Vec<GridRow>) -> Result<Doc> {
        // A nonempty row sequence needs a column specification
        if alignments.is_empty() {
            return if rows.is_empty() { Ok(Doc::Empty) } else { Err(Error::GridWithoutColumns) };
        }
        // Validate before normalizing so malformed blank rows remain errors
        for row in &rows {
            if let GridRow::Cells(docs) = row
                && docs.len() != alignments.len()
            {
                let error = Error::GridCellCount { expected: alignments.len(), actual: docs.len() };
                return Err(error);
            }
        }
        let is_gap = |row: &GridRow| matches!(row, GridRow::Gap);
        let is_blank = |row: &GridRow| match row {
            GridRow::Cells(docs) => docs.iter().all(Doc::is_empty),
            GridRow::Spanning(doc) => doc.is_empty(),
            GridRow::Gap => false,
        };
        let rows = Doc::normalize_rows(rows, is_gap, is_blank);
        if rows.is_empty() { Ok(Doc::Empty) } else { Ok(Doc::Grid(alignments, rows)) }
    }

    /// Removes empty documents from a centered stack.
    #[allow(dead_code)]
    pub(crate) fn stacked(docs: Vec<Doc>) -> Doc {
        let docs: Vec<_> = docs.into_iter().filter(|doc| !doc.is_empty()).collect();
        if docs.is_empty() { Doc::Empty } else { Doc::Stacked(docs) }
    }

    /// Collapses a left stack with at most one nonempty document.
    pub(crate) fn left_stack(docs: Vec<Doc>) -> Doc {
        let mut docs: Vec<_> = docs.into_iter().filter(|doc| !doc.is_empty()).collect();
        match docs.len() {
            0 => Doc::Empty,
            1 => docs.pop().unwrap(),
            _ => Doc::LeftStack(docs),
        }
    }

    /// Numbers only nonempty premise documents.
    pub(crate) fn numbered(docs: Vec<Doc>) -> Doc {
        let docs: Vec<_> = docs.into_iter().filter(|doc| !doc.is_empty()).collect();
        if docs.is_empty() { Doc::Empty } else { Doc::Numbered(docs) }
    }

    /// Removes empty lines and redundant gaps between gathered blocks.
    pub(crate) fn gathered(blocks: Vec<Block>) -> Doc {
        let is_gap = |block: &Block| matches!(block, Block::Gap);
        let is_blank = |block: &Block| match block {
            Block::Line(doc) => doc.is_empty(),
            Block::Gap => false,
        };
        let blocks = Doc::normalize_rows(blocks, is_gap, is_blank);
        Doc::Gathered(blocks)
    }
}
