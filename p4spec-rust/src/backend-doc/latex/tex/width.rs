//! Flat widths of unresolved documents, in approximate text columns
//!
//! ```text
//! Styled(Mathsf, "abc")           3   text bytes
//! Fixed(Rightarrow)               2   ASCII spelling "=>"
//! Quad                            2
//! Concat([x, Space, y])           3
//! Sup(x, Styled(Mathsf, "abc"))   3   script at half, rounded up
//! LeftStack([x, abc])             3   widest row
//! ```

use super::doc::*;
use num_bigint::BigInt;

// == Width parameters
//
//   Cells([x, y])         -> width x + INTERCOLUMN_SPACING + width y
//   Delimited(Paren, x)   -> DELIMITER_MARGIN + width x

pub(super) const INTERCOLUMN_SPACING: usize = 2;
pub(super) const DELIMITER_MARGIN: usize = 2;
pub(super) const FRACTION_MARGIN: usize = 2;
const NUMBERED_LABEL_MARGIN: usize = 2;

// == Helpers

// - Scripts
//
//   flat_script_width(Styled(Mathsf, "abc"))   -> 2

/// Measures a script at half size, rounded upward.
pub(super) fn flat_script_width(doc: &Doc) -> usize {
    let width = flat(doc);
    width.div_ceil(2)
}

// - Columns
//
//   flat_columns([3, 4])                         -> 3 + 2 + 4
//   flat_column_widths([[ab, c], [d, efg, h]])   -> [2, 3, 1]

/// Measures columns including the gaps between adjacent columns.
pub(super) fn flat_columns(widths: &[usize]) -> usize {
    let width_columns = widths.iter().sum::<usize>();
    let width_gaps = INTERCOLUMN_SPACING * widths.len().saturating_sub(1);
    width_columns + width_gaps
}

/// Finds the maximum width of each existing column across ragged rows.
pub(super) fn flat_column_widths<'a>(rows: impl IntoIterator<Item = &'a [Doc]>) -> Vec<usize> {
    let mut widths = Vec::new();
    // Merge each row without discarding columns absent from a shorter row
    for docs in rows {
        for (idx, doc) in docs.iter().enumerate() {
            let width = flat(doc);
            if idx == widths.len() {
                widths.push(width);
            } else {
                widths[idx] = widths[idx].max(width);
            }
        }
    }
    widths
}

// - Widest rows
//
//   flat_widest([x, long])   -> width long

fn flat_widest(docs: &[Doc]) -> usize {
    docs.iter().map(flat).max().unwrap_or(0)
}

// - Numbered gutters
//
//   flat_numbered_gutter(12)   -> 2 + 2 + 2

/// Reserves the numeric label, its parentheses, and intercolumn spacing.
pub(super) fn flat_numbered_gutter(count: usize) -> usize {
    let width_label = count.to_string().len();
    width_label + NUMBERED_LABEL_MARGIN + INTERCOLUMN_SPACING
}

// == Documents

// - Document
//
//   Concat([Styled(Mathsf, "x"), Space, Styled(Mathsf, "y")])   -> 3

/// Measures the approximate width of one unresolved line.
pub(crate) fn flat(doc: &Doc) -> usize {
    match doc {
        Doc::Empty => flat_empty(),
        Doc::Styled(_, text) => flat_styled(text),
        Doc::Badge(text) => flat_badge(text),
        Doc::Decimal(num) => flat_decimal(num),
        Doc::Hexadecimal(num) => flat_hexadecimal(num),
        Doc::Fixed(symbol) => flat_fixed(*symbol),
        Doc::Space => flat_space(),
        Doc::ThinSpace => flat_thin_space(),
        Doc::Quad => flat_quad(),
        Doc::Concat(docs) => flat_concat(docs),
        Doc::Group(doc) => flat_group(doc),
        Doc::Mathbin(doc) => flat_mathbin(doc),
        Doc::Mathrel(doc) => flat_mathrel(doc),
        Doc::Displaystyle(doc) => flat_displaystyle(doc),
        Doc::Delimited(_, doc) => flat_delimited(doc),
        Doc::Sub(doc_base, doc_sub) => flat_sub(doc_base, doc_sub),
        Doc::Sup(doc_base, doc_sup) => flat_sup(doc_base, doc_sup),
        Doc::Subsup(doc_base, doc_sub, doc_sup) => flat_subsup(doc_base, doc_sub, doc_sup),
        Doc::Fraction(doc_num, doc_den) => flat_fraction(doc_num, doc_den),
        Doc::Link(_, doc) => flat_link(doc),
        Doc::SoftBreak(soft) => flat_soft_break(*soft),
        Doc::LayoutGroup(doc) => flat_layout_group(doc),
        Doc::Nest(_, doc) => flat_nest(doc),
        Doc::Fill(_, separator, docs) => flat_fill(separator, docs),
        Doc::Aligned(rows) => flat_aligned(rows),
        Doc::Grid(_, rows) => flat_grid(rows),
        Doc::Stacked(docs) => flat_stacked(docs),
        Doc::LeftStack(docs) => flat_left_stack(docs),
        Doc::Numbered(docs) => flat_numbered(docs),
        Doc::Gathered(blocks) => flat_gathered(blocks),
    }
}

// - Empty documents
//
//   Empty   -> 0

fn flat_empty() -> usize {
    0
}

// - Styled documents
//
//   Styled(_, "abc")   -> 3

fn flat_styled(text: &str) -> usize {
    text.len()
}

// - Badges
//
//   Badge("rule")   -> 4

fn flat_badge(text: &str) -> usize {
    text.len()
}

// - Decimal numbers
//
//   Decimal(123)   -> 3

fn flat_decimal(num: &BigInt) -> usize {
    num.to_string().len()
}

// - Hexadecimal numbers
//
//   Hexadecimal(255)   -> 4   "0xff"

fn flat_hexadecimal(num: &BigInt) -> usize {
    format!("{num:#x}").len()
}

// - Fixed symbols
//
//   Fixed(Turnstile)   -> 2
//   Fixed(Epsilon)     -> 1

/// Measures a fixed symbol using its approximate textual spelling.
fn flat_fixed(symbol: Symbol) -> usize {
    let text = match symbol {
        Symbol::Equal => "=",
        Symbol::NotEqual => "=/=",
        Symbol::Less => "<",
        Symbol::Greater => ">",
        Symbol::LessEqual => "<=",
        Symbol::GreaterEqual => ">=",
        Symbol::Plus => "+",
        Symbol::Minus => "-",
        Symbol::Question => "?",
        Symbol::Ast => "*",
        Symbol::Slash => "/",
        Symbol::Comma => ",",
        Symbol::Semicolon => ";",
        Symbol::Colon => ":",
        Symbol::DoubleColon => "::",
        Symbol::Cat => "++",
        Symbol::Production => "::=",
        Symbol::VerticalBar => "|",
        Symbol::Dot => ".",
        Symbol::Dot2 => "..",
        Symbol::Ellipsis => "...",
        Symbol::Epsilon => "e",
        Symbol::In => "in",
        Symbol::Neg => "~",
        Symbol::Land => "/\\",
        Symbol::Lor => "\\/",
        Symbol::Rightarrow => "=>",
        Symbol::Leftrightarrow => "<=>",
        Symbol::Cdot => "*",
        Symbol::Bmod => "\\",
        Symbol::Turnstile => "|-",
        Symbol::Tilesturn => "-|",
        Symbol::To => "->",
        Symbol::Longrightarrow => "==>",
        Symbol::Hookrightarrow => "~>",
        Symbol::Mapsto => "|->",
        Symbol::Sim => "~~",
        Symbol::Setminus => "\\",
        Symbol::EmptySet => "0",
        Symbol::LeftParen => "(",
        Symbol::RightParen => ")",
        Symbol::LeftBracket => "[",
        Symbol::RightBracket => "]",
        Symbol::LeftBrace => "{",
        Symbol::RightBrace => "}",
    };
    text.len().max(1)
}

// - Spaces
//
//   Space   -> 1

fn flat_space() -> usize {
    1
}

// - Thin spaces
//
//   ThinSpace   -> 1

fn flat_thin_space() -> usize {
    1
}

// - Quad spaces
//
//   Quad   -> 2

fn flat_quad() -> usize {
    2
}

// - Concatenated documents
//
//   Concat([x, Space, y])   -> width x + 1 + width y

fn flat_concat(docs: &[Doc]) -> usize {
    docs.iter().map(flat).sum()
}

// - Groups
//
//   Group(x)   -> width x

fn flat_group(doc: &Doc) -> usize {
    flat(doc)
}

// - Binary classifications
//
//   Mathbin(x)   -> width x

fn flat_mathbin(doc: &Doc) -> usize {
    flat(doc)
}

// - Relation classifications
//
//   Mathrel(x)   -> width x

fn flat_mathrel(doc: &Doc) -> usize {
    flat(doc)
}

// - Display style
//
//   Displaystyle(x)   -> width x

fn flat_displaystyle(doc: &Doc) -> usize {
    flat(doc)
}

// - Delimited documents
//
//   Delimited(Paren, x)   -> 2 + width x

fn flat_delimited(doc: &Doc) -> usize {
    DELIMITER_MARGIN + flat(doc)
}

// - Subscripts
//
//   Sub(x, abc)   -> width x + 2

fn flat_sub(doc_base: &Doc, doc_sub: &Doc) -> usize {
    let width_sub = flat_script_width(doc_sub);
    flat(doc_base) + width_sub
}

// - Superscripts
//
//   Sup(x, abc)   -> width x + 2

fn flat_sup(doc_base: &Doc, doc_sup: &Doc) -> usize {
    let width_sup = flat_script_width(doc_sup);
    flat(doc_base) + width_sup
}

// - Paired scripts
//
//   Subsup(x, i, n)   -> width x + max(script width i, script width n)

/// Measures a base followed by the wider of its two scripts.
fn flat_subsup(doc_base: &Doc, doc_sub: &Doc, doc_sup: &Doc) -> usize {
    let width_sub = flat_script_width(doc_sub);
    let width_sup = flat_script_width(doc_sup);
    flat(doc_base) + width_sub.max(width_sup)
}

// - Fractions
//
//   Fraction(p, q)   -> 2 + max(width p, width q)

/// Measures the wider of numerator and denominator plus the fraction margin.
fn flat_fraction(doc_num: &Doc, doc_den: &Doc) -> usize {
    let width_num = flat(doc_num);
    let width_den = flat(doc_den);
    FRACTION_MARGIN + width_num.max(width_den)
}

// - Links
//
//   Link(_, x)   -> width x

fn flat_link(doc: &Doc) -> usize {
    flat(doc)
}

// - Soft breaks
//
//   SoftBreak(SoftCut)     -> 0
//   SoftBreak(SoftSpace)   -> 1

fn flat_soft_break(soft: Soft) -> usize {
    match soft {
        Soft::SoftCut => 0,
        Soft::SoftSpace => 1,
    }
}

// - Layout groups
//
//   LayoutGroup(x)   -> width x

fn flat_layout_group(doc: &Doc) -> usize {
    flat(doc)
}

// - Nested documents
//
//   Nest(2, x)   -> width x

fn flat_nest(doc: &Doc) -> usize {
    flat(doc)
}

// - Fills
//
//   Fill(_, ThinSpace, [x, y])   -> width x + 1 + width y

fn flat_fill(separator: &Doc, docs: &[Doc]) -> usize {
    Doc::fill_line(separator, docs).map(flat).sum()
}

// - Aligned documents
//
//   Aligned([[x, =, y], [xyz]])   -> width xyz + 2 + 1 + 2 + width y

/// Measures shared columns across aligned rows.
fn flat_aligned(rows: &[Vec<Doc>]) -> usize {
    let rows = rows.iter().map(Vec::as_slice);
    let widths = flat_column_widths(rows);
    flat_columns(&widths)
}

// - Grids
//
//   Grid(_, [Cells([x]), Spanning(long)])   -> max(width x, width long)

/// Measures the wider of shared columns and spanning rows.
fn flat_grid(rows: &[GridRow]) -> usize {
    // Measure shared columns independently of spanning content
    let rows_cell = rows.iter().filter_map(|row| match row {
        GridRow::Cells(docs) => Some(docs.as_slice()),
        _ => None,
    });
    let widths = flat_column_widths(rows_cell);
    let width_cells = flat_columns(&widths);
    // Retain the widest span even when there are no cell rows
    let width_spanning = rows
        .iter()
        .filter_map(|row| match row {
            GridRow::Spanning(doc) => Some(flat(doc)),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    width_cells.max(width_spanning)
}

// - Stacked documents
//
//   Stacked([x, long])   -> width long

fn flat_stacked(docs: &[Doc]) -> usize {
    flat_widest(docs)
}

// - Left-stacked documents
//
//   LeftStack([x, long])   -> width long

fn flat_left_stack(docs: &[Doc]) -> usize {
    flat_widest(docs)
}

// - Numbered documents
//
//   Numbered([p, q])   -> gutter width + max(width p, width q)

fn flat_numbered(docs: &[Doc]) -> usize {
    let width_gutter = flat_numbered_gutter(docs.len());
    width_gutter + flat_widest(docs)
}

// - Gathered documents
//
//   Gathered([Line(x), Gap, Line(long)])   -> width long

/// Measures the widest gathered line.
fn flat_gathered(blocks: &[Block]) -> usize {
    let docs = blocks.iter().filter_map(|block| match block {
        Block::Line(doc) => Some(doc),
        Block::Gap => None,
    });
    docs.map(flat).max().unwrap_or(0)
}
