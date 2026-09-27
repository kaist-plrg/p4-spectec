//! Width-sensitive resolution of soft breaks, fills, and shared columns
//!
//! ```text
//! LayoutGroup([a, Nest(4, [SoftBreak, +, Space, b])])
//!   fits        a + b
//!   overflows   a
//!                   + b
//!
//! Fill(2, ThinSpace, [a, b, c, d])
//!   fits        a b c d
//!   overflows   a b
//!                 c
//!                 d
//! ```
//!
//! `Aligned` and `Grid` columns iterate until their widths stop changing.

use super::{doc::*, link, width as measure};
use crate::backend_doc::latex::error::{Error, Result};

// == Resolution state
//
//   Place { width: 80, column: 12, column_next: 4, width_suffix: 3 }
//     starts at column 12 of an 80-column line,
//     continues on later lines at column 4,
//     and keeps 3 columns free after the document

/// Chooses how soft breaks render within one layout group.
#[derive(Clone, Copy)]
enum Mode {
    /// Keeps soft breaks as their empty or space spelling.
    Flat,
    /// Starts an indented continuation at each soft break.
    Broken,
}

/// Line position and width budget of one document during resolution.
#[derive(Clone, Copy)]
struct Place {
    /// Total line-width budget for the document.
    width: usize,
    /// Column where the document begins on its current line.
    column: usize,
    /// Column where continuation lines begin.
    column_next: usize,
    /// Columns reserved after the document on the same line.
    width_suffix: usize,
}

impl Place {
    /// Starts a fresh line at column zero with no reserved suffix.
    fn start(width: usize) -> Self {
        Self { width, column: 0, column_next: 0, width_suffix: 0 }
    }
}

// == Helpers

// - Resolved lines
//
//   Doc::of_lines([x, y])            -> LeftStack([x, y])
//   LeftStack([x, y]).into_lines()   -> [x, y]
//   concat_lines([a, b], [c, d])     -> [a, Concat([b, c]), d]
//   Doc::of_indent(5)                -> Concat([Quad, Quad, Space])

impl Doc {
    /// Builds continuation indentation from quads and an optional space.
    fn of_indent(column_next: usize) -> Doc {
        let mut docs = vec![Doc::Quad; column_next / 2];
        if !column_next.is_multiple_of(2) {
            docs.push(Doc::Space);
        }
        Doc::concat(docs)
    }

    /// Exposes concrete left-stack rows while retaining other wrappers.
    fn into_lines(self) -> Vec<Doc> {
        match self {
            Doc::LeftStack(docs) => docs,
            doc => vec![doc],
        }
    }

    /// Collapses zero or one resolved lines without filtering empty rows.
    fn of_lines(mut docs: Vec<Doc>) -> Doc {
        match docs.len() {
            0 => Doc::Empty,
            1 => docs.pop().unwrap(),
            _ => Doc::LeftStack(docs),
        }
    }
}

/// Joins the last left line with the first right line.
fn concat_lines(mut lines_l: Vec<Doc>, lines_r: Vec<Doc>) -> Vec<Doc> {
    let mut lines_r = lines_r.into_iter();
    // Resolution yields at least one line on each side
    let line_l = lines_l.pop().expect("left lines are nonempty");
    let line_r = lines_r.next().expect("right lines are nonempty");
    let line_joined = Doc::concat(vec![line_l, line_r]);
    lines_l.push(line_joined);
    lines_l.extend(lines_r);
    lines_l
}

/// Measures the final column, including the initial offset on one line.
fn column_after_lines(column: usize, lines: &[Doc]) -> usize {
    match lines {
        [] => column,
        [doc] => column + measure::flat(doc),
        [.., doc] => measure::flat(doc),
    }
}

// - Suffix widths
//
//   width_suffix_of_docs(Flat, 3, [y, SoftBreak(SoftSpace), z])     -> width y + 1 + width z + 3
//   width_suffix_of_docs(Broken, 3, [y, SoftBreak(SoftSpace), z])   -> width y

/// Finds the same-mode prefix width and whether a soft break ends it.
fn width_before_break(doc: &Doc) -> (usize, bool) {
    match doc {
        Doc::Concat(docs) => width_before_break_in_docs(docs),
        Doc::Group(doc)
        | Doc::Mathbin(doc)
        | Doc::Mathrel(doc)
        | Doc::Displaystyle(doc)
        | Doc::Link(_, doc)
        | Doc::Nest(_, doc) => width_before_break(doc),
        Doc::SoftBreak(_) => (0, true),
        Doc::Fill(_, separator, docs) => {
            let docs = Doc::fill_line(separator, docs);
            width_before_break_in_docs(docs)
        }
        _ => (measure::flat(doc), false),
    }
}

/// Counts only the prefix before the first same-mode soft break.
fn width_before_break_in_docs<'a>(docs: impl IntoIterator<Item = &'a Doc>) -> (usize, bool) {
    let mut width = 0;
    // Nested layout groups choose their own mode and remain atomic here
    for doc in docs {
        let (width_doc, has_break) = width_before_break(doc);
        width += width_doc;
        if has_break {
            return (width, true);
        }
    }
    (width, false)
}

/// Reserves either the full suffix or only its first unbroken prefix.
fn width_suffix_of_docs(mode: Mode, width_suffix: usize, docs: &[Doc]) -> usize {
    match mode {
        Mode::Flat => {
            let width_docs = docs.iter().map(measure::flat).sum::<usize>();
            width_suffix + width_docs
        }
        Mode::Broken => {
            let (width, has_break) = width_before_break_in_docs(docs);
            if has_break { width } else { width + width_suffix }
        }
    }
}

// - Concatenation
//
//   Concat([x, y])   -> y starts where the last line of x ends

/// Resolves each child after the lines produced by its predecessors.
fn resolve_concat_lines(
    mode: Mode,
    place: Place,
    docs: &[Doc],
    resolve: impl Fn(Place, &Doc) -> Vec<Doc>,
) -> Vec<Doc> {
    let mut lines = vec![Doc::Empty];
    for (idx, doc) in docs.iter().enumerate() {
        let docs_rest = &docs[idx + 1..];
        let width_suffix = width_suffix_of_docs(mode, place.width_suffix, docs_rest);
        let column = column_after_lines(place.column, &lines);
        let place_doc = Place { column, width_suffix, ..place };
        let lines_doc = resolve(place_doc, doc);
        lines = concat_lines(lines, lines_doc);
    }
    lines
}

// - Scripts
//
//   Sub(x, i)   -> x reserves the script width of i after it;
//                  i resolves within twice the columns left after x

/// Converts remaining normal-size columns into a positive script budget.
fn width_script_budget(place: Place, doc_base: &Doc) -> usize {
    let width_base = measure::flat(doc_base);
    let width_used = place.column + place.width_suffix + width_base;
    let width_remaining = place.width.saturating_sub(width_used);
    (2 * width_remaining).max(1)
}

/// Resolves a script base while reserving its widest script after it.
fn resolve_script_base(
    place: Place,
    doc_base: &Doc,
    docs_script: &[&Doc],
    resolve: impl Fn(Place, &Doc) -> Doc,
) -> Doc {
    let widths_script = docs_script
        .iter()
        .map(|doc| measure::flat_script_width(doc));
    let width_script = widths_script.max().unwrap_or(0);
    let width_suffix = place.width_suffix + width_script;
    let place_base = Place { width_suffix, ..place };
    resolve(place_base, doc_base)
}

/// Resolves a script at column zero within the budget left by its base.
fn resolve_script(
    place: Place,
    doc_base: &Doc,
    doc_script: &Doc,
    resolve: impl Fn(Place, &Doc) -> Doc,
) -> Doc {
    let width = width_script_budget(place, doc_base);
    let place_script = Place { width, column: 0, width_suffix: 0, ..place };
    resolve(place_script, doc_script)
}

// - Fractions
//
//   Fraction(p, q)   -> p and q each resolve from column zero at width - 2

/// Resolves numerator and denominator as fresh lines inside the fraction margin.
fn resolve_fraction_parts(
    place: Place,
    doc_num: &Doc,
    doc_den: &Doc,
    resolve: impl Fn(Place, &Doc) -> Doc,
) -> (Doc, Doc) {
    let width = place.width.saturating_sub(measure::FRACTION_MARGIN).max(1);
    let place_child = Place::start(width);
    let doc_num = resolve(place_child, doc_num);
    let doc_den = resolve(place_child, doc_den);
    (doc_num, doc_den)
}

// - Fresh rows
//
//   resolve_rows_fresh(80, [x, y])   -> [x, y], each resolved from column zero

/// Resolves each row as a fresh line of the same width.
fn resolve_rows_fresh(width: usize, docs: &[Doc]) -> Vec<Doc> {
    let place = Place::start(width);
    docs.iter().map(|doc| resolve_doc(place, doc)).collect()
}

// == Documents

// - Document
//
//   resolve_doc(Place::start(80), Concat([x, SoftBreak(SoftSpace), y]))   -> Concat([x, Space, y])

/// Resolves children at their current column and reserved suffix width.
fn resolve_doc(place: Place, doc: &Doc) -> Doc {
    match doc {
        Doc::Empty
        | Doc::Styled(..)
        | Doc::Badge(_)
        | Doc::Decimal(_)
        | Doc::Hexadecimal(_)
        | Doc::Fixed(_)
        | Doc::Space
        | Doc::ThinSpace
        | Doc::Quad => resolve_atom(doc),
        Doc::Concat(docs) => resolve_concat(place, docs),
        Doc::Group(doc) => resolve_group(place, doc),
        Doc::Mathbin(doc) => resolve_mathbin(place, doc),
        Doc::Mathrel(doc) => resolve_mathrel(place, doc),
        Doc::Displaystyle(doc) => resolve_displaystyle(place, doc),
        Doc::Delimited(delimiter, doc) => resolve_delimited(place, *delimiter, doc),
        Doc::Sub(doc_base, doc_sub) => resolve_sub(place, doc_base, doc_sub),
        Doc::Sup(doc_base, doc_sup) => resolve_sup(place, doc_base, doc_sup),
        Doc::Subsup(doc_base, doc_sub, doc_sup) => {
            resolve_subsup(place, doc_base, doc_sub, doc_sup)
        }
        Doc::Fraction(doc_num, doc_den) => resolve_fraction(place, doc_num, doc_den),
        Doc::Link(target, doc) => resolve_link(place, target, doc),
        Doc::SoftBreak(soft) => resolve_soft_break(*soft),
        Doc::LayoutGroup(doc) => resolve_layout_group(place, doc),
        Doc::Nest(indent, doc) => resolve_nest(place, *indent, doc),
        Doc::Fill(indent, separator, docs) => resolve_fill(place, *indent, separator, docs),
        Doc::Aligned(rows) => resolve_aligned(place, rows),
        Doc::Grid(alignments, rows) => resolve_grid(place, alignments, rows),
        Doc::Stacked(docs) => resolve_stacked(place, docs),
        Doc::LeftStack(docs) => resolve_left_stack(place, docs),
        Doc::Numbered(docs) => resolve_numbered(place, docs),
        Doc::Gathered(blocks) => resolve_gathered(place, blocks),
    }
}

// - Atomic documents
//
//   Styled(Mathsf, "x")   -> Styled(Mathsf, "x")

fn resolve_atom(doc: &Doc) -> Doc {
    doc.clone()
}

// - Concatenated documents
//
//   Concat([x, y])   -> the resolved lines of x, joined with those of y

fn resolve_concat(place: Place, docs: &[Doc]) -> Doc {
    let resolve = |place: Place, doc: &Doc| {
        let doc = resolve_doc(place, doc);
        doc.into_lines()
    };
    let lines = resolve_concat_lines(Mode::Flat, place, docs, resolve);
    Doc::of_lines(lines)
}

// - Groups
//
//   Group(x)   -> Group(resolved x)

fn resolve_group(place: Place, doc: &Doc) -> Doc {
    let doc = resolve_doc(place, doc);
    Doc::Group(Box::new(doc))
}

// - Binary classifications
//
//   Mathbin(x)   -> Mathbin(resolved x)

fn resolve_mathbin(place: Place, doc: &Doc) -> Doc {
    let doc = resolve_doc(place, doc);
    Doc::Mathbin(Box::new(doc))
}

// - Relation classifications
//
//   Mathrel(x)   -> Mathrel(resolved x)

fn resolve_mathrel(place: Place, doc: &Doc) -> Doc {
    let doc = resolve_doc(place, doc);
    Doc::Mathrel(Box::new(doc))
}

// - Display style
//
//   Displaystyle(x)   -> Displaystyle(resolved x)

fn resolve_displaystyle(place: Place, doc: &Doc) -> Doc {
    let doc = resolve_doc(place, doc);
    Doc::Displaystyle(Box::new(doc))
}

// - Delimited documents
//
//   Delimited(Paren, x)   -> Delimited(Paren, x resolved at width - 2)

fn resolve_delimited(place: Place, delimiter: Delimiter, doc: &Doc) -> Doc {
    let width = place.width.saturating_sub(measure::DELIMITER_MARGIN).max(1);
    let place_child = Place { width, ..place };
    let doc = resolve_doc(place_child, doc);
    Doc::Delimited(delimiter, Box::new(doc))
}

// - Subscripts
//
//   Sub(x, i)   -> Sub(x resolved before script i, i resolved in the script budget)

fn resolve_sub(place: Place, doc_base: &Doc, doc_sub: &Doc) -> Doc {
    let doc_base = resolve_script_base(place, doc_base, &[doc_sub], resolve_doc);
    let doc_sub = resolve_script(place, &doc_base, doc_sub, resolve_doc);
    Doc::Sub(Box::new(doc_base), Box::new(doc_sub))
}

// - Superscripts
//
//   Sup(x, n)   -> Sup(x resolved before script n, n resolved in the script budget)

fn resolve_sup(place: Place, doc_base: &Doc, doc_sup: &Doc) -> Doc {
    let doc_base = resolve_script_base(place, doc_base, &[doc_sup], resolve_doc);
    let doc_sup = resolve_script(place, &doc_base, doc_sup, resolve_doc);
    Doc::Sup(Box::new(doc_base), Box::new(doc_sup))
}

// - Paired scripts
//
//   Subsup(x, i, n)   -> x reserves the wider of i and n; both scripts share one budget

fn resolve_subsup(place: Place, doc_base: &Doc, doc_sub: &Doc, doc_sup: &Doc) -> Doc {
    let docs_script = [doc_sub, doc_sup];
    let doc_base = resolve_script_base(place, doc_base, &docs_script, resolve_doc);
    let doc_sub = resolve_script(place, &doc_base, doc_sub, resolve_doc);
    let doc_sup = resolve_script(place, &doc_base, doc_sup, resolve_doc);
    Doc::Subsup(Box::new(doc_base), Box::new(doc_sub), Box::new(doc_sup))
}

// - Fractions
//
//   Fraction(p, q)   -> Fraction(p and q resolved at width - 2)

fn resolve_fraction(place: Place, doc_num: &Doc, doc_den: &Doc) -> Doc {
    let (doc_num, doc_den) = resolve_fraction_parts(place, doc_num, doc_den, resolve_doc);
    Doc::Fraction(Box::new(doc_num), Box::new(doc_den))
}

// - Links
//
//   Link(a, LeftStack([x, y]))   -> LeftStack([Link(a, x), Link(a, y)])

fn resolve_link(place: Place, target: &Target, doc: &Doc) -> Doc {
    let doc = resolve_doc(place, doc);
    link::link_resolved_doc(target, doc)
}

// - Soft breaks
//
//   SoftBreak(SoftCut)     -> Empty
//   SoftBreak(SoftSpace)   -> Space

fn resolve_soft_break(soft: Soft) -> Doc {
    match soft {
        Soft::SoftCut => Doc::Empty,
        Soft::SoftSpace => Doc::Space,
    }
}

// - Layout groups
//
//   LayoutGroup([a, Nest(4, [SoftBreak(SoftSpace), +, Space, b])])
//     fits        a + b
//     overflows   a
//                     + b

/// Chooses a flat or broken mode including the pending suffix width.
fn resolve_layout_group(place: Place, doc: &Doc) -> Doc {
    let width_flat = place.column + measure::flat(doc) + place.width_suffix;
    let mode = if width_flat <= place.width { Mode::Flat } else { Mode::Broken };
    let lines = resolve_in_mode(mode, place, doc);
    Doc::of_lines(lines)
}

// - Nested documents
//
//   Nest(2, x)   -> x, with continuation lines 2 columns further in

fn resolve_nest(place: Place, indent: usize, doc: &Doc) -> Doc {
    let column_next = place.column_next + indent;
    let place_nested = Place { column_next, ..place };
    resolve_doc(place_nested, doc)
}

// - Fills
//
//   Fill(2, ThinSpace, [a, b, c, d])
//     fits        a b c d
//     overflows   a b
//                   c
//                   d

/// Packs subsequent items while reserving the enclosing suffix for the last.
fn resolve_fill(place: Place, indent: usize, separator: &Doc, docs: &[Doc]) -> Doc {
    let column_next = place.column_next + indent;
    let place = Place { column_next, ..place };
    // The first item starts on the current line without a separator
    let Some((doc_head, docs)) = docs.split_first() else {
        return Doc::Empty;
    };
    let width_suffix_head = if docs.is_empty() { place.width_suffix } else { 0 };
    let place_head = Place { width_suffix: width_suffix_head, ..place };
    let doc_head = resolve_doc(place_head, doc_head);
    let mut lines = doc_head.into_lines();
    let width_separator = measure::flat(separator);
    // Overflow starts a fresh indented line and drops the separator
    for (idx, doc) in docs.iter().enumerate() {
        let is_last = idx + 1 == docs.len();
        let width_suffix = if is_last { place.width_suffix } else { 0 };
        let column = column_after_lines(place.column, &lines);
        let width_needed = width_separator + measure::flat(doc) + width_suffix;
        // Continue the current line after the separator
        if column + width_needed <= place.width {
            let column_doc = column + width_separator;
            let place_doc = Place { column: column_doc, width_suffix, ..place };
            let doc = resolve_doc(place_doc, doc);
            let doc = Doc::concat(vec![separator.clone(), doc]);
            let lines_doc = doc.into_lines();
            lines = concat_lines(lines, lines_doc);
            continue;
        }
        // Start a new line at the continuation column
        let place_doc = Place { column: place.column_next, width_suffix, ..place };
        let doc = resolve_doc(place_doc, doc);
        let mut lines_doc = doc.into_lines();
        if let Some(line_head) = lines_doc.first_mut() {
            let doc_indent = Doc::of_indent(place.column_next);
            let doc_head = std::mem::replace(line_head, Doc::Empty);
            *line_head = Doc::concat(vec![doc_indent, doc_head]);
        }
        lines.extend(lines_doc);
    }
    Doc::of_lines(lines)
}
// - Aligned documents
//
//   Aligned([[f(x), =, y]])   -> each cell resolves within the width the other columns leave,
//                                repeated until the column widths stop changing

/// Resolves each cell with space reserved for the other columns.
fn resolve_cells(
    width: usize,
    column_widths: &[usize],
    alignments: Option<&[Alignment]>,
    docs: &[Doc],
) -> Vec<Doc> {
    let mut column = 0;
    let mut docs_resolved = Vec::new();
    // Candidate widths determine both alignment padding and remaining space
    for (idx, doc) in docs.iter().enumerate() {
        let width_doc = measure::flat(doc);
        let width_cell = column_widths.get(idx).copied().unwrap_or(width_doc);
        let width_remaining = match column_widths.get(idx + 1..) {
            // Shared columns include one gap each after the current cell
            Some(widths) => {
                let width_columns = widths.iter().sum::<usize>();
                width_columns + measure::INTERCOLUMN_SPACING * widths.len()
            }
            // A ragged row can introduce columns absent from the candidate
            None => {
                let docs_rest = &docs[idx + 1..];
                let width_docs = docs_rest.iter().map(measure::flat).sum::<usize>();
                width_docs + measure::INTERCOLUMN_SPACING * docs_rest.len()
            }
        };
        let alignment = alignments.and_then(|alignments| alignments.get(idx));
        let padding = match alignment {
            Some(Alignment::Center) => width_cell.saturating_sub(width_doc) / 2,
            Some(Alignment::Right) => width_cell.saturating_sub(width_doc),
            _ => 0,
        };
        let width_local = width.saturating_sub(column + width_remaining).max(1);
        let place_line = Place::start(width_local);
        let place_cell = Place { column: padding, ..place_line };
        let doc_resolved = resolve_doc(place_cell, doc);
        docs_resolved.push(doc_resolved);
        column += width_cell + measure::INTERCOLUMN_SPACING;
    }
    docs_resolved
}

/// Stabilizes shared widths and retains the narrowest candidate on a cycle.
fn resolve_rows<'a>(
    width: usize,
    alignments: Option<&[Alignment]>,
    rows: impl Iterator<Item = &'a [Doc]> + Clone,
) -> Vec<Vec<Doc>> {
    let mut column_widths = measure::flat_column_widths(rows.clone());
    let mut seen = vec![column_widths.clone()];
    let mut best: Option<(Vec<usize>, Vec<Vec<Doc>>)> = None;
    // Always resolve the original rows at the current candidate widths
    loop {
        let rows_resolved: Vec<_> = rows
            .clone()
            .map(|docs| resolve_cells(width, &column_widths, alignments, docs))
            .collect();
        let rows_measured = rows_resolved.iter().map(Vec::as_slice);
        let column_widths_resolved = measure::flat_column_widths(rows_measured);
        // Keep the narrowest resolution seen so far
        let width_resolved = measure::flat_columns(&column_widths_resolved);
        let is_narrower = match &best {
            None => true,
            Some((widths_best, _)) => width_resolved < measure::flat_columns(widths_best),
        };
        if is_narrower {
            best = Some((column_widths_resolved.clone(), rows_resolved.clone()));
        }
        // A fixed point wins even if an earlier candidate was narrower
        if column_widths_resolved == column_widths {
            return rows_resolved;
        }
        // A repeated candidate means a cycle
        if seen.contains(&column_widths_resolved) {
            return best.unwrap().1;
        }
        seen.push(column_widths_resolved.clone());
        column_widths = column_widths_resolved;
    }
}

/// Resolves shared-width rows without alignment padding.
fn resolve_aligned(place: Place, rows: &[Vec<Doc>]) -> Doc {
    let rows = rows.iter().map(Vec::as_slice);
    let rows = resolve_rows(place.width, None, rows);
    Doc::Aligned(rows)
}

// - Grids
//
//   Grid(_, [Cells(docs), Spanning(c)])   -> cells share columns; c resolves at the full width

/// Replaces cell rows in order and independently resolves spanning rows.
fn resolve_grid(place: Place, alignments: &[Alignment], rows: &[GridRow]) -> Doc {
    let rows_cell = rows.iter().filter_map(|row| match row {
        GridRow::Cells(docs) => Some(docs.as_slice()),
        _ => None,
    });
    let rows_cell = resolve_rows(place.width, Some(alignments), rows_cell);
    let mut rows_cell = rows_cell.into_iter();
    // The same cell-row filter determines both production and consumption
    let rows = rows
        .iter()
        .map(|row| match row {
            GridRow::Cells(_) => {
                let docs = rows_cell
                    .next()
                    .expect("each input cell row has one resolved row");
                GridRow::Cells(docs)
            }
            GridRow::Spanning(doc) => {
                let place_line = Place::start(place.width);
                let doc = resolve_doc(place_line, doc);
                GridRow::Spanning(doc)
            }
            GridRow::Gap => GridRow::Gap,
        })
        .collect();
    Doc::Grid(alignments.to_vec(), rows)
}

// - Stacked documents
//
//   Stacked([p, q])   -> Stacked([resolved p, resolved q]), each from column zero

fn resolve_stacked(place: Place, docs: &[Doc]) -> Doc {
    let docs = resolve_rows_fresh(place.width, docs);
    Doc::Stacked(docs)
}

// - Left-stacked documents
//
//   LeftStack([x, y])   -> LeftStack([resolved x, resolved y]), each from column zero

fn resolve_left_stack(place: Place, docs: &[Doc]) -> Doc {
    let docs = resolve_rows_fresh(place.width, docs);
    Doc::LeftStack(docs)
}

// - Numbered documents
//
//   Numbered([p, q])   -> p and q resolved after reserving the label gutter

fn resolve_numbered(place: Place, docs: &[Doc]) -> Doc {
    let width_gutter = measure::flat_numbered_gutter(docs.len());
    let width = place.width.saturating_sub(width_gutter).max(1);
    let docs = resolve_rows_fresh(width, docs);
    Doc::Numbered(docs)
}

// - Gathered documents
//
//   Gathered([Line(x), Gap])   -> Gathered([Line(resolved x), Gap])

fn resolve_gathered(place: Place, blocks: &[Block]) -> Doc {
    let place_line = Place::start(place.width);
    let blocks = blocks
        .iter()
        .map(|block| match block {
            Block::Line(doc) => {
                let doc = resolve_doc(place_line, doc);
                Block::Line(doc)
            }
            Block::Gap => Block::Gap,
        })
        .collect();
    Doc::Gathered(blocks)
}

// == Mode-specific documents

// - Document
//
//   resolve_in_mode(Broken, place, Concat([x, SoftBreak(SoftSpace), y]))
//   -> [x, indentation to column_next + y]

/// Propagates one mode through wrappers while nested groups choose afresh.
fn resolve_in_mode(mode: Mode, place: Place, doc: &Doc) -> Vec<Doc> {
    match doc {
        Doc::Concat(docs) => resolve_concat_in_mode(mode, place, docs),
        Doc::Group(doc) => resolve_group_in_mode(mode, place, doc),
        Doc::Mathbin(doc) => resolve_mathbin_in_mode(mode, place, doc),
        Doc::Mathrel(doc) => resolve_mathrel_in_mode(mode, place, doc),
        Doc::Displaystyle(doc) => resolve_displaystyle_in_mode(mode, place, doc),
        Doc::Delimited(delimiter, doc) => resolve_delimited_in_mode(mode, place, *delimiter, doc),
        Doc::Sub(doc_base, doc_sub) => resolve_sub_in_mode(mode, place, doc_base, doc_sub),
        Doc::Sup(doc_base, doc_sup) => resolve_sup_in_mode(mode, place, doc_base, doc_sup),
        Doc::Subsup(doc_base, doc_sub, doc_sup) => {
            resolve_subsup_in_mode(mode, place, doc_base, doc_sub, doc_sup)
        }
        Doc::Fraction(doc_num, doc_den) => resolve_fraction_in_mode(mode, place, doc_num, doc_den),
        Doc::Link(target, doc) => resolve_link_in_mode(mode, place, target, doc),
        Doc::SoftBreak(soft) => resolve_soft_break_in_mode(mode, place, *soft),
        Doc::LayoutGroup(doc) => resolve_layout_group_in_mode(place, doc),
        Doc::Nest(indent, doc) => resolve_nest_in_mode(mode, place, *indent, doc),
        doc => resolve_leaf_in_mode(place, doc),
    }
}

/// Resolves a document in one mode and collapses its lines into one document.
fn resolve_single_in_mode(mode: Mode, place: Place, doc: &Doc) -> Doc {
    let lines = resolve_in_mode(mode, place, doc);
    Doc::of_lines(lines)
}

// - Concatenated documents
//
//   Concat([x, y])   -> the lines of x and y merged under the selected mode

fn resolve_concat_in_mode(mode: Mode, place: Place, docs: &[Doc]) -> Vec<Doc> {
    let resolve = |place: Place, doc: &Doc| resolve_in_mode(mode, place, doc);
    resolve_concat_lines(mode, place, docs, resolve)
}

// - Groups
//
//   Group(x)   -> Group around each resolved line of x

fn resolve_group_in_mode(mode: Mode, place: Place, doc: &Doc) -> Vec<Doc> {
    let lines = resolve_in_mode(mode, place, doc);
    lines.into_iter().map(Box::new).map(Doc::Group).collect()
}

// - Binary classifications
//
//   Mathbin(x)   -> Mathbin around each resolved line of x

fn resolve_mathbin_in_mode(mode: Mode, place: Place, doc: &Doc) -> Vec<Doc> {
    let lines = resolve_in_mode(mode, place, doc);
    lines.into_iter().map(Box::new).map(Doc::Mathbin).collect()
}

// - Relation classifications
//
//   Mathrel(x)   -> Mathrel around each resolved line of x

fn resolve_mathrel_in_mode(mode: Mode, place: Place, doc: &Doc) -> Vec<Doc> {
    let lines = resolve_in_mode(mode, place, doc);
    lines.into_iter().map(Box::new).map(Doc::Mathrel).collect()
}

// - Display style
//
//   Displaystyle(x)   -> [Displaystyle(resolved multiline x)]

fn resolve_displaystyle_in_mode(mode: Mode, place: Place, doc: &Doc) -> Vec<Doc> {
    let doc = resolve_single_in_mode(mode, place, doc);
    vec![Doc::Displaystyle(Box::new(doc))]
}

// - Delimited documents
//
//   Delimited(Paren, x)   -> [one delimiter pair around resolved multiline x]

fn resolve_delimited_in_mode(
    mode: Mode,
    place: Place,
    delimiter: Delimiter,
    doc: &Doc,
) -> Vec<Doc> {
    let width = place.width.saturating_sub(measure::DELIMITER_MARGIN).max(1);
    let place_child = Place { width, ..place };
    let doc = resolve_single_in_mode(mode, place_child, doc);
    vec![Doc::Delimited(delimiter, Box::new(doc))]
}

// - Subscripts
//
//   Sub(x, i)   -> [Sub(resolved x, resolved i)]

fn resolve_sub_in_mode(mode: Mode, place: Place, doc_base: &Doc, doc_sub: &Doc) -> Vec<Doc> {
    let resolve = |place: Place, doc: &Doc| resolve_single_in_mode(mode, place, doc);
    let doc_base = resolve_script_base(place, doc_base, &[doc_sub], resolve);
    let doc_sub = resolve_script(place, &doc_base, doc_sub, resolve);
    vec![Doc::Sub(Box::new(doc_base), Box::new(doc_sub))]
}

// - Superscripts
//
//   Sup(x, n)   -> [Sup(resolved x, resolved n)]

fn resolve_sup_in_mode(mode: Mode, place: Place, doc_base: &Doc, doc_sup: &Doc) -> Vec<Doc> {
    let resolve = |place: Place, doc: &Doc| resolve_single_in_mode(mode, place, doc);
    let doc_base = resolve_script_base(place, doc_base, &[doc_sup], resolve);
    let doc_sup = resolve_script(place, &doc_base, doc_sup, resolve);
    vec![Doc::Sup(Box::new(doc_base), Box::new(doc_sup))]
}

// - Paired scripts
//
//   Subsup(x, i, n)   -> [Subsup(resolved x, resolved i, resolved n)]

fn resolve_subsup_in_mode(
    mode: Mode,
    place: Place,
    doc_base: &Doc,
    doc_sub: &Doc,
    doc_sup: &Doc,
) -> Vec<Doc> {
    let resolve = |place: Place, doc: &Doc| resolve_single_in_mode(mode, place, doc);
    let docs_script = [doc_sub, doc_sup];
    let doc_base = resolve_script_base(place, doc_base, &docs_script, resolve);
    let doc_sub = resolve_script(place, &doc_base, doc_sub, resolve);
    let doc_sup = resolve_script(place, &doc_base, doc_sup, resolve);
    vec![Doc::Subsup(Box::new(doc_base), Box::new(doc_sub), Box::new(doc_sup))]
}

// - Fractions
//
//   Fraction(p, q)   -> [Fraction(resolved p, resolved q)]

fn resolve_fraction_in_mode(mode: Mode, place: Place, doc_num: &Doc, doc_den: &Doc) -> Vec<Doc> {
    let resolve = |place: Place, doc: &Doc| resolve_single_in_mode(mode, place, doc);
    let (doc_num, doc_den) = resolve_fraction_parts(place, doc_num, doc_den, resolve);
    vec![Doc::Fraction(Box::new(doc_num), Box::new(doc_den))]
}

// - Links
//
//   Link(a, x)   -> Link(a, _) around each resolved line of x

fn resolve_link_in_mode(mode: Mode, place: Place, target: &Target, doc: &Doc) -> Vec<Doc> {
    let lines = resolve_in_mode(mode, place, doc);
    lines
        .into_iter()
        .map(|doc| link::link_resolved_doc(target, doc))
        .collect()
}

// - Soft breaks
//
//   SoftBreak(SoftSpace) in Flat     -> [Space]
//   SoftBreak(SoftSpace) in Broken   -> [Empty, indentation to column_next]

fn resolve_soft_break_in_mode(mode: Mode, place: Place, soft: Soft) -> Vec<Doc> {
    match (mode, soft) {
        (Mode::Flat, Soft::SoftCut) => vec![Doc::Empty],
        (Mode::Flat, Soft::SoftSpace) => vec![Doc::Space],
        (Mode::Broken, _) => {
            let doc_indent = Doc::of_indent(place.column_next);
            vec![Doc::Empty, doc_indent]
        }
    }
}

// - Layout groups
//
//   LayoutGroup(x)   -> the lines of x, choosing flat or broken afresh

fn resolve_layout_group_in_mode(place: Place, doc: &Doc) -> Vec<Doc> {
    let doc = resolve_layout_group(place, doc);
    doc.into_lines()
}

// - Nested documents
//
//   Nest(2, x)   -> x in the selected mode, with continuation lines 2 columns further in

fn resolve_nest_in_mode(mode: Mode, place: Place, indent: usize, doc: &Doc) -> Vec<Doc> {
    let column_next = place.column_next + indent;
    let place_nested = Place { column_next, ..place };
    resolve_in_mode(mode, place_nested, doc)
}

// - Other documents
//
//   Fill(2, s, docs)   -> [resolve_doc(place, Fill(2, s, docs))]

fn resolve_leaf_in_mode(place: Place, doc: &Doc) -> Vec<Doc> {
    let doc = resolve_doc(place, doc);
    vec![doc]
}

// == Entry point
//
//   resolve(0, doc)    -> Err(InvalidLayoutWidth)
//   resolve(80, doc)   -> resolve_doc(Place::start(80), doc)

/// Resolves a document at a strictly positive line width.
pub(crate) fn resolve(width: usize, doc: &Doc) -> Result<Doc> {
    if width == 0 {
        return Err(Error::InvalidLayoutWidth);
    }
    let place = Place::start(width);
    let doc = resolve_doc(place, doc);
    Ok(doc)
}
