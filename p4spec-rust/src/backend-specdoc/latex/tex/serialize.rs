//! TeX text for resolved documents through the shared printer
//!
//! ```text
//! Styled(Mathsf, "TC_0")                  -> \mathsf{TC\_0}
//! Styled(Text, "a~b")                     -> \text{a}\sim{}\text{b}
//! Sub(x, i)                               -> {x}_{i}
//! Fraction(p, q)                          -> \frac{p}{q}
//! Delimited(Angle, Styled(Mathbb, "N"))   -> \left\langle\mathbb{N}\right\rangle
//! Grid(_, [Cells(x), Gap, Cells(y)])      -> x \\[1ex] y
//! ```
//!
//! A grid mixing cell and spanning rows renders once in `\mathrlap`,
//! then reserves its width with a link-free `\hphantom` copy.

use super::{doc::*, link};
use crate::lang::traits::print::{Print, Printer};
use num_bigint::BigInt;
use std::fmt::{self, Write};

// == Helpers

// - Text escaping
//
//   escape_char('_', Math)          -> \_
//   escape_char('~', Math)          -> \sim{}
//   render_text("texttt", "a\b")    -> \texttt{a}\backslash{}\texttt{b}

/// Escaping context of a styled command.
#[derive(Clone, Copy)]
enum Context {
    Math,
    Text,
}

/// Writes one character with escaping for the selected TeX context.
fn escape_char(output: &mut dyn Write, char: char, context: Context) -> fmt::Result {
    let is_math = matches!(context, Context::Math);
    match char {
        '#' => output.write_str(r"\#"),
        '$' => output.write_str(r"\$"),
        '%' => output.write_str(r"\%"),
        '&' => output.write_str(r"\&"),
        '_' => output.write_str(r"\_"),
        '{' => output.write_str(r"\{"),
        '}' => output.write_str(r"\}"),
        '\\' if is_math => output.write_str(r"\backslash{}"),
        '^' if is_math => output.write_str(r"\hat{}"),
        '~' if is_math => output.write_str(r"\sim{}"),
        char => output.write_char(char),
    }
}

/// Writes one styled run with the escaping rules of its context.
fn render_command(
    output: &mut dyn Write,
    command: &str,
    text: &str,
    context: Context,
) -> fmt::Result {
    write!(output, "\\{command}{{")?;
    // Escape content without changing its font context
    for char in text.chars() {
        escape_char(output, char, context)?;
    }
    output.write_char('}')
}

/// Splits text around math-only backslash, hat, and tilde glyphs.
fn render_text(output: &mut dyn Write, command: &str, text: &str) -> fmt::Result {
    // An empty text still produces its command
    if text.is_empty() {
        return render_command(output, command, "", Context::Text);
    }
    let mut start = 0;
    // Flush ordinary text before emitting a math-only glyph
    for (idx, char) in text.char_indices() {
        if !matches!(char, '\\' | '^' | '~') {
            continue;
        }
        if start < idx {
            let text_run = &text[start..idx];
            render_command(output, command, text_run, Context::Text)?;
        }
        escape_char(output, char, Context::Math)?;
        start = idx + char.len_utf8();
    }
    // A final special glyph has no empty trailing text command
    if start < text.len() {
        let text_run = &text[start..];
        render_command(output, command, text_run, Context::Text)?;
    }
    Ok(())
}

// - Arrays
//
//   render_array([Right, Left], [[x, y], [z, w]])
//   -> \begin{array}{rl}
//      x & y \\
//      z & w
//      \end{array}

/// Writes each column alignment in the supplied order.
fn render_alignments(output: &mut dyn Write, alignments: &[Alignment]) -> fmt::Result {
    for alignment in alignments {
        let text = match alignment {
            Alignment::Left => "l",
            Alignment::Center => "c",
            Alignment::Right => "r",
        };
        output.write_str(text)?;
    }
    Ok(())
}

/// Writes cells in their original order with TeX alignment separators.
fn render_cells(output: &mut dyn Write, docs: &[Doc]) -> fmt::Result {
    // Only intervening cells receive an alignment separator
    for (idx, doc) in docs.iter().enumerate() {
        if idx != 0 {
            output.write_str(" & ")?;
        }
        render_doc(output, doc)?;
    }
    Ok(())
}

/// Writes rows without a trailing line break.
fn render_array_rows<'a>(
    output: &mut dyn Write,
    rows: impl IntoIterator<Item = &'a [Doc]>,
) -> fmt::Result {
    // Array and aligned environments share the same row separators
    for (idx, docs) in rows.into_iter().enumerate() {
        if idx != 0 {
            output.write_str(" \\\\\n")?;
        }
        render_cells(output, docs)?;
    }
    Ok(())
}

/// Encloses aligned rows in a TeX array environment.
fn render_array<'a>(
    output: &mut dyn Write,
    alignments: &[Alignment],
    rows: impl IntoIterator<Item = &'a [Doc]>,
) -> fmt::Result {
    // Declare columns before serializing their content
    output.write_str(r"\begin{array}{")?;
    render_alignments(output, alignments)?;
    output.write_str("}\n")?;
    // Rows own separators; the enclosing environment owns outer newlines
    render_array_rows(output, rows)?;
    output.write_str("\n\\end{array}")
}

// == Documents

// - Document
//
//   Concat([Sub(x, i), Space, y])   -> {x}_{i} y

/// Dispatches each document variant to its TeX writer.
fn render_doc(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    match doc {
        Doc::Empty => render_empty(output),
        Doc::Styled(style, text) => render_styled(output, *style, text),
        Doc::Badge(text) => render_badge(output, text),
        Doc::Decimal(num) => render_decimal(output, num),
        Doc::Hexadecimal(num) => render_hexadecimal(output, num),
        Doc::Fixed(symbol) => render_fixed(output, *symbol),
        Doc::Space => render_space(output),
        Doc::ThinSpace => render_thin_space(output),
        Doc::Quad => render_quad(output),
        Doc::Concat(docs) => render_concat(output, docs),
        Doc::Group(doc) => render_group(output, doc),
        Doc::Mathbin(doc) => render_mathbin(output, doc),
        Doc::Mathrel(doc) => render_mathrel(output, doc),
        Doc::Displaystyle(doc) => render_displaystyle(output, doc),
        Doc::Delimited(delimiter, doc) => render_delimited(output, *delimiter, doc),
        Doc::Sub(doc_base, doc_sub) => render_sub(output, doc_base, doc_sub),
        Doc::Sup(doc_base, doc_sup) => render_sup(output, doc_base, doc_sup),
        Doc::Subsup(doc_base, doc_sub, doc_sup) => {
            render_subsup(output, doc_base, doc_sub, doc_sup)
        }
        Doc::Fraction(doc_num, doc_den) => render_fraction(output, doc_num, doc_den),
        Doc::Link(target, doc) => render_link(output, target, doc),
        Doc::SoftBreak(soft) => render_soft_break(output, *soft),
        Doc::LayoutGroup(doc) => render_layout_group(output, doc),
        Doc::Nest(_, doc) => render_nest(output, doc),
        Doc::Fill(_, separator, docs) => render_fill(output, separator, docs),
        Doc::Aligned(rows) => render_aligned(output, rows),
        Doc::Grid(alignments, rows) => render_grid(output, alignments, rows),
        Doc::Stacked(docs) => render_stacked(output, docs),
        Doc::LeftStack(docs) => render_left_stack(output, docs),
        Doc::Numbered(docs) => render_numbered(output, docs),
        Doc::Gathered(blocks) => render_gathered(output, blocks),
    }
}

/// Writes balanced opening and closing syntax around one document.
fn render_enclosed(output: &mut dyn Write, opening: &str, closing: &str, doc: &Doc) -> fmt::Result {
    output.write_str(opening)?;
    render_doc(output, doc)?;
    output.write_str(closing)
}

/// Writes a borrowed document sequence without implicit separators.
fn render_docs<'a>(output: &mut dyn Write, docs: impl IntoIterator<Item = &'a Doc>) -> fmt::Result {
    for doc in docs {
        render_doc(output, doc)?;
    }
    Ok(())
}

// - Empty documents
//
//   Empty   -> ""

fn render_empty(_output: &mut dyn Write) -> fmt::Result {
    Ok(())
}

// - Styled documents
//
//   Styled(Mathsf, "TC_0")   -> \mathsf{TC\_0}
//   Styled(Text, "a~b")      -> \text{a}\sim{}\text{b}

/// Selects the font command and escaping context of a style.
fn command_of_style(style: Style) -> (&'static str, Context) {
    match style {
        Style::Mathit => ("mathit", Context::Math),
        Style::Mathrm => ("mathrm", Context::Math),
        Style::Mathsf => ("mathsf", Context::Math),
        Style::Mathbb => ("mathbb", Context::Math),
        Style::Mathtt => ("mathtt", Context::Math),
        Style::Text => ("text", Context::Text),
        Style::Texttt => ("texttt", Context::Text),
    }
}

/// Writes styled text, splitting text-mode runs around math-only glyphs.
fn render_styled(output: &mut dyn Write, style: Style, text: &str) -> fmt::Result {
    let (command, context) = command_of_style(style);
    match context {
        Context::Math => render_command(output, command, text, Context::Math),
        Context::Text => render_text(output, command, text),
    }
}

// - Badges
//
//   Badge("R")   -> {\definecolor{...}...\fcolorbox{black}{...}{\scriptsize \texttt{R}}}

const BADGE_OPEN: &str = r"{\definecolor{ellatexrulelabelbg}{rgb}{0.94,0.94,0.92}\fcolorbox{black}{ellatexrulelabelbg}{\scriptsize ";

/// Writes a shaded rule-label box in small monospace text.
fn render_badge(output: &mut dyn Write, text: &str) -> fmt::Result {
    output.write_str(BADGE_OPEN)?;
    render_text(output, "texttt", text)?;
    output.write_str("}}")
}

// - Decimal numbers
//
//   Decimal(42)   -> 42

fn render_decimal(output: &mut dyn Write, num: &BigInt) -> fmt::Result {
    write!(output, "{num}")
}

// - Hexadecimal numbers
//
//   Hexadecimal(255)   -> \mathtt{0xff}

fn render_hexadecimal(output: &mut dyn Write, num: &BigInt) -> fmt::Result {
    write!(output, "\\mathtt{{{num:#x}}}")
}

// - Fixed symbols
//
//   Fixed(Turnstile)   -> \vdash
//   Fixed(Cat)         -> +\!\!+

/// Writes the fixed TeX spelling of a mathematical symbol.
fn render_fixed(output: &mut dyn Write, symbol: Symbol) -> fmt::Result {
    let text = match symbol {
        Symbol::Equal => "=",
        Symbol::NotEqual => r"\ne",
        Symbol::Less => "<",
        Symbol::Greater => ">",
        Symbol::LessEqual => r"\le",
        Symbol::GreaterEqual => r"\ge",
        Symbol::Plus => "+",
        Symbol::Minus => "-",
        Symbol::Question => "?",
        Symbol::Ast => r"\ast",
        Symbol::Slash => "/",
        Symbol::Comma => ",",
        Symbol::Semicolon => ";",
        Symbol::Colon => ":",
        Symbol::DoubleColon => "::",
        Symbol::Cat => r"+\!\!+",
        Symbol::Production => "::=",
        Symbol::VerticalBar => "|",
        Symbol::Dot => ".",
        Symbol::Dot2 => "..",
        Symbol::Ellipsis => r"\ldots",
        Symbol::Epsilon => r"\epsilon",
        Symbol::In => r"\in",
        Symbol::Neg => r"\neg",
        Symbol::Land => r"\land",
        Symbol::Lor => r"\lor",
        Symbol::Rightarrow => r"\Rightarrow",
        Symbol::Leftrightarrow => r"\Leftrightarrow",
        Symbol::Cdot => r"\cdot",
        Symbol::Bmod => r"\bmod",
        Symbol::Turnstile => r"\vdash",
        Symbol::Tilesturn => r"\dashv",
        Symbol::To => r"\to",
        Symbol::Longrightarrow => r"\Longrightarrow",
        Symbol::Hookrightarrow => r"\hookrightarrow",
        Symbol::Mapsto => r"\mapsto",
        Symbol::Sim => r"\sim",
        Symbol::Setminus => r"\setminus",
        Symbol::EmptySet => r"\varnothing",
        Symbol::LeftParen => "(",
        Symbol::RightParen => ")",
        Symbol::LeftBracket => "[",
        Symbol::RightBracket => "]",
        Symbol::LeftBrace => r"\{",
        Symbol::RightBrace => r"\}",
    };
    output.write_str(text)
}

// - Spaces
//
//   Space   -> " "

fn render_space(output: &mut dyn Write) -> fmt::Result {
    output.write_str(" ")
}

// - Thin spaces
//
//   ThinSpace   -> \,

fn render_thin_space(output: &mut dyn Write) -> fmt::Result {
    output.write_str(r"\,")
}

// - Quad spaces
//
//   Quad   -> \quad

fn render_quad(output: &mut dyn Write) -> fmt::Result {
    output.write_str(r"\quad")
}

// - Concatenated documents
//
//   Concat([x, Space, y])   -> x y

fn render_concat(output: &mut dyn Write, docs: &[Doc]) -> fmt::Result {
    render_docs(output, docs)
}

// - Groups
//
//   Group(x)   -> {x}

fn render_group(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_enclosed(output, "{", "}", doc)
}

// - Binary classifications
//
//   Mathbin(x)   -> \mathbin{x}

fn render_mathbin(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_enclosed(output, r"\mathbin{", "}", doc)
}

// - Relation classifications
//
//   Mathrel(x)   -> \mathrel{x}

fn render_mathrel(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_enclosed(output, r"\mathrel{", "}", doc)
}

// - Display style
//
//   Displaystyle(x)   -> {\displaystyle x}

fn render_displaystyle(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_enclosed(output, r"{\displaystyle ", "}", doc)
}

// - Delimited documents
//
//   Delimited(Paren, x)                     -> \left(x\right)
//   Delimited(Angle, Styled(Mathbb, "N"))   -> \left\langle\mathbb{N}\right\rangle

/// Sizes both delimiters to the enclosed mathematical document.
fn render_delimited(output: &mut dyn Write, delimiter: Delimiter, doc: &Doc) -> fmt::Result {
    // Select both sides together so the delimiter pair stays balanced
    let (text_l, text_r) = match delimiter {
        Delimiter::Paren => ("(", ")"),
        Delimiter::Bracket => ("[", "]"),
        Delimiter::Brace => (r"\{", r"\}"),
        Delimiter::Angle => (r"\langle", r"\rangle"),
        Delimiter::Bar => ("|", "|"),
    };
    // Enclose the content with automatically sized delimiters
    write!(output, "\\left{text_l}")?;
    render_doc(output, doc)?;
    write!(output, "\\right{text_r}")
}

// - Subscripts
//
//   Sub(x, i)   -> {x}_{i}

fn render_sub(output: &mut dyn Write, doc_base: &Doc, doc_sub: &Doc) -> fmt::Result {
    render_enclosed(output, "{", "}_{", doc_base)?;
    render_enclosed(output, "", "}", doc_sub)
}

// - Superscripts
//
//   Sup(x, n)   -> {x}^{n}

fn render_sup(output: &mut dyn Write, doc_base: &Doc, doc_sup: &Doc) -> fmt::Result {
    render_enclosed(output, "{", "}^{", doc_base)?;
    render_enclosed(output, "", "}", doc_sup)
}

// - Paired scripts
//
//   Subsup(x, i, n)   -> {x}_{i}^{n}

fn render_subsup(
    output: &mut dyn Write,
    doc_base: &Doc,
    doc_sub: &Doc,
    doc_sup: &Doc,
) -> fmt::Result {
    render_enclosed(output, "{", "}_{", doc_base)?;
    render_enclosed(output, "", "}^{", doc_sub)?;
    render_enclosed(output, "", "}", doc_sup)
}

// - Fractions
//
//   Fraction(p, q)   -> \frac{p}{q}

fn render_fraction(output: &mut dyn Write, doc_num: &Doc, doc_den: &Doc) -> fmt::Result {
    render_enclosed(output, r"\frac{", "}{", doc_num)?;
    render_enclosed(output, "", "}", doc_den)
}

// - Links
//
//   Link(Target("f"), x)   -> \href{#f}{x}

fn render_link(output: &mut dyn Write, target: &Target, doc: &Doc) -> fmt::Result {
    write!(output, "\\href{{#{}}}{{", target.0)?;
    render_enclosed(output, "", "}", doc)
}

// - Soft breaks
//
//   SoftBreak(SoftCut)     -> ""
//   SoftBreak(SoftSpace)   -> " "

fn render_soft_break(output: &mut dyn Write, soft: Soft) -> fmt::Result {
    match soft {
        Soft::SoftCut => Ok(()),
        Soft::SoftSpace => output.write_str(" "),
    }
}

// - Layout groups
//
//   LayoutGroup(x)   -> x

fn render_layout_group(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_doc(output, doc)
}

// - Nested documents
//
//   Nest(2, x)   -> x

fn render_nest(output: &mut dyn Write, doc: &Doc) -> fmt::Result {
    render_doc(output, doc)
}

// - Fills
//
//   Fill(_, ThinSpace, [x, y])   -> x\,y

fn render_fill(output: &mut dyn Write, separator: &Doc, docs: &[Doc]) -> fmt::Result {
    let docs = Doc::fill_line(separator, docs);
    render_docs(output, docs)
}

// - Aligned documents
//
//   Aligned([[x, =, y]])
//   -> \begin{aligned}
//      x & = & y
//      \end{aligned}

fn render_aligned(output: &mut dyn Write, rows: &[Vec<Doc>]) -> fmt::Result {
    output.write_str("\\begin{aligned}\n")?;
    let rows = rows.iter().map(Vec::as_slice);
    render_array_rows(output, rows)?;
    output.write_str("\n\\end{aligned}")
}

// - Grids
//
//   Grid([Left, Center, Left], [Cells([x, =, y]), Gap, Cells([z, =, w])])
//   -> \begin{array}{lcl}
//      x & = & y \\[1ex]
//      z & = & w
//      \end{array}
//
//   A mixed grid renders its visible array once in \mathrlap,
//   then reserves its widest row with a link-free \hphantom copy.

/// Fills every column after a spanning cell with an empty alignment cell.
fn render_grid_empty_cells(output: &mut dyn Write, columns: usize) -> fmt::Result {
    // Preserve the final separator's lack of a trailing space
    for idx in 1..columns {
        let text = if idx + 1 == columns { " &" } else { " & " };
        output.write_str(text)?;
    }
    Ok(())
}

/// Places spanning content over a link-free first-column width copy.
fn render_grid_spanning_row(
    output: &mut dyn Write,
    alignments: &[Alignment],
    docs_column_head: &[Doc],
    doc: &Doc,
) -> fmt::Result {
    // A grid without cell rows needs no first-column geometry
    if docs_column_head.is_empty() {
        render_doc(output, doc)?;
    } else {
        output.write_str(r"\mathrlap{\displaystyle ")?;
        render_doc(output, doc)?;
        output.write_str(r"}\smash{\hphantom{")?;
        let rows_column_head = docs_column_head.iter().map(std::slice::from_ref);
        render_array(output, &[Alignment::Right], rows_column_head)?;
        output.write_str("}}")?;
    }
    render_grid_empty_cells(output, alignments.len())
}

/// Writes content rows, widening the break before each gap's following row.
fn render_grid_rows(
    output: &mut dyn Write,
    alignments: &[Alignment],
    docs_column_head: &[Doc],
    rows: &[GridRow],
) -> fmt::Result {
    for (idx, row) in rows.iter().enumerate() {
        match row {
            // Render ordinary cells in column order
            GridRow::Cells(docs) => render_cells(output, docs)?,
            // Render spanning content over the first-column geometry
            GridRow::Spanning(doc) => {
                render_grid_spanning_row(output, alignments, docs_column_head, doc)?
            }
            // A gap only widens the preceding separator
            GridRow::Gap => continue,
        }
        // Separate this row from the next content row
        match rows.get(idx + 1) {
            Some(GridRow::Gap) => output.write_str(" \\\\[1ex]\n")?,
            Some(_) => output.write_str(" \\\\\n")?,
            None => {}
        }
    }
    Ok(())
}

/// Writes a grid array using first-column widths from its ordinary rows.
fn render_grid_array(
    output: &mut dyn Write,
    alignments: &[Alignment],
    rows_cell: &[&[Doc]],
    rows: &[GridRow],
) -> fmt::Result {
    let docs_column_head: Vec<_> = rows_cell
        .iter()
        .filter_map(|docs| docs.first())
        .map(|doc| link::strip_links(doc.clone()))
        .collect();
    output.write_str(r"\begin{array}{")?;
    render_alignments(output, alignments)?;
    output.write_str("}\n")?;
    render_grid_rows(output, alignments, &docs_column_head, rows)?;
    output.write_str("\n\\end{array}")
}

/// Reserves the maximum width of link-free cell and spanning documents.
fn render_grid_phantom(
    output: &mut dyn Write,
    alignments: &[Alignment],
    rows_cell: &[&[Doc]],
    docs_spanning: &[&Doc],
) -> fmt::Result {
    output.write_str("\\smash{\\hphantom{\\begin{array}{l}\n")?;
    // Remove links from the invisible copy of the ordinary rows
    let rows_cell: Vec<Vec<_>> = rows_cell
        .iter()
        .map(|docs| docs.iter().cloned().map(link::strip_links).collect())
        .collect();
    let rows_cell = rows_cell.iter().map(Vec::as_slice);
    render_array(output, alignments, rows_cell)?;
    // Spanning documents can be wider than the shared cell columns
    for doc in docs_spanning {
        output.write_str(" \\\\\n")?;
        let doc = (*doc).clone();
        let doc = link::strip_links(doc);
        render_doc(output, &doc)?;
    }
    output.write_str("\n\\end{array}}}")
}

/// Gives mixed grids the maximum of their cell and spanning widths.
fn render_grid(output: &mut dyn Write, alignments: &[Alignment], rows: &[GridRow]) -> fmt::Result {
    let rows_cell: Vec<_> = rows
        .iter()
        .filter_map(|row| match row {
            GridRow::Cells(docs) => Some(docs.as_slice()),
            _ => None,
        })
        .collect();
    let docs_spanning: Vec<_> = rows
        .iter()
        .filter_map(|row| match row {
            GridRow::Spanning(doc) => Some(doc),
            _ => None,
        })
        .collect();
    // Spanning-only grids use one left-aligned column
    if rows_cell.is_empty() {
        return render_grid_array(output, &[Alignment::Left], &[], rows);
    }
    // Cell-only grids need no width reservation
    if docs_spanning.is_empty() {
        return render_grid_array(output, alignments, &rows_cell, rows);
    }
    // Emit visible content once, then reserve its complete geometry
    output.write_str(r"\mathrlap{\displaystyle ")?;
    render_grid_array(output, alignments, &rows_cell, rows)?;
    output.write_str("}")?;
    render_grid_phantom(output, alignments, &rows_cell, &docs_spanning)
}

// - Stacked documents
//
//   Stacked([p, q])
//   -> \begin{aligned}
//      & p \\
//      & q
//      \end{aligned}

/// Writes stacked documents after an empty alignment cell on each row.
fn render_stacked(output: &mut dyn Write, docs: &[Doc]) -> fmt::Result {
    output.write_str("\\begin{aligned}\n")?;
    // Keep each document in the same aligned column
    for (idx, doc) in docs.iter().enumerate() {
        if idx != 0 {
            output.write_str(" \\\\\n")?;
        }
        output.write_str("& ")?;
        render_doc(output, doc)?;
    }
    output.write_str("\n\\end{aligned}")
}

// - Left-stacked documents
//
//   LeftStack([x, y])
//   -> \begin{array}{l}
//      x \\
//      y
//      \end{array}

fn render_left_stack(output: &mut dyn Write, docs: &[Doc]) -> fmt::Result {
    let rows = docs.iter().map(std::slice::from_ref);
    render_array(output, &[Alignment::Left], rows)
}

// - Numbered documents
//
//   Numbered([p, LeftStack([q, r])])
//   -> \begin{array}{r@{\quad}l}
//      {\scriptstyle\mathtt{(1)}} & p \\
//      {\scriptstyle\mathtt{(2)}} & q \\
//       & r
//      \end{array}

/// Writes an optional premise label followed by its body cell.
fn render_numbered_row(output: &mut dyn Write, num: Option<usize>, doc: &Doc) -> fmt::Result {
    if let Some(num) = num {
        write!(output, "{{\\scriptstyle\\mathtt{{({num})}}}}")?;
    }
    output.write_str(" & ")?;
    render_doc(output, doc)
}

/// Prints each premise number once, leaving continuation labels empty.
fn render_numbered(output: &mut dyn Write, docs: &[Doc]) -> fmt::Result {
    output.write_str("\\begin{array}{r@{\\quad}l}\n")?;
    // An empty left stack still consumes a number
    for (idx, doc) in docs.iter().enumerate() {
        let num = idx + 1;
        match doc {
            // Expand a multiline premise into one numbered and several plain rows
            Doc::LeftStack(lines) => {
                let Some((line_head, lines)) = lines.split_first() else {
                    continue;
                };
                render_numbered_row(output, Some(num), line_head)?;
                for line in lines {
                    output.write_str(" \\\\\n")?;
                    render_numbered_row(output, None, line)?;
                }
            }
            // Ordinary premises occupy one numbered row
            doc => render_numbered_row(output, Some(num), doc)?,
        }
        if num < docs.len() {
            output.write_str(" \\\\\n")?;
        }
    }
    output.write_str("\n\\end{array}")
}

// - Gathered documents
//
//   Gathered([Line(x), Gap, Line(y)])
//   -> \begin{gathered}
//      x \\[1ex]
//      y
//      \end{gathered}

/// Writes gathered lines, widening the break before each gap's following line.
fn render_gathered(output: &mut dyn Write, blocks: &[Block]) -> fmt::Result {
    output.write_str("\\begin{gathered}\n")?;
    for (idx, block) in blocks.iter().enumerate() {
        // A gap only widens the preceding separator
        let Block::Line(doc) = block else {
            continue;
        };
        render_doc(output, doc)?;
        // Separate this line from the next content line
        match blocks.get(idx + 1) {
            Some(Block::Gap) => output.write_str(" \\\\[1ex]\n")?,
            Some(Block::Line(_)) => output.write_str(" \\\\\n")?,
            None => {}
        }
    }
    output.write_str("\n\\end{gathered}")
}

// == Entry points
//
//   to_string(Fraction(p, q))   -> \frac{p}{q}

impl Print for Doc {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        render_doc(printer, self)
    }
}

/// Renders a document through its context-free printer.
pub(crate) fn to_string(doc: &Doc) -> String {
    Print::to_string(doc)
}
