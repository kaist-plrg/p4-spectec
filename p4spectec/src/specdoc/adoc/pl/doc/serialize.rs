//! Serialization of AsciiDoc prose, code spans, and document blocks
//!
//! ```text
//! Code(Seq([Token("a "), Link(Direct("f"), Token("b"))]))
//! -> ``a`` xref:f[``b``]
//!
//! Link(Direct("t"), Text("a[b]"))
//! -> <<t,a[b]>>
//!
//! Item { 0, Ordered(Some("arm")), Text("Done"), Empty }
//! -> . +++<span class="bk-arm-anchor" id="arm"></span>+++Done
//! ```

use std::collections::{BTreeMap, BTreeSet};

use crate::lang::common::source::Span;

use crate::diagnostic::{Diagnostic, Report};

use crate::specdoc::{
    adoc::error,
    anchor::{AnchorContext, Presentation},
};

use super::doc::{Block, Code, FallthroughLabel, Item, ItemKind, Link, Prose, Subject, Table};

// == Markup

// - Monospace
//
//   "a b"             -> ``a`` ``b``
//   "a  b"            -> ``a``  ``b``
//   "\"a b\""         -> ``"a`` ``b"``
//   "a \"b\" \"c\""   -> ``a`` ``{quot}b{quot}`` ``{quot}c{quot}``

/// Formats each nonempty word separately so code can wrap at spaces.
fn adoc_mono_chopped(text: &str) -> String {
    // Quotes spanning several phrases could start AsciiDoc quotation markup
    let text_escaped =
        if text.matches('"').count() > 2 { text.replace('"', "{quot}") } else { text.to_owned() };
    let texts_word: Vec<String> = text_escaped
        .split(' ')
        .map(|text_word| match text_word {
            "" => String::new(),
            _ => format!("``{text_word}``"),
        })
        .collect();
    texts_word.join(" ")
}

// - List markers
//
//   adoc_ordered_bullet(0)     -> ". "
//   adoc_ordered_bullet(2)     -> "  ... "
//   adoc_unordered_bullet(1)   -> " ** "

/// Returns an ordered-list marker at the requested nesting level.
pub(in crate::specdoc::adoc::pl) fn adoc_ordered_bullet(level: usize) -> String {
    let indent = " ".repeat(level);
    let marker = ".".repeat(level + 1);
    format!("{indent}{marker} ")
}

/// Returns an unordered-list marker at the requested nesting level.
pub(in crate::specdoc::adoc::pl) fn adoc_unordered_bullet(level: usize) -> String {
    let indent = " ".repeat(level);
    let marker = "*".repeat(level + 1);
    format!("{indent}{marker} ")
}

// == Ordered-list markers

// - Ordered-list styles
//
//   level 0, idx 1    -> 2
//   level 1, idx 0    -> a
//   level 1, idx 26   -> arm27
//   level 2, idx 3    -> iv
//   level 3, idx 1    -> B
//   level 4, idx 0    -> I
//   level 5, idx 0    -> 1

/// AsciiDoc's five-level ordered-list style cycle.
#[derive(Clone, Copy)]
enum OrderedStyle {
    Arabic,
    LowerAlpha,
    LowerRoman,
    UpperAlpha,
    UpperRoman,
}

impl OrderedStyle {
    // - Roman numerals
    //
    //   4    -> iv
    //   14   -> xiv
    //   90   -> xc

    fn roman_of_num(num: usize) -> String {
        // Labels follow the source renderer's two-digit conversion
        let units = ["", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"];
        let tens = ["", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc"];
        format!("{}{}", tens[num / 10 % 10], units[num % 10])
    }

    fn of_level(level: usize) -> OrderedStyle {
        match level % 5 {
            0 => OrderedStyle::Arabic,
            1 => OrderedStyle::LowerAlpha,
            2 => OrderedStyle::LowerRoman,
            3 => OrderedStyle::UpperAlpha,
            _ => OrderedStyle::UpperRoman,
        }
    }

    fn marker(self, idx: usize) -> String {
        let num = idx + 1;
        match self {
            OrderedStyle::Arabic => num.to_string(),
            OrderedStyle::LowerAlpha if idx < 26 => char::from(b'a' + idx as u8).to_string(),
            OrderedStyle::UpperAlpha if idx < 26 => char::from(b'A' + idx as u8).to_string(),
            // Alphabetic lists use an explicit fallback beyond the alphabet
            OrderedStyle::LowerAlpha | OrderedStyle::UpperAlpha => format!("arm{num}"),
            OrderedStyle::LowerRoman => OrderedStyle::roman_of_num(num),
            OrderedStyle::UpperRoman => OrderedStyle::roman_of_num(num).to_ascii_uppercase(),
        }
    }
}

impl Block {
    // - Arm markers
    //
    //   Item { 0, Ordered(None), Text("Choose"), Seq([
    //       Item { 1, Ordered(Some("one")), Fallthrough("two", Derived), Empty },
    //       Item { 1, Ordered(Some("two")), Text("Done"), Empty },
    //   ]) }
    //   -> {"one": "a", "two": "b"}

    /// Maps each ordered arm anchor to the marker displayed for its item.
    fn anchor_markers(&self) -> BTreeMap<String, String> {
        let mut markers = BTreeMap::new();
        self.collect_anchor_markers(&mut markers, &mut BTreeMap::new());
        markers
    }

    /// Records arm labels in document order, resetting lists at shallower entries.
    fn collect_anchor_markers(
        &self,
        markers: &mut BTreeMap<String, String>,
        ordinals: &mut BTreeMap<usize, usize>,
    ) {
        match self {
            Block::Item(Item { level, kind, block_body, .. }) => {
                // A shallower entry starts new nested lists
                ordinals.retain(|level_inner, _| level_inner <= level);
                match kind {
                    ItemKind::Unordered => {
                        // End the ordered list at this level
                        ordinals.remove(level);
                    }
                    ItemKind::Ordered(anchor_opt) => {
                        // Anchored and unanchored entries both advance the ordinal
                        let ordinal = ordinals.entry(*level).or_default();
                        if let Some(anchor) = anchor_opt {
                            let style = OrderedStyle::of_level(*level);
                            markers.insert(anchor.clone(), style.marker(*ordinal));
                        }
                        *ordinal += 1;
                    }
                }
                block_body.collect_anchor_markers(markers, ordinals);
            }
            Block::Concat(blocks) | Block::Seq(blocks) => {
                // Concatenation preserves the enclosing list's position
                for block in blocks {
                    block.collect_anchor_markers(markers, ordinals);
                }
            }
            Block::Empty | Block::Raw(_) | Block::Inline(_) | Block::Table(_) => {}
        }
    }
}

// == Anchor resolution
//
//   subject_name(Function(id_f))   -> Some("f")
//   Direct("t")                    -> Some("t")

/// Resolves a subject to its unqualified definition name.
pub fn subject_name(subject: &Subject) -> Option<String> {
    match subject {
        Subject::Function(id) | Subject::Relation(id) | Subject::Type(id) => Some(id.node.clone()),
    }
}

impl Link {
    fn target(&self, anchor_ctx: &AnchorContext<'_>) -> Option<String> {
        match self {
            Link::Direct(id) => Some(id.node.clone()),
            Link::Subject(Subject::Type(id)) => Some(id.node.clone()),
            Link::Subject(Subject::Function(id)) => anchor_ctx.func(Presentation::Prose, &id.node),
            Link::Subject(Subject::Relation(id)) => anchor_ctx.rel(Presentation::Prose, &id.node),
        }
    }
}

// == Serialization

/// Selects whether serialized code receives monospace markup.
#[derive(Clone, Copy)]
enum CodeStyle {
    /// Wraps each word in monospace markup, as in inline prose.
    Mono,
    /// Emits code text as is, as in link labels and table cells.
    Plain,
}

impl CodeStyle {
    fn render(self, text: &str) -> String {
        match self {
            Self::Mono => adoc_mono_chopped(text),
            Self::Plain => text.to_owned(),
        }
    }
}

/// Per-serialization anchor labels and warnings.
struct Serializer<'ctx, 'a> {
    anchor_ctx: &'ctx AnchorContext<'a>,
    warnings: &'ctx mut Vec<Report>,
    markers: BTreeMap<String, String>,
    warned: BTreeSet<String>,
}

impl<'ctx, 'a> Serializer<'ctx, 'a> {
    fn new(
        anchor_ctx: &'ctx AnchorContext<'a>,
        warnings: &'ctx mut Vec<Report>,
        markers: BTreeMap<String, String>,
    ) -> Self {
        Serializer { anchor_ctx, warnings, markers, warned: BTreeSet::new() }
    }

    // - Cross-references
    //
    //   adoc_link(link, "t", "x")      -> xref:t[x]
    //   adoc_link(link, "t", "a[b]")   -> <<t,a[b]>>
    //   adoc_link(link, "t", "a<b>")   -> xref:t[a<b>]

    /// Chooses cross-reference delimiters that do not collide with the label.
    fn adoc_link(&mut self, link: &Link, target: &str, text: &str) -> String {
        // Brackets require the alternate cross-reference syntax
        if !text.contains(['[', ']']) {
            format!("xref:{target}[{text}]")
        } else if !text.contains(['<', '>']) {
            format!("<<{target},{text}>>")
        } else {
            // Neither delimiter can represent this label
            self.warnings
                .push(error::link_text_invalid(link, text).into());
            text.to_owned()
        }
    }

    // - Warnings
    //
    //   Link(Direct("a"), Link(Direct("b"), Text("x")))
    //   -> xref:a[x], warning that "b" is dropped inside "a"
    //
    //   Link(Direct(""), Text("x"))
    //   -> xref:[x], warning about the empty target

    fn warn(&mut self, diagnostic: Diagnostic) {
        if self.warned.insert(diagnostic.message.clone()) {
            self.warnings.push(diagnostic.into());
        }
    }

    fn warn_empty_target(&mut self, lint: bool, link: &Link, target: &str) {
        if lint && target.is_empty() {
            self.warn(error::link_target_empty(link));
        }
    }

    fn warn_nested(&mut self, lint: bool, link_outer: &Link, link_inner: &Link) {
        if lint {
            self.warn(error::link_nested(link_outer, link_inner));
        }
    }

    // - Code
    //
    //   Seq([Token("a "), Link(Direct("f"), Token("b")), Token(" c")]) in Mono
    //   -> ``a`` xref:f[``b``] ``c``
    //
    //   Seq([Link(Direct("f"), Token("a")), Link(Direct("f"), Token("b"))])
    //   -> xref:f[``a``]xref:f[``b``]

    /// Collects adjacent tokens while serializing each resolved link separately.
    fn collect_code(
        &mut self,
        link_ctx: Option<&Link>,
        lint: bool,
        style: CodeStyle,
        code: &Code,
        text_pending: &mut String,
        text: &mut String,
    ) {
        match code {
            Code::Token(text_token) => text_pending.push_str(text_token),
            Code::Link(link, code_inner) => {
                // Unresolved and suppressed links leave the token run intact
                let Some(text_link) = self.ser_link_code(link_ctx, lint, style, link, code_inner)
                else {
                    self.collect_code(link_ctx, lint, style, code_inner, text_pending, text);
                    return;
                };
                // Empty links emit no markup and cannot split adjacent tokens
                if !text_link.is_empty() {
                    text.push_str(&style.render(text_pending));
                    text_pending.clear();
                    text.push_str(&text_link);
                }
            }
            Code::Seq(codes) => {
                // Sequence boundaries do not split adjacent tokens
                for code in codes {
                    self.collect_code(link_ctx, lint, style, code, text_pending, text);
                }
            }
            Code::Empty => {}
        }
    }

    /// Collects a link's body as text, diagnosing resolved nested references.
    fn collect_code_text(&mut self, link_outer: &Link, lint: bool, code: &Code, text: &mut String) {
        match code {
            Code::Token(text_token) => text.push_str(text_token),
            Code::Link(link, code_inner) => {
                // Only resolved references conflict with the enclosing link
                if let Some(target) = link.target(self.anchor_ctx) {
                    self.warn_empty_target(lint, link, &target);
                    self.warn_nested(lint, link_outer, link);
                }
                self.collect_code_text(link_outer, lint, code_inner, text);
            }
            Code::Seq(codes) => {
                // Format only after the full link body has been collected
                for code in codes {
                    self.collect_code_text(link_outer, lint, code, text);
                }
            }
            Code::Empty => {}
        }
    }

    /// Serializes one resolved link, or leaves its body in the enclosing token run.
    fn ser_link_code(
        &mut self,
        link_ctx: Option<&Link>,
        lint: bool,
        style: CodeStyle,
        link: &Link,
        code_inner: &Code,
    ) -> Option<String> {
        // Unresolved subjects keep their bodies without creating a boundary
        let target = link.target(self.anchor_ctx)?;
        self.warn_empty_target(lint, link, &target);
        // An outer link suppresses this reference while retaining its body
        if let Some(link_outer) = link_ctx {
            self.warn_nested(lint, link_outer, link);
            return None;
        }
        // Preserve the empty-body warning even when no tokens will be emitted
        if lint && code_inner.is_empty() {
            self.warn(error::link_body_empty(link));
        }
        let mut text = String::new();
        self.collect_code_text(link, lint, code_inner, &mut text);
        if text.is_empty() {
            return Some(text);
        }
        // Choose delimiters with the original link and its complete display text
        let text = style.render(&text);
        Some(self.adoc_link(link, &target, &text))
    }

    /// Serializes token runs and individual links with the selected code style.
    fn ser_code(
        &mut self,
        link_ctx: Option<&Link>,
        lint: bool,
        style: CodeStyle,
        code: &Code,
    ) -> String {
        let mut text = String::new();
        let mut text_pending = String::new();
        self.collect_code(link_ctx, lint, style, code, &mut text_pending, &mut text);
        text.push_str(&style.render(&text_pending));
        text
    }

    // - Prose
    //
    //   Seq([Text("the "), Code(Token("x"))])   -> the ``x``
    //   PlainCode(Token("x y"))                 -> x y

    /// Serializes inline prose with the enclosing cross-reference context.
    fn ser_prose(&mut self, link_ctx: Option<&Link>, lint: bool, prose: &Prose) -> String {
        match prose {
            Prose::Text(text) => Serializer::ser_text_prose(text),
            Prose::Code(code) => self.ser_code(link_ctx, lint, CodeStyle::Mono, code),
            Prose::PlainCode(code) => self.ser_code(link_ctx, lint, CodeStyle::Plain, code),
            Prose::Link(link, prose_inner) => {
                self.ser_link_prose(link_ctx, lint, link, prose_inner)
            }
            Prose::Fallthrough(target, label) => self.ser_fallthrough_prose(target, label),
            Prose::Seq(proses) => self.ser_seq_prose(link_ctx, lint, proses),
            Prose::Empty => Serializer::ser_empty_prose(),
        }
    }

    // - Text prose
    //
    //   Text("a *b*")   -> a *b*

    fn ser_text_prose(text: &str) -> String {
        text.to_owned()
    }

    // - Linked prose
    //
    //   Link(Direct("t"), Text("x"))                             -> xref:t[x]
    //   Link(Direct("t"), Text("a[b]"))                          -> <<t,a[b]>>
    //   Link(Direct("a"), Code(Link(Direct("b"), Token("x"))))   -> xref:a[``x``]

    fn ser_link_prose(
        &mut self,
        link_ctx: Option<&Link>,
        lint: bool,
        link: &Link,
        prose_inner: &Prose,
    ) -> String {
        // Preserve the body when the enclosing document has no target
        let Some(target) = link.target(self.anchor_ctx) else {
            return self.ser_prose(link_ctx, lint, prose_inner);
        };
        self.warn_empty_target(lint, link, &target);
        if let Some(link_outer) = link_ctx {
            // An outer link takes precedence over nested links
            self.warn_nested(lint, link_outer, link);
            return self.ser_prose(link_ctx, lint, prose_inner);
        }

        // Format the complete body before choosing link delimiters
        let text = self.ser_prose(Some(link), lint, prose_inner);
        if lint && text.is_empty() {
            self.warn(error::link_body_empty(link));
        }
        self.adoc_link(link, &target, &text)
    }

    // - Fallthrough prose
    //
    //   Fallthrough("t", Explicit("else"))
    //   -> +++<sub class="bk-mark">[<a href="#t">→ else</a>]</sub>+++
    //
    //   Fallthrough("t", Derived), where the arm t has marker b
    //   -> +++<sub class="bk-mark">[<a href="#t">→ b</a>]</sub>+++

    fn ser_fallthrough_prose(&self, target: &str, label: &FallthroughLabel) -> String {
        // Derived labels refer to the target arm's displayed ordinal
        let text = match label {
            FallthroughLabel::Derived => self
                .markers
                .get(target)
                .unwrap_or_else(|| panic!("no ordered-list marker for arm anchor {target:?}")),
            FallthroughLabel::Explicit(text) => text,
        };
        format!("+++<sub class=\"bk-mark\">[<a href=\"#{target}\">→ {text}</a>]</sub>+++")
    }

    // - Prose sequences
    //
    //   Seq([Text("a"), Text("b")])   -> ab

    fn ser_seq_prose(&mut self, link_ctx: Option<&Link>, lint: bool, proses: &[Prose]) -> String {
        proses
            .iter()
            .map(|prose| self.ser_prose(link_ctx, lint, prose))
            .collect()
    }

    // - Empty prose
    //
    //   Empty   -> (empty)

    fn ser_empty_prose() -> String {
        String::new()
    }

    // - Block
    //
    //   Inline(Text("x"))           -> x
    //   Seq([Raw("x"), Raw("y")])   -> x
    //                                  y

    /// Serializes blocks using the fragment's collected arm markers.
    fn ser_block(&mut self, block: &Block) -> String {
        match block {
            Block::Empty => Serializer::ser_empty_block(),
            Block::Raw(text) => Serializer::ser_raw_block(text),
            Block::Inline(prose) => self.ser_prose(None, true, prose),
            Block::Concat(blocks) => self.ser_concat_block(blocks),
            Block::Seq(blocks) => self.ser_seq_block(blocks),
            Block::Item(item) => self.ser_item_block(item),
            Block::Table(table) => self.ser_table_block(table),
        }
    }

    // - Empty blocks
    //
    //   Empty   -> (empty)

    fn ser_empty_block() -> String {
        String::new()
    }

    // - Raw blocks
    //
    //   Raw("x")   -> x

    fn ser_raw_block(text: &str) -> String {
        text.to_owned()
    }

    // - Concatenated blocks
    //
    //   Concat([Raw("x"), Raw("y")])   -> xy

    fn ser_concat_block(&mut self, blocks: &[Block]) -> String {
        blocks.iter().map(|block| self.ser_block(block)).collect()
    }

    // - Block sequences
    //
    //   Seq([Raw("x"), Raw("y")])   -> x
    //                                  y

    fn ser_seq_block(&mut self, blocks: &[Block]) -> String {
        let texts: Vec<String> = blocks.iter().map(|block| self.ser_block(block)).collect();
        texts.join("\n")
    }

    // - List items
    //
    //   Item { 0, Ordered(None), Text("A"), Empty }
    //   -> . A
    //
    //   Item { 1, Unordered, Text("B"), Empty }
    //   ->  ** B
    //
    //   Item { 1, Ordered(Some("arm")), Text("If x"),
    //          Item { 2, Ordered(None), Text("Return y."), Empty } }
    //   ->  .. +++<span class="bk-arm-anchor" id="arm"></span>+++If x
    //        ... Return y.

    fn ser_item_block(&mut self, item: &Item) -> String {
        let Item { level, kind, prose_head, block_body } = item;
        let level = *level;
        let text_bullet = match kind {
            ItemKind::Unordered => adoc_unordered_bullet(level),
            ItemKind::Ordered(_) => adoc_ordered_bullet(level),
        };
        // Only ordered arms emit anchors
        let text_anchor = match kind {
            ItemKind::Ordered(Some(anchor)) => {
                format!("+++<span class=\"bk-arm-anchor\" id=\"{anchor}\"></span>+++")
            }
            ItemKind::Ordered(None) | ItemKind::Unordered => String::new(),
        };
        let text_head = self.ser_prose(None, true, prose_head);
        let mut text = format!("{text_bullet}{text_anchor}{text_head}");
        // Empty bodies leave no trailing newline
        let text_body = self.ser_block(block_body);
        if !text_body.is_empty() {
            text.push('\n');
            text.push_str(&text_body);
        }
        text
    }

    // - Tables
    //
    //   Table { [Text("Input"), Text("Output")], [[Token("a"), Token("b")]] }
    //   -> [cols="2", options="header"]
    //      |===
    //      | Input | Output
    //
    //      | a | b
    //
    //      |===

    fn ser_table_block(&mut self, table: &Table) -> String {
        let Table { header, rows } = table;
        // Use the header as the single source of the column count
        let cols = header.len();
        let texts_header: Vec<String> = header
            .iter()
            .map(|prose| self.ser_prose(None, true, prose))
            .collect();
        let text_header = texts_header.join(" | ");
        // Resolve cell links in the enclosing document's context
        let mut texts_row = Vec::new();
        for row in rows {
            let texts_cell: Vec<String> = row
                .iter()
                .map(|code| self.ser_code(None, false, CodeStyle::Plain, code))
                .collect();
            texts_row.push(format!("| {}", texts_cell.join(" | ")));
        }
        let text_rows = texts_row.join("\n");
        format!(
            "[cols=\"{cols}\", options=\"header\"]\n|===\n| {text_header} \n\n{text_rows}\n\n|==="
        )
    }
}

// - Entry points
//
//   ser_prose(&subject_name, Link(Subject(Function("f")), Text("x")))
//   -> xref:f[x]
//
//   ser_prose_in_link(Link(Direct("t"), Code(Token("x"))))
//   -> ``x``
//
//   ser_code(&subject_name, Seq([Token("a "), Link(Direct("f"), Token("b"))]))
//   -> a xref:f[b]

/// Serializes prose and collects warnings at each link's source span.
pub fn ser_prose(
    anchor_ctx: &AnchorContext<'_>,
    warnings: &mut Vec<Report>,
    prose: &Prose,
) -> String {
    let mut serializer = Serializer::new(anchor_ctx, warnings, BTreeMap::new());
    serializer.ser_prose(None, true, prose)
}

/// Serializes a link label without creating nested cross-references.
pub fn ser_prose_in_link(prose: &Prose) -> String {
    // The empty outer target suppresses direct links as well as subjects
    let anchor_ctx = AnchorContext::default();
    Serializer::new(&anchor_ctx, &mut Vec::new(), BTreeMap::new()).ser_prose(
        Some(&Link::Direct(crate::phrase! { node: String::new(), span: Span::default() })),
        false,
        prose,
    )
}

/// Serializes code without monospace markup and collects delimiter warnings.
pub fn ser_code(anchor_ctx: &AnchorContext<'_>, warnings: &mut Vec<Report>, code: &Code) -> String {
    let mut serializer = Serializer::new(anchor_ctx, warnings, BTreeMap::new());
    serializer.ser_code(None, false, CodeStyle::Plain, code)
}

/// Resolves arm labels and collects warnings while serializing a fragment.
///
/// Derived fallthrough labels must name an ordered arm within this block,
/// as produced by the PL renderer's arm and next-target construction.
pub fn ser_block(
    anchor_ctx: &AnchorContext<'_>,
    warnings: &mut Vec<Report>,
    block: &Block,
) -> String {
    let markers = block.anchor_markers();
    let mut serializer = Serializer::new(anchor_ctx, warnings, markers);
    serializer.ser_block(block)
}
