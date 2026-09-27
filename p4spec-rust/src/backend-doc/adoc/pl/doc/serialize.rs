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

// - Cross-references
//
//   adoc_link("t", "x")      -> xref:t[x]
//   adoc_link("t", "a[b]")   -> <<t,a[b]>>
//   adoc_link("t", "a<b>")   -> xref:t[a<b>]

/// Chooses cross-reference delimiters that do not collide with the label.
fn adoc_link(target: &str, text: &str) -> String {
    // Brackets require the alternate cross-reference syntax
    if !text.contains(['[', ']']) {
        format!("xref:{target}[{text}]")
    } else if !text.contains(['<', '>']) {
        format!("<<{target},{text}>>")
    } else {
        // Neither delimiter can represent this label
        eprintln!(
            "Warning: Asciidoc link text contains both brackets and angle brackets. \
             Link may not render correctly.\n\t{text}"
        );
        text.to_owned()
    }
}

// - List markers
//
//   adoc_ordered_bullet(0)     -> ". "
//   adoc_ordered_bullet(2)     -> "  ... "
//   adoc_unordered_bullet(1)   -> " ** "

/// Returns an ordered-list marker at the requested nesting level.
pub(in crate::backend_doc::adoc::pl) fn adoc_ordered_bullet(level: usize) -> String {
    let indent = " ".repeat(level);
    let marker = ".".repeat(level + 1);
    format!("{indent}{marker} ")
}

/// Returns an unordered-list marker at the requested nesting level.
pub(in crate::backend_doc::adoc::pl) fn adoc_unordered_bullet(level: usize) -> String {
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
//   subject_name(Function("f"))        -> Some("f")
//   Link::Direct("t").target(anchor)   -> Some("t")

/// Resolves a subject to its unqualified definition name.
pub fn subject_name(subject: &Subject) -> Option<String> {
    match subject {
        Subject::Function(id) | Subject::Relation(id) => Some(id.clone()),
    }
}

impl Link {
    fn target(&self, anchor: &dyn Fn(&Subject) -> Option<String>) -> Option<String> {
        match self {
            Link::Direct(target) => Some(target.clone()),
            Link::Subject(subject) => anchor(subject),
        }
    }
}

// == Serialization

/// A run of code tokens sharing one link target.
struct CodeSegment {
    target: Option<String>,
    text: String,
}

/// Selects whether serialized code receives monospace markup.
#[derive(Clone, Copy)]
enum CodeStyle {
    /// Wraps each word in monospace markup, as in inline prose.
    Mono,
    /// Emits code text as is, as in link labels and table cells.
    Plain,
}

/// Per-serialization anchor labels and warnings.
struct Serializer<'a> {
    anchor: &'a dyn Fn(&Subject) -> Option<String>,
    markers: BTreeMap<String, String>,
    warned: BTreeSet<String>,
}

impl<'a> Serializer<'a> {
    fn new(
        anchor: &'a dyn Fn(&Subject) -> Option<String>,
        markers: BTreeMap<String, String>,
    ) -> Self {
        Serializer { anchor, markers, warned: BTreeSet::new() }
    }

    // - Warnings
    //
    //   Link(Direct("a"), Link(Direct("b"), Text("x")))
    //   -> xref:a[x], warning that "b" is dropped inside "a"
    //
    //   Link(Direct(""), Text("x"))
    //   -> xref:[x], warning about the empty target

    fn warn(&mut self, text: String) {
        if self.warned.insert(text.clone()) {
            eprintln!("Warning: prose: {text}");
        }
    }

    fn warn_empty_target(&mut self, lint: bool, target: &str) {
        if lint && target.is_empty() {
            self.warn("link with empty target".into());
        }
    }

    fn warn_nested(&mut self, lint: bool, target_outer: &str, target_inner: &str) {
        if lint {
            self.warn(format!(
                "nested link: cross-reference to {target_inner:?} is dropped inside the link \
                 to {target_outer:?} (asciidoc cannot nest cross-references)"
            ));
        }
    }

    // - Code segments
    //
    //   Seq([Token("a "), Link(Direct("f"), Token("b")), Token(" c")])
    //   -> [(None, "a "), (Some("f"), "b"), (None, " c")]

    /// Flattens code spans and coalesces adjacent tokens with the same target.
    fn collect_code(
        &mut self,
        code: &Code,
        target: Option<&str>,
        link_ctx: Option<&str>,
        lint: bool,
        segments: &mut Vec<CodeSegment>,
    ) {
        match code {
            Code::Token(text) => Serializer::collect_token_code(text, target, segments),
            Code::Link(link, code_inner) => {
                self.collect_link_code(link, code_inner, target, link_ctx, lint, segments)
            }
            Code::Seq(codes) => {
                // Adjacent sequences participate in the same span coalescing
                for code in codes {
                    self.collect_code(code, target, link_ctx, lint, segments);
                }
            }
            Code::Empty => {}
        }
    }

    fn collect_token_code(text: &str, target: Option<&str>, segments: &mut Vec<CodeSegment>) {
        // Empty tokens cannot split an existing span
        if text.is_empty() {
            return;
        }
        let segment_last = segments
            .last_mut()
            .filter(|segment| segment.target.as_deref() == target);
        if let Some(segment) = segment_last {
            segment.text.push_str(text);
        } else {
            let target = target.map(str::to_owned);
            segments.push(CodeSegment { target, text: text.to_owned() });
        }
    }

    fn collect_link_code(
        &mut self,
        link: &Link,
        code_inner: &Code,
        target: Option<&str>,
        link_ctx: Option<&str>,
        lint: bool,
        segments: &mut Vec<CodeSegment>,
    ) {
        // Unresolved subjects retain the surrounding link context
        let Some(target_inner) = link.target(self.anchor) else {
            self.collect_code(code_inner, target, link_ctx, lint, segments);
            return;
        };
        self.warn_empty_target(lint, &target_inner);
        if let Some(target_outer) = link_ctx {
            // Cross-references cannot nest in AsciiDoc
            self.warn_nested(lint, target_outer, &target_inner);
            self.collect_code(code_inner, target, link_ctx, lint, segments);
            return;
        }

        // The outermost resolved link owns its entire code span
        if lint && code_inner.is_empty() {
            self.warn(format!("link to {target_inner:?} has empty body"));
        }
        let target_inner = Some(target_inner.as_str());
        self.collect_code(code_inner, target_inner, target_inner, lint, segments);
    }

    // - Code
    //
    //   Seq([Token("a "), Link(Direct("f"), Token("b")), Token(" c")]) in Mono
    //   -> ``a`` xref:f[``b``] ``c``
    //
    //   Seq([Token("a "), Link(Direct("f"), Token("b")), Token(" c")]) in Plain
    //   -> a xref:f[b] c

    /// Serializes coalesced code, applying monospace only for inline prose.
    fn ser_code(
        &mut self,
        style: CodeStyle,
        code: &Code,
        link_ctx: Option<&str>,
        lint: bool,
    ) -> String {
        let mut segments = Vec::new();
        self.collect_code(code, None, link_ctx, lint, &mut segments);
        // Formatting after coalescing keeps adjacent tokens in one code span
        segments
            .into_iter()
            .map(|segment| {
                let text = match style {
                    CodeStyle::Mono => adoc_mono_chopped(&segment.text),
                    CodeStyle::Plain => segment.text,
                };
                match segment.target {
                    Some(target) => adoc_link(&target, &text),
                    None => text,
                }
            })
            .collect()
    }

    // - Prose
    //
    //   Seq([Text("the "), Code(Token("x"))])   -> the ``x``
    //   PlainCode(Token("x y"))                 -> x y

    /// Serializes inline prose with the enclosing cross-reference context.
    fn ser_prose(&mut self, prose: &Prose, link_ctx: Option<&str>, lint: bool) -> String {
        match prose {
            Prose::Text(text) => Serializer::ser_text_prose(text),
            Prose::Code(code) => self.ser_code(CodeStyle::Mono, code, link_ctx, lint),
            Prose::PlainCode(code) => self.ser_code(CodeStyle::Plain, code, link_ctx, lint),
            Prose::Link(link, prose_inner) => {
                self.ser_link_prose(link, prose_inner, link_ctx, lint)
            }
            Prose::Fallthrough(target, label) => self.ser_fallthrough_prose(target, label),
            Prose::Seq(proses) => self.ser_seq_prose(proses, link_ctx, lint),
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
        link: &Link,
        prose_inner: &Prose,
        link_ctx: Option<&str>,
        lint: bool,
    ) -> String {
        // Preserve the body when the enclosing document has no target
        let Some(target) = link.target(self.anchor) else {
            return self.ser_prose(prose_inner, link_ctx, lint);
        };
        self.warn_empty_target(lint, &target);
        if let Some(target_outer) = link_ctx {
            // An outer link takes precedence over nested links
            self.warn_nested(lint, target_outer, &target);
            return self.ser_prose(prose_inner, link_ctx, lint);
        }

        // Format the complete body before choosing link delimiters
        let text = self.ser_prose(prose_inner, Some(&target), lint);
        if lint && text.is_empty() {
            self.warn(format!("link to {target:?} has empty body"));
        }
        adoc_link(&target, &text)
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

    fn ser_seq_prose(&mut self, proses: &[Prose], link_ctx: Option<&str>, lint: bool) -> String {
        proses
            .iter()
            .map(|prose| self.ser_prose(prose, link_ctx, lint))
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
            Block::Inline(prose) => self.ser_prose(prose, None, true),
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
        let text_head = self.ser_prose(prose_head, None, true);
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
            .map(|prose| self.ser_prose(prose, None, true))
            .collect();
        let text_header = texts_header.join(" | ");
        // Resolve cell links in the enclosing document's context
        let mut texts_row = Vec::new();
        for row in rows {
            let texts_cell: Vec<String> = row
                .iter()
                .map(|code| self.ser_code(CodeStyle::Plain, code, None, false))
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

/// Serializes prose using the enclosing document's anchor resolver.
pub fn ser_prose(anchor: &dyn Fn(&Subject) -> Option<String>, prose: &Prose) -> String {
    let mut serializer = Serializer::new(anchor, BTreeMap::new());
    serializer.ser_prose(prose, None, true)
}

/// Serializes a link label without creating nested cross-references.
pub fn ser_prose_in_link(prose: &Prose) -> String {
    // The empty outer target suppresses direct links as well as subjects
    Serializer::new(&|_| None, BTreeMap::new()).ser_prose(prose, Some(""), false)
}

/// Serializes code without monospace markup using the given anchor resolver.
pub fn ser_code(anchor: &dyn Fn(&Subject) -> Option<String>, code: &Code) -> String {
    let mut serializer = Serializer::new(anchor, BTreeMap::new());
    serializer.ser_code(CodeStyle::Plain, code, None, false)
}

/// Resolves arm labels before serializing a fragment with the given anchor resolver.
pub fn ser_block(anchor: &dyn Fn(&Subject) -> Option<String>, block: &Block) -> String {
    let markers = block.anchor_markers();
    let mut serializer = Serializer::new(anchor, markers);
    serializer.ser_block(block)
}
