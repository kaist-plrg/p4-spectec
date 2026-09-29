//! AsciiDoc prose, code spans, and document blocks
//!
//! ```text
//! Prose::join(", ", [x, y])       -> Seq([x, Text(", "), y])
//! Block::item_ordered(0, prose)   -> Item(Item { 0, Ordered(None), prose, Empty })
//! ```

// == Documents

// - Prose
//
//   Seq([Text("the "), Code(Token("x y"))])   -> the ``x`` ``y``
//   PlainCode(Token("x y"))                   -> x y

/// Inline prose and embedded code.
#[derive(Clone, Debug, PartialEq)]
pub enum Prose {
    /// Emits AsciiDoc text verbatim, including any existing markup.
    Text(String),
    /// Renders linked code with monospace markup around each word.
    Code(Code),
    /// Renders linked code without adding monospace markup.
    PlainCode(Code),
    /// Links the body, suppressing resolved links nested inside it.
    Link(Link, Box<Prose>),
    /// Links to an arm or group using its anchor and displayed label.
    Fallthrough(String, FallthroughLabel),
    /// Concatenates prose without inserting separators.
    Seq(Vec<Prose>),
    /// Emits no text and permits capitalization to continue.
    Empty,
}

// - Code
//
//   Seq([Token("a"), Token("b")])       -> ``ab``
//   Link(Direct("f"), Token("f(x)"))   -> xref:f[``f(x)``]

/// Code tokens with optional cross-references.
#[derive(Clone, Debug, PartialEq)]
pub enum Code {
    /// Emits pre-escaped code text, coalescing adjacent compatible tokens.
    Token(String),
    /// Links the enclosed code unless an outer link already owns the span.
    Link(Link, Box<Code>),
    /// Concatenates code without inserting separators or span boundaries.
    Seq(Vec<Code>),
    /// Emits no tokens and does not split a code span.
    Empty,
}

// - Links
//
//   Link(Direct("t"), Text("x"))              -> xref:t[x]
//   Link(Subject(Function("f")), Text("x"))   -> xref:f[x]
//   ... with an unresolving anchor            -> x

/// A concrete target or a reference resolved by the enclosing document.
#[derive(Clone, Debug, PartialEq)]
pub enum Link {
    /// Uses the target verbatim, bypassing the subject resolver.
    Direct(String),
    /// Resolves the subject, preserving only the body if unresolved.
    Subject(Subject),
}

/// A definition referenced by prose.
#[derive(Clone, Debug, PartialEq)]
pub enum Subject {
    /// Identifies a function by its source name without the dollar prefix.
    Function(String),
    /// Identifies a relation by its source name.
    Relation(String),
}

// - Fallthrough labels
//
//   Fallthrough("t", Explicit("else"))
//   -> +++<sub class="bk-mark">[<a href="#t">→ else</a>]</sub>+++

/// A fallthrough marker inferred from its target or supplied by a group.
#[derive(Clone, Debug, PartialEq)]
pub enum FallthroughLabel {
    /// Uses the target arm's list marker from the same serialized block.
    ///
    /// Block serialization panics if the target has no ordered arm anchor.
    /// Standalone prose serialization cannot resolve derived labels.
    Derived,
    /// Uses the supplied label, such as a group name or otherwise marker.
    Explicit(String),
}

// - Blocks
//
//   Item { 0, Ordered(None), Text("A"), Empty }   -> . A
//   Item { 1, Unordered, Text("B"), Empty }       ->  ** B
//   Seq([Raw("x"), Raw("y")])                     -> x
//                                                    y
//   Concat([Raw("x"), Raw("y")])                  -> xy

/// A document fragment with explicit list nesting and table boundaries.
#[derive(Clone, Debug, PartialEq)]
pub enum Block {
    /// Emits no text and does not advance list counters.
    Empty,
    /// Emits AsciiDoc verbatim without inspecting links or list markers.
    Raw(String),
    /// Renders prose without adding line breaks or a list marker.
    Inline(Prose),
    /// Renders a list heading followed by a nonempty body on the next line.
    Item(Item),
    /// Concatenates blocks without inserting separators.
    Concat(Vec<Block>),
    /// Joins blocks with one newline between adjacent blocks.
    Seq(Vec<Block>),
    /// Renders a table with its column count derived from the header.
    Table(Table),
}

/// A list entry: its marker, heading, and continuation.
#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    /// The zero-based nesting level used for bullets and arm labels.
    pub level: usize,
    /// The list style and optional ordered arm anchor.
    pub kind: ItemKind,
    /// The prose following the list marker.
    pub prose_head: Prose,
    /// The continuation, with nested entries carrying their own levels.
    pub block_body: Box<Block>,
}

/// The marker style of a list entry, optionally defining an ordered arm anchor.
#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    /// Advances the ordered list, optionally defining an arm anchor.
    Ordered(Option<String>),
    /// Emits a bullet and resets the ordered counter at this level.
    Unordered,
}

/// A table of prose header cells over code rows.
#[derive(Clone, Debug, PartialEq)]
pub struct Table {
    /// The header cells, including their inline prose formatting.
    pub header: Vec<Prose>,
    /// Code cells resolved at serialization without monospace markup.
    ///
    /// Each row must have as many cells as the header.
    pub rows: Vec<Vec<Code>>,
}

// == Constructors

// - Interleaving
//
//   join_items(|idx| s_idx, [a, b, c])   -> [a, s_1, b, s_2, c]

/// Interleaves items with separators selected by the following item's index.
fn join_items<Item>(
    mut separator: impl FnMut(usize) -> Item,
    items: impl IntoIterator<Item = Item>,
) -> Vec<Item> {
    let mut items_joined = Vec::new();
    // Move each item into one buffer without allocating intermediate lists
    for (idx, item) in items.into_iter().enumerate() {
        // A separator belongs only between two items
        if idx > 0 {
            items_joined.push(separator(idx));
        }
        items_joined.push(item);
    }
    items_joined
}

impl Prose {
    // - Prose constructors
    //
    //   Prose::text("x")                           -> Text("x")
    //   Prose::link(link, prose)                   -> Link(link, prose)
    //   Prose::fallthrough(a, l)                   -> Fallthrough(a, l)
    //   Prose::join(", ", [x, y])                  -> Seq([x, Text(", "), y])
    //   Prose::join_with(|idx| s_idx, [x, y, z])   -> Seq([x, s_1, y, s_2, z])

    pub fn text(text: impl Into<String>) -> Prose {
        Prose::Text(text.into())
    }

    pub fn code(code: Code) -> Prose {
        Prose::Code(code)
    }

    pub fn link(link: Link, prose: Prose) -> Prose {
        Prose::Link(link, Box::new(prose))
    }

    pub fn fallthrough(anchor: String, label: FallthroughLabel) -> Prose {
        Prose::Fallthrough(anchor, label)
    }

    pub fn seq(proses: impl IntoIterator<Item = Prose>) -> Prose {
        Prose::Seq(proses.into_iter().collect())
    }

    pub fn join(separator: &str, proses: impl IntoIterator<Item = Prose>) -> Prose {
        let proses_joined = join_items(|_| Prose::text(separator), proses);
        Prose::seq(proses_joined)
    }

    pub fn join_with(
        separator: impl FnMut(usize) -> Prose,
        proses: impl IntoIterator<Item = Prose>,
    ) -> Prose {
        let proses_joined = join_items(separator, proses);
        Prose::seq(proses_joined)
    }
}

impl Code {
    // - Code constructors
    //
    //   Code::token("x")           -> Token("x")
    //   Code::join(", ", [x, y])   -> Seq([x, Token(", "), y])

    pub fn token(text: impl Into<String>) -> Code {
        Code::Token(text.into())
    }

    pub fn link(link: Link, code: Code) -> Code {
        Code::Link(link, Box::new(code))
    }

    pub fn seq(codes: impl IntoIterator<Item = Code>) -> Code {
        Code::Seq(codes.into_iter().collect())
    }

    pub fn join(separator: &str, codes: impl IntoIterator<Item = Code>) -> Code {
        let codes_joined = join_items(|_| Code::token(separator), codes);
        Code::seq(codes_joined)
    }
}

// == Queries

impl Code {
    // - Emptiness
    //
    //   Seq([Token(""), Link(link, Empty)])   -> empty
    //   Seq([Empty, Token("x")])              -> nonempty

    /// Tests whether the code emits no tokens.
    pub(super) fn is_empty(&self) -> bool {
        match self {
            Code::Token(text) => text.is_empty(),
            Code::Link(_, code) => code.is_empty(),
            Code::Seq(codes) => codes.iter().all(Code::is_empty),
            Code::Empty => true,
        }
    }
}

impl Block {
    // - Block constructors
    //
    //   Block::item(0, kind, prose, body)   -> Item { 0, kind, prose, body }
    //   Block::item_ordered(0, prose)       -> Item { 0, Ordered(None), prose, Empty }
    //   Block::item_unordered(1, prose)     -> Item { 1, Unordered, prose, Empty }

    pub fn raw(text: impl Into<String>) -> Block {
        Block::Raw(text.into())
    }

    pub fn inline(prose: Prose) -> Block {
        Block::Inline(prose)
    }

    pub fn concat(blocks: impl IntoIterator<Item = Block>) -> Block {
        Block::Concat(blocks.into_iter().collect())
    }

    pub fn seq(blocks: impl IntoIterator<Item = Block>) -> Block {
        Block::Seq(blocks.into_iter().collect())
    }

    pub fn item(level: usize, kind: ItemKind, prose_head: Prose, block_body: Block) -> Block {
        Block::Item(Item { level, kind, prose_head, block_body: Box::new(block_body) })
    }

    pub fn item_ordered(level: usize, prose_head: Prose) -> Block {
        Block::item(level, ItemKind::Ordered(None), prose_head, Block::Empty)
    }

    pub fn item_unordered(level: usize, prose_head: Prose) -> Block {
        Block::item(level, ItemKind::Unordered, prose_head, Block::Empty)
    }
}
