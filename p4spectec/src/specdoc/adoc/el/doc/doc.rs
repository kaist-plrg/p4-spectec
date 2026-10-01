//! Width-sensitive documents for elaboration-language AsciiDoc
//!
//! ```text
//! Doc::text("")                  -> Empty
//! Doc::nest(0, doc)              -> doc
//! Doc::join(sep, [x, y])         -> Concat([x, sep, y])
//! Doc::flow([x, y])              -> Concat([x, Group(Concat([Break(" "), y]))])
//! ```

// == Documents
//
//   Text("x")                 -> x
//   Break(" ")                -> " " when flat, newline and indentation when broken
//   Line                      -> newline and indentation
//   Nest(4, Line)             -> newline and four more columns of indentation

/// A tree describing possible flat and broken layouts.
#[derive(Clone, Debug)]
pub(crate) enum Doc {
    /// Emits nothing.
    Empty,
    /// Emits text without splitting it.
    Text(String),
    /// Emits flat text or starts an indented line.
    Break(String),
    /// Always starts an indented line.
    Line,
    /// Renders documents consecutively without nesting by sequence length.
    Concat(Vec<Doc>),
    /// Adds the offset to indentation at subsequent breaks in the document.
    Nest(usize, Box<Doc>),
    /// Chooses a flat layout when the enclosed document fits.
    Group(Box<Doc>),
}

// == Constructors

impl Doc {
    // - Primitive documents
    //
    //   Doc::text("")             -> Empty
    //   Doc::text("x")            -> Text("x")
    //   Doc::break_(" ")          -> Break(" ")
    //   Doc::nest(0, doc)         -> doc
    //   Doc::nest(2, doc)         -> Nest(2, doc)
    //   Doc::group(doc)           -> Group(doc)

    /// Constructs an indivisible text document, collapsing empty text.
    pub(crate) fn text(text: impl Into<String>) -> Doc {
        let text = text.into();
        if text.is_empty() { Doc::Empty } else { Doc::Text(text) }
    }

    /// Constructs a break with the text used by its flat layout.
    pub(crate) fn break_(text: impl Into<String>) -> Doc {
        Doc::Break(text.into())
    }

    /// Nests a document by the requested indentation.
    pub(crate) fn nest(indent: usize, doc: Doc) -> Doc {
        if indent == 0 { doc } else { Doc::Nest(indent, Box::new(doc)) }
    }

    /// Groups a document so its breaks flatten together when they fit.
    pub(crate) fn group(doc: Doc) -> Doc {
        Doc::Group(Box::new(doc))
    }

    // - Sequences
    //
    //   Doc::concat([x, y])       -> Concat([x, y])
    //   Doc::join(sep, [])        -> Empty
    //   Doc::join(sep, [x, y, z]) -> Concat([x, sep, y, sep, z])
    //   Doc::flow([x, y, z])      -> Concat([x, Group(Concat([Break(" "), y])),
    //                                           Group(Concat([Break(" "), z]))])

    /// Concatenates documents in iteration order.
    pub(crate) fn concat(docs: impl IntoIterator<Item = Doc>) -> Doc {
        Doc::Concat(docs.into_iter().collect())
    }

    /// Joins documents with a shared separator.
    pub(crate) fn join(separator: Doc, docs: impl IntoIterator<Item = Doc>) -> Doc {
        let mut docs = docs.into_iter();
        let Some(doc_head) = docs.next() else {
            return Doc::Empty;
        };
        // Store siblings together so dropping a wide list does not recurse
        let mut docs_joined = vec![doc_head];
        for doc in docs {
            docs_joined.push(separator.clone());
            docs_joined.push(doc);
        }
        Doc::concat(docs_joined)
    }

    /// Joins documents with spaces that may break independently.
    pub(crate) fn flow(docs: impl IntoIterator<Item = Doc>) -> Doc {
        let mut docs = docs.into_iter();
        let Some(doc_head) = docs.next() else {
            return Doc::Empty;
        };
        let mut docs_flowed = vec![doc_head];
        for doc in docs {
            let doc_spaced = Doc::concat([Doc::break_(" "), doc]);
            docs_flowed.push(Doc::group(doc_spaced));
        }
        Doc::concat(docs_flowed)
    }
}
