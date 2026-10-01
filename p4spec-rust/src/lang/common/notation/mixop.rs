//! Mixops, mixfix shapes with unfilled argument positions
//!
//! A `Mixop` is a `Mixfix<()>`:
//! the atoms of a notation form and where its arguments go.
//! `fill` puts arguments back in left-to-right order;
//! `shape` parses a mixop from its text once and caches it.

use std::{cell::RefCell, collections::HashMap, error::Error, fmt, rc::Rc};

use crate::lang::{
    common::ds::set::IdSet,
    traits::{
        eq::SyntaxEq,
        free::FreeIds,
        print::{Print, Printer},
    },
};

use crate::frontend;

use super::mixfix::Mixfix;

/// A mixfix shape with unfilled argument positions.
pub type Mixop = Mixfix<()>;

impl Print for Mixop {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.print_with(printer, |(), printer| printer.write("%"))
    }
}

// == Syntax operations

impl SyntaxEq for () {
    fn syntax_eq(&self, _other: &Self) -> bool {
        true
    }
}

impl FreeIds for () {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// = Shape parsing

thread_local! {
    /// Parsed mixops by their source text.
    static SHAPE_CACHE: RefCell<HashMap<Rc<str>, Rc<Mixop>>> = RefCell::new(HashMap::new());
}

/// Parses a mixop from its text, reusing an earlier parse of the same text.
pub(crate) fn shape(shape_text: &str) -> Rc<Mixop> {
    SHAPE_CACHE.with(|cache| {
        // Cached: share it
        if let Some(mixop) = cache.borrow().get(shape_text).cloned() {
            return mixop;
        }

        // First use: parse and remember
        let mixop = frontend::parse::parse_mixop(shape_text)
            .expect("value constructor contains a valid SpecTec mixop");
        let mixop = Rc::new(mixop);
        cache
            .borrow_mut()
            .insert(Rc::from(shape_text), Rc::clone(&mixop));
        mixop
    })
}

// == Converting a mixfix to a mixop

impl<T> Mixfix<T> {
    /// Replaces every argument with an unfilled mixop position.
    pub fn to_mixop(&self) -> Mixop {
        self.map(|_| ())
    }

    /// Separates the mixop shape from its arguments.
    pub fn split(&self) -> (Mixop, Vec<&T>) {
        (self.to_mixop(), self.args())
    }
}

// == Filling a mixop with arguments

/// An error caused by a mismatch between mixop arity and supplied arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArityMismatch {
    /// Fewer arguments were supplied than the mixop requires.
    ArgumentCountTooFew,
    /// More arguments were supplied than the mixop requires.
    ArgumentCountTooMany,
}

impl fmt::Display for ArityMismatch {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ArgumentCountTooFew => fmt.write_str("Mixop.fill: too few arguments"),
            Self::ArgumentCountTooMany => fmt.write_str("Mixop.fill: too many arguments"),
        }
    }
}

impl Error for ArityMismatch {}

impl Mixop {
    /// Fills a mixfix operator with arguments.
    pub fn fill<T>(
        mixop: &Self,
        args: impl IntoIterator<Item = T>,
    ) -> Result<Mixfix<T>, ArityMismatch> {
        // Consume arguments in tree order; leftovers are too many
        let mut args = args.into_iter();
        let mixfix = mixop.fill_inner(&mut args)?;
        if args.next().is_some() { Err(ArityMismatch::ArgumentCountTooMany) } else { Ok(mixfix) }
    }

    /// Fills the positions of this subtree, taking arguments from the iterator.
    fn fill_inner<T>(
        &self,
        args: &mut impl Iterator<Item = T>,
    ) -> Result<Mixfix<T>, ArityMismatch> {
        match self {
            // A hole takes the next argument
            Self::Arg(()) => args
                .next()
                .map(Mixfix::Arg)
                .ok_or(ArityMismatch::ArgumentCountTooFew),
            // Atoms are copied
            Self::Atom(atom) => Ok(Mixfix::Atom(atom.clone())),
            // Compound shapes fill their parts left to right
            Self::Brack(atom_l, mixfix, atom_r) => Ok(Mixfix::Brack(
                atom_l.clone(),
                Box::new(mixfix.fill_inner(args)?),
                atom_r.clone(),
            )),
            Self::Infix(mixfix_l, atom, mixfix_r) => Ok(Mixfix::Infix(
                Box::new(mixfix_l.fill_inner(args)?),
                atom.clone(),
                Box::new(mixfix_r.fill_inner(args)?),
            )),
            Self::Seq(mixfixes) => Ok(Mixfix::Seq(
                mixfixes
                    .iter()
                    .map(|mixfix| mixfix.fill_inner(args))
                    .collect::<Result<_, _>>()?,
            )),
        }
    }
}
