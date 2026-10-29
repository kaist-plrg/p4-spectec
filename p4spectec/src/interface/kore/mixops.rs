//! Constructor notation of the KORE encoder and decoder
//!
//! These shapes follow `spec-k/0.1-aux.watsup`, `spec-k/1-syntax.watsup`,
//! and `runres` of `spec-k/2-relation.watsup`,
//! and are interned once for the definition.

use crate::lang::data::{
    arena::Arena,
    notation::flat::{
        Mixop,
        parse::{ParseMixopError, ToMixop},
    },
};

type Result<T> = std::result::Result<T, String>;

/// The notation of the cases read and written here, interned once.
pub struct Mixops {
    // KORE as kompile writes it, `kore` and the declarations
    pub(super) kvar: Mixop,
    pub(super) ksetvar: Mixop,
    pub(super) kapp: Mixop,
    pub(super) kml: Mixop,
    pub(super) kstr: Mixop,
    pub(super) kdv: Mixop,
    pub(super) kattr: Mixop,
    pub(super) kaxiom: Mixop,
    pub(super) ksymbol: Mixop,
    // Terms, `term` and `pattern`
    pub(super) app: Mixop,
    pub(super) dv: Mixop,
    pub(super) int: Mixop,
    pub(super) bool: Mixop,
    pub(super) string: Mixop,
    pub(super) float: Mixop,
    pub(super) bytes: Mixop,
    pub(super) map: Mixop,
    pub(super) set: Mixop,
    pub(super) list: Mixop,
    // Sorts
    pub(super) sort: Mixop,
    pub(super) sortvar: Mixop,
    // Maps and sets
    pub(super) pair: Mixop,
    pub(super) braces: Mixop,
    // Results
    pub(super) ok: Mixop,
    pub(super) error: Mixop,
    pub(super) run_final: Mixop,
    pub(super) run_error: Mixop,
    pub(super) run_initfail: Mixop,
}

fn mixop(arena: &mut Arena, shape: &str) -> Result<Mixop> {
    shape
        .to_mixop(&mut arena.mixop)
        .map_err(|error| match error {
            ParseMixopError::Syntax(error) => panic!("a valid mixop: {error}"),
            ParseMixopError::Intern(error) => error.to_string(),
        })
}

impl Mixops {
    pub fn new(arena: &mut Arena) -> Result<Self> {
        Ok(Self {
            kvar: mixop(arena, "KVAR varid sort")?,
            ksetvar: mixop(arena, "KSETVAR varid sort")?,
            kapp: mixop(arena, "KAPP symid sort* kore*")?,
            kml: mixop(arena, "KML text sort* kore*")?,
            kstr: mixop(arena, "KSTR text")?,
            kdv: mixop(arena, "KDV pattern")?,
            kattr: mixop(arena, "KATTR text sort* kore*")?,
            kaxiom: mixop(arena, "AXIOM kore kattr*")?,
            ksymbol: mixop(arena, "KSYMBOL sortid* sort* '->' sort kattr*")?,
            app: mixop(arena, "APP symid sort* term*")?,
            dv: mixop(arena, "DV sort text")?,
            int: mixop(arena, "INT int")?,
            bool: mixop(arena, "BOOL bool")?,
            string: mixop(arena, "STRING text")?,
            float: mixop(arena, "FLOAT text")?,
            bytes: mixop(arena, "BYTES nat*")?,
            map: mixop(arena, "MAP sort m")?,
            set: mixop(arena, "SET sort s")?,
            list: mixop(arena, "LIST sort term*")?,
            sort: mixop(arena, "SORT sortid sort*")?,
            sortvar: mixop(arena, "SORTVAR sortid")?,
            pair: mixop(arena, "k ':' v")?,
            braces: mixop(arena, "`{ k `}")?,
            ok: mixop(arena, "OK x")?,
            error: mixop(arena, "ERROR")?,
            run_final: mixop(arena, "FINAL nat term")?,
            run_error: mixop(arena, "ERROR nat term")?,
            run_initfail: mixop(arena, "INITFAIL")?,
        })
    }
}
