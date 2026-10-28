//! Constructor notation shared by the KORE encoder and decoder
//!
//! These shapes follow `spec-k/0.1-aux.watsup` and `spec-k/1-syntax.watsup`,
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
    pub(super) sort: Mixop,
    pub(super) sortvar: Mixop,
    pub(super) pair: Mixop,
    pub(super) braces: Mixop,
    pub(super) ok: Mixop,
    pub(super) error: Mixop,
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
        })
    }
}
