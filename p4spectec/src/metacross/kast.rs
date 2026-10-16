//! Meta values as KAST JSON version 4, for `spec-meta-k/`
//!
//! Labels are a function of a case's meta type and leading keyword, list
//! symbols of the element type, and option symbols of the element type;
//! the tables mirror the frozen `common/1-syntax.k` and `al/1-syntax.k`.
//! A `FUNC` type becomes `funcT()` without its signature, and the type
//! arguments of `VAR` and `CALL` are `TArgList`s although they are encoded
//! as `typ*`. An unknown shape is an error, never a fallback label.

use std::fmt::Write;

use serde_json::Value as Json;

use crate::{
    lang::{
        common::{Iter, prim::num},
        data::{arena::Arena, typ::TypKind, value::flat::Value, value::flat::ValueKind},
    },
    util::json::json as extern_json,
};

use super::{Result, error::emit_invalid, leading_keyword, typ_name, typ_text};

/// The KAST JSON format version this module emits.
pub const VERSION: u64 = 4;

// == KAST JSON nodes
//
// Objects are assembled by hand: `json!` would copy every interpolated
// subterm, making the cons lists below quadratic in their length.

fn object(fields: Vec<(&str, Json)>) -> Json {
    Json::Object(
        fields
            .into_iter()
            .map(|(key, value)| (key.to_string(), value))
            .collect(),
    )
}

fn klabel(name: &str) -> Json {
    object(vec![("node", "KLabel".into()), ("name", name.into()), ("params", Json::Array(vec![]))])
}

fn kapply(label: &str, args: Vec<Json>) -> Json {
    object(vec![
        ("node", "KApply".into()),
        ("label", klabel(label)),
        ("arity", args.len().into()),
        ("args", Json::Array(args)),
    ])
}

fn ktoken(sort: &str, token: String) -> Json {
    let sort = object(vec![
        ("node", "KSort".into()),
        ("name", sort.into()),
        ("params", Json::Array(vec![])),
    ]);
    object(vec![("node", "KToken".into()), ("sort", sort), ("token", token.into())])
}

/// A cons list ending in its terminator.
fn klist(symbol: &str, terminator: &str, items: Vec<Json>) -> Json {
    items
        .into_iter()
        .rev()
        .fold(kapply(terminator, vec![]), |tail, item| kapply(symbol, vec![item, tail]))
}

fn kopt(none: &str, some: &str, item: Option<Json>) -> Json {
    match item {
        None => kapply(none, vec![]),
        Some(item) => kapply(some, vec![item]),
    }
}

// == Tokens

fn bool_token(value: bool) -> Json {
    ktoken("Bool", value.to_string())
}

fn int_token(digits: String) -> Json {
    ktoken("Int", digits)
}

/// A string token in K's literal syntax.
fn string_token(text: &str) -> Json {
    let mut token = String::with_capacity(text.len() + 2);
    token.push('"');
    for ch in text.chars() {
        match ch {
            '"' => token.push_str("\\\""),
            '\\' => token.push_str("\\\\"),
            '\n' => token.push_str("\\n"),
            '\t' => token.push_str("\\t"),
            '\r' => token.push_str("\\r"),
            '\x0c' => token.push_str("\\f"),
            ch if (ch as u32) < 0x20 || ch == '\x7f' => {
                write!(token, "\\x{:02x}", ch as u32).expect("writing to a String cannot fail");
            }
            ch if ch.is_ascii() => token.push(ch),
            ch if (ch as u32) <= 0xffff => {
                write!(token, "\\u{:04x}", ch as u32).expect("writing to a String cannot fail");
            }
            ch => {
                write!(token, "\\U{:08x}", ch as u32).expect("writing to a String cannot fail");
            }
        }
    }
    token.push('"');
    ktoken("String", token)
}

// == Extern JSON as K `JSON` terms

fn json_term(value: &extern_json) -> Json {
    match value {
        extern_json::Null => kapply("JSONnull", vec![]),
        extern_json::Bool(value) => bool_token(*value),
        extern_json::Number(number) if number.is_f64() => ktoken("Float", number.to_string()),
        extern_json::Number(number) => int_token(number.to_string()),
        extern_json::String(text) => string_token(text),
        extern_json::Array(values) => {
            let items = values.iter().map(json_term).collect();
            kapply("JSONList", vec![klist("JSONs", ".JSONs", items)])
        }
        extern_json::Object(fields) => {
            let entries = fields
                .iter()
                .map(|(key, value)| kapply("JSONEntry", vec![string_token(key), json_term(value)]))
                .collect();
            kapply("JSONObject", vec![klist("JSONs", ".JSONs", entries)])
        }
    }
}

// == Symbol tables

/// The label of a case by its meta type and leading keyword.
fn case_label(typ: &str, keyword: Option<&str>) -> Option<&'static str> {
    let label = match (typ, keyword) {
        ("typ", Some("BOOL")) => "boolT",
        ("typ", Some("NAT")) => "natT",
        ("typ", Some("INT")) => "intT",
        ("typ", Some("TEXT")) => "textT",
        ("typ", Some("VAR")) => "varT",
        ("typ", Some("TUP")) => "tupT",
        ("typ", Some("ITER")) => "iterT",
        ("typ", Some("FUNC")) => "funcT",
        ("deftyp", Some("ALIAS")) => "aliasDT",
        ("deftyp", Some("STRUCT")) => "structDT",
        ("deftyp", Some("VARIANT")) => "variantDT",
        ("iter", Some("QUEST")) => "quest",
        ("iter", Some("STAR")) => "star",
        ("exp" | "val", Some("NAT")) => "natN",
        ("exp" | "val", Some("INT")) => "intN",
        ("exp", Some("BOOL")) => "boolE",
        ("exp", Some("TEXT")) => "textE",
        ("exp", Some("VAR")) => "varE",
        ("exp", Some("UN")) => "unE",
        ("exp", Some("BIN")) => "binE",
        ("exp", Some("CMP")) => "cmpE",
        ("exp", Some("UPCAST")) => "upCastE",
        ("exp", Some("DOWNCAST")) => "downCastE",
        ("exp", Some("SUB")) => "subE",
        ("exp", Some("MATCH")) => "matchE",
        ("exp", Some("TUP")) => "tupE",
        ("exp", Some("INJ")) => "injE",
        ("exp", Some("STR")) => "strE",
        ("exp", Some("OPT")) => "optE",
        ("exp", Some("LIST")) => "listE",
        ("exp", Some("CONS")) => "consE",
        ("exp", Some("CAT")) => "catE",
        ("exp", Some("MEM")) => "memE",
        ("exp", Some("LEN")) => "lenE",
        ("exp", Some("DOT")) => "dotE",
        ("exp", Some("IDX")) => "idxE",
        ("exp", Some("SLICE")) => "sliceE",
        ("exp", Some("UPD")) => "updE",
        ("exp", Some("CALL")) => "callE",
        ("exp", Some("ITER")) => "iterE",
        ("val", Some("BOOL")) => "boolV",
        ("val", Some("TEXT")) => "textV",
        ("val", Some("STR")) => "strV",
        ("val", Some("INJ")) => "injV",
        ("val", Some("TUP")) => "tupV",
        ("val", Some("OPT")) => "optV",
        ("val", Some("LIST")) => "listV",
        ("val", Some("FUNC")) => "funcV",
        ("val", Some("EXT")) => "extV",
        ("pattern", Some("INJ")) => "injPat",
        ("pattern", Some("CONS")) => "consPat",
        ("pattern", Some("FIXED")) => "fixedPat",
        ("pattern", Some("NIL")) => "nilPat",
        ("pattern", Some("SOME")) => "somePat",
        ("pattern", Some("NONE")) => "nonePat",
        ("path", Some("ROOT")) => "rootPath",
        ("path", Some("IDX")) => "idxPath",
        ("path", Some("SLICE")) => "slicePath",
        ("path", Some("DOT")) => "dotPath",
        ("arg", Some("EXP")) => "expA",
        ("arg", Some("FUN")) => "funA",
        ("param", Some("EXP")) => "expParam",
        ("param", Some("FUN")) => "funParam",
        ("prem", Some("REL")) => "relPr",
        ("prem", Some("IF")) => "ifPr",
        ("prem", Some("IFHOLD")) => "ifHoldPr",
        ("prem", Some("IFNOTHOLD")) => "ifNotHoldPr",
        ("prem", Some("LET")) => "letPr",
        ("prem", Some("ITER")) => "iterPr",
        ("prem", Some("DEBUG")) => "debugPr",
        ("defn", Some("EXTTYP")) => "extTypD",
        ("defn", Some("TYP")) => "typD",
        ("defn", Some("EXTREL")) => "extRelD",
        ("defn", Some("REL")) => "relD",
        ("defn", Some("EXTFUNC")) => "extFuncD",
        ("defn", Some("BUILTINFUNC")) => "builtinFuncD",
        ("defn", Some("TABLEFUNC")) => "tableFuncD",
        ("defn", Some("FUNC")) => "funcD",
        ("unop", Some("NOT")) => "notOp",
        ("unop", Some("PLUS")) => "plusOp",
        ("unop", Some("MINUS")) => "minusOp",
        ("binop", Some("AND")) => "andOp",
        ("binop", Some("OR")) => "orOp",
        ("binop", Some("IMPL")) => "implOp",
        ("binop", Some("EQUIV")) => "equivOp",
        ("binop", Some("ADD")) => "addOp",
        ("binop", Some("SUB")) => "subOp",
        ("binop", Some("MUL")) => "mulOp",
        ("binop", Some("DIV")) => "divOp",
        ("binop", Some("MOD")) => "modOp",
        ("binop", Some("POW")) => "powOp",
        ("cmpop", Some("EQ")) => "eqOp",
        ("cmpop", Some("NE")) => "neOp",
        ("cmpop", Some("LT")) => "ltOp",
        ("cmpop", Some("LE")) => "leOp",
        ("cmpop", Some("GT")) => "gtOp",
        ("cmpop", Some("GE")) => "geOp",
        ("vari", None) => "vari",
        ("typfield", None) => "typField",
        ("typcase", None) => "typCase",
        ("valfield", None) => "valField",
        ("valcase", None) => "valCase",
        ("expfield", None) => "expField",
        ("expcase", None) => "expCase",
        ("iterexp", None) => "iterExp",
        ("iterprem", None) => "iterPrem",
        ("rulmatch", None) => "rulMatch",
        ("rulpath", None) => "rulPath",
        ("rulgroup", None) => "rulGroup",
        ("elsgroup", None) => "elsGroup",
        ("clause", None) => "clause",
        ("tblrow", None) => "tblRow",
        _ => return None,
    };
    Some(label)
}

/// The `List{_}` symbol and terminator of a list by its element type.
fn list_symbols(elem: &TypKind) -> Option<(&'static str, &'static str)> {
    // Mixops are matrices of atoms typed `text**`
    if matches!(elem, TypKind::Iter(inner, Iter::List) if matches!(inner.node, TypKind::Text)) {
        return Some(("mixop", ".mixop"));
    }
    let symbols = match typ_name(elem)? {
        "id" => ("tparamList", ".tparamList"),
        "typ" => ("typList", ".typList"),
        "atom" => ("atomList", ".atomList"),
        "iter" => ("iterList", ".iterList"),
        "vari" => ("variList", ".variList"),
        "exp" => ("expList", ".expList"),
        "val" => ("valList", ".valList"),
        "arg" => ("argList", ".argList"),
        "param" => ("paramList", ".paramList"),
        "prem" => ("premList", ".premList"),
        "typfield" => ("typFieldList", ".typFieldList"),
        "typcase" => ("typCaseList", ".typCaseList"),
        "valfield" => ("valFieldList", ".valFieldList"),
        "expfield" => ("expFieldList", ".expFieldList"),
        "rulgroup" => ("rulGroupList", ".rulGroupList"),
        "rulpath" => ("rulPathList", ".rulPathList"),
        "clause" => ("clauseList", ".clauseList"),
        "tblrow" => ("tblRowList", ".tblRowList"),
        _ => return None,
    };
    Some(symbols)
}

/// The none and some symbols of an option by its element type.
fn opt_symbols(elem: &TypKind) -> Option<(&'static str, &'static str)> {
    let symbols = match typ_name(elem)? {
        "val" => ("noVal", "someVal"),
        "exp" => ("noExp", "someExp"),
        "elsgroup" => ("noElsGroup", "someElsGroup"),
        "elsclause" => ("noElsClause", "someElsClause"),
        _ => return None,
    };
    Some(symbols)
}

// == Walking meta values

/// The KAST term of a meta value.
pub fn term(arena: &Arena, value: Value) -> Result<Json> {
    let typ = arena.value.typ(value.note);
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => Ok(bool_token(*value)),
        ValueKind::Num(num) => Ok(int_token(num::to_int(num).to_string())),
        ValueKind::Text(text) => Ok(string_token(text)),
        ValueKind::List(values) => {
            // The script itself is typed `script`, not `defn*`
            if typ_name(typ) == Some("script") {
                return Ok(klist("script", ".script", terms(arena, values)?));
            }
            let TypKind::Iter(elem, Iter::List) = &**typ else {
                return Err(emit_invalid("a list value without a list type"));
            };
            let (symbol, terminator) = list_symbols(&elem.node)
                .ok_or_else(|| emit_invalid(format!("no K list for `{}`", typ_text(&elem.node))))?;
            Ok(klist(symbol, terminator, terms(arena, values)?))
        }
        ValueKind::Opt(value) => {
            let TypKind::Iter(elem, Iter::Opt) = &**typ else {
                return Err(emit_invalid("an option value without an option type"));
            };
            let (none, some) = opt_symbols(&elem.node).ok_or_else(|| {
                emit_invalid(format!("no K option for `{}`", typ_text(&elem.node)))
            })?;
            Ok(kopt(none, some, value.map(|value| term(arena, value)).transpose()?))
        }
        ValueKind::Extern(json) => Ok(json_term(json)),
        ValueKind::Case(case) => {
            let name = typ_name(typ)
                .ok_or_else(|| emit_invalid(format!("a case value of type `{}`", typ_text(typ))))?;
            let keyword = leading_keyword(arena, *case.mixop());
            let label = case_label(name, keyword.as_deref()).ok_or_else(|| {
                emit_invalid(format!(
                    "no K label for `{}` of type `{name}`",
                    keyword.unwrap_or_default()
                ))
            })?;
            // The frozen K syntax has `funcT()` without its signature
            if label == "funcT" {
                return Ok(kapply(label, vec![]));
            }
            let args = case
                .args()
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    // Type arguments are `TArgList`s although encoded as `typ*`
                    if index == 1 && matches!(label, "varT" | "callE") {
                        targs(arena, *arg)
                    } else {
                        term(arena, *arg)
                    }
                })
                .collect::<Result<_>>()?;
            Ok(kapply(label, args))
        }
        ValueKind::Struct(_) | ValueKind::Tuple(_) | ValueKind::Func(_) => {
            Err(emit_invalid("struct, tuple, and function values have no KAST term"))
        }
    }
}

fn terms(arena: &Arena, values: &[Value]) -> Result<Vec<Json>> {
    values.iter().map(|value| term(arena, *value)).collect()
}

fn targs(arena: &Arena, value: Value) -> Result<Json> {
    let ValueKind::List(values) = arena.value.kind(value.node) else {
        return Err(emit_invalid("type arguments are a list"));
    };
    Ok(klist("targList", ".targList", terms(arena, values)?))
}

// == Entry points

fn document(term: Json) -> Json {
    object(vec![("format", "KAST".into()), ("version", VERSION.into()), ("term", term)])
}

/// The KAST document of a `script` meta value, of sort `Script`.
pub fn script(arena: &Arena, value: Value) -> Result<Json> {
    Ok(document(term(arena, value)?))
}

/// The KAST document of a `val` meta value wrapped as `someP4(val)`, of sort `P4Opt`.
pub fn p4(arena: &Arena, value: Value) -> Result<Json> {
    Ok(document(kapply("someP4", vec![term(arena, value)?])))
}
