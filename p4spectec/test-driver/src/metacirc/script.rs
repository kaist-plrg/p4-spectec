//! Compiler-produced source AL round trips through the meta codec
//!
//! The small fixture exposes canonical input positions and otherwise groups.
//! Complete meta and P4 specifications exercise real definitions and syntax.

use p4spectec::interface::al::encode::Encode;

use p4spectec::interface::al::decode::Decode;

use std::path::Path;

use p4spectec::lang::{al::ast, data::arena::Arena, traits::eq::SyntaxEq};

use super::{MetaRunner, Result, check_guard, fixture, meta_runner, source};

/// Checks source AL from the target fixture and both complete specifications.
pub fn run() -> Result<()> {
    let mut arena = Arena::new();
    let mut runner = meta_runner(&mut arena)?;

    // Assert the focused fixture's canonical relation input order
    let spec_al = check_script(&mut arena, &mut runner, &fixture("target.watsup"))?;
    let rel = spec_al
        .iter()
        .find_map(|def_al| match &def_al.node {
            ast::DefKind::Rel(ast::RelDef::Defined(rel)) if rel.id.node.as_ref() == "Split" => {
                Some(rel)
            }
            _ => None,
        })
        .expect("target fixture must define Split");
    let indices = rel
        .input_hint
        .indices()
        .iter()
        .map(|idx| idx.node)
        .collect::<Vec<_>>();
    assert_eq!(indices, [0, 1], "decoded inputs must precede outputs");
    assert!(rel.else_group.is_some(), "Split must retain its otherwise group");

    // Use full specifications to exercise recursive and higher-order syntax
    for name in ["spec-meta", "spec"] {
        check_script(&mut arena, &mut runner, &source(name))?;
    }
    Ok(())
}

/// Preserves the encoded script after source AL decoding and re-encoding.
fn check_script(arena: &mut Arena, runner: &mut MetaRunner, path: &Path) -> Result<ast::Spec> {
    let spec_al = p4spectec::algo(&[path.to_path_buf()])?;
    let num_vars = spec_al
        .iter()
        .filter(|def_al| matches!(def_al.node, ast::DefKind::Var(_)))
        .count();

    // Variable declarations have no executable meta definition
    let value_meta = Encode::encode(&spec_al, arena)?;
    let spec_decode_al = <ast::Spec as Decode>::decode(arena, value_meta)?;
    assert_eq!(spec_decode_al.len() + num_vars, spec_al.len());
    assert!(
        !spec_decode_al
            .iter()
            .any(|def_al| matches!(def_al.node, ast::DefKind::Var(_)))
    );

    // Canonical metadata may change, but the complete encoded script must not
    let value_again = Encode::encode(&spec_decode_al, arena)?;
    assert!(
        value_meta.view(arena).syntax_eq(&value_again.view(arena)),
        "{}: script round trip",
        path.display()
    );
    check_guard(runner, arena, "check_script", &[value_meta])?;
    check_guard(runner, arena, "check_script", &[value_again])?;

    // Guard type-definition payloads independently of the outer script
    for def_al in &spec_al {
        if let ast::DefKind::Typ(ast::TypDef::Defined(typdef_al)) = &def_al.node {
            check_deftyp(arena, runner, &typdef_al.def_typ)?;
        }
    }
    eprintln!(
        "metacirc script: {}: {} definitions, {num_vars} variable declarations omitted",
        path.file_name().unwrap().display(),
        spec_decode_al.len()
    );
    Ok(spec_decode_al)
}

/// Checks each compiler-produced type definition with its own meta guard.
fn check_deftyp(arena: &mut Arena, runner: &mut MetaRunner, deftyp_al: &ast::DefTyp) -> Result<()> {
    let value_meta = Encode::encode(deftyp_al, arena)?;
    let deftyp_decode_al = <ast::DefTyp as Decode>::decode(arena, value_meta)?;
    let value_again = Encode::encode(&deftyp_decode_al, arena)?;
    assert!(value_meta.view(arena).syntax_eq(&value_again.view(arena)));
    check_guard(runner, arena, "check_deftyp", &[value_meta])
}
