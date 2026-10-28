//! Comparing a K-in-ST run with krun's
//!
//! Both are written as `print_runres` prints a run: a first line, the steps or
//! how the run failed, and then the configuration, compared in normal form
//! (`interface::kore::normal`).

use p4spectec::interface::kore::{ast::Pattern, info::Info, normal, parse};

/// Whether a run's output agrees with krun's: the same first line, which
/// gives the steps and how the run ended, and the same configuration in normal form.
pub fn compare(info: &Info, output: &str, expected: &str) -> Result<(), String> {
    let (head_output, config_output) = split(output);
    let (head_expected, config_expected) = split(expected);
    if head_output != head_expected {
        return Err(format!("{head_output}, krun: {head_expected}"));
    }
    match (config_output, config_expected) {
        (None, None) => Ok(()),
        (Some(config_output), Some(config_expected)) => {
            if normal_text(info, config_output)? == normal_text(info, config_expected)? {
                Ok(())
            } else {
                Err("the configuration differs".to_owned())
            }
        }
        _ => Err("one run has a configuration, the other none".to_owned()),
    }
}

/// Whether a run ended in one of the final configurations K allows, `finals`,
/// as the `search` binary writes them: one configuration, or several under `\or`.
pub fn reached(info: &Info, output: &str, finals: &[u8]) -> Result<(), String> {
    let (head, config) = split(output);
    let config = match config {
        Some(config) if head.bytes().all(|byte| byte.is_ascii_digit()) => config,
        _ => return Err("the run did not end in a final configuration".to_owned()),
    };
    let normal = normal_text(info, config)?;
    let finals = match parse::parse_pattern(finals).map_err(|error| error.message)? {
        Pattern::App(name, _, args) if name == "\\or" => args,
        Pattern::App(name, _, _) if name == "\\bottom" => vec![],
        pattern => vec![pattern],
    };
    if finals
        .iter()
        .any(|pattern| normal::normal_text(info, pattern) == normal)
    {
        Ok(())
    } else {
        Err(format!("is none of the {} final configurations K allows", finals.len()))
    }
}

/// A configuration in KORE text, in normal form.
fn normal_text(info: &Info, config: &str) -> Result<String, String> {
    parse::parse_pattern(config.as_bytes())
        .map(|pattern| normal::normal_text(info, &pattern))
        .map_err(|error| error.message)
}

/// The first line of an output and the configuration after it, if any.
fn split(output: &str) -> (&str, Option<&str>) {
    let output = output.trim_end();
    match output.split_once('\n') {
        Some((head, config)) => (head, Some(config)),
        None => (output, None),
    }
}
