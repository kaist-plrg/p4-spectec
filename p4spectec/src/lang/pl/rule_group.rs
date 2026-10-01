//! Rule groups extracted from prose-language dispatch blocks
//!
//! ```text
//! Even dispatch tree                   -> [Even/nil, Even/cons]
//! Route([[Group(a)], [If(Group(b))]])  -> [a, b]
//! ```

use super::{
    annot::Hints,
    ast::{
        CaseInstr, CheckLetMatchInstr, CheckLetSubInstr, DispatchBlock, DispatchInstr, Exp,
        GroupBlock, HoldCase, HoldInstr, Id, IfInstr, InstrKind, OptionGetInstr, RelSignature,
        RouteInstr, RuleGroupInstr, TierInstr,
    },
};

/// A rule group together with the hints of its dispatch step.
pub struct RuleGroup<'a> {
    pub hints: &'a Hints,
    pub id_group: &'a Id,
    pub id_rel: &'a Id,
    pub rel_signature: &'a RelSignature,
    pub exps_input: &'a [Exp],
    pub block: &'a GroupBlock,
}

/// Collects the rule groups of a dispatch block in depth-first source order.
pub fn collect_rule_groups(block: &DispatchBlock) -> Vec<RuleGroup<'_>> {
    let mut rule_groups = Vec::new();
    for instr in block {
        match &instr.node.node {
            InstrKind::If(IfInstr { block, .. }) => {
                rule_groups.extend(collect_rule_groups(block));
            }
            InstrKind::Hold(HoldInstr { hold_case, .. }) => match hold_case {
                HoldCase::Both(block_hold, block_not_hold) => {
                    rule_groups.extend(collect_rule_groups(block_hold));
                    rule_groups.extend(collect_rule_groups(block_not_hold));
                }
                HoldCase::Hold(block, _) | HoldCase::NotHold(block, _) => {
                    rule_groups.extend(collect_rule_groups(block));
                }
            },
            InstrKind::Case(CaseInstr { cases, .. }) => {
                for case in cases {
                    rule_groups.extend(collect_rule_groups(&case.block));
                }
            }
            InstrKind::CheckLetSub(CheckLetSubInstr { block, .. })
            | InstrKind::CheckLetMatch(CheckLetMatchInstr { block, .. })
            | InstrKind::OptionGet(OptionGetInstr { block, .. }) => {
                rule_groups.extend(collect_rule_groups(block));
            }
            InstrKind::Tier(TierInstr { tier: DispatchInstr::Route(RouteInstr { blocks }) }) => {
                for block in blocks {
                    rule_groups.extend(collect_rule_groups(block));
                }
            }
            // A rule group is a leaf; everything else only nests
            InstrKind::Tier(TierInstr { tier: DispatchInstr::Group(rule_group_instr) }) => {
                let RuleGroupInstr { id_rel, id_group, rel_signature, exps_input, block } =
                    rule_group_instr;
                let rule_group = RuleGroup {
                    hints: &instr.hints,
                    id_group,
                    id_rel,
                    rel_signature,
                    exps_input,
                    block,
                };
                rule_groups.push(rule_group);
            }
            InstrKind::Let(_) | InstrKind::Debug(_) | InstrKind::Destruct(_) => {}
        }
    }
    rule_groups
}
