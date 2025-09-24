(* Splice anchors within the skeleton document *)

type t =
  | Syntax of {
      start : string;
      name : string;
      prefix : string;
      suffix : string;
    }
  | Relation of {
      start : string;
      name : string;
      prefix : string;
      suffix : string;
    }
  | RuleGroup of {
      start : string;
      name : string;
      prefix : string;
      suffix : string;
    }
  | RuleProse of { start : string; name : string }

(* Predefined anchors *)

let syntax =
  Syntax
    {
      start = "$";
      name = "syntax";
      prefix = "[source,bison]\n----\n";
      suffix = "\n----\n";
    }

let relation =
  Relation
    {
      start = "$";
      name = "relation";
      prefix = "[source]\n----\n";
      suffix = "\n----\n";
    }

let rulegroup =
  RuleGroup
    {
      start = "$";
      name = "rulegroup";
      prefix = "[source]\n----\n";
      suffix = "\n----\n";
    }

let ruleprose = RuleProse { start = "$"; name = "ruleprose" }
