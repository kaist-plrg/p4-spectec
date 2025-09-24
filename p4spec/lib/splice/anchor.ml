(* Splice anchors within the skeleton document *)

type t =
  | Syntax of {
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

(* Syntax anchor *)

let syntax =
  Syntax
    {
      start = "$";
      name = "syntax";
      prefix = "[source,bison]\n----\n";
      suffix = "\n----\n";
    }

let rule =
  RuleGroup
    {
      start = "$";
      name = "rulegroup";
      prefix = "[source]\n----\n";
      suffix = "\n----\n";
    }
