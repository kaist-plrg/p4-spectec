open Backend_latex_test_support
open Doc

let () =
  print_width "badge-width" (badge "Rule_label");
  print_width "fill-width"
    (fill ~separator:thin_space [ styled_mathit "aa"; styled_mathit "bbb" ]);
  print_width "numbered-width"
    (numbered [ styled_mathit "first"; styled_mathit "second" ]);
  print_width "styled-width" (styled_mathit "x_escaped");
  print_width "decimal-width"
    (decimal (Bigint.of_string "123456789012345678901234567890"));
  print_width "hex-width"
    (hexadecimal (Bigint.Hex.of_string "0x123456789abcdef0123456789abcdef"));
  print_width "spacing-width"
    (concat [ styled_mathit "x"; space; thin_space; quad; styled_mathit "y" ]);
  print_width "script-width"
    (subsup (styled_mathit "base")
       (styled_mathit "subscript")
       (styled_mathit "sup"));
  print_width "fraction-width"
    (fraction (styled_mathit "numerator") (styled_mathit "den"));
  print_width "link-width"
    (link (Link.target_of_string "target") (styled_mathit "linked_value"))
