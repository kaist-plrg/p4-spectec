let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec adoc_files directory =
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun entry ->
         let path = Filename.concat directory entry in
         if Sys.is_directory path then adoc_files path
         else if String.equal (Filename.extension path) ".adoc" then [ path ]
         else [])

let count_substring needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec count index total =
    if index + needle_length > text_length then total
    else if String.equal (String.sub text index needle_length) needle then
      count (index + needle_length) (total + 1)
    else count (index + 1) total
  in
  count 0 0

let contains needle text = count_substring needle text > 0

let () =
  let root = Sys.argv.(1) in
  let skeleton = Filename.concat root "sections-skeleton" in
  let skeleton_text =
    adoc_files skeleton |> List.map read_file |> String.concat "\n"
  in
  let count_anchor name = count_substring ("${" ^ name ^ ":") skeleton_text in
  let main = read_file (Filename.concat root "P4-16-spec-new.adoc") in
  let docinfo = read_file (Filename.concat root "docinfo.html") in
  let stem_active =
    main |> String.split_on_char '\n'
    |> List.exists (fun line ->
           String.equal (String.trim line) ":stem: latexmath")
  in
  let print_count name = Printf.printf "%s: %d\n" name (count_anchor name) in
  let print_bool name value = Printf.printf "%s: %b\n" name value in
  List.iter print_count
    [
      "syntax";
      "relation-title-source";
      "relation-title-latex";
      "rulegroup-prose-else";
      "rulegroup-source";
      "rulegroup-latex";
      "func-title-source";
      "func-title-latex";
      "func-source";
      "func-latex";
      "table-source";
      "table-latex";
    ];
  print_bool "stem-active" stem_active;
  print_bool "mathjax-3.2.2" (contains "mathjax@3.2.2" docinfo);
  print_bool "mathjax-2.7.9" (contains "2.7.9" docinfo);
  print_bool "mathjax-lazy" (contains "'ui/lazy'" docinfo);
  print_bool "mathjax-html-loader" (contains "'[tex]/html'" docinfo);
  print_bool "mathjax-mathtools-loader" (contains "'[tex]/mathtools'" docinfo);
  print_bool "mathjax-color-loader" (contains "'[tex]/color'" docinfo);
  print_bool "mathjax-textmacros-loader" (contains "'[tex]/textmacros'" docinfo);
  print_bool "mathjax-html-package"
    (contains "'[+]': ['html', 'mathtools', 'color', 'textmacros']" docinfo);
  print_bool "mathjax-mathtools-package"
    (contains "'[+]': ['html', 'mathtools', 'color', 'textmacros']" docinfo);
  print_bool "mathjax-color-package"
    (contains "'[+]': ['html', 'mathtools', 'color', 'textmacros']" docinfo);
  print_bool "mathjax-textmacros-package"
    (contains "'[+]': ['html', 'mathtools', 'color', 'textmacros']" docinfo);
  print_bool "mathjax-color-padding" (contains "padding: '3px'" docinfo);
  print_bool "mathjax-color-border" (contains "borderWidth: '1px'" docinfo);
  print_bool "lazy-margin-300px" (contains "lazyMargin: '300px'" docinfo)
