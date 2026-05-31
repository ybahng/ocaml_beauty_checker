let metrics : (module Metric.METRIC) list = [
  (module Functional_purity);
  (module Pattern_quality);
  (module Naming_clarity);
  (module Structural_depth);
]

let analyse filename =
  let ast = Ast_walker.parse_file filename in
  let results = Scorer.run metrics ast in
  let score = Scorer.total results in
  Reporter.report filename results score

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] ->
    Printf.eprintf "Usage: ocaml-beauty <file.ml> [file.ml ...]\n";
    exit 1
  | files -> List.iter analyse files
