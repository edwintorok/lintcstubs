open Sexplib

let () =
  let rules =
    stdin
    |> Sexp.input_rev_sexps
    |> List.rev_map Dune_rules.Parse.Rule.t_of_sexp
  in
  if List.length rules > 0 then print_endline "OK"
