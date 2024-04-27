open Sexplib

let () =
  let (_:_ list) = stdin |> Sexp.input_rev_sexps |> List.rev_map Dune_rules.Parse.Rule.t_of_sexp
  in ();
  print_endline "OK"
  
