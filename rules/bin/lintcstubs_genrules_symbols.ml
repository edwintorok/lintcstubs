(* See https://dune.readthedocs.io/en/stable/howto/rule-generation.html

   Dynamic rule generation wouldn't work for .symbols, because 'glob_files_rec ../*.c' would
   introduce a cyclic dependency on the 'run' subfolder containing 'dynamic_include'.

   Use rule generation with promotion instead.
*)

let gen_dune_include dirs =
  dirs
  |> List.map (Printf.sprintf "(source_tree %s)")
  |> String.concat " "
  |> Printf.sprintf
       {|
    (subdir lintcstubs_generate
      (rule
        (deps %s)
        (target c_ml_files.sexp)
        (action
            (dynamic-run %%{bin:lintcstubs_gen_find_targets} --output %%{target} --ext c --ext ml --paths %%{deps})
        )
      )

      (rule
        (deps (include c_ml_files.sexp))
        (action
          (with-stdout-to dune.inc
            (run %%{bin:lintcstubs_genrules} --symbols %%{deps})
        )
      )

    (subdir lintcstubs_run
      (dynamic-include ../lintcstubs_generate/dune.inc)
    )
  |}

let gen_symbols_ml dep =
  Printf.printf
    {|
    (rule
      (deps %s)
      (alias gensymbols)
      (action 
        (with-stdout-to %s.symbols
          (run %%{bin:lintcstubs_primitives_of_ml} %%{deps})
        )
      )
    )
  |}
    dep dep

let gen_symbols_c dep =
  let noext = Filename.chop_extension dep in
  Printf.printf
    {|
    (rule
      (deps %s.o)
      (alias gensymbols)
      (action
        (with-stdout-to %s.c.symbols
          (run nm -gAP %%{deps})
        )
      )
    )
  |}
    noext noext

let gen_symbols dep =
  match Filename.extension dep with
  | ".c" ->
      gen_symbols_c dep
  | ".ml" ->
      gen_symbols_ml dep
  | _ ->
      failwith (Printf.sprintf "Unknown extension for %S" dep)

let () =
  let usage = Printf.sprintf "%s [OPTIONS...] [dependencies]" Sys.argv.(0) in
  let symbols = ref false in
  Arg.parse
    [("--symbols", Arg.Set symbols, "generate rules for symbols")]
    gen_symbols usage ;
  if !symbols = false then
    gen_dune_include ()
