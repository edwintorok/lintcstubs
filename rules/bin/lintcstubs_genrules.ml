(* See https://dune.readthedocs.io/en/stable/howto/rule-generation.html

   Given a list of toplevel directories generate the initial dune rules for [lintcstubs].

   We need the toplevel directories on the command-line, to avoid cycling dependencies in the rule generation
   (we cannot discover the toplevel dirs from inside a dune rule).
*)

(** [gen_dune_include_ml_c ~target dirs] generates dune rules that discover all the [.c] and [.ml] files
  in the current directory and the specified [dirs], and writes this into [target] as an S-expression.

  It discovers both files present in the source tree and generated files.
  For generated files that are in subdirectories, it doesn't depend on their generation.

  To avoid cyclic dependencies we cannot use [source_tree .], and can only discover files in the current
  directory by depending on them with a glob. Usually the root of a project doesn't actually contain source files,
  so in practice this shouldn't be a problem.

  (a recursive glob would depend on the generation of all files, which we may want to avoid
   when generating dune rules (otherwise every dune target will depend on these generated files transitively,
   even if it doesn't use them).
  )
 *)
let gen_dune_include_ml_c ~target dirs =
  dirs
  |> List.map (Printf.sprintf "(source_tree ../%s)")
  |> String.concat " "
  |> Printf.printf
       {|
    (subdir lintcstubs_generate
      (rule
        (target %s)
        (deps
           (glob_files *.c) (glob_files *.ml)
            %s
        )
        (action
            (dynamic-run %%{bin:lintcstubs_gen_find_targets} --output %%{target} --ext c --ext ml --paths %%{deps})
        )
      )
    )
|}
       target

let gen_dune_include_for ~ml_c_sexp () =
  Printf.printf
    {|
    (subdir lintcstubs_generate
      (rule
        (deps (include %s))
        (action
          (with-stdout-to dune.inc
            (run %%{bin:lintcstubs_genrules} --symbols %%{deps})
          )
        )
      )
    )

    (subdir lintcstubs_run
      (dynamic_include ../lintcstubs_generate/dune.inc)      
    )
  |}
    ml_c_sexp

let gen_dune_default dirs =
  let dirs =
    match dirs with
    | [] ->
        Sys.readdir "."
        |> Array.to_list
        |> List.filter (function
             | "." | ".." ->
                 false
             | dir ->
                 Sys.is_directory dir
             )
    | l ->
        l
  in
  let ml_c_sexp = "ml_c_files.sexp" in
  gen_dune_include_ml_c ~target:ml_c_sexp dirs ;
  gen_dune_include_for ~ml_c_sexp ()

let drop_dotdot path =
  (* target cannot be in parent, so have to remove leading ../ *)
  if String.starts_with ~prefix:"../" path then
    String.sub path 3 (String.length path - 3)
  else
    path

(* [(subdir)] stanza cannot be used from include,
  and we cannot generate rules that write into subdirs directly either.
  So have to convert paths to not contain [/], yet still be unique.
 *)
let symbols_name filename =
  filename |> String.map (function '/' -> '_' | c -> c)

let gen_symbols ~cmd dep =
  let target = drop_dotdot dep in
  Printf.printf
    {|
      (rule
        (deps %s)
        (alias gensymbols)
        (action
          (with-stdout-to %s.symbols
            (run %s %%{deps})
          )
        )
      )
  |}
    dep (symbols_name target) cmd

let gen_symbols_ml = gen_symbols ~cmd:"%{bin:lintcstubs_primitives_of_ml}"

let gen_symbols_c dep =
  let dep = Filename.chop_suffix dep ".c" ^ ".o" in
  gen_symbols ~cmd:"nm -gAP" dep

let gen_symbols_one dep =
  match Filename.extension dep with
  | ".c" ->
      gen_symbols_c dep
  | ".ml" ->
      gen_symbols_ml dep
  | _ ->
      failwith (Printf.sprintf "Unknown extension for %S" dep)

let gen_symbols deps =
  deps |> List.iter gen_symbols_one ;
  exit 0

let () =
  let usage = Printf.sprintf "%s [--symbols SOURCEFILE ...]" Sys.argv.(0) in
  let dirs = ref [] in
  Arg.parse
    [("--symbols", Arg.Rest_all gen_symbols, "generate rules for .symbols")]
    (fun s -> dirs := s :: !dirs)
    usage ;
  gen_dune_default !dirs
