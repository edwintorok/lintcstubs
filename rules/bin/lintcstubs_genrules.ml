(** See https://dune.readthedocs.io/en/stable/howto/rule-generation.html

   Given a list of toplevel directories generate the initial dune rules for [lintcstubs].

   We need the toplevel directories on the command-line, to avoid cycling dependencies in the rule generation
   (we cannot discover the toplevel dirs from inside a dune rule).
*)

(** {1 Generate toplevel dune.inc rules} *)

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
            (dynamic-run %%{bin:lintcstubs_gen_find_targets} --output %%{target} --ext c --ext ml --ext cmt --paths %%{deps})
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
          (with-stdout-to dune.1.inc
            (run %%{bin:lintcstubs_genrules} --symbols %%{deps})
          )
        )
      )
    )

    (subdir lintcstubs_run
      (dynamic_include ../lintcstubs_generate/dune.1.inc)      
    )

    (subdir lintcstubs_generate2
      (rule
        (deps (glob_files ../lintcstubs_run/*.symbols))
        (action
          (with-stdout-to dune.2.inc
           (run %%{bin:lintcstubs_genrules} --group-symbols %%{deps})
          )
        )
      )
    )
    
    (subdir lintcstubs_run2
      (dynamic_include ../lintcstubs_generate2/dune.2.inc)
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

(** {1 Generating rules for .symbols files} *)

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
let symbols_name filename = filename |> Digest.string |> Digest.to_hex

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

(** {1 Generate rules for groups of C and ML files} *)
module PathSet = Set.Make (String)

let dune_gen_ml_h_wrap cmt_file =
  let noext = Filename.chop_extension cmt_file in
  Printf.sprintf
    {|
    (rule
      (with-stdout-to %s.ml.h
        (action (run %%{bin:lintcstubs_arity_cmt} %%{dep:%s}))
      )
    )

    (rule
      (with-stdout-to %s.wrap.c
        (action
          (run %%{bin:lintcstubs_genwrap} %%{dep:%s})
        )
      )
    )

    (rule
      (deps (glob_files *.wrap.h))
      (with-stdout-to primitives.h
        (action (cat %%{deps}))
      )
    )
  |}
    noext noext cmt_file cmt_file

let dune_gen_ml_main c_files cmt_files =
  let deps_cmt = cmt_files |> PathSet.elements |> String.concat " "
  and representative = PathSet.choose cmt_files
  and deps_c =
    c_files
    |> PathSet.map (fun p -> p ^ ".wrap.c")
    |> PathSet.elements
    |> String.concat " "
  in
  Printf.sprintf
    {|
    (rule
      (deps %s)
      (with-stdout-to %s.model.c
        (action (run %%{bin:lintcstubs_genmain} %%{deps}))
      )
    )

    (rule
      (aliases runtest sarif)
      (deps primitives.h %s.model.c %s)
      (target %s.sarif)
      (action (run %%{bin:lintcstubs} -o %%{target} -I . -I %%{ocaml_where} --conf lintcstubs.json %%{deps}))
    )
  |}
    deps_cmt representative representative deps_c representative

let dune_generate_grouped paths =
  prerr_endline ("HERE: " ^ String.concat " " (PathSet.elements paths)) ;
  let c_files = PathSet.filter (fun p -> Filename.extension p = ".c") paths
  and ml_files = PathSet.filter (fun p -> Filename.extension p = ".ml") paths
  and cmt_files =
    PathSet.filter (fun p -> Filename.extension p = ".cmt") paths
  in
  exit 0 ()
(*ml_files |> List.iter dune_gen_ml_h TODO*)

(** {1 Group symbols}

  Find pairs of undefined ([U]) and defined ([T]) symbols and generate rules
  to call the static analyzer on these files.

  This maximizes parallelization opportunities: we can analyze files that have independent symbols in parallel. 
  We cannot do that when symbols are defined in different files: all those need to be analyzed together.

  We assume that symbols are globally unique in a project, duplicate symbols will result in errors
  (even if they'be linked into different executables)
*)

module UF = UnionFind.Make (UnionFind.StoreMap)
module SymbolSet = Set.Make (String)
module SymbolMap = Map.Make (String)
module PathMap = Map.Make (String)

let read_symbols file =
  In_channel.with_open_text file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.rev_map @@ fun line ->
     Scanf.sscanf line "%s@: %s" @@ fun file symbol -> (file, symbol)

let symbols_of_files symbol_files =
  PathSet.fold
    (fun symfile map ->
      let symbols = read_symbols symfile in
      symbols
      |> List.fold_left
           (fun map (file, symbol) ->
             PathMap.update file
               (fun old ->
                 let old = Option.value ~default:SymbolSet.empty old in
                 Some (SymbolSet.add symbol old)
               )
               map
           )
           map
    )
    symbol_files PathMap.empty

let group_symbols symbol_files =
  let symbols = symbols_of_files symbol_files in
  let uf = UF.new_store () in
  let ufiles = Stack.create () in
  let (_ : _ SymbolMap.t) =
    PathMap.fold
      (fun file symbols map ->
        let file = UF.make uf (PathSet.singleton file) in
        Stack.push file ufiles ;
        SymbolSet.fold
          (fun symbol ->
            SymbolMap.update symbol (fun uref_opt ->
                let uref = Option.value ~default:file uref_opt in
                Some (UF.merge uf PathSet.union uref file)
            )
          )
          symbols map
      )
      symbols SymbolMap.empty
  in
  ufiles
  |> Stack.to_seq
  |> Seq.filter_map @@ fun ufile ->
     if UF.is_representative uf ufile then
       Some (UF.get uf ufile)
     else
       None

let dune_group_symbols symbol_files =
  symbol_files
  |> PathSet.of_list
  |> group_symbols
  |> Seq.iter dune_generate_grouped ;
  exit 0

let () =
  let usage = Printf.sprintf "%s [--symbols SOURCEFILE ...]" Sys.argv.(0) in
  let dirs = ref [] in
  Arg.parse
    [
      ("--symbols", Arg.Rest_all gen_symbols, "generate rules for .symbols")
    ; ( "--group-symbols"
      , Arg.Rest_all dune_group_symbols
      , "generate rules for groups of undefined/defined symbols"
      )
    ; ( "--grouped"
      , Arg.Rest_all (fun l -> l |> PathSet.of_list |> dune_generate_grouped)
      , "generate rules for groups of C and ML files (for debugging)"
      )
    ]
    (fun s -> dirs := s :: !dirs)
    usage ;
  gen_dune_default !dirs
