open Sexplib

(* TODO: split by subdir or library/exe and parallelize that way? *)

let is_empty_file path =
  let st = Unix.LargeFile.stat (Fpath.to_string path) in
  Int64.compare st.Unix.LargeFile.st_size 0L = 0

(* Applicative syntactic sugar for {!module:Option}. *)

let ( let+ ) t f = Option.map f t

(** [cwd] is the absolute path of the current working directory *)
let cwd = Fpath.(Sys.getcwd () |> v)

let debug = ref false

(** [project_root] is the relative path from the current directory to the project root.

  The project root is the directory that contains `dune-project`.
*)
let project_root =
  let rec find_dune_project path =
    if Fpath.is_root Fpath.(cwd // path |> normalize) then
      (* avoid infinite ../../../ tests *)
      failwith
        "Cannot find `dune-project` in any of the parent directories. This \
         command must be run inside a dune project."
    else if Sys.file_exists Fpath.(path / "dune-project" |> to_string) then (
      if !debug then
        Format.eprintf "Found dune-project at %a@.CWD is %a@." Fpath.pp path
          Fpath.pp cwd ;
      path
    ) else
      find_dune_project (Fpath.parent path)
  in
  find_dune_project Fpath.(v ".")

(** [project_path rule_path] returns [rule_path] relative to the current directory. *)
let project_path path = Fpath.(project_root // v path |> normalize)

let project_path' path = Fpath.(project_root // path |> normalize)

let build_path path = Fpath.(project_root / "_build" // v path |> normalize)

let cwd_from_root =
  let root_abs = Fpath.(cwd // project_root) in
  Fpath.relativize ~root:root_abs cwd |> Option.get |> Fpath.normalize

open Dune_rules

let parse_rules_from_stdin () =
  stdin |> Sexp.input_rev_sexps |> List.filter_map Rule.of_sexp

let foreach set f =
  set
  |> Fpath.Set.to_seq
  |> Seq.map f
  |> Seq.fold_left Dune_stanzas.merge Dune_stanzas.empty

let read_lines file =
  let ch = open_in (Fpath.to_string file) in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) @@ fun () ->
  List.of_seq
    (ch
    |> Seq.unfold @@ fun ch ->
       try Some (input_line ch, ch) with End_of_file -> None
    )

let read_symbols file =
  file
  |> read_lines
  |> List.rev_map @@ fun line ->
     match String.split_on_char ' ' line with
     | file :: symbol :: _ ->
         let file = String.sub file 0 (String.length file - 1) in
         (Fpath.v file, symbol)
     | _ ->
         invalid_arg ("Cannot parse symbols line: " ^ line)

(* TODO

   let do_group_symbols files =
     let symbols_of_files=
       Fpath.Set.fold
         (fun file map ->
           let symbols = read_symbols file in
           symbols
           |> List.fold_left
                (fun map (file, symbol) ->
                  Fpath.Map.update file
                    (fun old ->
                      let old = Option.value ~default:SymbolSet.empty old in
                      Some (SymbolSet.add symbol old)
                    )
                    map
                )
                map
         )
         files Fpath.Map.empty
     in
     let uf = UF.new_store () in
     let ufiles = Stack.create () in
     let (_ : _ SymbolMap.t) =
       Fpath.Map.fold
         (fun file symbols map ->
           let file = UF.make uf (Fpath.Set.singleton file) in
           Stack.push file ufiles ;
           SymbolSet.fold
             (fun symbol ->
               SymbolMap.update symbol (fun uref_opt ->
                   let uref = Option.value ~default:file uref_opt in
                   Some (UF.merge uf Fpath.Set.union uref file)
               )
             )
             symbols map
         )
         symbols_of_files SymbolMap.empty
     in
     ufiles
     |> Stack.iter @@ fun ufile ->
        if UF.is_representative uf ufile then
          let files = UF.get uf ufile in
          let main = Fpath.Set.choose files |> Option.get in
          Rule.
            [
              rule
                [
                  deps_glob_paths "*.ml.h"
                    (files
                    |> Fpath.Set.map (fun file ->
                           match Fpath.get_ext file with
                           | ".ml" ->
                               Fpath.set_ext ~multi:true ".cmt.model.c" file
                           | ".o" ->
                               Fpath.set_ext ~multi:true ".c" file
                           | _ ->
                               assert false
                       )
                    |> Fpath.Set.elements
                    )
                ; target (Fpath.set_ext "sarif" main)
                  (* TODO: compile_commands.json *)
                ; alias "lintcstubs"
                ; action
                  @@ run
                       [
                         "%{bin:lintcstubs}"
                       ; "--conf"
                       ; "lintcstubs.json"
                       ; "-o"
                       ; "%{target}"
                       ; "-I"
                       ; "%{ocaml_where}"
                       ; "%{deps}"
                       ]
                ]
            ]
          |> List.iter (Format.printf "%a@," @@ Sexp.pp_hum_indent 2)

   let () =
     let usage_msg =
       Printf.sprintf "%s [--root root] [--foreach glob FILE...]"
         Sys.executable_name
     in
*)

module UF = UnionFind.Make (UnionFind.StoreMap)
module SymbolSet = Set.Make (String)
module SymbolMap = Map.Make (String)

let symbols_of_files symbol_files =
  Fpath.Set.fold
    (fun file map ->
      let symbols = read_symbols file in
      symbols
      |> List.fold_left
           (fun map (_, symbol) ->
             Fpath.Map.update file
               (fun old ->
                 let old = Option.value ~default:SymbolSet.empty old in
                 Some (SymbolSet.add symbol old)
               )
               map
           )
           map
    )
    symbol_files Fpath.Map.empty

let group_symbols symbol_files =
  if !debug then
    Format.eprintf "Grouping %a@." Fpath.Set.dump symbol_files ;
  let symbols = symbols_of_files symbol_files in
  let uf = UF.new_store () in
  let ufiles = Stack.create () in
  let (_ : _ SymbolMap.t) =
    Fpath.Map.fold
      (fun file symbols map ->
        let file = UF.make uf (Fpath.Set.singleton file) in
        Stack.push file ufiles ;
        SymbolSet.fold
          (fun symbol ->
            SymbolMap.update symbol (fun uref_opt ->
                let uref = Option.value ~default:file uref_opt in
                Some (UF.merge uf Fpath.Set.union uref file)
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
       let files = UF.get uf ufile in
       let main = Fpath.Set.choose files |> Option.get in
       Some (main, files)
     else
       None

let update_rules ~filter self_sexp deps =
  let to_cmtfile' cmt_file =
    let cmt_dir, cmt_base = Fpath.split_base cmt_file in
    Fpath.((cmt_dir |> parent |> parent) // cmt_base)
  in
  if !debug then Format.eprintf "update_rules: %a@." Fpath.Set.dump deps ;
  let ml_files = Fpath.Set.filter (Fpath.has_ext ".ml") deps
  (* TODO: too many, including from ML, we really want just the ones from .c! *)
  and o_files =
    Fpath.Set.filter
      (fun p -> Fpath.has_ext ".o" p && not (Fpath.has_ext ".model.o" p))
      deps
  and cmt_files = Fpath.Set.filter (Fpath.has_ext ".cmt") deps
  and symbols = Fpath.Set.filter (Fpath.has_ext ".symbols") deps in

  let symbol_groups =
    if filter then
      group_symbols symbols
    else
      Seq.return
        ( Fpath.v "lintcstubs"
        , Fpath.Set.union ml_files o_files
          |> Fpath.Set.map (Fpath.add_ext ".symbols")
        )
  in

  let empty_symbols = Fpath.Set.filter is_empty_file symbols in
  let ml_files =
    Fpath.Set.diff ml_files (Fpath.Set.map Fpath.rem_ext empty_symbols)
  in
  let symbols = Fpath.Set.diff symbols empty_symbols in

  let cmt_files' = Fpath.Set.map to_cmtfile' cmt_files in

  let neg_cmt =
    Fpath.Set.map (Fpath.set_ext ~multi:true ".cmt") empty_symbols
  in
  let cmt_files' = Fpath.Set.diff cmt_files' neg_cmt in

  if !debug then
    Format.eprintf
      "empty_symbols: %a@,\
       ml_files: %a@,\
       symbols: %a@,\
       neg_cmt: %a@,\
       cmt_files: %a@." Fpath.Set.dump empty_symbols Fpath.Set.dump ml_files
      Fpath.Set.dump symbols Fpath.Set.dump neg_cmt Fpath.Set.dump cmt_files' ;

  let model_files = Fpath.Set.map (Fpath.add_ext ".model.c") cmt_files'
  and h_files = Fpath.Set.map (Fpath.set_ext ".ml.h") cmt_files' in
  let all_deps =
    List.fold_left Fpath.Set.union Fpath.Set.empty
      [
        ml_files
      ; o_files
      ; cmt_files
      ; symbols
      ; Fpath.Set.map (Fpath.add_ext ".symbols") ml_files
      ; model_files
      ; h_files
      ; Fpath.Set.map (Fpath.add_ext ".symbols") o_files
      ]
  in
  let self =
    let open Dune_stanzas in
    rule ~mode:`promote (target self_sexp)
      (all_deps |> Fpath.Set.to_seq |> Seq.map dep_file |> List.of_seq)
    @@ with_stdout_to_target
    @@ run "%{bin:lintcstubs_gen_rules}" ["--update"; "%{deps}"]
  in
  let seq =
    symbol_groups
    |> Seq.map @@ fun (main, symbols) ->
       if !debug then
         Format.eprintf "main: %a, symbols: %a@." Fpath.pp main Fpath.Set.dump
           symbols ;
       let filter_ext ext files =
         if !debug then
           Format.eprintf "Filtering: %a, symbols: %a@." Fpath.Set.dump files
             Fpath.Set.dump symbols ;
         Fpath.Set.inter files
           (Fpath.Set.map (Fpath.set_ext ~multi:true ext) symbols)
       in

       let filter_cmt files =
         files
         |> Fpath.Set.filter @@ fun file ->
            let dir, base = Fpath.split_base file in
            let base = base |> Fpath.rem_ext ~multi:true |> Fpath.to_string in
            let last_base =
              String.split_on_char '_' base
              |> List.rev
              |> List.hd
              |> String.lowercase_ascii
            in
            let lookup =
              Fpath.(dir // (set_ext ".ml.symbols" @@ v last_base))
            in
            if !debug then
              Format.eprintf "lookup: %a@." Fpath.pp lookup ;
            Fpath.Set.mem lookup symbols
       in

       let cmt_files' = filter_cmt cmt_files'
       and o_files = filter_ext ".o" o_files
       and ml_files = filter_ext ".ml" ml_files in
       if !debug then
         Format.eprintf "cmt_files': %a@." Fpath.Set.dump cmt_files' ;

       let model_files = Fpath.Set.map (Fpath.add_ext ".model.c") cmt_files'
       and h_files = Fpath.Set.map (Fpath.set_ext ".ml.h") cmt_files' in
       let c_files = Fpath.Set.map (Fpath.set_ext ".c") o_files in
       let open Dune_stanzas in
       let primitives_h = Fpath.set_ext ~multi:true ".primitives.h" main in
       let ml_symbols_rules =
         foreach ml_files @@ fun ml_file ->
         rule (target [Fpath.add_ext ".symbols" ml_file]) [dep_file ml_file]
         @@ with_stdout_to_target
         @@ run "%{bin:lintcstubs_primitives_of_ml}" ["%{deps}"]
       and c_symbols_rules =
         foreach o_files @@ fun o_file ->
         rule (target [Fpath.add_ext ".symbols" o_file]) [dep_file o_file]
         @@ with_stdout_to_target
         @@ run "nm" ["-A"; "-g"; "-P"; "%{deps}"]
       and cmt_rules =
         foreach cmt_files @@ fun cmt_file ->
         (* TODO: assumes there are exactly 2 dirs here, use a ml to cmt map instead *)
         let cmt_file' = to_cmtfile' cmt_file in
         (* FIXME: better tracking between ml -> cmt dep *)
         if not (Fpath.Set.mem cmt_file' cmt_files') then
           empty
         else
           merge
             (rule
                (target [Fpath.add_ext ".model.c" cmt_file'])
                [dep_file cmt_file]
             @@ with_stdout_to_target
             @@ progn
                  [
                    run "%{bin:lintcstubs_genwrap}" ["%{deps}"]
                  ; run "%{bin:lintcstubs_genmain}" ["%{deps}"]
                  ]
             )
             (rule
                (target [Fpath.set_ext ".ml.h" cmt_file'])
                [dep_file cmt_file]
             @@ with_stdout_to_target
             @@ run "%{bin:lintcstubs_arity_cmt}" ["%{deps}"]
             )
       and lint_rules c_files =
         List.fold_left merge empty
           [
             rule (target [primitives_h])
               (h_files |> Fpath.Set.to_seq |> Seq.map dep_file |> List.of_seq)
             @@ with_stdout_to_target
             @@ run "cat" ["%{deps}"]
           ; rule ~alias:"runtest"
               (target [Fpath.set_ext ~multi:true ".sarif" main])
               (* TODO: flags *)
               (Fpath.Set.union c_files h_files
               |> Fpath.Set.add primitives_h
               |> Fpath.Set.to_seq
               |> Seq.map dep_file
               |> List.of_seq
               )
             @@ run "%{bin:lintcstubs}"
                  [
                    "-o"
                  ; "%{target}"
                  ; "-I"
                  ; "."
                  ; "-I"
                  ; "%{ocaml_where}"
                  ; "--conf"
                  ; "lintcstubs.json"
                  ; "%{deps}"
                  ]
           ]
       in
       [
         ml_symbols_rules
       ; c_symbols_rules
       ; cmt_rules
       ; lint_rules (Fpath.Set.union c_files model_files)
       ]
       |> List.fold_left merge empty
  in
  Seq.fold_left Dune_stanzas.merge self seq

let is_rooted_cwd = Fpath.is_rooted ~root:(Fpath.v ".")

let generate_rules self_sexp =
  let rules = parse_rules_from_stdin () in
  let source_map = Rule.source_map_of rules and deps = Rule.deps_of rules in
  if !debug then Format.eprintf "source map: %a@." Rule.pp_source_map source_map ;
  let deps =
    deps
    |> Fpath.Set.map @@ fun dep ->
       Fpath.Map.find_opt dep source_map |> Option.value ~default:dep
  in
  let cmt_map = Rule.cmt_map_of rules in
  let cmt_files =
    Fpath.Set.of_seq
      (cmt_map
      |> Fpath.Map.to_seq
      |> Seq.map @@ fun (ml, cmt) ->
         let ml =
           Fpath.Map.find_opt ml source_map |> Option.value ~default:ml
         in
         Fpath.(parent ml // cmt |> normalize)
      )
  in
  let deps = Fpath.Set.union deps cmt_files in
  update_rules ~filter:false self_sexp (deps |> Fpath.Set.filter is_rooted_cwd)

let parse_cmdline () =
  (* When called without arguments it expects 'dune rules' on stdin,
     and produce dune rules on output, that will self-update on @lintcstubsgen and @runtest.

     When called with arguments it filters its arguments (e.g. eliminated empty symbol files),
     and produces an update dune rule on output. Stdin is not used in this case.
  *)
  let usage_msg =
    String.concat "\n"
      [
        Printf.sprintf "dune rules -r | %s [-d]" Sys.executable_name
      ; Printf.sprintf
          "%s [-d] --update [FILE.ml.symbols... FILE.c.symbols... FILE.cmt...]"
          Sys.executable_name
      ]
  in
  (* use Arg for parsing to minimize dependencies *)
  let files = ref Fpath.Set.empty in
  let set_files lst = files := Fpath.Set.of_list (Conv.list_map Fpath.v lst) in
  Arg.parse
    [
      ("-d", Arg.Set debug, "enable debug messages")
    ; ("--update", Arg.Rest_all set_files, "internal call to update dune rules")
    ]
    (fun _ ->
      invalid_arg
        "Files can only be specified as an argument when --update is used"
    )
    usage_msg ;
  !files

let () =
  let files = parse_cmdline () in
  let self_sexp = Fpath.v "lintcstubs.sexp" in
  let rules =
    if Fpath.Set.is_empty files then
      generate_rules [self_sexp]
    else
      update_rules ~filter:true [self_sexp] files
  in
  rules
  |> Dune_stanzas.sexp_list_of_t
  |> List.iter (Format.printf "%a@." @@ Sexp.pp_hum_indent 2)
