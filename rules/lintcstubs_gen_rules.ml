open Sexplib

type mode = Initial | Foreach of Fpath.Set.t

let root = ref None

module DuneRule = struct
  open Sexp

  let subdir dir rules =
    if Fpath.is_current_dir dir then
      rules
    else
      [List (Atom "subdir" :: Atom (Fpath.to_string dir) :: rules)]

  let atom a = Atom a

  let fpath p = p |> Fpath.to_string |> atom

  let progn sexps = List (Atom "progn" :: sexps)

  let run cmd = List (Atom "run" :: Conv.list_map atom cmd)

  let include_ path = List [Atom "include"; fpath path]

  let target path = List [Atom "target"; fpath path]

  let action sexp = List [Atom "action"; sexp]

  let with_stdout_to_target rule =
    List [Atom "with-stdout-to"; Atom "%{target}"; rule]

  let pipe_stdout pipe1 pipe2 = List [Atom "pipe-stdout"; pipe1; pipe2]

  let echo lst = List (Atom "echo" :: Conv.list_map atom lst)

  let rule sexps = List (Atom "rule" :: sexps)

  let subdir_target dst sexps =
    let dir, file = Fpath.split_base dst in
    subdir dir [rule (target file :: sexps)]

  let deps_glob glob =
    List [Atom "deps"; List [Atom "glob_files_rec"; Atom glob]]

  let deps paths = List (Atom "deps" :: Conv.list_map fpath paths)

  let deps_glob_paths glob paths =
    List
      (Atom "deps"
      :: List [Atom "glob_files_rec"; Atom glob]
      :: Conv.list_map fpath paths
      )

  let deps_include path = List [Atom "deps"; List [Atom "include"; fpath path]]

  let alias x = List [Atom "alias"; Atom x]

  let _alias_deps x deps =
    List
      [Atom "alias"; List [Atom "name"; Atom x]; List [Atom "deps"; Atom deps]]

  let _diff path1 path2 = List [Atom "diff"; fpath path1; fpath path2]
end

let is_empty_file path =
  let st = Unix.LargeFile.stat (Fpath.to_string path) in
  Int64.compare st.Unix.LargeFile.st_size 0L = 0

(* Applicative syntactic sugar for {!module:Option}. *)

let ( let+ ) t f = Option.map f t

let ( and+ ) a b = match (a, b) with Some a, Some b -> Some (a, b) | _ -> None

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

let build_path path = Fpath.(project_root / "_build" // v path |> normalize)

let cwd_from_root =
  let root_abs = Fpath.(cwd // project_root) in
  Fpath.relativize ~root:root_abs cwd |> Option.get |> Fpath.normalize
  

module File = struct
  type t = Fpath.t

  (** [of_sexp sexp] parses a file dependency.

     [(File (In_source_tree example/dune/foostubs.c))]
   *)
  let of_sexp =
    let open Sexp in
    function
    | List [Atom "File"; List [Atom _; Atom path]] ->
        Some (project_path path)
    | file ->
        if !debug then Format.eprintf "Could not parse: %a@." Sexp.pp_hum file ;
        None
end


module Target = struct
  type t = Fpath.t

  let fpath_of_sexp s = s |> Conv.string_of_sexp |> build_path

  (** [of_sexp sexp] parses a Target.
  
     [(files (file1.c))]
     [(In_build_dir file1.c)]
  *)
  let of_sexp =
    let open Sexp in
    function
    | List [Atom "files"; List files] ->
        Conv.list_of_sexp fpath_of_sexp (List files)
    | List (Atom "In_build_dir" :: files) ->
        Conv.list_of_sexp fpath_of_sexp (List files)
    | sexp ->
        if !debug then
          Format.eprintf "Could not parse target: %a@." Sexp.pp_hum sexp ;
        []
end

module Action = struct
  type t =
    | ChdirRun of {dir: Fpath.t; run: string list}
    | Copy of {source: Fpath.t; target: Fpath.t}

  (** [of_sexp sexp] parses an action.
  
   [(chdir  _build/default/example/dune (run /usr/lib64/ccache/gcc ...))]
   [(copy example/Makefile/foostubs.c _build/default/example/Makefile/foostubs.c)]
  *)
  let of_sexp_list =
    let open Sexp in
    function
    | [Atom "chdir"; Atom dir; List (Atom "run" :: run)] ->
        Some
          (ChdirRun
             {
               dir= Fpath.v dir
             ; run= Conv.list_of_sexp Conv.string_of_sexp (List run)
             }
          )
    | [Atom "copy"; Atom source; Atom target] ->
        Fpath.(relativize ~root:cwd_from_root (v source)) |> Option.map @@ fun source ->
          (Copy
             {
        (* TODO: bugfix dune-compiledb too !*)
               source= Fpath.normalize source
             ; target= project_path target
             }
          )
    | sexp ->
        if !debug then
          Format.eprintf "@[<v2>Unknown action:@,%a@]@." Sexp.pp_hum (List sexp) ;
        None
end

module Rule = struct
  type t = {deps: File.t list; targets: Target.t list; action: Action.t}

  (** [of_sexp sexp] parses a dune rule.

    [((deps (...)) (targets (...)) (action (...)))]
    [((deps (...)) (targets (...)) (context ...) (action (...)))]

    See also [dune rules --help]
  *)
  let of_sexp =
    let open Sexp in
    function
    | ( List
          [
            List [Atom "deps"; List deps]
          ; List [Atom "targets"; List targets]
          ; List [Atom "action"; List action]
          ]
      | List
          [
            List [Atom "deps"; List deps]
          ; List [Atom "targets"; List targets]
          ; List (Atom "context" :: _)
          ; List [Atom "action"; List action]
          ] ) as sexp ->
        if !debug then Format.eprintf "@[<v2>Parsing rule:@,%a@]@." pp_hum sexp ;

        let deps = List.filter_map File.of_sexp deps in
        if !debug then
          Format.eprintf "@[<v2>Dependencies:@,%a@]@."
            (Format.pp_print_list Fpath.pp)
            deps ;

        let targets = List.rev_map Target.of_sexp targets |> List.concat in
        if !debug then
          Format.eprintf "@[<v2>Targets:@,%a@]@."
            (Format.pp_print_list Fpath.pp)
            targets ;

        let+ action = Action.of_sexp_list action in
        {deps; targets; action}
    | sexp ->
        if !debug then
          Format.eprintf "@[<v2>Could not parse rule:@,%a@]@." pp_hum sexp ;
        None

  let copy_rule = function
    | {action= Action.Copy {source; target}; _} ->
        Some (target, source)
    | _ ->
        None

  (** [source_map_of rules] builds a map from targets to sources by looking at [copy] actions. *)
  let source_map_of rules =
    rules |> List.to_seq |> Seq.filter_map copy_rule |> Fpath.Map.of_seq

  let copy_rule_mlc = function
    (* TODO: won't see generated .ml and .c? *)
    | {action= Action.Copy {source; _}; _}
      when Fpath.has_ext ".ml" source || Fpath.has_ext ".c" source ->
        Some source
    | _ ->
        None

  let dep_of r = r.deps |> List.to_seq |> Seq.map @@ fun dep ->
    let dep' = Fpath.(cwd // dep) in
    let dep'' = Fpath.normalize dep' in
    let dep'''= Fpath.relativize ~root:cwd dep'' |> Option.get  in
    if !debug then
      Format.eprintf "dep: %a -> %a -> %a@," Fpath.pp dep' Fpath.pp dep'' Fpath.pp dep''';
    dep'''

  let deps_of rules =
    rules |> List.to_seq |> Seq.concat_map dep_of |> Fpath.Set.of_seq

  let cmt_map_of_entry = function
    | {action = Action.ChdirRun _; targets; deps} ->
      let ml_file = List.find_opt (Fpath.has_ext ".ml") deps
      and cmt_file = List.find_opt (Fpath.has_ext ".cmt") targets in
      (match ml_file, cmt_file with
      | Some ml, Some cmt -> 
        let root = Fpath.parent ml in
        let res = Fpath.relativize ~root cmt |> Option.get |> Fpath.normalize in
        if !debug then Format.eprintf "cmt: %a, root: %a -> %a@." Fpath.pp cmt Fpath.pp root Fpath.pp res;
        Some (ml, res)
      | _ -> None
      )
    | _ -> None

  let cmt_map_of rules =
    rules |> List.to_seq |> Seq.filter_map cmt_map_of_entry |> Fpath.Map.of_seq
    
  let pp_source_map =
    Fpath.Map.pp @@ fun ppf (dst, src) ->
    Format.fprintf ppf "%a <- %a" Fpath.pp dst Fpath.pp src

end

let parse_rules_from_stdin () =
  stdin |> Sexp.input_rev_sexps |> List.filter_map Rule.of_sexp

let foreach set f =
  set
  |> Fpath.Set.to_seq
  |> Seq.concat_map @@ fun file -> file |> f |> List.to_seq

let update_rules ~filter self_sexp deps =
  let to_cmtfile' cmt_file =
    let cmt_dir, cmt_base = Fpath.split_base cmt_file in
     Fpath.((cmt_dir |> parent |> parent) // cmt_base)
   in
  if !debug then Format.eprintf "update_rules: %a@." Fpath.Set.dump deps; 
  let ml_files = Fpath.Set.filter (Fpath.has_ext ".ml") deps
  and o_files = Fpath.Set.filter (fun p -> Fpath.has_ext ".o" p && not (Fpath.has_ext ".model.o" p)) deps
  and cmt_files = Fpath.Set.filter (Fpath.has_ext ".cmt") deps
  and symbols =
    Fpath.Set.filter (Fpath.has_ext ".symbols") deps
  in

  let empty_symbols = Fpath.Set.filter is_empty_file symbols in
  let ml_files = Fpath.Set.diff ml_files (Fpath.Set.map Fpath.rem_ext empty_symbols) in
  let symbols = Fpath.Set.diff symbols empty_symbols in

  let cmt_files' = Fpath.Set.map to_cmtfile' cmt_files in

  let neg_cmt = Fpath.Set.map (Fpath.set_ext ~multi:true ".cmt") empty_symbols in
  let cmt_files' = Fpath.Set.diff cmt_files' neg_cmt in

  if !debug then Format.eprintf "empty_symbols: %a@,ml_files: %a@,symbols: %a@,neg_cmt: %a@,cmt_files: %a@." Fpath.Set.dump empty_symbols
    Fpath.Set.dump ml_files Fpath.Set.dump symbols Fpath.Set.dump neg_cmt Fpath.Set.dump cmt_files';

  let model_files = Fpath.Set.map (Fpath.add_ext ".model.c") cmt_files'
  and h_files = Fpath.Set.map (Fpath.set_ext ".ml.h") cmt_files'
  in
  let c_files = Fpath.Set.map (Fpath.set_ext ".c") o_files in
  let all_deps =
    List.fold_left Fpath.Set.union Fpath.Set.empty
      [ml_files; o_files; cmt_files; symbols
      ; Fpath.Set.map (Fpath.add_ext ".symbols") ml_files
      ; model_files
      ; h_files
      ; Fpath.Set.map (Fpath.add_ext ".symbols") o_files
    ]
  in
  let open DuneRule in
  let ml_symbols_rules =
    foreach ml_files @@ fun ml_file ->
    subdir_target
      (Fpath.add_ext ".symbols" ml_file)
      [
        deps [Fpath.base ml_file]
      ; action
        @@ with_stdout_to_target
        @@ run ["%{bin:lintcstubs_primitives_of_ml}"; "%{deps}"]
      ]
  and c_symbols_rules =
    foreach o_files @@ fun o_file ->
    subdir_target
      (Fpath.add_ext ".symbols" o_file)
      [
        deps [o_file |> Fpath.base]
      ; action
        @@ with_stdout_to_target
        @@ run ["nm"; "-A"; "-g"; "-P"; "%{deps}"]
      ]
  and cmt_rules =
    foreach cmt_files @@ fun cmt_file ->
    (* TODO: assumes there are exactly 2 dirs here, use a ml to cmt map instead *)
    let cmt_file' = to_cmtfile' cmt_file in
    (* FIXME: better tracking between ml -> cmt dep *)
    if not (Fpath.Set.mem cmt_file' cmt_files') then []
    else
    let cmt_file = Fpath.relativize ~root:Fpath.(parent cmt_file') cmt_file |> Option.get in
    List.concat
      [
       (* TODO: subdir_target should make the adjustments *)
        subdir_target
          (Fpath.add_ext ".model.c" cmt_file')
          [
            deps [cmt_file]
          ; action
            @@ with_stdout_to_target
            @@ progn
                 [
                   run ["%{bin:lintcstubs_genwrap}"; "%{deps}"]
                 ; run ["%{bin:lintcstubs_genmain}"; "%{deps}"]
                 ]
          ]
      ; subdir_target
          (Fpath.set_ext ".ml.h" cmt_file')
          [
            deps [cmt_file]
          ; action
            @@ with_stdout_to_target
            @@ run ["%{bin:lintcstubs_arity_cmt}"; "%{deps}"]
          ]
      ]
  (* TODO: group based on symbols, fall back to everything in one when no symbols *)
  and lint_rules c_files =
    Seq.return
    @@ rule
         [
           target (Fpath.v "lintcstubs.sarif") (* TODO: flags *)
         ; deps (Fpath.Set.elements (Fpath.Set.union c_files h_files))
         ; action
           @@ run ["%{bin:lintcstubs}"; "-o"; "%{target}"; "-I"; "%{ocaml_where}"; "--conf"; "lintcstubs.json"; "%{deps}"]
         ]
  and self =
    Seq.return
    @@ rule
         [
           target self_sexp
         ; deps (Fpath.Set.elements all_deps)
         ; List [Atom "mode"; Atom "promote"]
         ; action
           @@ with_stdout_to_target
           @@ run ["%{bin:lintcstubs_gen_rules}"; "--update"; "%{deps}"]
         ]
  in
  [ml_symbols_rules; c_symbols_rules; cmt_rules;
   lint_rules (Fpath.Set.union c_files model_files);
   self]
  |> List.to_seq
  |> Seq.concat
  |> List.of_seq

let is_rooted_cwd = Fpath.is_rooted ~root:(Fpath.v ".")  

let generate_rules self_sexp =
  let rules = parse_rules_from_stdin () in
  let source_map = Rule.source_map_of rules
  and deps = Rule.deps_of rules in
  if !debug then Format.eprintf "source map: %a@." Rule.pp_source_map source_map;
  let deps = deps |> Fpath.Set.map @@ fun dep ->
    Fpath.Map.find_opt dep source_map |> Option.value ~default:dep in
  let cmt_map = Rule.cmt_map_of rules in
  let cmt_files =
    Fpath.Set.of_seq (
      cmt_map |> Fpath.Map.to_seq |> Seq.map @@ fun (ml, cmt) ->
      let ml = Fpath.Map.find_opt ml source_map |> Option.value ~default:ml in
      Fpath.((parent ml) // cmt |> normalize))
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
    Printf.sprintf
      "dune rules -r | %s [-d]"
      Sys.executable_name
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
      generate_rules self_sexp
    else
      update_rules ~filter:true self_sexp files
  in
  rules |> List.iter (Format.printf "%a@." @@ Sexp.pp_hum_indent 2)
