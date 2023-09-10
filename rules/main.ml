open Sexplib.Sexp

let rec apply_template var subst = function
  | Atom _ as a ->
      a
  | List [Atom x] when String.equal x var ->
      List (Atom x :: List.map (fun path -> Atom (Fpath.to_string path)) subst)
  | List l ->
      List (Sexplib.Conv.list_map (apply_template var subst) l)

let rec apply_template' var subst = function
  | Atom x when String.equal x var ->
      Atom (Fpath.to_string subst)
  | Atom _ as a ->
      a
  | List l ->
      List (Sexplib.Conv.list_map (apply_template' var subst) l)

let cmt_rule =
  Sexplib.Sexp.of_string
    {|
    (rule
      (enabled_if %{bin-available:lintcstubs_genmain})
      (targets %{model} %{primitives})
      (deps (:cmt) %{bin:lintcstubs_arity_cmt} %{bin:lintcstubs_genwrap} %{bin:lintcstubs_genmain})
      (action
       (progn
        (with-stdout-to %{primitives} (run %{bin:lintcstubs_arity_cmt} %{cmt}))
        (with-stdout-to %{model}
          (progn
             (run %{bin:lintcstubs_genwrap} %{cmt})
             (run %{bin:lintcstubs_genmain} %{cmt})
          )
        )
       )
      )
    )
  |}

(* can be long running, ensure we see something when running dune with '--no-buffer',
    add a 2nd debug target

   depend on (package) to get all files installed, e.g. json conf files
*)
let analyze_rule =
  Sexplib.Sexp.of_string
    {|
    (rule
      (enabled_if %{bin-available:lintcstubs})
      (targets %{log} %{sarif})
      (deps compile_commands.json (:primitives) (:model) %{bin:lintcstubs} (package lintcstubs))
      (action
        (with-stdout-to %{log}
          (run %{bin:lintcstubs} --conf lintcstubs.json -o %{sarif} -I %{ocaml_where} --set dbg.solver-stats-interval 0 compile_commands.json %{model})
        )
      )
    )
    |}

let analyze_alias_rule =
  Sexplib.Sexp.of_string
    {|
    (rule
      (alias analyze)
      (deps (:log))
      (action (diff lintcstubs.out.reference %{deps}))
    )
  |}

let incgen_rule =
  Sexplib.Sexp.of_string_many
    {|
(rule
 (enabled_if (and %{bin-available:lintcstubs-dune-rules} %{bin-available:lintcstubs}))
 (deps
  (:mlfiles
   (glob_files_rec *.ml))
  (:cfiles
   (glob_files_rec *.c)))
 (action
  (with-stdout-to
   dune.analysis.inc.gen
   (run %{bin:lintcstubs-dune-rules} %{mlfiles} %{cfiles}))))

(rule
 (alias runtest)
 (package lintcstubs)
 (enabled_if %{bin-available:lintcstubs})
 (action
  (diff dune.analysis.inc dune.analysis.inc.gen)))

(rule
  (target compile_commands.json)
  (action
    (pipe-stdout
      (run dune rules)
      (run %{bin:dune-compiledb})
    )
  )
)
|}

let group_by_dirs paths =
  Seq.fold_left
    (fun acc path ->
      Fpath.Map.update (Fpath.parent path)
        (fun elements ->
          Some
            (Fpath.Set.add path
            @@ Option.value ~default:Fpath.Set.empty elements
            )
        )
        acc
    )
    Fpath.Map.empty paths

let () =
  let tool_name = Sys.executable_name in
  (* TODO: keep-going flag to ignore errors *)
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    let usage_msg = Printf.sprintf "%s [FILE.ml...]" tool_name in
    Arg.parse [] (fun file -> lst := Fpath.v file :: !lst) usage_msg ;
    !lst
  in
  let filter f x =
    (* TODO: keep-going flag *)
    f x |> Result.get_ok
  in
  let files_seq = List.to_seq files in
  let ml_files_primitives =
    files_seq
    |> Seq.filter Fpath.(has_ext "ml")
    |> Seq.filter (filter Filter_primitives.has_primitives)
  in
  Format.printf "@[<h>; AUTO-GENERATED by %a@]@,; DO NOT EDIT@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
    (Array.to_list Sys.argv) ;
  List.iter (Format.printf "%a@." Sexplib.Sexp.pp_hum) incgen_rule ;
  group_by_dirs ml_files_primitives
  |> Fpath.Map.iter @@ fun dir mls ->
     let to_cmtfile mlfile =
       let dir, file = Fpath.split_base mlfile in
       let filebase = Fpath.rem_ext file in
       let objs_dir = Printf.sprintf ".%s.objs" (Fpath.to_string filebase) in
       Fpath.(dir / objs_dir / "byte" // (filebase + ".cmt"))
     in
     let primitives_file = Fpath.(dir / "primitives.h") in
     let model_files = mls |> Fpath.Set.map (Fpath.set_ext "model.c") in
     let cmt_rules =
       List.of_seq
         (mls
         |> Fpath.Set.to_seq
         |> Seq.map @@ fun mlfile ->
            let cmt_file = to_cmtfile mlfile in
            let model_file = Fpath.set_ext "model.c" mlfile in
            cmt_rule
            |> apply_template ":cmt" [cmt_file]
            |> apply_template' "%{primitives}" primitives_file
            |> apply_template' "%{model}" model_file
         )
     in
     let log_file = Fpath.(dir / "lintcstubs.log") in
     let sarif_file = Fpath.set_ext ".sarif" log_file in
     let analyze_rule =
       analyze_rule
       |> apply_template' "%{log}" log_file
       |> apply_template' "%{sarif}" sarif_file
       |> apply_template ":primitives" [primitives_file]
       |> apply_template ":model" (Fpath.Set.to_seq model_files |> List.of_seq)
     in
     let analyze_alias_rule =
       analyze_alias_rule |> apply_template ":log" [log_file]
     in
     cmt_rules
     |> List.iter @@ fun cmt_rule ->
        Format.printf "%a@." Sexplib.Sexp.pp_hum cmt_rule ;
        Format.printf "%a@." Sexplib.Sexp.pp_hum analyze_rule ;
        Format.printf "%a@." Sexplib.Sexp.pp_hum analyze_alias_rule
