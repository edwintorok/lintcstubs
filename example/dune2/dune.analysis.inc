; AUTO-GENERATED by ../../../install/default/bin/lintcstubs-dune-rules foo2.ml foo2.model.c foostubs.c
; DO NOT EDIT
(rule
 (enabled_if
  (and %{bin-available:lintcstubs-dune-rules} %{bin-available:lintcstubs}))
 (deps (:mlfiles (glob_files_rec *.ml)) (:cfiles (glob_files_rec *.c)))
 (action
  (with-stdout-to dune.analysis.inc.gen
   (run %{bin:lintcstubs-dune-rules} %{mlfiles} %{cfiles}))))
(rule (alias runtest) (enabled_if %{bin-available:lintcstubs})
 (action (diff dune.analysis.inc dune.analysis.inc.gen)))
(rule (targets ./lintcstubs.log ./lintcstubs.sarif)
 (enabled_if %{bin-available:lintcstubs})
 (deps (:primitives ./primitives.h) (:model foo2.model.c) %{bin:lintcstubs}
  (package lintcstubs) compile_commands.json)
 (action
  (with-stdout-to ./lintcstubs.log
   (run %{bin:lintcstubs} --conf lintcstubs.json -o ./lintcstubs.sarif -I
    %{ocaml_where} --set dbg.solver-stats-interval 0 compile_commands.json
    %{model}))))
(rule (action (with-stdout-to compile_commands.json (echo [])))
 (mode fallback))
(rule (alias analyze) (deps ./lintcstubs.log)
 (action (diff lintcstubs.out.reference %{deps})))
(rule (enabled_if %{bin-available:lintcstubs_genmain})
 (targets foo2.model.c ./primitives.h)
 (deps (:cmt ./.foo2.objs/byte/foo2.cmt) %{bin:lintcstubs_arity_cmt}
  %{bin:lintcstubs_genwrap} %{bin:lintcstubs_genmain})
 (action
  (progn
   (with-stdout-to ./primitives.h (run %{bin:lintcstubs_arity_cmt} %{cmt}))
   (with-stdout-to foo2.model.c
    (progn (run %{bin:lintcstubs_genwrap} %{cmt})
     (run %{bin:lintcstubs_genmain} %{cmt}))))))
