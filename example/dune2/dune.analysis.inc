(rule (targets foo2.model.c ./primitives.h)
 (deps (:cmt ./.foo2.objs/byte/foo2.cmt) %{bin:lintcstubs_arity_cmt}
  %{bin:lintcstubs_genwrap} %{bin:lintcstubs_genmain})
 (action
  (progn
   (with-stdout-to ./primitives.h (run %{bin:lintcstubs_arity_cmt} %{cmt}))
   (with-stdout-to foo2.model.c
    (progn (run %{bin:lintcstubs_genwrap} %{cmt})
     (run %{bin:lintcstubs_genmain} %{cmt}))))))
(rule (alias cstubs) (deps (:ofiles foo2.model.o foostubs.o))
 (action (echo "for BEAR")))
(rule (targets ./lintcstubs.log ./lintcstubs.sarif)
 (deps compile_commands.json (:primitives ./primitives.h)
  (:model foo2.model.c) %{bin:lintcstubs} (package lintcstubs))
 (action
  (with-stdout-to ./lintcstubs.log
   (run %{bin:lintcstubs} --conf lintcstubs.json -o ./lintcstubs.sarif -I
    %{ocaml_where} --set dbg.solver-stats-interval 0 compile_commands.json
    %{model}))))
(rule (alias analyze) (deps (:log ./lintcstubs.log))
 (action (diff lintcstubs.out.reference %{deps})))
