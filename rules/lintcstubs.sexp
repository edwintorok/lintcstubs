(rule (target lintcstubs.sarif) (deps)
  (action
    (run %{bin:lintcstubs} -o %{target} -I %{ocaml_where} --conf
      lintcstubs.json %{deps})))
(rule (target lintcstubs.sexp) (deps) (mode promote)
  (action
    (with-stdout-to %{target}
      (run %{bin:lintcstubs_gen_rules} --update %{deps}))))
