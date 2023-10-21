(rule (target primitives.h) (deps)
  (action (with-stdout-to %{target} (run cat %{deps}))))
(rule (target lintcstubs.sarif) (deps primitives.h)
  (action
    (run %{bin:lintcstubs} -o %{target} -I . -I %{ocaml_where} --conf
      lintcstubs.json %{deps})))
(alias (name runtest) (deps lintcstubs.sarif))
(rule (target lintcstubs.sexp) (deps) (mode promote)
  (action
    (with-stdout-to %{target}
      (run %{bin:lintcstubs_gen_rules} --update %{deps}))))
