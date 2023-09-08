Given a file `foo.ml` that contains a C stub.

Generate a "model" on how the OCaml C primitives are called:
```
(rule
 (with-stdout-to
  primitives.model.c
  (progn
   (run %{bin:lintcstubs_genwrap} %{dep:.foo.objs/byte/foo.cmt})
   (run %{bin:lintcstubs_genmain} %{dep:.foo.objs/byte/foo.cmt}))))
```

(If you have more files then you can use dune's `glob` feature to find them all).


Generate a prototype for the C primitives:
```
(rule
 (with-stdout-to
  primitives.model.c
  (progn
   (run %{bin:lintcstubs_genwrap} %{dep:.foo.objs/byte/foo.cmt})
   (run %{bin:lintcstubs_genmain} %{dep:.foo.objs/byte/foo.cmt}))))
```

Run the static analyzer producing a SARIF report and a logfile:
```
(rule
 (target foo.sarif)
 (action (with-stdout-to foo.log (run %{bin:lintcstubs} --conf lintcstubs.json %{dep:foostubs.c} %{dep:primitives.model.c} -I %{ocaml_where} -o %{target})))

)

(rule
 (alias runtest)
 (deps foo.log)
 (action (diff foo.log.reference %{deps}))
)
```

The SARIF report can be uploaded in a Github Action workflow to display the error in the UI.
