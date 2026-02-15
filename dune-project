(lang dune 3.6)
(cram enable)
(using dune_site 0.1)
(using action-plugin 0.1)

(generate_opam_files)
(name lintcstubs)
(source (github edwintorok/lintcstubs))
(authors "Edwin Török <edwin.torok@cloud.com>")
(maintainers "Edwin Török <edwin.torok@cloud.com>")
(license LGPL-2.1-or-later)

; Separate packages because the generator also works on OCaml 5
; The static analyzer doesn't yet because released versions of goblint don't support it yet (but dev version does)

(package
 (name lintcstubs-gen)
 (synopsis "OCaml C stub wrapper generator")
 (description "Generates a C model for how OCaml C primitives can be called. Link with a C model of the OCaml runtime, or run a static analyzer to find incorrect API/macro usage that leads to race conditions.")
 (depends
  (ocaml (>= 4.13))
  (lintcstubs-arity (>= 0.5.1))
 )
)

(package
 (name lintcstubs)
 (synopsis "OCaml C stub static analyzer")
 (description "Uses a generated C model for how OCaml C primitives can be called. Run a static analyzer to find incorrect API/macro usage that leads to race conditions.")
 (depends
  (ocaml (>= 4.13))
  (lintcstubs-gen (= :version))
  (goblint (>= 2.7.1))
  goblint-cil
  dune-compiledb
  fpath
  (fmt (>= 0.9.0))
  (sexplib (>= v0.15.0))
  (conf-graphviz :with-test)
 )
)
