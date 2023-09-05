(lang dune 3.0)
(cram enable)
(using dune_site 0.1)

(generate_opam_files)
(name lintcstubs)
(source (github edwintorok/lintcstubs))
(authors "Edwin Török <edwin.torok@cloud.com>")
(maintainers "Edwin Török <edwin.torok@cloud.com>")
(license LGPL-2.1-or-later)

(package
 (name lintcstubs)
 (synopsis "OCaml C stub static analyzer")
 (description "Generates a C model for how OCaml C primitives can be called. Link with a C model of the OCaml runtime, and run a static analyzer to find incorrect API/macro usage that leads to race conditions.")
 (depends
  (ocaml (>= 4.14))
  (goblint (>= 2.1.0))
  (lintcstubs-arity (>= 0.2))
 )
)