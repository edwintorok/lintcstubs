Setup base dune file:
  $ lintcstubs_genrules >dune.inc

Setup some build files for testing:
  $ echo '(lang dune 3.14)' >dune-project
  $ echo '(using action-plugin 0.1)' >>dune-project
  $ touch lintcstubs.opam

  $ echo '(include dune.inc)' >>dune

Generate symbol files:
  $ dune build @gensymbols

Check symbol files. To keep the output portable across systems do not look at symbol value and size:
  $ grep foo_ _build/default/lintcstubs_run/*.symbols | cut -f1-3 -d ' '
  _build/default/lintcstubs_run/example_dune2_foo2.ml.symbols:../example/dune2/foo2.ml: foo_bad U
  _build/default/lintcstubs_run/example_dune2_foo2.ml.symbols:../example/dune2/foo2.ml: foo_good U
  _build/default/lintcstubs_run/example_dune2_foostubs.o.symbols:../example/dune2/foostubs.o: foo_bad T
  _build/default/lintcstubs_run/example_dune2_foostubs.o.symbols:../example/dune2/foostubs.o: foo_good T
