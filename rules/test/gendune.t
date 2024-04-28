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
  $ cd _build/default/lintcstubs_run
  $ grep foo_ *.symbols | cut -f1-3 -d ' '
  0694a2e55f7ef1ebaebba36e042431b5.symbols:../example/dune2/foo2.ml: foo_bad U
  0694a2e55f7ef1ebaebba36e042431b5.symbols:../example/dune2/foo2.ml: foo_good U
  1cd6b0e9ad031c09963b56a0bcdd6a04.symbols:../example/dune2/foostubs.o: foo_bad T
  1cd6b0e9ad031c09963b56a0bcdd6a04.symbols:../example/dune2/foostubs.o: foo_good T
