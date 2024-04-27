Setup base dune file:
  $ cd example
  $ lintcstubs_genrules >dune.inc

Setup some build files for testing:
  $ echo '(lang dune 3.14)' >dune-project
  $ touch lintcstubs.opam

We cannot modify dune directly (it is readonly, because we run as part of the build here)
  $ cat dune >dune.tmp
  $ echo '(include dune.inc)' >>dune.tmp
  $ mv dune.tmp dune

Build symbol files, the first runtest is needed to update dune.inc:
  $ dune build @gensymbols

The second run actually generates the symbols:
  $ dune build @gensymbols

Check symbol files. To keep the output portable across systems do not look at symbol value and size:
  $ grep foo_ _build/default/*.symbols | cut -f1-3 -d ' '
  _build/default/foo2.ml.symbols:foo2.ml: foo_bad U
  _build/default/foo2.ml.symbols:foo2.ml: foo_good U
  _build/default/foostubs.c.symbols:foostubs.o: foo_bad T
  _build/default/foostubs.c.symbols:foostubs.o: foo_good T
