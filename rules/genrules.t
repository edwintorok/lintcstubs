Define a project in a subdir:
  $ mkdir -p testdir && cd testdir  
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ touch dune
  $ mkdir src && cd src

Define a C primitive:
  $ mkdir c_stubs
  $ cat >c_stubs/dune <<EOF
  > (library
  >  (name mystubs)
  >  (foreign_stubs
  >  (language c)
  >  (names foostubs)))
  > EOF
  $ cat >c_stubs/foostubs.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/memory.h>
  > #include <caml/alloc.h>
  > #include <caml/threads.h>
  > value foo_good (value v)
  > {
  >   CAMLparam1(v);
  >   CAMLreturn(Val_unit);
  > }
  > EOF

The module invoking it can be elsewhere:
  $ mkdir other
  $ cat >other/foo2.ml <<EOF
  > external foo_good : unit -> int = "foo_good"
  > EOF
  $ cat >other/dune <<EOF
  > (test
  >   (name main)
  >   (libraries mystubs mystubs2)
  > )
  > EOF

We can also have other modules that don't use C primitives:
  $ cat >other/main.ml <<EOF
  > let (_:int) = Foo2.foo_good ()
  > let (_:int) = Mystubs2.Foo3.foo_good2 ()
  > let (_:int) = Mystubs2.Foo3.foo_good3 ()
  > let () = print_endline "Hello"
  > EOF

And another module, but using symbols from 2 C libs, with ML in same library:
  $ mkdir cstubs2
  $ cat >cstubs2/foo3.ml <<EOF
  > external foo_good2 : unit -> int = "foo_good2"
  > external foo_good3 : unit -> int = "foo_good3"
  > EOF
  $ cat >cstubs2/dune <<EOF
  > (library
  >  (name mystubs2)
  >  (foreign_stubs
  >  (language c)
  >  (names foostubs2 foostubs3)))
  > EOF
  $ cat >cstubs2/foostubs2.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/memory.h>
  > #include <caml/alloc.h>
  > #include <caml/threads.h>
  > value foo_good2 (value v)
  > {
  >   CAMLparam1(v);
  >   CAMLreturn(Val_unit);
  > }
  > EOF
  $ cat >cstubs2/foostubs3.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/memory.h>
  > #include <caml/alloc.h>
  > #include <caml/threads.h>
  > value foo_good3 (value v)
  > {
  >   CAMLparam1(v);
  >   CAMLreturn(Val_unit);
  > }
  > EOF

This builds and runs:
  $ cd ..
  $ dune runtest

Now generate rules:
  $ dune clean
  $ cd src
  $ dune rules -r --root=.. | lintcstubs_gen_rules >lintcstubs.sexp
  Entering directory '..'
  Leaving directory '..'
  $ cat lintcstubs.sexp
  (rule (target lintcstubs.sarif)
    (deps (file cstubs2/mystubs2__Foo3.cmt.model.c)
      (file cstubs2/mystubs2__Foo3.ml.h)
      (file other/dune__exe__Foo2.cmt.model.c)
      (file other/dune__exe__Foo2.ml.h) (file primitives.h))
    (action
      (run %{bin:lintcstubs} -o %{target} -I . -I %{ocaml_where} --conf
        lintcstubs.json %{deps}))
    (alias runtest))
  (rule (target lintcstubs.sexp)
    (deps (file cstubs2/.mystubs2.objs/byte/mystubs2__Foo3.cmt)
      (file cstubs2/foo3.ml) (file cstubs2/foo3.ml.symbols)
      (file cstubs2/mystubs2__Foo3.cmt.model.c)
      (file cstubs2/mystubs2__Foo3.ml.h)
      (file other/.main.eobjs/byte/dune__exe__Foo2.cmt)
      (file other/dune__exe__Foo2.cmt.model.c)
      (file other/dune__exe__Foo2.ml.h) (file other/foo2.ml)
      (file other/foo2.ml.symbols) (file other/main.ml)
      (file other/main.ml.symbols))
    (action
      (with-stdout-to %{target}
        (run %{bin:lintcstubs_gen_rules} --update %{deps})))
    (mode promote))
  (rule (target primitives.h)
    (deps (file cstubs2/mystubs2__Foo3.ml.h) (file other/dune__exe__Foo2.ml.h))
    (action (with-stdout-to %{target} (run cat %{deps}))))
  (subdir cstubs2/
    (rule (target foo3.ml.symbols) (deps (file foo3.ml))
      (action
        (with-stdout-to %{target}
          (run %{bin:lintcstubs_primitives_of_ml} %{deps})))))
  (subdir cstubs2/
    (rule (target mystubs2__Foo3.cmt.model.c)
      (deps (file .mystubs2.objs/byte/mystubs2__Foo3.cmt))
      (action
        (with-stdout-to %{target}
          (progn (run %{bin:lintcstubs_genwrap} %{deps})
            (run %{bin:lintcstubs_genmain} %{deps}))))))
  (subdir cstubs2/
    (rule (target mystubs2__Foo3.ml.h)
      (deps (file .mystubs2.objs/byte/mystubs2__Foo3.cmt))
      (action
        (with-stdout-to %{target} (run %{bin:lintcstubs_arity_cmt} %{deps})))))
  (subdir other/
    (rule (target dune__exe__Foo2.cmt.model.c)
      (deps (file .main.eobjs/byte/dune__exe__Foo2.cmt))
      (action
        (with-stdout-to %{target}
          (progn (run %{bin:lintcstubs_genwrap} %{deps})
            (run %{bin:lintcstubs_genmain} %{deps}))))))
  (subdir other/
    (rule (target dune__exe__Foo2.ml.h)
      (deps (file .main.eobjs/byte/dune__exe__Foo2.cmt))
      (action
        (with-stdout-to %{target} (run %{bin:lintcstubs_arity_cmt} %{deps})))))
  (subdir other/
    (rule (target foo2.ml.symbols) (deps (file foo2.ml))
      (action
        (with-stdout-to %{target}
          (run %{bin:lintcstubs_primitives_of_ml} %{deps})))))
  (subdir other/
    (rule (target main.ml.symbols) (deps (file main.ml))
      (action
        (with-stdout-to %{target}
          (run %{bin:lintcstubs_primitives_of_ml} %{deps})))))
  $ echo "(include lintcstubs.sexp)" >dune
  $ cp lintcstubs.sexp lintcstubs0.sexp
  $ (cd .. && dune build src/lintcstubs.sexp)
  $ diff -U 1 lintcstubs0.sexp lintcstubs.sexp | tail -n+3
  @@ -17,4 +17,3 @@
       (file other/dune__exe__Foo2.ml.h) (file other/foo2.ml)
  -    (file other/foo2.ml.symbols) (file other/main.ml)
  -    (file other/main.ml.symbols))
  +    (file other/foo2.ml.symbols))
     (action
  @@ -58,7 +57,2 @@
       (action
  -      (with-stdout-to %{target}
  -        (run %{bin:lintcstubs_primitives_of_ml} %{deps})))))
  -(subdir other/
  -  (rule (target main.ml.symbols) (deps (file main.ml))
  -    (action
         (with-stdout-to %{target}
  $ cd ..
  $ dune runtest --root=..
  Entering directory '..'
  Leaving directory '..'
