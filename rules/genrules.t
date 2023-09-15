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
  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ dune runtest
