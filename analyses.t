Test rules from https://v2.ocaml.org/manual/intfc.html#s:c-gc-harmony

Rule 1. CAMLparam
  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <goblint.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam0();
  >   CAMLreturn0;
  > }
  > EOF

  $ lintcstubs --set mainfun[+] foo --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <goblint.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam3(v1, v2, v3);
  >   CAMLreturn0;
  > }
  > EOF
  $ lintcstubs --set mainfun[+] foo --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > void foo (value v1, value v2, value v3)
  > {
  > }
  > EOF
  $ lintcstubs --set mainfun[+] foo --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c

  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > void foo (value v1, value v2, value v3)
  > {
  >   CAMLparam3(v1,v2,v3);
  >   CAMLlocal1(result);
  >   result = caml_alloc(3, 0);
  >   CAMLreturn(result);
  > }
  > EOF
  $ lintcstubs --conf lintcstubs.json --set mainfun[+] foo -I $(ocamlc -where) test.c  | sed -e '/unroll.*/d'

Cannot dereference OCaml values after releasing the runtime lock:
  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <caml/threads.h>
  > #include <goblint.h>
  > void foo (value v)
  > {
  >   CAMLparam1(v);
  >   int x;
  >   caml_enter_blocking_section();
  >   x = *(int*)Data_abstract_val(v);
  >   caml_leave_blocking_section();
  >   CAMLreturn(Val_int(x));
  > }
  > EOF

  $ lintcstubs --set mainfun[+] foo --set warn.race-threshold 1000 --disable warn.imprecise --disable warn.info --enable dbg.regression --disable warn.deadcode -I $(ocamlc -where) test.c 2>&1 | sed -e 's^/[^ ]*/^^g'
  [Warning][Unknown] unlocking mutex which may not be held (ocaml_runtime.model.c:568:5-568:62)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (test.c:9:3-9:39)
  [Error][Race] DomainLock: must be held when dereferencing OCaml value v (test.c:9:3-9:39)

Correct would be:
  $ cat >test.c <<EOF
  > #include <caml/memory.h>
  > #include <caml/threads.h>
  > #include <goblint.h>
  > void foo (value v)
  > {
  >   CAMLparam1(v);
  >   int x = *(int*) Data_abstract_val(v);
  >   caml_enter_blocking_section();
  >   caml_leave_blocking_section();
  >   CAMLreturn(x);
  > }
  > EOF

  $ lintcstubs --set mainfun[+] foo --disable warn.imprecise --enable dbg.regression --disable warn.info --disable warn.deadcode -I $(ocamlc -where) test.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (test.c:7:7-7:44)
  [Warning][Unknown] unlocking mutex which may not be held (/var/home/edwin/git/lint/lintcstubs/_build/install/default/share/goblint/lib/stub/src/ocaml_runtime.model.c:568:5-568:62)

