TODO: the float test is disabled for now, we will need an analysis in goblint to track just the low bit of a 'value',
the existing analysis that are enabled by default do not do that.

Test primitive types:
  $ cat >test.ml <<EOF
  > external int_ok: unit -> int = "stub_int_ok"
  > external int_bad: unit -> int = "stub_int_bad"
  > external char_ok: unit -> char = "stub_char_ok"
  > external char_bad: unit -> char = "stub_char_bad"
  > (* external block_ok : unit -> float = "stub_float_ok"
  > external block_bad : unit -> float = "stub_float_bad" *)
  > EOF
  $ ocamlc -c -bin-annot test.ml
  $ lintcstubs_arity_cmt test.cmt >primitives.h
  $ lintcstubs_genwrap test.cmt >test_analyze.c

  $ cat >test_stubs.c <<EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value stub_int_ok(value arg)
  > {
  >    (void)arg;
  >    return Val_int(3);
  > }
  > 
  > CAMLprim value stub_int_bad(value arg)
  > {
  >    (void)arg;
  >    return 2;
  > }
  > 
  > CAMLprim value stub_char_ok(value arg)
  > {
  >    (void)arg;
  >    return Val_int(0xff);
  > }
  > 
  > CAMLprim value stub_char_bad(value arg)
  > {
  >    (void)arg;
  >    return Val_int(0x100);
  > }
  > 
  > CAMLprim value stub_float_ok(value arg)
  > {
  >    (void)arg;
  >    return caml_copy_double(2.0);
  > }
  > 
  > CAMLprim value stub_float_bad(value arg)
  > {
  >    (void)arg;
  >    return (value)2.0;
  > }
  > 
  > EOF

  $ cat test_analyze.c >test_main.c
  $ cat >>test_main.c <<EOF
  > int main(char *argv, int argc) {
  >  if (1 == argc) __wrap_stub_int_ok(Val_unit);
  >  if (2 == argc) __wrap_stub_int_bad(Val_unit);
  >  if (3 == argc) __wrap_stub_char_ok(Val_unit);
  >  if (4 == argc) __wrap_stub_char_bad(Val_unit);
  > }
  > EOF

  $ goblint -I $(ocamlc -where) --enable dbg.regression --disable warn.deadcode --disable warn.info test_main.c test_stubs.c
  [Error][Assert] Assertion "(res & 1L) != 0L" will fail. Expected: SUCCESS -> failed (test_main.c:84:4-84:40)
  [Error][Assert] Assertion "res >> 1 <= 255L" will fail. Expected: SUCCESS -> failed (test_main.c:126:4-126:42)

Now generate a main function, this introduces multi-threading:
  $ lintcstubs_genmain test.cmt >>test_analyze.c
  $ goblint  --set 'sem.int.signed_overflow' 'assume_wraparound' --set 'ana.activated[+]' 'assert' --enable warn.assert -I $(ocamlc -where) --disable warn.integer --enable dbg.regression --disable warn.info --disable warn.unsound --disable warn.deadcode test_analyze.c test_stubs.c ocaml_runtime.model.c
  [Error][Assert] Assertion "res >> 1 <= 255L" will fail. Expected: SUCCESS -> failed (test_analyze.c:126:4-126:42)
  [Error][Assert] Assertion "(res & 1L) != 0L" will fail. Expected: SUCCESS -> failed (test_analyze.c:84:4-84:40)

