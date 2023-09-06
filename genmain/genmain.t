Test primitive types:
  $ cat >test.ml <<EOF
  > external seek_in : in_channel -> char -> unit = "caml_ml_seek_in_char"
  > external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
  > external seek_in_pair: in_channel * int -> unit = "caml_ml_seek_in_pair"
  > type int_endo = int -> int
  > external f : int_endo -> int_endo = "f"
  > external g : (int -> int) -> (int -> int) = "g"
  > EOF
  $ ocamlc -c -bin-annot test.ml
  $ lintcstubs_genmain test.cmt >test_call.c
  $ cat test_call.c
  #include "primitives.h"
  #include <goblint.h>
  #include "caml/threads.h"
  int __VERIFIER_nondet_int(void);
  int32_t __VERIFIER_nondet_int32_t(void);
  int64_t __VERIFIER_nondet_int64_t(void);
  intnat __VERIFIER_nondet_intnat(void);
  value __VERIFIER_nondet_value(void);
  double __VERIFIER_nondet_double(void);
  value __VERIFIER_nondet_value(void);
  void __caml_maybe_run_gc(void);
  static void __call_caml_ml_seek_in_char(void) {
  	(void)__wrap_caml_ml_seek_in_char(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  }
  
  static void __call_caml_ml_seek_in(void) {
  	(void)__wrap_caml_ml_seek_in(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  }
  
  static void __call_caml_ml_seek_in_pair(void) {
  	(void)__wrap_caml_ml_seek_in_pair(__VERIFIER_nondet_value());
  }
  
  static void __call_f(void) {
  	(void)__wrap_f(__VERIFIER_nondet_value());
  }
  
  static void __call_g(void) {
  	(void)__wrap_g(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  }
  
  static void* __call__all(void* arg) {
  	(void)arg;
  	caml_leave_blocking_section();
  	switch(__VERIFIER_nondet_int()) {
  	case 0: __call_caml_ml_seek_in(); break;
  	case 1: __call_caml_ml_seek_in_char(); break;
  	case 2: __call_caml_ml_seek_in_pair(); break;
  	case 3: __call_f(); break;
  	case 4: __call_g(); break;
  	default: __caml_maybe_run_gc(); break;
  	}
  	caml_enter_blocking_section();
  	return NULL;
  }
  
  #include <pthread.h>
  int main(void)
  {
  	pthread_t thread;
  	int rc = pthread_create(&thread, NULL, __call__all, NULL);
  	__goblint_assume(!rc);
  	(void)__call__all(NULL);
  	rc = pthread_join(thread, NULL);
  	__goblint_assume(!rc);
  	return 0;
  }

Test that we can compile the generated code (using pwd below is important because ocamlc runs the compiler in a temp dir):
  $ lintcstubs_arity_cmt test.cmt >primitives.h
  $ lintcstubs_genwrap test.cmt >test_analyze.c
  $ cat test_call.c >>test_analyze.c
  $ ocamlc -ccopt -I -ccopt $(pwd)/../model/include -ccopt -Wall -c test_analyze.c


