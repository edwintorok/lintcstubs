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
  $ lintcstubs_genmain test.cmt
  #include "primitives.h"
  #include <goblint.h>
  #include "caml/threads.h"
  
  #ifndef CAMLnoalloc
  /* GC status assertions.
  
     CAMLnoalloc at the start of a block means that the GC must not be
     invoked during the block. */
  #if defined(__GNUC__) && defined(DEBUG)
  int caml_noalloc_begin(void);
  void caml_noalloc_end(int*);
  void caml_alloc_point_here(void);
  #define CAMLnoalloc                          \
    int caml__noalloc                          \
    __attribute__((cleanup(caml_noalloc_end),unused)) \
      = caml_noalloc_begin()
  #define CAMLalloc_point_here (caml_alloc_point_here())
  #else
  #define CAMLnoalloc
  #define CAMLalloc_point_here ((void)0)
  #endif
  #endif
      int __VERIFIER_nondet_int(void);
  void __access_Val(value);
  value __VERIFIER_nondet_value(void);double __VERIFIER_nondet_double(void);int32_t __VERIFIER_nondet_int32_t(void);int64_t __VERIFIER_nondet_int64_t(void);intnat __VERIFIER_nondet_intnat(void);void __caml_maybe_run_gc(void);
  static void __call_caml_ml_seek_in_char(void) {
  	value res = caml_ml_seek_in_char(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  	__access_Val(res);
  }
  
  static void __call_caml_ml_seek_in(void) {
  	value res = caml_ml_seek_in(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  	__access_Val(res);
  }
  
  static void __call_caml_ml_seek_in_pair(void) {
  	value res = caml_ml_seek_in_pair(__VERIFIER_nondet_value());
  	__access_Val(res);
  }
  
  static void __call_f(void) {
  	value res = f(__VERIFIER_nondet_value());
  	__access_Val(res);
  }
  
  static void __call_g(void) {
  	value res = g(__VERIFIER_nondet_value(), __VERIFIER_nondet_value());
  	__access_Val(res);
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


