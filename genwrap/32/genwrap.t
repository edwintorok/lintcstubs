TODO: records, variants, polymorphic variant, objects
FAIL:
  $ false
  [1]

Test primitive types:
  $ cat >test.ml <<EOF
  > external seek_in : in_channel -> int -> unit = "caml_ml_seek_in2"
  > external closure_test: int -> (float -> float) -> char -> unit = "closure_test"
  > external type_test :
  >      int
  >   -> (unit -> unit)
  >   -> string
  >   -> bytes
  >   -> float
  >   -> Float.Array.t
  >   -> float array
  >   -> int32
  >   -> int64
  >   -> nativeint
  >   -> char
  >   -> bool
  >   -> unit
  >   -> (int * float)
  >   -> int array
  >   -> int32 array
  >   -> char list
  >   -> (float [@unboxed])
  >   -> (int32 [@unboxed])
  >   -> (int64 [@unboxed])
  >   -> (nativeint [@unboxed])
  >   -> (int [@untagged])
  >   -> unit = "stub_type_test_byte" "stub_type_test_nat"
  > external type_test_res :
  >   (unit -> unit) ->
  >      int
  >   * (unit -> unit)
  >   * string
  >   * bytes
  >   * float
  >   * Float.Array.t
  >   * float array
  >   * int32
  >   * int64
  >   * nativeint
  >   * char
  >   * bool
  >   * unit
  >   * (int * float)
  >   * int array
  >   * int32 array
  >   * char list
  >   * unit = "stub_type_test_byte_res" "stub_type_test_nat_res"
  > EOF
  $ ocamlc -c -bin-annot test.ml
  $ lintcstubs_arity_cmt test.cmt >test.ml.h
  $ lintcstubs_genwrap test.cmt >test_wrap.c
  $ cat test_wrap.c
  
  #define DEBUG
  #include "test.ml.h"
  #include "caml/threads.h"
  #include "caml/address_class.h"
  #include <assert.h>
  
  #ifndef CAMLnoalloc
    /* GC status assertions.
  
       CAMLnoalloc at the start of a block means that the GC must not be
       invoked during the block.
    */
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
  
  #ifdef __has_include
   #if __has_include(<goblint.h>)
     #define __HAS_GOBLINT 1
   #endif
  #endif
  
  #ifdef __HAS_GOBLINT
   #include <goblint.h>
   #define __WRAPPER static
   #define __REAL(f) f
   #define ASSERT_ARG(x) __goblint_assume(x)
   #define ASSERT_RES(x) assert(x)
  #else
   #define __WRAPPER
   #define __REAL(f) __real_##f
   #define ASSERT_ARG(x) assert(x)
   #define ASSERT_RES(x) assert(x)
  #endif
  
  #ifndef Caml_check_caml_state
    #define Caml_check_caml_state()
  #endif
  
  CAMLprim value __REAL(caml_ml_seek_in2)(value arg0, value arg1);
  __WRAPPER CAMLprim value __wrap_caml_ml_seek_in2(value arg0, value arg1)
  {
     
     /* in_channel */
     if (Is_block(arg0)) {
         (void)Tag_val(arg0);
     }
     
     /* int */
     ASSERT_ARG(Is_long(arg1));
     
     value res = __REAL(caml_ml_seek_in2)(arg0, arg1);
     
     
     /* unit */
     ASSERT_RES(Is_long(res));
     ASSERT_RES(0L == Long_val(res));
     
     Caml_check_caml_state();
     return res;
  }
  
  
  CAMLprim value __REAL(closure_test)(value arg0, value arg1, value arg2);
  __WRAPPER CAMLprim value __wrap_closure_test(value arg0, value arg1, value arg2)
  {
     
     /* int */
     ASSERT_ARG(Is_long(arg0));
     
     /* float -> float */
     ASSERT_ARG(Is_block(arg1));
     ASSERT_ARG(Is_in_value_area(arg1));
     ASSERT_ARG(Closure_tag == Tag_val(arg1));
     ASSERT_ARG(!!Code_val(arg1));
     
     /* char */
     ASSERT_ARG(Is_long(arg2));
     ASSERT_ARG(0L <= Long_val(arg2));
     ASSERT_ARG(Long_val(arg2) <= 255L);
     
     value res = __REAL(closure_test)(arg0, arg1, arg2);
     
     
     /* unit */
     ASSERT_RES(Is_long(res));
     ASSERT_RES(0L == Long_val(res));
     
     Caml_check_caml_state();
     return res;
  }
  
  
  CAMLprim value __REAL(stub_type_test_nat)(value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8, value arg9, value arg10, value arg11, value arg12, value arg13, value arg14, value arg15, value arg16, double arg17, int32_t arg18, int64_t arg19, value arg20, intnat arg21);
  __WRAPPER CAMLprim value __wrap_stub_type_test_nat(value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8, value arg9, value arg10, value arg11, value arg12, value arg13, value arg14, value arg15, value arg16, double arg17, int32_t arg18, int64_t arg19, value arg20, intnat arg21)
  {
     
     /* int */
     ASSERT_ARG(Is_long(arg0));
     
     /* unit -> unit */
     ASSERT_ARG(Is_block(arg1));
     ASSERT_ARG(Is_in_value_area(arg1));
     ASSERT_ARG(Closure_tag == Tag_val(arg1));
     ASSERT_ARG(!!Code_val(arg1));
     
     /* string */
     ASSERT_ARG(Is_block(arg2));
     ASSERT_ARG(Is_in_value_area(arg2));
     ASSERT_ARG(String_tag == Tag_val(arg2));
     ASSERT_ARG(1 <= Wosize_val(arg2));
     ASSERT_ARG(Wosize_val(arg2) <= 4194303UL);
     
     /* bytes */
     ASSERT_ARG(Is_block(arg3));
     ASSERT_ARG(Is_in_value_area(arg3));
     ASSERT_ARG(String_tag == Tag_val(arg3));
     ASSERT_ARG(1 <= Wosize_val(arg3));
     ASSERT_ARG(Wosize_val(arg3) <= 4194303UL);
     
     /* float */
     ASSERT_ARG(Is_block(arg4));
     ASSERT_ARG(Is_in_value_area(arg4));
     ASSERT_ARG(2 == Wosize_val(arg4));
     ASSERT_ARG(Double_tag == Tag_val(arg4));
     
     /* Float.Array.t */
     if (Is_block(arg5)) {
         (void)Tag_val(arg5);
     }
     
     /* float array */
     if (Is_block(arg6)) {
         (void)Tag_val(arg6);
     }
     
     /* int32 */
     ASSERT_ARG(Is_block(arg7));
     ASSERT_ARG(Is_in_value_area(arg7));
     ASSERT_ARG(2 == Wosize_val(arg7));
     ASSERT_ARG(Custom_tag == Tag_val(arg7));
     
     /* int64 */
     ASSERT_ARG(Is_block(arg8));
     ASSERT_ARG(Is_in_value_area(arg8));
     ASSERT_ARG(3 == Wosize_val(arg8));
     ASSERT_ARG(Custom_tag == Tag_val(arg8));
     
     /* nativeint */
     ASSERT_ARG(Is_block(arg9));
     ASSERT_ARG(Is_in_value_area(arg9));
     ASSERT_ARG(2 == Wosize_val(arg9));
     ASSERT_ARG(Custom_tag == Tag_val(arg9));
     
     /* char */
     ASSERT_ARG(Is_long(arg10));
     ASSERT_ARG(0L <= Long_val(arg10));
     ASSERT_ARG(Long_val(arg10) <= 255L);
     
     /* bool */
     ASSERT_ARG(Is_long(arg11));
     ASSERT_ARG(0L <= Long_val(arg11));
     ASSERT_ARG(Long_val(arg11) <= 1L);
     
     /* unit */
     ASSERT_ARG(Is_long(arg12));
     ASSERT_ARG(0L == Long_val(arg12));
     
     /* int * float */
     ASSERT_ARG(Is_block(arg13));
     ASSERT_ARG(!Tag_val(arg13));
     ASSERT_ARG(Wosize_val(arg13) == 2);
     value arg13_0 = Field(arg13, 0);
     
     ASSERT_ARG(Is_long(arg13_0));
     value arg13_1 = Field(arg13, 1);
     
     ASSERT_ARG(Is_block(arg13_1));
     ASSERT_ARG(Is_in_value_area(arg13_1));
     ASSERT_ARG(2 == Wosize_val(arg13_1));
     ASSERT_ARG(Double_tag == Tag_val(arg13_1));
     
     /* int array */
     if (Is_block(arg14)) {
         (void)Tag_val(arg14);
     }
     
     /* int32 array */
     if (Is_block(arg15)) {
         (void)Tag_val(arg15);
     }
     
     /* char list */
     if (Is_block(arg16)) {
         (void)Tag_val(arg16);
     }
     
     /* float */
     (void)arg17;
     
     /* int32 */
     ASSERT_ARG((int32_t)-2147483648L <= arg18);
     ASSERT_ARG(arg18 <= (int32_t)2147483647L);
     
     /* int64 */
     
     /* nativeint */
     ASSERT_ARG(Is_long(arg20));
     
     /* int */
     ASSERT_ARG((intnat)-2147483648L <= arg21);
     ASSERT_ARG(arg21 <= (intnat)2147483647L);
     
     value res = __REAL(stub_type_test_nat)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
     
     
     /* unit */
     ASSERT_RES(Is_long(res));
     ASSERT_RES(0L == Long_val(res));
     
     Caml_check_caml_state();
     return res;
  }
  
  CAMLprim value __REAL(stub_type_test_byte)(value* arg0, int arg1);
  __WRAPPER CAMLprim value __wrap_stub_type_test_byte(value* arg0, int arg1)
  {
     
     /*  */
     ASSERT_ARG(arg0);
     (void)arg0[21];
     
     /*  */
     ASSERT_ARG((int)22L == arg1);
     
     value res = __REAL(stub_type_test_byte)(arg0, arg1);
     
     
     /*  */
     if (Is_block(res)) {
         (void)Tag_val(res);
     }
     
     Caml_check_caml_state();
     return res;
  }
  
  
  CAMLprim value __REAL(stub_type_test_nat_res)(value arg0);
  __WRAPPER CAMLprim value __wrap_stub_type_test_nat_res(value arg0)
  {
     
     /* unit -> unit */
     ASSERT_ARG(Is_block(arg0));
     ASSERT_ARG(Is_in_value_area(arg0));
     ASSERT_ARG(Closure_tag == Tag_val(arg0));
     ASSERT_ARG(!!Code_val(arg0));
     
     value res = __REAL(stub_type_test_nat_res)(arg0);
     
     
     /* int * (unit -> unit) * string * bytes * float * Float.Array.t * float array *
  int32 * int64 * nativeint * char * bool * unit * (int * float) * int array *
  int32 array * char list * unit */
     ASSERT_RES(Is_block(res));
     ASSERT_RES(!Tag_val(res));
     ASSERT_RES(Wosize_val(res) == 18);
     value res_0 = Field(res, 0);
     
     ASSERT_RES(Is_long(res_0));
     value res_1 = Field(res, 1);
     
     ASSERT_RES(Is_block(res_1));
     ASSERT_RES(Is_in_value_area(res_1));
     ASSERT_RES(Closure_tag == Tag_val(res_1));
     ASSERT_RES(!!Code_val(res_1));
     value res_2 = Field(res, 2);
     
     ASSERT_RES(Is_block(res_2));
     ASSERT_RES(Is_in_value_area(res_2));
     ASSERT_RES(String_tag == Tag_val(res_2));
     ASSERT_RES(1 <= Wosize_val(res_2));
     ASSERT_RES(Wosize_val(res_2) <= 4194303UL);
     value res_3 = Field(res, 3);
     
     ASSERT_RES(Is_block(res_3));
     ASSERT_RES(Is_in_value_area(res_3));
     ASSERT_RES(String_tag == Tag_val(res_3));
     ASSERT_RES(1 <= Wosize_val(res_3));
     ASSERT_RES(Wosize_val(res_3) <= 4194303UL);
     value res_4 = Field(res, 4);
     
     ASSERT_RES(Is_block(res_4));
     ASSERT_RES(Is_in_value_area(res_4));
     ASSERT_RES(2 == Wosize_val(res_4));
     ASSERT_RES(Double_tag == Tag_val(res_4));
     value res_5 = Field(res, 5);
     
     if (Is_block(res_5)) {
         (void)Tag_val(res_5);
     }
     value res_6 = Field(res, 6);
     
     if (Is_block(res_6)) {
         (void)Tag_val(res_6);
     }
     value res_7 = Field(res, 7);
     
     ASSERT_RES(Is_block(res_7));
     ASSERT_RES(Is_in_value_area(res_7));
     ASSERT_RES(2 == Wosize_val(res_7));
     ASSERT_RES(Custom_tag == Tag_val(res_7));
     value res_8 = Field(res, 8);
     
     ASSERT_RES(Is_block(res_8));
     ASSERT_RES(Is_in_value_area(res_8));
     ASSERT_RES(3 == Wosize_val(res_8));
     ASSERT_RES(Custom_tag == Tag_val(res_8));
     value res_9 = Field(res, 9);
     
     ASSERT_RES(Is_block(res_9));
     ASSERT_RES(Is_in_value_area(res_9));
     ASSERT_RES(2 == Wosize_val(res_9));
     ASSERT_RES(Custom_tag == Tag_val(res_9));
     value res_10 = Field(res, 10);
     
     ASSERT_RES(Is_long(res_10));
     ASSERT_RES(0L <= Long_val(res_10));
     ASSERT_RES(Long_val(res_10) <= 255L);
     value res_11 = Field(res, 11);
     
     ASSERT_RES(Is_long(res_11));
     ASSERT_RES(0L <= Long_val(res_11));
     ASSERT_RES(Long_val(res_11) <= 1L);
     value res_12 = Field(res, 12);
     
     ASSERT_RES(Is_long(res_12));
     ASSERT_RES(0L == Long_val(res_12));
     value res_13 = Field(res, 13);
     
     ASSERT_RES(Is_block(res_13));
     ASSERT_RES(!Tag_val(res_13));
     ASSERT_RES(Wosize_val(res_13) == 2);
     value res_13_0 = Field(res_13, 0);
     
     ASSERT_RES(Is_long(res_13_0));
     value res_13_1 = Field(res_13, 1);
     
     ASSERT_RES(Is_block(res_13_1));
     ASSERT_RES(Is_in_value_area(res_13_1));
     ASSERT_RES(2 == Wosize_val(res_13_1));
     ASSERT_RES(Double_tag == Tag_val(res_13_1));
     value res_14 = Field(res, 14);
     
     if (Is_block(res_14)) {
         (void)Tag_val(res_14);
     }
     value res_15 = Field(res, 15);
     
     if (Is_block(res_15)) {
         (void)Tag_val(res_15);
     }
     value res_16 = Field(res, 16);
     
     if (Is_block(res_16)) {
         (void)Tag_val(res_16);
     }
     value res_17 = Field(res, 17);
     
     ASSERT_RES(Is_long(res_17));
     ASSERT_RES(0L == Long_val(res_17));
     
     Caml_check_caml_state();
     return res;
  }
  
  CAMLprim value __REAL(stub_type_test_byte_res)(value arg0);
  __WRAPPER CAMLprim value __wrap_stub_type_test_byte_res(value arg0)
  {
     
     /*  */
     if (Is_block(arg0)) {
         (void)Tag_val(arg0);
     }
     
     value res = __REAL(stub_type_test_byte_res)(arg0);
     
     
     /*  */
     if (Is_block(res)) {
         (void)Tag_val(res);
     }
     
     Caml_check_caml_state();
     return res;
  }
  
  

  $ ocamlc -ccopt -Wall -ccopt -Wextra -ccopt -Wstrict-prototypes -ccopt -g -c test_wrap.c

Test that runtime wrapping works when the code has no errors:
  $ cat >test_stubs.c <<EOF
  >  #include <caml/memory.h>
  >  #include <caml/mlvalues.h>
  >  #include <caml/alloc.h>
  >  #include <assert.h>
  >  CAMLprim value caml_ml_seek_in2(value arg0, value arg1)
  >  {
  >      (void)arg0; (void)arg1;
  >      return Val_unit;
  >  }
  > 
  >  CAMLprim value closure_test(value arg0, value arg1, value arg2)
  >  {
  >    (void)arg0; (void)arg1;(void)arg2;
  >    return Val_unit;
  >  }
  > 
  >  CAMLprim value stub_type_test_nat(value arg0, value arg1, value arg2, value arg3, value arg4, value arg5, value arg6, value arg7, value arg8, value arg9, value arg10, value arg11, value arg12, value arg13, value arg14, value arg15, value arg16, double arg17, int32_t arg18, int64_t arg19, value arg20, intnat arg21)
  >  {
  >    (void)arg0; (void)arg1;(void)arg2;(void)arg3;(void)arg4;(void)arg5;(void)arg6;(void)arg7;(void)arg8;(void)arg9;(void)arg10;(void)arg11;(void)arg12;(void)arg13;(void)arg14;(void)arg15;(void)arg16;(void)arg17;(void)arg18;(void)arg19;(void)arg20;(void)arg21;
  >    return Val_unit;
  >  }
  > 
  >  CAMLprim value stub_type_test_byte(value* argv, int argn)
  >  {
  >    assert(argn == 22);
  >    return stub_type_test_nat(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10], argv[11], argv[12], argv[13], argv[14], argv[15], argv[16], argv[17], argv[18], argv[19], argv[20], argv[21]);
  >  }
  > 
  >  CAMLprim value stub_type_test_nat_res(value arg0)
  >  {
  >    CAMLparam1(arg0);
  >    CAMLlocal1(result);
  >    CAMLlocal5(fa, ia, i32a, pair, cl);
  > 
  >    fa = caml_alloc_float_array(2);
  >    Store_double_field(fa, 0, 3.0);
  >    Store_double_field(fa, 1, 4.0);
  > 
  >    ia = caml_alloc_tuple(2);
  >    Store_field(ia, 0, Val_int(10));
  >    Store_field(ia, 1, Val_int(11));
  > 
  >    i32a = caml_alloc_tuple(2);
  >    Store_field(ia, 0, caml_copy_int32(10));
  >    Store_field(ia, 1, caml_copy_int32(11));
  > 
  >    pair = caml_alloc_tuple(2);
  >    Store_field(pair, 0, Val_int(8));
  >    Store_field(pair, 1, caml_copy_double(9.0));
  > 
  >    cl = caml_alloc_tuple(2);
  >    Store_field(cl, 0, Val_int('x'));
  >    Store_field(cl, 1, Val_emptylist);
  > 
  >    result = caml_alloc_tuple(18);      
  >    Store_field(result, 0, Val_int(1));
  >    Store_field(result, 1, arg0);
  >    Store_field(result, 2, caml_copy_string("test"));
  >    Store_field(result, 3, caml_copy_string("bytes"));
  >    Store_field(result, 4, caml_copy_double(2.0));
  >    Store_field(result, 5, fa);
  >    Store_field(result, 6, fa);
  >    Store_field(result, 7, caml_copy_int32(5));
  >    Store_field(result, 8, caml_copy_int64(6));
  >    Store_field(result, 9, caml_copy_nativeint(7));
  >    Store_field(result, 10, Val_int('c'));
  >    Store_field(result, 11, Val_bool(1));
  >    Store_field(result, 12, Val_unit);
  >    Store_field(result, 13, pair);
  >    Store_field(result, 14, ia);
  >    Store_field(result, 15, i32a);
  >    Store_field(result, 16, cl);
  >    Store_field(result, 17, Val_int(0));
  > 
  >    CAMLreturn(result);
  >  }
  > 
  >  CAMLprim value stub_type_test_byte_res(value arg0)
  >  {
  >    return stub_type_test_nat_res(arg0);
  >  }
  > EOF
  $ ocamlc -ccopt -Wall -ccopt -Wextra -ccopt -Wstrict-prototypes -ccopt -g -c test_stubs.c
  $ cat >call.ml <<EOF
  > let () =
  >   Test.seek_in stdin 0;
  >   Test.closure_test 4 Fun.id 'c';
  >   Test.type_test 1 ignore "foo" (Bytes.of_string "foo") 2.0 (Float.Array.make 2 3.0) [|5.0;6.0|] 7l 8L 9n 'a' true () (10, 11.) [|12|] [|13l; 14l|] ['b'; 'c'] 15.0 17l 18L 19n 20;
  >   let _ = Test.type_test_res ignore in ()
  > EOF

If we are on Linux then test '-wrap':
  $ if [ $(uname) = "Linux" ]; then
  > ocamlc -custom test.ml call.ml test_wrap.o test_stubs.o -ccopt -Wl,-wrap,caml_ml_seek_in2,-wrap,closure_test,-wrap,stub_type_test_byte,-wrap,stub_type_test_nat,-wrap,stub_type_test_byte_res,-wrap,stub_type_test_nat_res -o call.byte;
  > ./call.byte;
  > if command ocamlopt 2>/dev/null; then
  >   ocamlopt -ccopt -no-pie test.ml call.ml test_wrap.o test_stubs.o -ccopt -Wl,-wrap,caml_ml_seek_in2,-wrap,closure_test,-wrap,stub_type_test_byte,-wrap,stub_type_test_nat,-wrap,stub_type_test_byte_res,-wrap,stub_type_test_nat_res -o call.nat;
  >   ./call.nat;
  > fi
  > fi
