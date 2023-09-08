#include "foo.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>

value foo_bad (value v)
 {
   CAMLparam1(v);
   int x;
   caml_enter_blocking_section();
   x = *(int*)Data_abstract_val(v);
   caml_leave_blocking_section();
   CAMLreturn(Val_int(x));
 }

value foo_good (value v)
 {
   CAMLparam1(v);
   int x = *(int*) Data_abstract_val(v);
   caml_enter_blocking_section();
   caml_leave_blocking_section();
   CAMLreturn(Val_int(x));
 }
