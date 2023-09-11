(*
 * Copyright (C) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Load a .cmt file which contains a Typedtree,
    and use it to extract primitives along with the shapes of their arguments,
    and generate wrapper functions that check pre and post conditions of the primitive calls.
    These can be used either as runtime assertion checks (in the style of `ortac`), or
    as input to a static analyzer.

    [ocamlc -dtypedtree foo.ml] can be used to see how the typedtree looks
    like.

    A Typedtree is better than a Parsetree for this purpose because it contains
    resolved types and type immediacy information from the compiler itself.
 *)

let usage_msg = Printf.sprintf "%s [FILE.cmt...]" Sys.executable_name

(* The generated code follows binutils's 'wrap' style:

   #ifdef __ANALYZER
   #define __real_foo foo
   #else
   CAMLprim value __real_foo( /* ... params ... */);
   #endif

   CAMLprim __wrap_foo(/* params ... */)
   {
    value result;
    /* assume/assert shape/size of input parameters */
    result = __real_foo(/* params */);
    /* assert shape and size of returned value, and state of runtime lock */
    return result;
   }

   Binutils will then interpose this function inbetween the OCaml code and the real C stub when linking with
   `-cclib -Wl,-wrap,foo`.

   The static analyzer won't interpose, for it we'll need to define __real_foo = foo.

   The generated assertions attempt to be independent of the static analyzer used,
   although currently only tested with Goblint.
*)

(* For __has_include see:
      https://isocpp.org/std/standing-documents/sd-6-sg10-feature-test-recommendations#testing-for-the-presence-of-a-header-__has_include
   We assume that goblint is used with a new enough GCC that implements this (e.g. 5.x+)
*)

let header =
  format_of_string
    {|
#define DEBUG
#include "%s"
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
|}

let pp_ctype ppf t =
  Format.pp_print_string ppf @@ Shapes_of_types.ctype_of_shape t

let pp_sep ppf () = Format.pp_print_string ppf ", "

let pp_arg ppf (i, _) = Format.fprintf ppf "arg%d" i

let pp_param ppf ((_, (_, t)) as arg) =
  Format.fprintf ppf "%a %a" pp_ctype t pp_arg arg

let pp_params = Format.pp_print_list ~pp_sep pp_param

let pp_args = Format.pp_print_list ~pp_sep pp_arg

type kind = Result of string | Arg of string

let assert_of_kind = function Result _ -> "RES" | Arg _ -> "ARG"

let print_assert kind format =
  Format.printf ("ASSERT_%s(" ^^ format ^^ ");@,") (assert_of_kind kind)

let assert_block kind ?words ~tag name =
  print_assert kind "Is_block(%s)" name ;

  (* on 4.14 in naked pointer mode this can perform additional checks:
     we may have received an invalid pointer
  *)
  print_assert kind "Is_in_value_area(%s)" name ;
  let () =
    words
    |> Option.iter @@ fun words ->
       print_assert kind "%d == Wosize_val(%s)" words name
  in
  print_assert kind "%s == Tag_val(%s)" tag name

let counter = ref 0

let field value shape i =
  let (Result s | Arg s) = value in
  let name = Format.asprintf "%s_%s" s i in
  Format.printf "%a %s = Field(%s, %s);@," pp_ctype shape name s i ;
  match value with Result _ -> Result name | Arg _ -> Arg name

let rec assert_type value shape =
  let open Shapes_of_types in
  let open Format in
  let name = match value with Result s | Arg s -> s in
  printf "@," ;
  match shape with
  | Shape.Unknown | Boxed Object ->
      (* access the 'value' to check for dangling pointers *)
      printf "if @[<v>(Is_block(%s)) {@, " name ;

      (* TODO: goblint cannot determine state of this assert
         print_assert value "Is_in_value_area(%s)" name ;
      *)
      printf "(void)Tag_val(%s);@]@,}@," name
  | Boxed Double ->
      assert_block value ~words:(64 / Sys.word_size) ~tag:"Double_tag" name
  | Shape.Boxed Int32 ->
      assert_block value ~words:2 ~tag:"Custom_tag" name
  | Shape.Boxed (IntN {words}) ->
      assert_block value ~words:(words + 1) ~tag:"Custom_tag" name
  (* TODO: could assert the custom ops too! *)
  | Boxed (String _) ->
      assert_block value ~tag:"String_tag" name ;
      print_assert value "1 <= Wosize_val(%s)" name ;
      (* padding always there *)
      print_assert value "Wosize_val(%s) <= %uUL" name
        (1 + (Sys.max_string_length * 8 / Sys.word_size))
      (*
      print_assert value "Field(%s, Wosize_val(%s) - 1) < %d" name name
        (Sys.word_size / 8) *)
  | Boxed (Tuple tuple) ->
      print_assert value "Is_block(%s)" name ;
      print_assert value "!Tag_val(%s)" name ;
      print_assert value "Wosize_val(%s) == %d" name (Array.length tuple) ;
      tuple
      |> Array.iteri @@ fun i e ->
         assert_type (field value e @@ string_of_int i) e
  | Boxed (Array {elements}) ->
      print_assert value "Is_block(%s)" name ;
      print_assert value "!Tag_val(%s)" name ;
      printf "for (unsigned i=0;i<Wosize_val(%s);i++) {@,@[<2>" name ;
      assert_type (field value elements "i") elements ;
      printf "@]@,}@,"
  | Boxed (Block {tag; elements}) ->
      assert_block value ~words:(Array.length elements) ~tag:(string_of_int tag)
        name ;
      elements
      |> Array.iteri @@ fun i e ->
         assert_type (field value e @@ string_of_int i) e
  | Unboxed (TaggedInt range) ->
      print_assert value "Is_long(%s)" name ;
      if Int.equal range.min range.max then
        print_assert value "%dL == Long_val(%s)" range.min name
      else (
        if not (Int.equal range.min Int.min_int) then
          print_assert value "%dL <= Long_val(%s)" range.min name ;
        if not (Int.equal range.max Int.max_int) then
          print_assert value "Long_val(%s) <= %dL" name range.max
      )
  | Unboxed (UntaggedInt (typ, range)) ->
      if Int64.equal range.min range.max then
        print_assert value "(%s)%LdL == %s" typ range.min name
      else (
        if not (Int64.equal range.min Int64.min_int) then
          print_assert value "(%s)%LdL <= %s" typ range.min name ;
        if not (Int64.equal range.max Int64.max_int) then
          print_assert value "%s <= (%s)%LdL" name typ range.max
      )
  | Bytecode_argv n ->
      print_assert value "%s" name ;
      printf "(void)%s[%d];@," name (n - 1)
  | Arrow _ ->
      assert_block value ~tag:"Closure_tag" name ;
      print_assert value "!!Code_val(%s)" name
  | _ ->
      (* TODO: could insert more assertions based on actual type, e.g. variants *)
      printf "(void)%s;@," name (* void avoids the unused variable warning *)

let print_wrapper ~noalloc (res_type, res) name args =
  (* TODO: use CIL to construct this *)
  let open Format in
  let args = List.mapi (fun i e -> (i, e)) args in

  printf "@,CAMLprim %a __REAL(%s)(%a);@," pp_ctype res name pp_params args ;
  printf "__WRAPPER CAMLprim %a __wrap_%s(%a)@,{@[<v2>@," pp_ctype res name
    pp_params args ;
  if noalloc then
    (* macro that uses 'cleanup' feature to get also called when leaving the scope *)
    printf "CAMLnoalloc;@," ;

  counter := 0 ;
  let () =
    args
    |> List.iter @@ fun (i, (arg_type, arg)) ->
       printf "@,/* %a */" Format.(pp_print_option pp_print_string) arg_type ;
       assert_type (Arg (Format.asprintf "%a" pp_arg (i, arg))) arg
  in
  printf "@,%a res = __REAL(%s)(%a);@,@," pp_ctype res name pp_args args ;
  printf "@,/* %a */" Format.(pp_print_option pp_print_string) res_type ;
  assert_type (Result "res") res ;

  (* On OCaml 5+ CAMLparam0 already calls this, but it is also useful after return.
      It checks that the (per-domain) runtime lock is held. When returning from a C primitive it must be held.
      On OCaml 4.x this is currently a no-op, although we may attempt to acquire and release the lock to check its validity.
      But doing so would allow the GC to run, and we may be inside a 'noalloc' call. Also the wrapper doesn't register the parameters
      or return value with the GC (to simplify static analysis), but releasing the lock would require doing that.

     When the static analyzer is used it could define this macro to an assertion about the lock state (although it currently doesn't).
  *)
  printf "@,Caml_check_caml_state();@," ;

  printf "return res;@]@,}@,"

let printed = Hashtbl.create 7

let print_c_call ~noalloc ret name args =
  if not (Hashtbl.mem printed name) then (
    Hashtbl.add printed name (ret, args) ;
    (* TODO: check that type matches if its a dup *)
    print_wrapper ~noalloc ret name args
  )

let unknown = (None, Shapes_of_types.Shape.Unknown)

let print_c_call_arity arity byte_name =
  print_c_call unknown byte_name @@ List.init arity (fun _ -> unknown)

let primitive_description type_expr desc =
  let open Primitives_of_cmt in
  let ret, args = Shapes_of_types.shape_of_primitive type_expr desc in
  (* TODO: a .t that covers all primitive types supported in shapes *)
  (* print native first *)
  let noalloc = not desc.alloc in
  print_c_call ~noalloc ret desc.native_name args ;
  (* if the bytecode one is different, print it *)
  if desc.native_name <> desc.byte_name then
    if desc.arity <= 5 then
      print_c_call_arity ~noalloc desc.arity desc.byte_name
    else
      let open Shapes_of_types in
      print_c_call ~noalloc unknown desc.byte_name
        [
          (None, Shape.Bytecode_argv desc.arity)
        ; (None, Shapes_of_types.Shape.untagged_constant desc.arity)
        ]
  else
    (* according to https://v2.ocaml.org/manual/intfc.html#ss:c-prim-impl
       if the primitive takes more than 5 arguments then bytecode and native
       mode implementations must be different *)
    assert (desc.arity <= 5) ;
  Format.printf "@,"

let () =
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in
  let headername =
    match files with
    | [onefile] ->
        (Filename.basename onefile |> Filename.chop_extension) ^ ".ml.h"
    | _ ->
        "primitives.h"
  in

  Printf.printf header headername ;
  flush stdout ;
  Format.printf "@[<v>" ;

  Primitives_of_cmt.with_report_exceptions @@ fun () ->
  let () =
    files
    |> List.iter @@ fun path ->
       Primitives_of_cmt.iter_primitives_exn ~path primitive_description
  in
  Format.printf "@]@."
