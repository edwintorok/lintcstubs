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
    and generate a 'main' function to call them all for the purpose of static
    analysis.

    [ocamlc -dtypedtree foo.ml] can be used to see how the typedtree looks
    like.

    A Typedtree is better than a Parsetree for this purpose because it contains
    resolved types and type immediacy information from the compiler itself.
 *)

let usage_msg = Printf.sprintf "%s [FILE.cmt...]" Sys.executable_name

(** [nondet ctype] is a generator for [ctype].
    See [sv-comp.c] in [goblint], these are the nondeterministic value
    generators used in static verifier competitions, and supported by various
    static analyzers
 *)
let nondet typ = "__VERIFIER_nondet_" ^ typ

let print_nondet_prototype t =
  let open Shapes_of_types in
  let ctype = ctype_of_shape t in
  Printf.printf "%s %s(void);\n" ctype (nondet ctype)

let gen_of_native_arg args =
  let open Shapes_of_types in
  function
  | Shape.Bytecode_argv _ ->
      Printf.sprintf "value[]{%s}"
      @@ String.concat ", "
      @@ List.map ctype_of_shape args
  | Shape.Unboxed (UntaggedInt (_, {min; max})) when Nativeint.equal min max ->
      Nativeint.to_string min
  | Shape.Unboxed (TaggedInt {min; max}) when Int.equal min max ->
      Printf.sprintf "Val_int(%d)" min
  | arg ->
      nondet @@ ctype_of_shape arg ^ "()"

module StringSet = Set.Make (String)

let calls = ref StringSet.empty

(* move this to a separate module/tool: genwrapper,
     that generates
       __real_foo( ... params ... );
       __wrap_foo(..)
       {
       ... add some __goblint_assume here about inputs (but that get compiled away on a real program) ....
       __real_foo()
       ... check postconditions ...

    These will need to be defined as static when in static analysis mode, and global otherwise (use some macro at the beginning to do that..., e.g. WRAP and REAL)

    Then use '-ccopt -Wl,-wrap -ccopt <symbol>' for all symbols to redirect symbols through the checker for runtime checking.

    For static analysis generate a 2nd file that we can include in the first that redefines __real_foo = foo

     Also generate some __call_foo that calls __wrap_foo(...) with some nondet code depending on the input type,
   like what genmain does now
   }
*)

let print_c_call _res name args =
  let open Printf in
  if not @@ StringSet.mem name !calls then (
    calls := StringSet.add name !calls ;
    let args = List.map snd args in
    printf "static void __call_%s(void) {\n" name ;
    printf "\t(void)__wrap_%s(%s);\n" name
    @@ String.concat ", "
    @@ List.map (gen_of_native_arg args) args ;
    (*  suppress unused value warning *)
    print_endline "}"
  )

let unknown = (None, Shapes_of_types.Shape.Unknown)

let print_c_call_arity arity byte_name =
  print_c_call unknown byte_name @@ List.init arity (fun _ -> unknown)

let primitive_description type_expr desc =
  let open Primitives_of_cmt in
  let ret, args = Shapes_of_types.shape_of_primitive type_expr desc in
  (* TODO:use ret and args *)
  (* TODO: a .t that covers all primitive types supported in shapes *)
  (* print native first *)
  print_c_call ret desc.native_name args ;
  (* if the bytecode one is different, print it *)
  if desc.native_name <> desc.byte_name then
    if desc.arity <= 5 then
      print_c_call_arity desc.arity desc.byte_name
    else
      let open Shapes_of_types in
      print_c_call unknown desc.byte_name
        [
          (None, Shape.Bytecode_argv desc.arity)
        ; (None, Shape.untagged_constant desc.arity)
        ]
  else
    (* according to https://v2.ocaml.org/manual/intfc.html#ss:c-prim-impl
       if the primitive takes more than 5 arguments then bytecode and native
       mode implementations must be different *)
    assert (desc.arity <= 5) ;
  print_endline ""

let print_call_all () =
  (* TODO: could use Format module *)
  print_endline "static void* __call__all(void* arg) {" ;
  print_endline "\t(void)arg;" ;
  print_endline "\tcaml_leave_blocking_section();" ;
  (* some of these may raise exceptions, so use a nondet to choose which one to
     call, to ensure they are all seen as called *)
  print_endline "\tswitch(__VERIFIER_nondet_int()) {" ;
  let () =
    !calls
    |> StringSet.elements
    |> List.iteri @@ fun i name ->
       Printf.printf "\tcase %d: __call_%s(); break;\n" i name
  in
  print_endline "\tdefault: __caml_maybe_run_gc(); break;" ;
  print_endline "\t}" ;
  print_endline "\tcaml_enter_blocking_section();" ;
  print_endline "\treturn NULL;" ;
  print_endline "}" ;

  print_endline "" ;
  print_endline "#include <pthread.h>" ;
  print_endline "int main(void)" ;
  print_endline "{" ;
  print_endline "\tpthread_t thread;" ;
  print_endline "\tint rc = pthread_create(&thread, NULL, __call__all, NULL);" ;
  print_endline "\t__goblint_assume(!rc);" ;
  (* don't model thread creation failure *)
  print_endline "\t(void)__call__all(NULL);" ;
  print_endline "\trc = pthread_join(thread, NULL);" ;
  print_endline "\t__goblint_assume(!rc);" ;
  (* don't model thread creation failure *)
  print_endline "\treturn 0;" ;
  print_endline "}"

let () =
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in

  print_endline {|#include "primitives.h"|} ;
  print_endline {|#include <goblint.h>|} ;
  print_endline {|#include "caml/threads.h"|} ;

  let () =
    (* TODO: put in a header *)
    Printf.printf "int __VERIFIER_nondet_int(void);\n" ;
    Shapes_of_types.basic |> List.iter @@ fun t -> print_nondet_prototype t
  in
  print_endline "void __caml_maybe_run_gc(void);" ;
  Primitives_of_cmt.with_report_exceptions @@ fun () ->
  let () =
    files
    |> List.iter @@ fun path ->
       Primitives_of_cmt.iter_primitives_exn ~path primitive_description
  in
  print_call_all ()
