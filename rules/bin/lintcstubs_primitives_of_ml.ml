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

(** Parse a .ml file, extract all 'external ...' primitives,
    and print a list of all symbols required

    Uses compiler-libs, which has an unstable API that can change between
    compiler versions, so extract only the minimal information needed here.
    If this breaks with newer compiler versions then
    ocaml-migrate-parsetree could be used.
    Currently require a 4.08 AST minimum (although this could be relaxed with
    migrate-parsetree).

    [ocamlc -dparsetree foo.ml] can be used to see how the parsetree looks
    like.
 *)

(** [value_description _ vd] is invoked by the AST iterator for value
    descriptions, including primitives ('external ...').

    @see <https://v2.ocaml.org/api/compilerlibref/Parsetree.html#2_Valuedescriptions>
*)
let value_description file _ vd =
  let open Parsetree in
  match vd.pval_prim with
  | [] ->
      () (* not a primitive *)
  | builtin :: _ when builtin = "" || builtin.[0] = '%' ->
      () (* call to builtin primitive, nothing to verify *)
  | symbols ->
      (* print external symbols in [nm -gAPu] compatible format *)
      symbols |> List.iter (Printf.printf "%s: %s U\n" file)

let () =
  let tool_name = Sys.executable_name in
  let files =
    (* use Arg for parsing to minimize dependencies *)
    let lst = ref [] in
    let usage_msg = Printf.sprintf "%s [FILE.ml...]" tool_name in
    Arg.parse [] (fun file -> lst := file :: !lst) usage_msg ;
    !lst
  in
  try
    let open Ast_iterator in
    (* use the AST iterator, because primitives might be declared inside a
       module, not necessarily at top level. *)
    files
    |> List.iter @@ fun file ->
       let primitives_iterator =
         {default_iterator with value_description= value_description file}
       in
       (* have to parse the implementation, because the .mli may hide that it
          is a C stub by defining a 'val name ...' instead of 'external name ...'. *)
       file
       |> Pparse.parse_implementation ~tool_name
       |> primitives_iterator.structure primitives_iterator
  with e ->
    (* if there are any syntax errors, or other exceptions escaping from
       compiler-libs this will report them properly *)
    Location.report_exception Format.err_formatter e ;
    exit 2
