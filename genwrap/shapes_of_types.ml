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

(** [Typedtree] and [Primitive] have an unstable API (depends on compiler version),
    so extract the parts we need and convert to types defined in this file.
    If the build breaks with new compiler versions then only this module needs
    to be updated (perhaps by using Dune's support to conditionally select
    files based on compiler versions)

    This currently only works on 4.14+
*)

module Shape = struct
  (** https://v2.ocaml.org/manual/intfc.html#s%3Ac-ocaml-datatype-repr *)

  (** range of an integer. Needed because [unit], [bool], [char], [int] and simple variants are all
    represented as integers when interfacing with C, but for value analysis it is useful to know
    their range.
   *)
  type 'a range = {
      min: 'a  (** minimum possible value for type *)
    ; max: 'a  (** maximum possible value for type *)
  }

  (** Integers and float array elements are stored directly in values *)
  type unboxed =
    | TaggedInt of int range  (** OCaml integer with given range *)
    | UntaggedInt of string * int64 range  (** OCaml integer with given range *)
    | DoubleArrayElement  (** An unboxed float array element *)

  (** information about the size and layout of an OCaml type *)
  type t =
    | Unboxed of unboxed  (** directly stored in a value *)
    | Boxed of boxed  (** pointer stored in value, allocated separately *)
    | Exception  (** exceptions have dedicated API calls *)
    | Variant of unboxed option * boxed array
        (** a variant can contain both boxed and unboxed types: [A | B of ... | C ...] *)
    | Arrow of (Types.type_expr * t) * (Types.type_expr * t)  (** [e1 -> e2] *)
    | Unknown  (** a value we cannot yet analyze (e.g. abstract type) *)
    | Bytecode_argv of int  (** an array of values of given size *)

  and boxed =
    | Double  (** OCaml [float] *)
    | Int32  (** can be smaller than a word, special case *)
    | IntN of {words: int}  (** [int64], [nativeint] *)
    | String of {writable: bool  (** [string] is not writable *)}
        (** [string] or [bytes] *)
    | Tuple of t array  (** (e1,...,en) each element can have different shape *)
    | Array of {elements: t}
        (** [| element; ... |], all elements have same shape *)
    | Block of {tag: int; elements: t array}
        (** {field1: ...; ...; fieldN: ...} *)
    | Object
  (*  | Self of {levels:int} *)

  let untagged_constant n =
    let n = Int64.of_int n in
    Unboxed (UntaggedInt ("int", {min= n; max= n}))

  let _string_size =
    TaggedInt {min= 0; max= 1 + (Sys.max_string_length * 8 / Sys.word_size)}

  let int_range min max = Unboxed (TaggedInt {min; max})

  let unit = int_range 0 0

  let bool = int_range 0 1

  let char = int_range 0 255

  let int = int_range min_int max_int

  let untagged_int typ min max = Unboxed (UntaggedInt (typ, {min; max}))

  let _constructor x = Unboxed (TaggedInt {min= x; max= x})

  let _block tag elements = Boxed (Block {tag; elements})

  let bytes = Boxed (String {writable= true})

  let string = Boxed (String {writable= false})

  let float = Boxed Double

  let int32 = Boxed Int32

  let int64 = Boxed (IntN {words= 64 / Sys.word_size})

  let nativeint = Boxed (IntN {words= 1})

  let tuple lst = Boxed (Tuple (Array.of_list lst))

  let is_double = function Boxed Double -> true | _ -> false

  let exn = Exception

  let _record lst =
    if List.for_all is_double lst then
      (* TODO: depends on compiler version/flags? *)
      Boxed (Array {elements= Unboxed DoubleArrayElement})
    else
      tuple lst

  let _array elements = Boxed (Array {elements})

  (* TODO: depends on compiler version/flags? *)
  let floatarray = Boxed (Array {elements= Unboxed DoubleArrayElement})

  let obj = Boxed Object

  let arrow e1 e2 = Arrow (e1, e2)

  let predef =
    let open Predef in
    [
      (path_int, int)
    ; (path_char, char)
    ; (path_string, string)
    ; (path_bytes, bytes)
    ; (path_float, float)
    ; (path_bool, bool)
    ; (path_unit, unit)
    ; (path_nativeint, nativeint)
    ; (path_unit, unit)
    ; (path_int32, int32)
    ; (path_int64, int64)
    ; (path_exn, exn)
    ; (path_floatarray, floatarray)
    ]
    |> List.to_seq
    |> Path.Map.of_seq

  let rec of_type_expr e =
    match Get_desc.get_desc e with
    | Ttuple lst ->
        tuple (List.map of_type_expr lst)
    | Tobject _ ->
        obj
    | Tarrow (_, e1, e2, _) ->
        arrow (e1, of_type_expr e1) (e2, of_type_expr e2)
    | Tvar _ ->
        Unknown
    | Tconstr (path, [], _) ->
        Path.Map.find_opt path predef |> Option.value ~default:Unknown
    | Tconstr _ | Tfield _ | Tnil | Tunivar _ | Tpackage _ ->
        Unknown
    | Tlink e | Tsubst (e, _) | Tpoly (e, _) ->
        (* TODO: substitute type variables in call... *)
        of_type_expr e
    | Tvariant _ ->
        Unknown (* TODO: use constructor_description here *)
end

let basic =
  let open Shape in
  [
    untagged_int "int32_t"
      (Int32.min_int |> Int64.of_int32)
      (Int32.max_int |> Int64.of_int32)
  ; untagged_int "int64_t" Int64.min_int Int64.max_int
  ; untagged_int "intnat"
      (Int64.of_nativeint Nativeint.min_int)
      (Int64.of_nativeint Nativeint.max_int)
  ; int
  ; Unboxed DoubleArrayElement
  ; Unknown
  ]

let ctype_of_shape =
  let open Shape in
  function
  | Unboxed (TaggedInt _) ->
      "value"
  | Unboxed (UntaggedInt (typ, _)) ->
      typ
  | Unboxed DoubleArrayElement ->
      "double"
  | Boxed _ | Exception | Variant _ | Arrow _ | Unknown ->
      "value"
  | Bytecode_argv _ ->
      "value*"

let rec arrow_of_shape typ = function
  | Shape.Arrow ((t1, e1), (t2, e2)) ->
      Seq.cons (Some t1, e1) (arrow_of_shape t2 e2)
  | shape ->
      Seq.return (Some typ, shape)

let get_arrow e =
  match List.of_seq (arrow_of_shape e @@ Shape.of_type_expr e) with
  | [] ->
      assert false
  | [(_, Unknown)] ->
      None
  | [_] ->
      assert false
  | lst ->
      let rev = List.rev lst in
      let ret = List.hd rev and args = rev |> List.tl |> List.rev in
      Some (ret, args)

let shape_of_primitive type_expr prim =
  let open Primitives_of_cmt in
  let n = List.length prim.native_args in
  let shape_of ((typ, shape), t) =
    ( Option.map (Format.asprintf "%a" Printtyp.type_expr) typ
    , match t with
      | Value ->
          shape
      | Double ->
          Shape.(Unboxed DoubleArrayElement)
      | Int32 ->
          Shape.untagged_int "int32_t"
            (Int32.min_int |> Int64.of_int32)
            (Int32.max_int |> Int64.of_int32)
      | Int64 ->
          Shape.untagged_int "int64_t" Int64.min_int Int64.max_int
      | Intnat {untagged_int= true} ->
          Shape.untagged_int "intnat"
            (Int64.of_nativeint Nativeint.min_int)
            (Int64.of_nativeint Nativeint.max_int)
      | Intnat {untagged_int= false} ->
          Shape.int
      | Bytecode_argv ->
          Shape.Bytecode_argv n
      | Bytecode_argn ->
          Shape.untagged_constant n
    )
  in

  let ret_shape, args_shape =
    match get_arrow type_expr with
    | Some ((_, args) as shape)
      when List.length args = List.length prim.native_args ->
        shape
    | _ ->
        (* not fatal: treat them as unknown *)
        ( (None, Shape.Unknown)
        , prim.native_args |> List.map @@ fun _ -> (None, Shape.Unknown)
        )
  in
  ( shape_of (ret_shape, prim.native_result)
  , List.combine args_shape prim.native_args |> List.map shape_of
  )
