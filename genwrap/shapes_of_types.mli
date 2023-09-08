(** Memory layout and value range of an OCaml type *)
module Shape : sig
  type 'a range = {min: 'a; max: 'a}

  type unboxed =
    | TaggedInt of int range
    | UntaggedInt of string * int64 range
    | DoubleArrayElement

  (** memory layout and value range of an OCaml type *)
  type t =
    | Unboxed of unboxed  (** passed directly, not a pointer *)
    | Boxed of boxed  (** pointer to an allocated block *)
    | Exception  (** an OCaml exception *)
    | Variant of unboxed option * boxed array
        (** an OCaml variant, can contain both unboxed and boxed elements *)
    | Arrow of (Types.type_expr * t) * (Types.type_expr * t)  (** a function *)
    | Unknown  (** not yet supported *)
    | Bytecode_argv of int  (** [value[argn]] *)

  and boxed =
    | Double  (** double-precision floating point value *)
    | Int32  (** 32-bit integer, can be less than an OCaml word in size *)
    | IntN of {words: int}
        (** a boxed integer, given number of words in size *)
    | String of {writable: bool  (** true only for [bytes] *)}
        (** an OCaml [string], or [bytes] *)
    | Tuple of t array  (** a tuple of possibly different shapes *)
    | Array of {elements: t  (** shape of an array element *)}
        (** an array where each elements has shape [elements] *)
    | Block of {
          tag: int  (** allocated OCaml block with this [tag] in the header *)
        ; elements: t array
              (** elements of possibly different shapes, e.g. a record *)
      }
    | Object  (** an OCaml object, more precise analysis not supported yet *)

  val untagged_constant : int -> t
end

val basic : Shape.t list
(** [basic] built-in types *)

val ctype_of_shape : Shape.t -> string
(** [ctype_of_shape shape] returns the C type corresponding to OCaml value [shape]. *)

val shape_of_primitive :
     Types.type_expr
  -> Primitives_of_cmt.t
  -> (string option * Shape.t) * (string option * Shape.t) list
(** [shape_of_primitive type_expr primitive_type] infers the shape of a primitive argument or return value.

  @param type_expr the type expression from the TypedTree
  @param primitive_type {!type:Primitives_of_cmt.t} describing whether the type is unboxed, untagged or regular OCaml value
  @returns {!type:Shape.t} describing memory layout and value range
 *)
