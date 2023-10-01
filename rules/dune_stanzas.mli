(* See https://dune.readthedocs.io/en/stable/dune-files.html#rule
   This is not the full list of stanzas and options, just the ones that are used in this tool
*)

module Action : sig
  type t
end

module Filename : sig
  type t
end

module Target: sig
  type fullpath
  type basepath
  
  type 'a t

  val pp : 'a t Fmt.t
end

module Glob : sig
  type t
end

module Include : sig
  type t
end

module Dep : sig
  type t
end

module Deps : sig
  type t
end

module Mode : sig
  type t
end

module Rule : sig
  type 'a t
end

type t

val rule: ?mode:Mode.t -> Target.fullpath Target.t -> Deps.t -> Action.t -> t

val merge : t -> t -> t

val sexp_of_t : t -> Sexplib0.Sexp.t


