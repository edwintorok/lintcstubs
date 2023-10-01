(* See https://dune.readthedocs.io/en/stable/dune-files.html#rule
   This is not the full list of stanzas and options, just the ones that are used in this tool
*)

type t

type alias

type action

type target

type dep

val empty : t

val dep_glob_ext : ?recurse:bool -> string -> dep

val dep_file : Fpath.t -> dep

val target : Fpath.t list -> target

val run : string -> string list -> action

val progn : action list -> action

val with_stdout_to_target : action -> action

val rule :
     ?mode:[`standard | `fallback | `promote]
  -> ?alias:string
  -> target
  -> dep list
  -> action
  -> t

val merge : t -> t -> t

val sexp_list_of_t : t -> Sexplib0.Sexp.t list
