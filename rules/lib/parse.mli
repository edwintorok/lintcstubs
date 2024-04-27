(** Parses the output of [dune rules -r].
  The format is described in [dune rules --help]:
  {v
          ((deps    (<dependencies>))
          (targets (<targets>))
          (context <context-name>)
          (action  <action>))
  v}

  Another alternative would be to parse [dune describe --format=csexp --lang=0.1], but that is incomplete,
  it doesn't include rules for the C stubs.

  We only parse the subset of rules needed by [lintcstubs].
*)

module File : sig
  (** a file dependency, or [None] if we couldn't parse it *)
  type t = Fpath.t option
end

module Target : sig
  (** build target *)
  type t = [`directories of Fpath.t list | `files of Fpath.t list]
end

module Action : sig
  (** A {{:https://dune.readthedocs.io/en/stable/reference/actions/index.html#actions}dune user action}.

    [unknown] is for actions that we don't parse yet
   *)
  type t =
    [ `chdir of Fpath.t * [`run of string list]
    | `copy of Fpath.t list
    | `unknown of Sexplib0.Sexp.t ]
end

module Rule : sig
  type t = {
      deps: File.t list
    ; targets: Target.t list
    ; action: Action.t
    ; context: string option
  }

  val t_of_sexp : Sexplib0.Sexp.t -> t
end
