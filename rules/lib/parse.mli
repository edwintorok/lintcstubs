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

  We only parse the subset of rules needed by [lintcstubs] for now.
*)

(** File dependency for a {!module:Rule} *)
module File : sig
  (** a file dependency, or [None] if we couldn't parse it *)
  type t = Fpath.t option
end

(** {!module:Rule} target *)
module Target : sig
  (** build target *)
  type t = [`directories of Fpath.t list | `files of Fpath.t list]
end

(** {!module:Rule} action *)
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
  (** [dune rules] output as an OCaml type *)
  type t = {
      deps: File.t list  (** dependencies of this rule *)
    ; targets: Target.t list  (** targets created by this rule *)
    ; action: Action.t  (** action executed by the rule *)
    ; context: string option  (** build context, if any *)
  }

  val t_of_sexp : Sexplib0.Sexp.t -> t
  (** [t_of_sexp sexp] parses a dune rule from [dune rules].

    Example usage:
    [stdin |> Sexp.input_rev_sexps |> List.rev_map Dune_rules.Parse.Rule.t_of_sexp]
   *)
end
