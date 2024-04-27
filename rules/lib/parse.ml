open Sexplib
open Sexplib.Std

type fpath = Fpath.t

let fpath_of_sexp s = s |> Sexplib.Conv.string_of_sexp |> Fpath.v

module File = struct
  type t = Fpath.t option

  (* the code between [@@deriving_inline] and [@@end] is updated with:
     {v dune build @lint --auto-promote v}
  *)

  type file =
    | In_source_tree of fpath
    | In_build_dir of fpath
    | External of fpath
  [@@deriving_inline of_sexp]

  let _ = fun (_ : file) -> ()

  let file_of_sexp =
    ( let error_source__003_ = "rules/lib/parse.ml.File.file" in
      function
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom
             (("in_source_tree" | "In_source_tree") as _tag__006_)
          :: sexp_args__007_
          ) as _sexp__005_ -> (
        match sexp_args__007_ with
        | arg0__008_ :: [] ->
            let res0__009_ = fpath_of_sexp arg0__008_ in
            In_source_tree res0__009_
        | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args error_source__003_
              _tag__006_ _sexp__005_
      )
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("in_build_dir" | "In_build_dir") as _tag__011_)
          :: sexp_args__012_
          ) as _sexp__010_ -> (
        match sexp_args__012_ with
        | arg0__013_ :: [] ->
            let res0__014_ = fpath_of_sexp arg0__013_ in
            In_build_dir res0__014_
        | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args error_source__003_
              _tag__011_ _sexp__010_
      )
      | Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom (("external" | "External") as _tag__016_)
          :: sexp_args__017_
          ) as _sexp__015_ -> (
        match sexp_args__017_ with
        | arg0__018_ :: [] ->
            let res0__019_ = fpath_of_sexp arg0__018_ in
            External res0__019_
        | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args error_source__003_
              _tag__016_ _sexp__015_
      )
      | Sexplib0.Sexp.Atom ("in_source_tree" | "In_source_tree") as sexp__004_
        ->
          Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
      | Sexplib0.Sexp.Atom ("in_build_dir" | "In_build_dir") as sexp__004_ ->
          Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
      | Sexplib0.Sexp.Atom ("external" | "External") as sexp__004_ ->
          Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__003_
            sexp__002_
      | Sexplib0.Sexp.List [] as sexp__002_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_
            sexp__002_
      | sexp__002_ ->
          Sexplib0.Sexp_conv_error.unexpected_stag error_source__003_ sexp__002_
      : Sexplib0.Sexp.t -> file
      )

  let _ = file_of_sexp

  [@@@end]

  (* we don't parse [glob] and [Alias], so define its arguments as [Sexp.t] *)

  type parse = [`File of file | `glob of Sexp.t | `Alias of Sexp.t | `Universe]
  [@@deriving_inline of_sexp]

  let _ = fun (_ : parse) -> ()

  let __parse_of_sexp__ =
    ( let error_source__025_ = "rules/lib/parse.ml.File.parse" in
      function
      | Sexplib0.Sexp.Atom atom__021_ as _sexp__023_ -> (
        match atom__021_ with
        | "Universe" ->
            `Universe
        | "File" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__025_
              _sexp__023_
        | "glob" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__025_
              _sexp__023_
        | "Alias" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__025_
              _sexp__023_
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__021_ :: sexp_args__024_) as
        _sexp__023_ -> (
        match atom__021_ with
        | "File" as _tag__032_ -> (
          match sexp_args__024_ with
          | arg0__033_ :: [] ->
              let res0__034_ = file_of_sexp arg0__033_ in
              `File res0__034_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__025_
                _tag__032_ _sexp__023_
        )
        | "glob" as _tag__029_ -> (
          match sexp_args__024_ with
          | arg0__030_ :: [] ->
              let res0__031_ = Sexp.t_of_sexp arg0__030_ in
              `glob res0__031_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__025_
                _tag__029_ _sexp__023_
        )
        | "Alias" as _tag__026_ -> (
          match sexp_args__024_ with
          | arg0__027_ :: [] ->
              let res0__028_ = Sexp.t_of_sexp arg0__027_ in
              `Alias res0__028_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__025_
                _tag__026_ _sexp__023_
        )
        | "Universe" ->
            Sexplib0.Sexp_conv_error.ptag_no_args error_source__025_ _sexp__023_
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__022_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
            error_source__025_ sexp__022_
      | Sexplib0.Sexp.List [] as sexp__022_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
            error_source__025_ sexp__022_
      : Sexplib0.Sexp.t -> parse
      )

  let _ = __parse_of_sexp__

  let parse_of_sexp =
    ( let error_source__036_ = "rules/lib/parse.ml.File.parse" in
      fun sexp__035_ ->
        try __parse_of_sexp__ sexp__035_
        with Sexplib0.Sexp_conv_error.No_variant_match ->
          Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__036_
            sexp__035_
      : Sexplib0.Sexp.t -> parse
      )

  let _ = parse_of_sexp

  [@@@end]

  let t_of_sexp s =
    match parse_of_sexp s with
    | `File (In_source_tree p | In_build_dir p | External p) ->
        Some p
    | `glob _ | `Alias _ | `Universe ->
        None
end

module Target = struct
  type t = [`files of fpath list | `directories of fpath list]
  [@@deriving_inline of_sexp]

  let _ = fun (_ : t) -> ()

  let __t_of_sexp__ =
    ( let error_source__045_ = "rules/lib/parse.ml.Target.t" in
      function
      | Sexplib0.Sexp.Atom atom__038_ as _sexp__040_ -> (
        match atom__038_ with
        | "files" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__045_
              _sexp__040_
        | "directories" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__045_
              _sexp__040_
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__038_ :: sexp_args__041_) as
        _sexp__040_ -> (
        match atom__038_ with
        | "files" as _tag__046_ -> (
          match sexp_args__041_ with
          | arg0__047_ :: [] ->
              let res0__048_ = list_of_sexp fpath_of_sexp arg0__047_ in
              `files res0__048_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__045_
                _tag__046_ _sexp__040_
        )
        | "directories" as _tag__042_ -> (
          match sexp_args__041_ with
          | arg0__043_ :: [] ->
              let res0__044_ = list_of_sexp fpath_of_sexp arg0__043_ in
              `directories res0__044_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__045_
                _tag__042_ _sexp__040_
        )
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__039_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
            error_source__045_ sexp__039_
      | Sexplib0.Sexp.List [] as sexp__039_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
            error_source__045_ sexp__039_
      : Sexplib0.Sexp.t -> t
      )

  let _ = __t_of_sexp__

  let t_of_sexp =
    ( let error_source__050_ = "rules/lib/parse.ml.Target.t" in
      fun sexp__049_ ->
        try __t_of_sexp__ sexp__049_
        with Sexplib0.Sexp_conv_error.No_variant_match ->
          Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__050_
            sexp__049_
      : Sexplib0.Sexp.t -> t
      )

  let _ = t_of_sexp

  [@@@end]
end

module Action = struct
  (**  parses an action.
  
   [(chdir  _build/default/example/dune (run /usr/lib64/ccache/gcc ...))]
   [(copy example/Makefile/foostubs.c _build/default/example/Makefile/foostubs.c)]
  *)

  type parse =
    [`chdir of fpath * [`run of string list] | `copy of fpath list [@sexp.list]]
  [@@deriving_inline of_sexp]

  let _ = fun (_ : parse) -> ()

  let __parse_of_sexp__ =
    ( let error_source__066_ = "rules/lib/parse.ml.Action.parse" in
      function
      | Sexplib0.Sexp.Atom atom__052_ as _sexp__054_ -> (
        match atom__052_ with
        | "chdir" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__066_
              _sexp__054_
        | "copy" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__066_
              _sexp__054_
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__052_ :: sexp_args__055_) as
        _sexp__054_ -> (
        match atom__052_ with
        | "chdir" as _tag__057_ -> (
          match sexp_args__055_ with
          | arg0__073_ :: [] ->
              let res0__074_ =
                match arg0__073_ with
                | Sexplib0.Sexp.List [arg0__068_; arg1__069_] ->
                    let res0__070_ = fpath_of_sexp arg0__068_
                    and res1__071_ =
                      let sexp__067_ = arg1__069_ in
                      try
                        match sexp__067_ with
                        | Sexplib0.Sexp.Atom atom__059_ as _sexp__061_ -> (
                          match atom__059_ with
                          | "run" ->
                              Sexplib0.Sexp_conv_error.ptag_takes_args
                                error_source__066_ _sexp__061_
                          | _ ->
                              Sexplib0.Sexp_conv_error.no_variant_match ()
                        )
                        | Sexplib0.Sexp.List
                            (Sexplib0.Sexp.Atom atom__059_ :: sexp_args__062_)
                          as _sexp__061_ -> (
                          match atom__059_ with
                          | "run" as _tag__063_ -> (
                            match sexp_args__062_ with
                            | arg0__064_ :: [] ->
                                let res0__065_ =
                                  list_of_sexp string_of_sexp arg0__064_
                                in
                                `run res0__065_
                            | _ ->
                                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                                  error_source__066_ _tag__063_ _sexp__061_
                          )
                          | _ ->
                              Sexplib0.Sexp_conv_error.no_variant_match ()
                        )
                        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as
                          sexp__060_ ->
                            Sexplib0.Sexp_conv_error
                            .nested_list_invalid_poly_var error_source__066_
                              sexp__060_
                        | Sexplib0.Sexp.List [] as sexp__060_ ->
                            Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                              error_source__066_ sexp__060_
                      with Sexplib0.Sexp_conv_error.No_variant_match ->
                        Sexplib0.Sexp_conv_error.no_matching_variant_found
                          error_source__066_ sexp__067_
                    in
                    (res0__070_, res1__071_)
                | sexp__072_ ->
                    Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                      error_source__066_ 2 sexp__072_
              in
              `chdir res0__074_
          | _ ->
              Sexplib0.Sexp_conv_error.ptag_incorrect_n_args error_source__066_
                _tag__057_ _sexp__054_
        )
        | "copy" as _tag__056_ ->
            `copy (Sexplib0.Sexp_conv.list_map fpath_of_sexp sexp_args__055_)
        | _ ->
            Sexplib0.Sexp_conv_error.no_variant_match ()
      )
      | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__053_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
            error_source__066_ sexp__053_
      | Sexplib0.Sexp.List [] as sexp__053_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
            error_source__066_ sexp__053_
      : Sexplib0.Sexp.t -> parse
      )

  let _ = __parse_of_sexp__

  let parse_of_sexp =
    ( let error_source__076_ = "rules/lib/parse.ml.Action.parse" in
      fun sexp__075_ ->
        try __parse_of_sexp__ sexp__075_
        with Sexplib0.Sexp_conv_error.No_variant_match ->
          Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__076_
            sexp__075_
      : Sexplib0.Sexp.t -> parse
      )

  let _ = parse_of_sexp

  [@@@end]

  type t = [parse | `unknown of Sexplib0.Sexp.t]

  let t_of_sexp sexp =
    try (parse_of_sexp sexp :> t)
    with Sexplib0.Sexp_conv_error.Of_sexp_error _ -> `unknown sexp
end

module Rule = struct
  (* parses a dune rule.

    [((deps (...)) (targets (...)) (action (...)))]
    [((deps (...)) (targets (...)) (context ...) (action (...)))]

    See also [dune rules --help]
  *)

  (** [dune rules] output as an OCaml type *)
  type t = {
      deps: File.t list
    ; targets: Target.t list
    ; action: Action.t
    ; context: string option [@sexp.option]
  }
  [@@deriving_inline of_sexp]

  let _ = fun (_ : t) -> ()

  let t_of_sexp =
    ( let error_source__078_ = "rules/lib/parse.ml.Rule.t" in
      fun x__079_ ->
        Sexplib0.Sexp_conv_record.record_of_sexp ~caller:error_source__078_
          ~fields:
            (Field
               {
                 name= "deps"
               ; kind= Required
               ; conv= list_of_sexp File.t_of_sexp
               ; rest=
                   Field
                     {
                       name= "targets"
                     ; kind= Required
                     ; conv= list_of_sexp Target.t_of_sexp
                     ; rest=
                         Field
                           {
                             name= "action"
                           ; kind= Required
                           ; conv= Action.t_of_sexp
                           ; rest=
                               Field
                                 {
                                   name= "context"
                                 ; kind= Sexp_option
                                 ; conv= string_of_sexp
                                 ; rest= Empty
                                 }
                           }
                     }
               }
            )
          ~index_of_field:(function
            | "deps" ->
                0
            | "targets" ->
                1
            | "action" ->
                2
            | "context" ->
                3
            | _ ->
                -1
            )
          ~allow_extra_fields:false
          ~create:(fun (deps, (targets, (action, (context, ())))) : t ->
            {deps; targets; action; context}
          )
          x__079_
      : Sexplib0.Sexp.t -> t
      )

  let _ = t_of_sexp

  [@@@end]
end
