(* See https://dune.readthedocs.io/en/stable/dune-files.html#rule
   This is not the full list of stanzas and options, just the ones that are used in this tool

   Cannot use record fields, because that would result in extra parenthesis when serialize by [ppx_sexp_conv]:
   [(targets (a b))] instead of [(targets a b)].
   Using lowercase polymorphic variants with the [@sexp.list] attribute ensures the correct output is created.
*)
open Sexplib0
open Sexp_conv

module Action = struct
  type t = [`action] [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    (fun `action -> Sexplib0.Sexp.Atom "action" : t -> Sexplib0.Sexp.t)

  let _ = sexp_of_t

  [@@@end]
end

module Filename = struct
  type t = Fpath.t

  let sexp_of_t t = t |> Fpath.to_string |> Sexp_conv.sexp_of_string

  module Map = Map.Make (Fpath)
end

let sexp_of_any _ = Sexp.List []

module Target : sig
  type fullpath

  type basepath

  type 'a t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val sexp_of_fullpath : fullpath -> Sexp.t

  val sexp_of_basepath : basepath -> Sexp.t

  val split_base : fullpath t -> Fpath.t * basepath t

  val compare : 'a t -> 'a t -> int

  val pp : 'a t Fmt.t
end = struct
  type fullpath

  type basepath

  let sexp_of_fullpath = sexp_of_any

  let sexp_of_basepath = sexp_of_any

  type 'a t = [`target of Filename.t | `targets of Filename.t list [@sexp.list]]
  [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
   fun _of_a__001_ -> function
    | `target v__002_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "target"; Filename.sexp_of_t v__002_]
    | `targets l__003_ ->
        Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom "targets"
          :: Sexplib0.Sexp_conv.list_map Filename.sexp_of_t l__003_
          )

  let _ = sexp_of_t

  [@@@end]

  let split_base : fullpath t -> Fpath.t * basepath t = function
    | `target p ->
        let base, p = Fpath.split_base p in
        (base, `target p)
    | `targets lst ->
        let targets =
          lst |> List.to_seq |> Seq.map Fpath.split_base |> Filename.Map.of_seq
        in
        if Filename.Map.cardinal targets = 1 then
          let base, p = Filename.Map.choose targets in
          (base, `target p)
        else
          Fmt.invalid_arg "No common base path for targets: %a"
            Fmt.(
              Dump.iter_bindings Filename.Map.iter (any "map") Fpath.pp Fpath.pp
            )
            targets

  let compare a b =
    match (a, b) with
    | `target f1, `target f2 ->
        Fpath.compare f1 f2
    | `targets l1, `targets l2 ->
        List.compare Fpath.compare l1 l2
    | `target _, `targets _ ->
        -1
    | `targets _, `target _ ->
        1

  let pp = Fmt.using (sexp_of_t sexp_of_any) Sexp.pp_hum
end

module TargetMap = Map.Make (struct
  type t = Target.basepath Target.t

  let compare = Target.compare
end)

module Glob = struct
  type t = string [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)

  let _ = sexp_of_t

  [@@@end]

  let of_ext ext = "*." ^ ext
end

module Include = struct
  type t = [`include_ of Filename.t]

  (* include is a keyword, needs manually written serializer *)
  let sexp_of_t (`include_ f) =
    Sexp.(List [Atom "include"; Filename.sexp_of_t f])
end

module Dep = struct
  type sandbox = [`always | `none | `preserve_file_kind]
  [@@deriving_inline sexp_of]

  let _ = fun (_ : sandbox) -> ()

  let sexp_of_sandbox =
    ( function
      | `always ->
          Sexplib0.Sexp.Atom "always"
      | `none ->
          Sexplib0.Sexp.Atom "none"
      | `preserve_file_kind ->
          Sexplib0.Sexp.Atom "preserve_file_kind"
      : sandbox -> Sexplib0.Sexp.t
      )

  let _ = sexp_of_sandbox

  [@@@end]

  type t =
    [ `alias of string
    | `alias_rec of string
    | `glob_files of Glob.t
    | `glob_files_rec of Glob.t
    | `source_tree of Filename.t
    | `universe
    | `package of string
    | `env_var of string
    | `sandbox of sandbox
    | Include.t ]
  [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    ( function
      | `alias v__004_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "alias"; sexp_of_string v__004_]
      | `alias_rec v__005_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "alias_rec"; sexp_of_string v__005_]
      | `glob_files v__006_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "glob_files"; Glob.sexp_of_t v__006_]
      | `glob_files_rec v__007_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "glob_files_rec"; Glob.sexp_of_t v__007_]
      | `source_tree v__008_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "source_tree"; Filename.sexp_of_t v__008_]
      | `universe ->
          Sexplib0.Sexp.Atom "universe"
      | `package v__009_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "package"; sexp_of_string v__009_]
      | `env_var v__010_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "env_var"; sexp_of_string v__010_]
      | `sandbox v__011_ ->
          Sexplib0.Sexp.List
            [Sexplib0.Sexp.Atom "sandbox"; sexp_of_sandbox v__011_]
      | #Include.t as v__012_ ->
          Include.sexp_of_t v__012_
      : t -> Sexplib0.Sexp.t
      )

  let _ = sexp_of_t

  [@@@end]
end

module Deps = struct
  type t = [`deps of Dep.t list [@sexp.list]] [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    ( fun (`deps l__013_) ->
        Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom "deps"
          :: Sexplib0.Sexp_conv.list_map Dep.sexp_of_t l__013_
          )
      : t -> Sexplib0.Sexp.t
      )

  let _ = sexp_of_t

  [@@@end]
end

module Mode = struct
  type t = [`standard | `fallback | `promote] [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    ( function
      | `standard ->
          Sexplib0.Sexp.Atom "standard"
      | `fallback ->
          Sexplib0.Sexp.Atom "fallback"
      | `promote ->
          Sexplib0.Sexp.Atom "promote"
      : t -> Sexplib0.Sexp.t
      )

  let _ = sexp_of_t

  [@@@end]
end

module Rule = struct
  type 'a t =
    ([`rule] * 'a Target.t * Deps.t * Action.t * Mode.t option[@sexp.option])
  [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
   fun _of_a__014_ (arg0__015_, arg1__016_, arg2__017_, arg3__018_, arg4__019_) ->
    let res0__020_ =
      let `rule = arg0__015_ in
      Sexplib0.Sexp.Atom "rule"
    and res1__021_ = Target.sexp_of_t _of_a__014_ arg1__016_
    and res2__022_ = Deps.sexp_of_t arg2__017_
    and res3__023_ = Action.sexp_of_t arg3__018_
    and res4__024_ = sexp_of_option Mode.sexp_of_t arg4__019_ in
    Sexplib0.Sexp.List
      [res0__020_; res1__021_; res2__022_; res3__023_; res4__024_]

  let _ = sexp_of_t

  [@@@end]

  let pp = Fmt.using (sexp_of_t sexp_of_any) Sexp.pp_hum
end

type t = Target.basepath Rule.t TargetMap.t Filename.Map.t

let rule ?mode target deps action : t =
  let base, target = Target.split_base target in
  Filename.Map.singleton base
  @@ TargetMap.singleton target (`rule, target, deps, action, mode)

let merge : t -> t -> t =
  Filename.Map.union @@ fun _ t1 t2 ->
  Some
    (TargetMap.union
       (fun target r1 r2 ->
         Fmt.invalid_arg "Duplicate rules for target %a: %a, %a" Target.pp
           target Rule.pp r1 Rule.pp r2
       )
       t1 t2
    )

module Subdir = struct
  type t = [`subdir] * Filename.t * Target.basepath Rule.t list
  [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t =
    ( fun (arg0__025_, arg1__026_, arg2__027_) ->
        let res0__028_ =
          let `subdir = arg0__025_ in
          Sexplib0.Sexp.Atom "subdir"
        and res1__029_ = Filename.sexp_of_t arg1__026_
        and res2__030_ =
          sexp_of_list (Rule.sexp_of_t Target.sexp_of_basepath) arg2__027_
        in
        Sexplib0.Sexp.List [res0__028_; res1__029_; res2__030_]
      : t -> Sexplib0.Sexp.t
      )

  let _ = sexp_of_t

  [@@@end]
end

let subdir base rules : Subdir.t = (`subdir, base, rules)

let sexp_of_t t =
  let rules =
    t
    |> Filename.Map.to_seq
    |> Seq.flat_map @@ fun (base, targets) ->
       let target_rules : Target.basepath Rule.t Seq.t =
         targets |> TargetMap.to_seq |> Seq.map snd
       in
       if Fpath.is_current_dir base then
         target_rules
       else
         Seq.return @@ subdir base @@ List.of_seq target_rules
  in
  Sexp.List (rules |> Seq.map (Rule.sexp_of_t sexp_of_any) |> List.of_seq)
