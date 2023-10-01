(* See https://dune.readthedocs.io/en/stable/dune-files.html#rule
   This is not the full list of stanzas and options, just the ones that are used in this tool

   Cannot use record fields, because that would result in extra parenthesis when serialize by [ppx_sexp_conv]:
   [(targets (a b))] instead of [(targets a b)].
   Using lowercase polymorphic variants with the [@sexp.list] attribute ensures the correct output is created.
*)
open Sexplib0
open Sexp_conv

(* records result in extra parenthesis, this function adjusts them *)
let rec fixup_sexp =
  let open Sexp in
  function
  | List [Atom atom; List args] ->
      List (Atom atom :: list_map fixup_sexp args)
  | Atom _ as s ->
      s
  | List l ->
      List (list_map fixup_sexp l)

type 'a with_stdout_to_target = [`with_stdout_to_target of 'a]

let sexp_of_with_stdout_to_target sexp_of_el (`with_stdout_to_target act) =
  Sexp.(List [Atom "with-stdout-to"; Atom "%{target}"; sexp_of_el act])

type action =
  [ `run of string list [@sexp.list]
  | `progn of action list
  | action with_stdout_to_target ]
[@@deriving_inline sexp_of]

let _ = fun (_ : action) -> ()

let rec sexp_of_action =
  ( function
    | `run l__001_ ->
        Sexplib0.Sexp.List
          (Sexplib0.Sexp.Atom "run"
          :: Sexplib0.Sexp_conv.list_map sexp_of_string l__001_
          )
    | `progn v__002_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "progn"; sexp_of_list sexp_of_action v__002_]
    | #with_stdout_to_target as v__003_ ->
        (sexp_of_with_stdout_to_target sexp_of_action) v__003_
    : action -> Sexplib0.Sexp.t
    )

let _ = sexp_of_action

[@@@end]

let run prog args : action = `run (prog :: args)

let progn lst : action = `progn lst

let with_stdout_to_target action : action = `with_stdout_to_target action

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

  val v : Fpath.t list -> fullpath t

  val sexp_of_basepath: basepath -> Sexp.t

  val split_base : fullpath t -> Fpath.t * basepath t

  val compare : 'a t -> 'a t -> int

  val pp: _ t Fmt.t

  val to_pair: basepath t -> Filename.t option * Filename.t list
end = struct
  type fullpath

  type basepath

  let sexp_of_basepath _ = Sexp.List []

  type 'a t = Filename.t list

  let to_pair = function
    | [one] -> Some one, []
    | lst -> None, lst

  let v lst = lst

  let split_base : fullpath t  -> Fpath.t * basepath t = fun lst ->
        let targets =
          lst |> List.to_seq |> Seq.map Fpath.split_base |> Filename.Map.of_seq
        in
        if Filename.Map.cardinal targets = 1 then
          let base, p = Filename.Map.choose targets in
          (base, targets |> Filename.Map.to_seq |> Seq.map snd |> List.of_seq)
        else
          Fmt.invalid_arg "No common base path for targets: %a"
            Fmt.(
              Dump.iter_bindings Filename.Map.iter (any "map") Fpath.pp Fpath.pp
            )
            targets

  let pp = Fmt.Dump.list Fpath.pp

  let compare = List.compare Fpath.compare
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
    [ `file of Filename.t
    | `alias of string
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
  (function
   | `file v__004_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "file"; Filename.sexp_of_t v__004_]
   | `alias v__005_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "alias"; sexp_of_string v__005_]
   | `alias_rec v__006_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "alias_rec"; sexp_of_string v__006_]
   | `glob_files v__007_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "glob_files"; Glob.sexp_of_t v__007_]
   | `glob_files_rec v__008_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "glob_files_rec"; Glob.sexp_of_t v__008_]
   | `source_tree v__009_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "source_tree"; Filename.sexp_of_t v__009_]
   | `universe -> Sexplib0.Sexp.Atom "universe"
   | `package v__010_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "package"; sexp_of_string v__010_]
   | `env_var v__011_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "env_var"; sexp_of_string v__011_]
   | `sandbox v__012_ ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp.Atom "sandbox"; sexp_of_sandbox v__012_]
   | #Include.t as v__013_ -> Include.sexp_of_t v__013_ : t ->
                                                            Sexplib0.Sexp.t)
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

module Alias = struct
  type t = string [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)

  let _ = sexp_of_t

  [@@@end]
end

module Rule = struct
  type 'a rule =
    { target: Filename.t option [@sexp.option]
    ; targets: Filename.t list [@sexp.list]
    ; deps: Dep.t list
    ; action: action option [@sexp.option]
    ; mode: Mode.t option [@sexp.option]
    ; alias: Alias.t option [@sexp.option]
  }
  [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a rule) -> ()

  
let sexp_of_rule : 'a . ('a -> Sexplib0.Sexp.t) -> 'a rule -> Sexplib0.Sexp.t
  =
  fun _of_a__014_ ->
    fun
      { target = target__016_; targets = targets__021_; deps = deps__024_;
        action = action__026_; mode = mode__030_; alias = alias__034_ }
      ->
      let bnds__015_ = ([] : _ Stdlib.List.t) in
      let bnds__015_ =
        match alias__034_ with
        | Stdlib.Option.None -> bnds__015_
        | Stdlib.Option.Some v__035_ ->
            let arg__037_ = Alias.sexp_of_t v__035_ in
            let bnd__036_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "alias"; arg__037_] in
            (bnd__036_ :: bnds__015_ : _ Stdlib.List.t) in
      let bnds__015_ =
        match mode__030_ with
        | Stdlib.Option.None -> bnds__015_
        | Stdlib.Option.Some v__031_ ->
            let arg__033_ = Mode.sexp_of_t v__031_ in
            let bnd__032_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "mode"; arg__033_] in
            (bnd__032_ :: bnds__015_ : _ Stdlib.List.t) in
      let bnds__015_ =
        match action__026_ with
        | Stdlib.Option.None -> bnds__015_
        | Stdlib.Option.Some v__027_ ->
            let arg__029_ = sexp_of_action v__027_ in
            let bnd__028_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "action"; arg__029_] in
            (bnd__028_ :: bnds__015_ : _ Stdlib.List.t) in
      let bnds__015_ =
        let arg__025_ = sexp_of_list Dep.sexp_of_t deps__024_ in
        ((Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "deps"; arg__025_]) ::
          bnds__015_ : _ Stdlib.List.t) in
      let bnds__015_ =
        if match targets__021_ with | [] -> true | _ -> false
        then bnds__015_
        else
          (let arg__023_ = (sexp_of_list Filename.sexp_of_t) targets__021_ in
           let bnd__022_ =
             Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "targets"; arg__023_] in
           (bnd__022_ :: bnds__015_ : _ Stdlib.List.t)) in
      let bnds__015_ =
        match target__016_ with
        | Stdlib.Option.None -> bnds__015_
        | Stdlib.Option.Some v__017_ ->
            let arg__019_ = Filename.sexp_of_t v__017_ in
            let bnd__018_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "target"; arg__019_] in
            (bnd__018_ :: bnds__015_ : _ Stdlib.List.t) in
      Sexplib0.Sexp.List bnds__015_
  let _ = sexp_of_rule

  [@@@end]

  type 'a t = [`rule of 'a rule] [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a t) -> ()

  
let sexp_of_t : 'a . ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a__038_ ->
    fun (`rule v__039_) ->
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "rule"; sexp_of_rule _of_a__038_ v__039_]
  let _ = sexp_of_t

  [@@@end]

  let pp = Fmt.using (sexp_of_t sexp_of_any) Sexp.pp_hum
end

type alias = Alias.t

type target = Target.fullpath Target.t

type dep = Dep.t

type glob = Glob.t

type t = Target.basepath Rule.t TargetMap.t Filename.Map.t

let dep_file f : dep = `file f

let dep_glob_ext ?(recurse = false) glob : dep =
  let g = Glob.of_ext glob in
  if recurse then
    `glob_files_rec g
  else
    `glob_files g

let target = Target.v

let empty = Filename.Map.empty

let rule ?mode ?alias orig_target deps action : t =
  let base, orig_target = Target.split_base orig_target in
  let target, targets = Target.to_pair orig_target in
  let rule : _ Rule.t =
    `rule Rule.{target;targets; deps; action= Some action; mode; alias}
  in
  Filename.Map.singleton base @@ TargetMap.singleton orig_target rule

let alias_deps name deps : t =
  let rule : Target.basepath Rule.t =
    `rule {target= None;targets=[]; deps; action= None; mode= None; alias= Some name}
  in
  let base, target = Target.split_base (target [Fpath.v name]) in
  Filename.Map.singleton base @@ TargetMap.singleton target rule

let merge : t -> t -> t =
  Filename.Map.union @@ fun _ t1 t2 ->
  Some
    (TargetMap.union
       (fun target r1 r2 ->
         Fmt.invalid_arg "Duplicate rules for target %a: %a, %a"
           Target.pp target Rule.pp r1 Rule.pp r2
       )
       t1 t2
    )

module Subdir = struct
  type t = [`subdir] * Filename.t * Target.basepath Rule.t list
  [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  
let sexp_of_t =
  (fun (arg0__040_, arg1__041_, arg2__042_) ->
     let res0__043_ = let `subdir = arg0__040_ in Sexplib0.Sexp.Atom "subdir"
     and res1__044_ = Filename.sexp_of_t arg1__041_
     and res2__045_ =
       sexp_of_list (Rule.sexp_of_t Target.sexp_of_basepath) arg2__042_ in
     Sexplib0.Sexp.List [res0__043_; res1__044_; res2__045_] : t ->
                                                                 Sexplib0.Sexp.t)

  let _ = sexp_of_t

  [@@@end]
end

let subdir base rules : Subdir.t = (`subdir, base, rules)

let sexp_list_of_t t =
  let rules =
    t
    |> Filename.Map.to_seq
    |> Seq.flat_map @@ fun (base, targets) ->
       let target_rules : Target.basepath Rule.t Seq.t =
         targets |> TargetMap.to_seq |> Seq.map snd
       in
       if Fpath.is_current_dir base then
         target_rules |> Seq.map (Rule.sexp_of_t sexp_of_any)
       else
         Seq.return
         @@ Subdir.sexp_of_t
         @@ subdir base
         @@ List.of_seq target_rules
  in
  rules |> Seq.map fixup_sexp |> List.of_seq
