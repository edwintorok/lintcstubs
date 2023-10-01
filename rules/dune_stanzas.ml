(* See https://dune.readthedocs.io/en/stable/dune-files.html#rule
   This is not the full list of stanzas and options, just the ones that are used in this tool

   Cannot use record fields, because that would result in extra parenthesis when serialize by [ppx_sexp_conv]:
   [(targets (a b))] instead of [(targets a b)].
   Using lowercase polymorphic variants with the [@sexp.list] attribute ensures the correct output is created.
*)
open Sexplib0
open Sexp_conv

let is_atom = function Sexp.Atom _ -> true | Sexp.List _ -> false

(* records result in extra parenthesis, this function adjusts them *)
let rec fixup_sexp =
  let open Sexp in
  function
  | List [(Atom ("deps" | "targets" | "rule" | "progn") as atom); List args] ->
      List (atom :: list_map fixup_sexp args)
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

type fullpath

type basepath

type relativepath

module Filename : sig
  type +'a t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val v : Fpath.t -> fullpath t

  val split_base : fullpath t -> Fpath.t * basepath t

  val relative_to : root:Fpath.t -> fullpath t -> relativepath t

  val compare : 'a t -> 'a t -> int

  val pp : _ t Fmt.t

  module Map : Map.S with type key = fullpath t
end = struct
  type +'a t = Fpath.t

  let v p = p

  let sexp_of_t _ t = t |> Fpath.to_string |> Sexp_conv.sexp_of_string

  let split_base = Fpath.split_base

  let relative_to ~root path = Fpath.relativize ~root path |> Option.get

  let compare = Fpath.compare

  let pp = Fpath.pp

  module Map = Map.Make (Fpath)
end

let sexp_of_basepath _ = Sexp.List []
let sexp_of_relativepath _ = Sexp.List []
let sexp_of_fullpath _ = Sexp.List []

let sexp_of_any _ = Sexp.List []

module Target : sig
  type +'a t

  val v : Fpath.t list -> fullpath t

  val split_base : fullpath t -> Fpath.t * basepath t

  val compare : 'a t -> 'a t -> int

  val pp : _ t Fmt.t

  val to_pair :
    basepath t -> basepath Filename.t option * basepath Filename.t list
end = struct
  type 'a t = 'a Filename.t list

  let to_pair = function [one] -> (Some one, []) | lst -> (None, lst)

  let v lst = List.map Filename.v lst

  let split_base : fullpath t -> Fpath.t * basepath t =
   fun lst ->
    let targets =
      lst |> List.to_seq |> Seq.map Filename.split_base |> Fpath.Map.of_seq
    in
    match Fpath.Map.choose_opt targets with
    | None ->
        invalid_arg "attempting to split empty targets"
    | Some (base, p) ->
        if Fpath.Map.cardinal targets = 1 then
          (base, targets |> Fpath.Map.to_seq |> Seq.map snd |> List.of_seq)
        else
          Fmt.invalid_arg "No common base path for targets: %a"
            Fmt.(
              Dump.iter_bindings Fpath.Map.iter (any "map") Fpath.pp Filename.pp
            )
            targets

  let pp ppf t = Fmt.Dump.list Filename.pp ppf t

  let compare lst = List.compare Filename.compare lst
end

module TargetMap = Map.Make (struct
  type t = basepath Target.t

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
  type 'a t = [`include_ of 'a Filename.t]

  (* include is a keyword, needs manually written serializer *)
  let sexp_of_t _ (`include_ f) =
    Sexp.(List [Atom "include"; Filename.sexp_of_t sexp_of_any f])
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

  type 'a t =
    [ `file of 'a Filename.t
    | `alias of string
    | `alias_rec of string
    | `glob_files of Glob.t
    | `glob_files_rec of Glob.t
    | `source_tree of 'a Filename.t
    | `universe
    | `package of string
    | `env_var of string
    | `sandbox of sandbox
    | 'a Include.t ]
  [@@deriving_inline sexp_of]

  
let _ = fun (_ : 'a t) -> ()
  
let sexp_of_t : 'a . ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a__004_ ->
    function
    | `file v__005_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "file"; Filename.sexp_of_t _of_a__004_ v__005_]
    | `alias v__006_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "alias"; sexp_of_string v__006_]
    | `alias_rec v__007_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "alias_rec"; sexp_of_string v__007_]
    | `glob_files v__008_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "glob_files"; Glob.sexp_of_t v__008_]
    | `glob_files_rec v__009_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "glob_files_rec"; Glob.sexp_of_t v__009_]
    | `source_tree v__010_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "source_tree";
          Filename.sexp_of_t _of_a__004_ v__010_]
    | `universe -> Sexplib0.Sexp.Atom "universe"
    | `package v__011_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "package"; sexp_of_string v__011_]
    | `env_var v__012_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "env_var"; sexp_of_string v__012_]
    | `sandbox v__013_ ->
        Sexplib0.Sexp.List
          [Sexplib0.Sexp.Atom "sandbox"; sexp_of_sandbox v__013_]
    | #Include.t as v__014_ -> (Include.sexp_of_t _of_a__004_) v__014_
  let _ = sexp_of_t

  [@@@end]

  let map f (t: fullpath t) : relativepath t = match t with
    | `file  p -> `file (f p)
    | `source_tree p -> `source_tree (f p)
    | `include_ p -> `include_ (f p)
    | (`alias _ | `alias_rec _ | `glob_files _ | `glob_files_rec _ | `universe | `package _ | `env_var _ | `sandbox _) as t ->t 

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
  type 'a rule = {
      target: basepath Filename.t option [@sexp.option]
    ; targets: basepath Filename.t list [@sexp.list]
    ; deps: relativepath Dep.t list
    ; action: action option [@sexp.option]
    ; mode: Mode.t option [@sexp.option]
    ; alias: Alias.t option [@sexp.option]
  }
  [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a rule) -> ()

  
let sexp_of_rule : 'a . ('a -> Sexplib0.Sexp.t) -> 'a rule -> Sexplib0.Sexp.t
  =
  fun _of_a__015_ ->
    fun
      { target = target__017_; targets = targets__022_; deps = deps__025_;
        action = action__027_; mode = mode__031_; alias = alias__035_ }
      ->
      let bnds__016_ = ([] : _ Stdlib.List.t) in
      let bnds__016_ =
        match alias__035_ with
        | Stdlib.Option.None -> bnds__016_
        | Stdlib.Option.Some v__036_ ->
            let arg__038_ = Alias.sexp_of_t v__036_ in
            let bnd__037_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "alias"; arg__038_] in
            (bnd__037_ :: bnds__016_ : _ Stdlib.List.t) in
      let bnds__016_ =
        match mode__031_ with
        | Stdlib.Option.None -> bnds__016_
        | Stdlib.Option.Some v__032_ ->
            let arg__034_ = Mode.sexp_of_t v__032_ in
            let bnd__033_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "mode"; arg__034_] in
            (bnd__033_ :: bnds__016_ : _ Stdlib.List.t) in
      let bnds__016_ =
        match action__027_ with
        | Stdlib.Option.None -> bnds__016_
        | Stdlib.Option.Some v__028_ ->
            let arg__030_ = sexp_of_action v__028_ in
            let bnd__029_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "action"; arg__030_] in
            (bnd__029_ :: bnds__016_ : _ Stdlib.List.t) in
      let bnds__016_ =
        let arg__026_ =
          sexp_of_list (Dep.sexp_of_t sexp_of_relativepath) deps__025_ in
        ((Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "deps"; arg__026_]) ::
          bnds__016_ : _ Stdlib.List.t) in
      let bnds__016_ =
        if match targets__022_ with | [] -> true | _ -> false
        then bnds__016_
        else
          (let arg__024_ =
             (sexp_of_list (Filename.sexp_of_t sexp_of_basepath))
               targets__022_ in
           let bnd__023_ =
             Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "targets"; arg__024_] in
           (bnd__023_ :: bnds__016_ : _ Stdlib.List.t)) in
      let bnds__016_ =
        match target__017_ with
        | Stdlib.Option.None -> bnds__016_
        | Stdlib.Option.Some v__018_ ->
            let arg__020_ = Filename.sexp_of_t sexp_of_basepath v__018_ in
            let bnd__019_ =
              Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "target"; arg__020_] in
            (bnd__019_ :: bnds__016_ : _ Stdlib.List.t) in
      Sexplib0.Sexp.List bnds__016_
  let _ = sexp_of_rule

  [@@@end]

  type 'a t = [`rule of 'a rule] [@@deriving_inline sexp_of]

  let _ = fun (_ : 'a t) -> ()

  
let sexp_of_t : 'a . ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a__039_ ->
    fun (`rule v__040_) ->
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "rule"; sexp_of_rule _of_a__039_ v__040_]
  let _ = sexp_of_t

  [@@@end]

  let pp = Fmt.using (sexp_of_t sexp_of_any) Sexp.pp_hum
end

type alias = Alias.t

type target = fullpath Target.t

type dep = fullpath Dep.t

type glob = Glob.t

type t = basepath Rule.t TargetMap.t Fpath.Map.t

let dep_file f : dep = `file (Filename.v f)

let dep_glob_ext ?(recurse = false) glob : dep =
  let g = Glob.of_ext glob in
  if recurse then
    `glob_files_rec g
  else
    `glob_files g

let target = Target.v

let empty = Fpath.Map.empty

let rule ?mode ?alias orig_target deps action : t =
  let base, orig_target = Target.split_base orig_target in
  let target, targets = Target.to_pair orig_target in
  let rule : _ Rule.t =
    `rule Rule.{target; targets; deps = List.map (Dep.map (Filename.relative_to ~root:base)) deps; action= Some action; mode; alias}
  in
  Fpath.Map.singleton base @@ TargetMap.singleton orig_target rule

let merge : t -> t -> t =
  Fpath.Map.union @@ fun _ t1 t2 ->
  Some
    (TargetMap.union
       (fun target r1 r2 ->
         Fmt.invalid_arg "Duplicate rules for target %a: %a, %a" Target.pp
           target Rule.pp r1 Rule.pp r2
       )
       t1 t2
    )

module Subdir = struct
  type t = [`subdir] * fullpath Filename.t * basepath Rule.t
  [@@deriving_inline sexp_of]

  let _ = fun (_ : t) -> ()

  
let sexp_of_t =
  (fun (arg0__041_, arg1__042_, arg2__043_) ->
     let res0__044_ = let `subdir = arg0__041_ in Sexplib0.Sexp.Atom "subdir"
     and res1__045_ = Filename.sexp_of_t sexp_of_fullpath arg1__042_
     and res2__046_ = Rule.sexp_of_t sexp_of_basepath arg2__043_ in
     Sexplib0.Sexp.List [res0__044_; res1__045_; res2__046_] : t ->
                                                                 Sexplib0.Sexp.t)

  let _ = sexp_of_t

  [@@@end]
end

let subdir base rules : Subdir.t = (`subdir, base, rules)

let sexp_list_of_t t =
  let rules =
    t
    |> Fpath.Map.to_seq
    |> Seq.flat_map @@ fun (base, targets) ->
       let target_rules : basepath Rule.t Seq.t =
         targets |> TargetMap.to_seq |> Seq.map snd
       in
       if Fpath.is_current_dir base then
         target_rules |> Seq.map (Rule.sexp_of_t sexp_of_any)
       else
         target_rules |> Seq.map (subdir @@ Filename.v base) |> Seq.map Subdir.sexp_of_t
  in
  rules |> Seq.map fixup_sexp |> List.of_seq
