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

open GoblintCil
open Analyses
open! Cilint

(* M.tracing is not enabled by default in upstream goblint build for
   performance reasons, use a boolean to turn tracing on/off just for this
   module.

   Usage on the command line: '--enable warn.debug'
*)
let trace_name = __MODULE__

let tracing_enabled = lazy (GobConfig.get_bool "warn.debug")

let tracing () = Lazy.force tracing_enabled

let tracel fmt = M.tracel trace_name (fmt ^^ "\n")

module DomainLock = struct
  (* This simulates OCaml 4.x semantics with a single global lock,
      it should instead be configurable to use per-domain locks (e.g. N threads with M domains)
  *)
  let runtime_lock_var =
    let g = ref None in
    fun () ->
      match !g with
      | Some v ->
          v
      | None -> (
          let k = "__VERIFIER_ocaml_runtime_lock" in
          match VarQuery.varqueries_from_names !Cilfacade.current_file [k] with
          | [VarQuery.Global v], _ ->
              g := Some v ;
              v
          | _ ->
              let v = Cilfacade.create_var @@ makeGlobalVar k intType in
              g := Some v ;
              v
        )

  let runtime_lock_event () = Goblint_lib.LockDomain.Addr.of_var @@ runtime_lock_var ()

  let runtime_lock () = AddrOf (Cil.var @@ runtime_lock_var ())

  let must_be_held ctx what name =
    let lockset = ctx.ask Queries.MustLockset in
    if tracing () then
      tracel "OCaml domain lock must be held, current lockset is %a"
        Queries.LS.pretty lockset ;
    if not @@ Queries.LS.mem (runtime_lock_var (), `NoOffset) lockset then
      (* we could use something similar to MayLocks to track may lock and give
         a better warning message: is the lock maybe held on some paths, or
         surely not held? *)
      Messages.error ~category:Messages.Category.Race
        "DomainLock: must be held when %s %s" what name ;
    ctx.local

  let must_be_protected_by ctx write (arg : varinfo) =
    if tracing () then
      tracel
        "OCaml domain lock must protect access to OCaml value %s (write=%b)"
        arg.vname write ;
    let must =
      ctx.ask
        Queries.(
          MustBeProtectedBy {mutex= runtime_lock_event (); write; global= arg;protection=Protection.Strong}
        )
    in
    if not must then
      Messages.error ~category:Messages.Category.Race
        "DomainLock: must be held when dereferencing OCaml value %s" arg.vname ;
    if tracing () then
      tracel
        "OCaml domain lock must protect access to OCaml value %s (write=%b, \
         must = %b)"
        arg.vname write must ;
    (* sometimes the must above answers true even if the domain lock is not
       held? *)
    must_be_held ctx "dereferencing OCaml value" arg.vname ;
    (* TODO: this should say accessing OCaml value,
       not runtime function *)
    ctx.local
end

let size_of_word = SizeOf voidPtrType

let plus1 exp = constFoldBinOp true PlusA exp (kinteger IULong 1) ulongType

let plus_word exp = constFoldBinOp true PlusA exp size_of_word ulongType

let caml_alloc count =
  LibraryDesc.Calloc {count= plus1 count; size= size_of_word}

(* uninit return *)
let caml_malloc count =
  LibraryDesc.Malloc
    (constFoldBinOp true Mult (plus1 count) size_of_word ulongType)

let cstubs = ref []

module Cstub = struct
  let call_caml_runtime ctx f _arglist =
    DomainLock.must_be_held ctx "calling OCaml runtime function" f.vname ;
    ctx.local
end

let is_ocaml_value_type = function
  | TNamed ({tname= "value"; _}, _) ->
      true
  | _ ->
      false

class exp_ocaml_value_extractor (acc : varinfo list ref) =
  object
    inherit nopCilVisitor

    method! vvrbl v =
      if tracing () then
        tracel "checking value use %s, type %a" v.vname Cil.d_type v.vtype ;
      if is_ocaml_value_type v.vtype then (
        acc := v :: !acc ;
        SkipChildren
      ) else
        DoChildren
  end

let ocaml_values_of_exp exp =
  let values = ref [] in
  let visitor = new exp_ocaml_value_extractor values in
  let (_ : exp) = visitCilExpr visitor exp in
  !values

class exp_ocaml_value_deref_extractor (acc : varinfo list ref) =
  object
    inherit nopCilVisitor

    method! vlval =
      function
      | Mem exp, _ ->
          if tracing () then
            tracel "checking exp %a" Cil.d_exp exp ;
          let ocaml_values = ocaml_values_of_exp exp in
          acc := List.rev_append ocaml_values !acc ;
          DoChildren
      | _ ->
          DoChildren
  end

let ocaml_value_derefs_of_exp exp =
  let values = ref [] in
  let visitor = new exp_ocaml_value_deref_extractor values in
  let (_ : exp) = visitCilExpr visitor exp in
  !values

class init_visitor ask acc =
  object
    inherit nopCilVisitor

    method! vinit _ _ =
      function
      | SingleInit e ->
          let typ = typeOf e in
          if tracing () then
            tracel "initializer %a (type %a)" Cil.d_exp e Cil.d_type typ ;
          if isFunctionType typ then (
            let lvals = ask Queries.(MayPointTo e) in
            if tracing () then
              tracel "initializer %a may point to %a" Cil.d_exp e
                Queries.LS.pretty lvals ;
            acc := List.rev_append (Queries.LS.elements lvals) !acc
          ) ;
          SkipChildren
      | CompoundInit _ ->
          DoChildren
  end

let rec function_ptrs_of_init acc = function
  | SingleInit e ->
      e :: acc
  | CompoundInit (_, lst) ->
      lst |> List.map snd |> List.fold_left function_ptrs_of_init acc

module VS = Set.Make (CilType.Varinfo)

let is_ocaml_value varinfo = is_ocaml_value_type varinfo.vtype

(** [value_parameters_variables f] is the set of local variables and parameters
    of type 'value' *)
let value_parameters_variables (f : fundec) =
  Seq.(append (f.sformals |> List.to_seq) (f.slocals |> List.to_seq))
  |> Seq.filter is_ocaml_value
  |> VS.of_seq

let ocaml_params_globals =
  lazy
    (let param0, param1 = (ref None, ref None) in
     Cil.iterGlobals !Cilfacade.current_file (function
       | GFun (g, _) -> (
         match g.svar.vname with
         | "__VERIFIER_camlparam0" ->
             param0 :=
               Some (g.slocals |> List.filter (fun v -> v.vname = "caml__frame"))
         | "__VERIFIER_camlparam1" ->
             param1 := Some g
         | _ ->
             ()
       )
       | _ ->
           ()
       ) ;
     match (!param0, !param1) with
     | Some p0, Some p1 ->
         (p0, p1)
     | _ ->
         failwith "Missing __VERIFIER_ocaml_param{0,1} in runtime.model.c"
    )

let caml_state =
  lazy
    ( match
        VarQuery.varqueries_from_names !Cilfacade.current_file ["Caml_state"]
      with
    | [VarQuery.Global v], _ ->
        v
    | _ ->
        failwith "Missing Caml_state"
    )

let d_varinfo () (v : varinfo) = Pretty.dprintf "%s" v.vname

let error_CAMLparam = Messages.Category.(Behavior (Undefined UseAfterFree))

let assert_begins_with_CAMLparam0 f =
  let caml_frame, _ = Lazy.force ocaml_params_globals in
  let similar_varinfo v1 v2 =
    String.equal v1.vname v2.vname && CilType.Typ.equal v1.vtype v2.vtype
  in
  let preamble = Batteries.List.take (List.length caml_frame) f.slocals in
  if
    false (* TODO: enable/disable with a flag, for now too strict *)
    && not (List.equal similar_varinfo preamble caml_frame)
  then
    (* TODO: relax this if the value is not actually used or we don't call
       functions that may call the GC... *)
    Messages.warn ~category:error_CAMLparam
      "Missing CAMLparam call in function containing 'value' typed \
       parameters/locals: the garbage collector may move these, and they must \
       be registered: preamble: %a"
      Pretty.(d_list "varinfo" d_varinfo)
      preamble

module Rules = struct
  (* see https://v2.ocaml.org/manual/intfc.html#ss:c-simple-gc-harmony *)

  let gc_rule_1 _ctx (f : fundec) =
    let values = value_parameters_variables f in
    (*  "A function that has parameters or local variables of type value" *)
    let has_values = not @@ VS.is_empty values in
    (* must begin with a call to one of the CAMLparam macros *)
    if has_values then
      assert_begins_with_CAMLparam0 f
  (* TODO: assert all value types are registered with the GC once! *)
end

module Spec : Analyses.MCPSpec = struct
  let name () = "ocamlcstubs"

  module D = Lattice.Unit
  module C = D

  let startstate _v = D.bot ()

  let exitstate _v = D.top ()

  include Analyses.IdentitySpec

  let body ctx (f : fundec) =
    Rules.gc_rule_1 ctx f ;
    (* TODO: set ctx bool that we're inside cstub, to avoid false positives on
       runtime inline functions *)
    ctx.local

  let return ctx _ (_f : fundec) = ctx.local

  let special (ctx : (D.t, G.t, C.t, V.t) ctx) (_lval : lval option)
      (f : varinfo) (arglist : exp list) =
    if tracing () then
      tracel "special(%s)" f.vname ;
    match f.vname with
    | "caml_stat_free" ->
        (* does not require runtime lock to be held! *)
        ctx.local
    | "caml_leave_blocking_section" ->
        ctx.local
    | "caml_alloc_custom" ->
        let local = Cstub.call_caml_runtime ctx f arglist in
        (* the argument may not be an immediate pointer to a global,
           query the points-to analyses on where it actually points to *)
        let custom_ops = ctx.ask Queries.(MayPointTo (List.nth arglist 0)) in
        if tracing () then
          tracel "caml_alloc_custom points to %a" Queries.LS.pretty custom_ops ;
        let () =
          if not @@ Queries.LS.is_top custom_ops then (
            (* it points somewhere, all the function pointers in that struct's
               initializer should be treated as C stubs
               therefore this should be a separate analysis that just determines
               whether it is a C stub or not that runs before this one....
               this may be a global, but not necessarily
            *)
            custom_ops
            |> Queries.LS.iter @@ function
               | {vinit= {init= None}; _}, _ ->
                   ()
               | {vinit= {init= Some init}; _}, _ ->
                   let funptrs =
                     init
                     |> function_ptrs_of_init []
                     |> List.map @@ fun exp -> ctx.ask (Queries.MayPointTo exp)
                   in
                   if tracing () then
                     tracel "found function pointers: %a"
                       (Pretty.d_list "," Queries.LS.pretty)
                       funptrs ;
                   funptrs
                   |> List.iter @@ fun funptr ->
                      let new_stubs =
                        funptr
                        |> Queries.LS.elements
                        |> List.map (fun (fn, _) -> fn.vname)
                      in
                      cstubs := List.rev_append new_stubs !cstubs
          )
        in
        (* TODO: find functions in struct and register as C stub roots... *)
        local
    | n when String.starts_with n ~prefix:"caml_" ->
        (* call into OCaml runtime system, must hold domain lock *)
        Cstub.call_caml_runtime ctx f arglist
    | _ ->
        ctx.local

  let event ctx e _octx =
    match e with
    | Events.Access {exp; kind= AccessKind.(Read | Write) as kind; reach; _} ->
        (* TODO: only for pointers *)
        if tracing () then
          tracel "access %a, kind %a, reach %b" Cil.d_exp exp AccessKind.pretty
            kind reach ;
        (* TODO: reject free and spawn kinds? *)
        exp
        |> ocaml_value_derefs_of_exp
        |> List.iter
           @@ DomainLock.must_be_protected_by ctx (kind = AccessKind.Write) ;
        ctx.local
    | _ ->
        ctx.local
end

let dep =
  [
    ThreadEscape.Spec.name ()
    (* without everything that gets its address taken is considered global *)
  ; AccessAnalysis.Spec.name () (* for Events.Access *)
  ; MutexAnalysis.Spec.name ()
    (* for Queries.{MustLockset, MustBeProtectedBy} *)
  ; MutexEventsAnalysis.Spec.name () (* for Events.Lock *)
  ; (let module M = (val Base.get_main ()) in
    M.name ()
    )
    (* for Queries.MayPointTo *)
  ]

let () =
  (*LibraryFunctions.register_library_functions ocaml_runtime_functions ;*)
  (* have to declare dependencies on analyses that can provide answers to
     the [ctx.ask Queries] and that generate the [Events] we need
  *)
  MCP.register_analysis ~dep (module Spec : MCPSpec)
