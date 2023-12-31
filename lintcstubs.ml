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

open Goblint_lib

let rec activate name =
  let id = MCPRegistry.find_id name in
  let deps = (MCPRegistry.find_spec id).dep in
  List.iter activate deps ;
  GobConfig.set_auto "ana.activated[+]" name

let find_stub_source ~stubdirs name =
  stubdirs
  |> List.map (fun dir -> Fpath.append dir name)
  |> List.find_all (fun path -> path |> Fpath.to_string |> Sys.file_exists)

(** [set_default_flags ()] initializes goblint with flags suitable for parsing OCaml C stubs *)
let set_default_flags () =
  (* all the flag names are documented in the JSON schema at:
      https://github.com/goblint/analyzer/blob/v2.1.0/src/util/options.schema.json

     list options can modified by adding with [+] or removing with [-]
  *)
  let open GobConfig in
  (* workaround for incomplete C11 support, CIL only implements the GCC
     attribute, not the C11 one:
     https://github.com/goblint/cil/issues/13#issuecomment-1359176037

     [pre.cppflags]: Pre-processing parameters (that you'd pass to [cpp])
  *)
  set_auto "pre.cppflags[+]" "-D_Alignas(x)=__attribute__((__aligned__(x)))" ;

  (* activate our own analyses

     [ana.activated]: List of activated analyses
  *)
  activate @@ Lintcstubs_analysis.Ocamlcstubs.Spec.name () ;

  (* do not disable multithreaded analysis, even though there are no thread
     creations in sight: we want to treat stubs as multi-threaded

     [ana.autotune.activated]: List of activated tuning options. By default all
     are activated.
  *)
  set_auto "ana.autotune.activated[-]" "singleThreaded" ;

  (* OCaml semantics: -fwrapv, C11 *)
  set_auto "sem.int.signed_overflow" "assume_wraparound" ;

  (* next version of goblint: et_auto "cil.cstd" "c11"; *)

  (* too many messages about successful assertions otherwise *)
  set_auto "dbg.regression" "true" ;

  (* OCaml runtime model - needed so we know what locks/unlocks the runtime
     lock
  *)
  let stubdirs = List.map Fpath.v Goblint_sites.lib_stub_src in
  match find_stub_source ~stubdirs Fpath.(v "ocaml_runtime.model.c") with
  | [] ->
      Fmt.failwith "OCaml runtime model not found in %a"
        Fmt.Dump.(list Fpath.pp)
        stubdirs
  | one :: _ ->
      set_auto "files[+]" @@ Fpath.to_string one

(** [enable_tracing_if_needed ()] enables tracing messages in our analyses
  if enabled on the CLI with [dbg.debug].
 *)
let enable_tracing_if_needed () =
  if Lintcstubs_analysis.Ocamlcstubs.tracing () then
    Tracing.addsystem Lintcstubs_analysis.Ocamlcstubs.trace_name

(** [with_goblint_tmpdir f] creates the [.goblint] temporary directory, runs
    [f] and cleans up *)
let with_goblint_tmpdir f =
  GoblintDir.init () ;
  Fun.protect ~finally:GoblintDir.finalize f

(** [report_results ()] reports the results in the configured formats.
    Errors/warnings are reported immediately on standard output channels,
    but additional formats can be requested.

    [--html] can be used on the CLI to request html output to [result/]
    [--enable gobview --set save_run DIR] can be used to request [gobview]
    output into [DIR]

    See https://goblint.readthedocs.io/en/latest/user-guide/inspecting/
    on how to view the output.

    If the verification fails then also set the tool's exitcode appropriately.
 *)
let report_results () =
  Maingoblint.do_html_output () ;
  (* if [--enable gobview --set save_run DIR] is used output extra information
     for [gobview] into [DIR]. *)
  Maingoblint.do_gobview () ;
  if !Goblintutil.verified = Some false then exit 3
(* verifier failed! *)

(** [main ()] entrypoint for our C stub static analyzer.

    Compared to [goblint.ml] this is simplified to bare minimum: no timing
    stats, no server mode.
 *)
let main () =
  Cilfacade.init () ;
  (* for now we use goblint's CLI *)
  Maingoblint.parse_arguments () ;
  set_default_flags () ;
  enable_tracing_if_needed () ;
  let file = with_goblint_tmpdir Maingoblint.preprocess_parse_merge in
  (* AutoTune.chooseConfig file ;*)
  file |> Maingoblint.do_analyze @@ Analyses.empty_increment_data () ;
  report_results ()

(* Based on goblint.ml:
   We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit main
