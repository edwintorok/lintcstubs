open Dune_action_plugin.V1
open O
module PathSet = Set.Make (String)

let rec join = function
  | [] ->
      return PathSet.empty
  | hd :: tl ->
      let+ hd = hd and+ tl = join tl in
      PathSet.union hd tl

let rec find ~glob paths =
  let dirs =
    paths |> PathSet.of_list |> PathSet.map Filename.dirname |> PathSet.elements
  in
  List.rev_append dirs paths
    |> List.map @@ fun path ->
       if Dune_glob.V1.test glob path then
         return (PathSet.singleton path)
       else if Sys.file_exists path && Sys.is_directory path then begin
         prerr_endline path;
         stage
           (* we want to discover subdirs containing cmt, so we have to ask for everything *)
           (read_directory_with_glob ~path:(Path.of_string path)
              ~glob:Dune_glob.V1.universal
           )
           ~f:(fun files ->
             let dirs = files |> List.filter (fun path' ->
               let path = Filename.concat path path' in
               prerr_endline path;
               Sys.file_exists path && Sys.is_directory path
             ) in
             (* TODO: recurse, FIXME: this crashes dune with an exception, cycle *)
             find ~glob dirs |> join
(*             files |> List.filter (Dune_glob.V1.test glob)
             |> PathSet.of_list |> return*)
           )
        end
       else
         return PathSet.empty
    

let () =
  let exts = ref [] and paths = ref [] and output = ref "" in
  let usage =
    Printf.sprintf "%s [--ext EXT ..] [--paths PATH..]" Sys.argv.(0)
  in
  Arg.parse
    [
      ( "--ext"
      , Arg.String (fun s -> exts := s :: !exts)
      , "file extension to look for"
      )
    ; ("--output", Arg.String (fun s -> output := s), "output path")
    ; ("--paths", Arg.Rest_all (fun l -> paths := l), "paths to check")
    ]
    ignore usage ;
  let glob = Dune_glob.V1.matching_extensions !exts in
  let result =
    stage (find ~glob !paths |> join) ~f:(fun files ->
        let data =
          Printf.sprintf "(%s)" @@ String.concat " " (PathSet.elements files)
        in
        write_file ~path:(Path.of_string !output) ~data
    )
  in
  run result
