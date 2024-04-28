open Dune_action_plugin.V1
open O

let rec join = function
  | [] ->
      return []
  | hd :: tl ->
      let+ hd = hd and+ tl = join tl in
      hd :: tl

let find ~glob paths =
  paths
  |> List.map @@ fun path ->
     if Dune_glob.V1.test glob path then
       return [path]
     else if Sys.file_exists path && Sys.is_directory path then
       read_directory_with_glob ~path:(Path.of_string path) ~glob
     else
       return []

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
      let data = Printf.sprintf "(%s)" @@ String.concat " " (List.flatten files) in
      write_file ~path:(Path.of_string !output) ~data
    )
  in
  run result
