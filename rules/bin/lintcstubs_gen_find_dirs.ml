open Dune_action_plugin.V1

let rec join = function
  | [] ->
      return ()
  | hd :: tl ->
      let open O in
      let+ (), () = both hd (join tl) in
      ()

let rec readdir dir =
  let read_subdir subdir =
    let fullpath = Filename.concat dir subdir in
    fullpath |> print_endline;
    if Sys.file_exists fullpath && Sys.is_directory fullpath then
      readdir fullpath
    else
      return ()
  in
  let f paths =
    paths |> List.map read_subdir |> join
  in
  read_directory_with_glob ~path:(Path.of_string dir)
    ~glob:Dune_glob.V1.universal
  |> stage ~f

let () = run (readdir ".")
