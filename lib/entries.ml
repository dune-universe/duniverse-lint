open Base

let opam_files dir =
  match Unix.opendir dir with
  | exception Unix.Unix_error _ -> []
  | dir_handle ->
      let rec loop acc =
        match Unix.readdir dir_handle with
        | exception End_of_file ->
            Unix.closedir dir_handle;
            acc
        | entry -> (
            match String.is_suffix ~suffix:".opam" entry with
            | true -> entry :: loop acc
            | false -> loop acc)
      in
      loop []
