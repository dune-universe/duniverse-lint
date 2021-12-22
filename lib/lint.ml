open Base
open O
module Sexp = Sexplib.Sexp

let check_version version =
  version
  |> String.is_suffix ~suffix:"+dune"
  |> Result.ok_if_true ~error:(`Msg "Version is missing +dune")

type dune_project = { name : string; version : string }

let parse_dune_project = function
  | Sexp.Atom _ -> Error (`Msg "Invalid dune-project")
  | Sexp.List xs -> (
      let name, version =
        List.fold xs ~init:(None, None) ~f:(fun (name, version) v ->
            match (name, version) with
            | Some _, Some _ -> (name, version)
            | _, _ -> (
                match v with
                | List (Atom "name" :: Atom name :: _) -> (Some name, version)
                | List (Atom "version" :: Atom version :: _) ->
                    (name, Some version)
                | Atom _ -> (name, version)
                | List _ -> (name, version)))
      in
      match (name, version) with
      | Some name, Some version -> Ok { name; version }
      | None, Some _ -> Error (`Msg "`name` missing in `dune-project`")
      | Some _, None -> Error (`Msg "`version` missing in `dune-project`")
      | None, None ->
          Error (`Msg "`name` and `version` missing in `dune-project`"))

let check_dune_project path =
  Stdio.In_channel.with_file path ~f:(fun chan ->
      let sexps = Sexplib.Sexp.input_sexps chan in
      let sexp = Sexplib.Sexp.List sexps in
      let* dune_project = parse_dune_project sexp in
      let* () = check_version dune_project.version in
      Result.return ())
