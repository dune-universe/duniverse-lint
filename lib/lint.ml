open Base
module Sexp = Sexplib.Sexp

type lint = (unit, [ `Msg of string ]) Result.t

type check = {
  label : string;
  f : unit -> [ `Finished of lint | `Recursive of lint * check list ];
}
(** A [check] is basically a function that checks a certain aspect plus a label
    to note what it is checking.

    The function is supposed to return the result of its check which is of type
    [lint] ([unit] on success and a string message if the test fails with a
    description why).

    The function as two options: if this is the only check that it can do, it
    should return [`Finished] of the lint result. But if as part of this check
    more checks are enabled (e.g. checking for existence of a file enables
    checking of the syntax of the file) then it can return its own check
    result, as well as further recursive checks using [`Recursive].
*)

let lint ~label f = { label; f }
let dune_n = Re.(seq [ str "+dune"; rep digit; eol ]) |> Re.compile

let upstream_version version =
  Re.replace ~all:false dune_n ~f:(fun _ -> "") version

let check_package_in_opam name version =
  OpamGlobalState.with_ `Lock_none @@ fun global_state ->
  OpamRepositoryState.with_ `Lock_none global_state @@ fun repos_state ->
  let repos_list = OpamGlobalState.repos_list global_state in
  let name = OpamPackage.Name.of_string name in
  let version = OpamPackage.Version.of_string (upstream_version version) in
  let package = OpamPackage.create name version in
  match OpamRepositoryState.find_package_opt repos_state repos_list package with
  | Some _ -> `Finished (Ok ())
  | None ->
      let msg = Fmt.str "%a is not in repository" Pp.package package in
      `Finished (Error (`Msg msg))

let check_version version =
  match version |> Re.execp dune_n with
  | true -> `Finished (Ok ())
  | false -> `Finished (Error (`Msg "Version is missing +dune suffix"))

let parse_dune_project = function
  | Sexp.Atom _ -> `Finished (Error (`Msg "Invalid dune-project"))
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
      | Some name, Some version ->
          let check_version_correctness =
            lint ~label:"Version is correct" (fun () -> check_version version)
          in
          let check_opam_availability =
            lint ~label:"Base version exists in opam-repository" (fun () ->
                check_package_in_opam name version)
          in
          `Recursive
            (Ok (), [ check_version_correctness; check_opam_availability ])
      | None, Some _ ->
          `Finished (Error (`Msg "`name` missing in `dune-project`"))
      | Some _, None ->
          `Finished (Error (`Msg "`version` missing in `dune-project`"))
      | None, None ->
          `Finished
            (Error (`Msg "`name` and `version` missing in `dune-project`")))

let dune_project_exists path =
  let checks =
    Stdio.In_channel.with_file path ~f:(fun chan ->
        let sexps = Sexplib.Sexp.input_sexps chan in
        let sexp = Sexplib.Sexp.List sexps in
        lint ~label:"Parsing dune-project" (fun () -> parse_dune_project sexp))
  in
  `Recursive (Ok (), [ checks ])

let dune_project path =
  lint ~label:"dune-project exists" (fun () -> dune_project_exists path)

let pp_lint fmt = function
  | Ok () -> Fmt.pf fmt "Ok"
  | Error (`Msg msg) -> Fmt.pf fmt "Failed: %s" msg

let rec eval check =
  match check.f () with
  | `Finished r ->
      Fmt.pr "%s: %a\n" check.label pp_lint r;
      [ r ]
  | `Recursive (r, checks) ->
      Fmt.pr "%s: %a\n" check.label pp_lint r;
      List.concat_map ~f:eval checks

let dune_in_build_check build =
  let r =
    build
    |> List.fold ~init:false ~f:(fun uses_dune (args, _filter) ->
           match uses_dune with
           | true -> true
           | false -> (
               match List.hd args with
               | None -> uses_dune
               | Some (OpamTypes.CString "dune", _) -> true
               | Some _ -> false))
  in
  match r with
  | true -> `Finished (Ok ())
  | false -> `Finished (Error (`Msg "Not using `dune` in build"))

let opam_uses_dune path =
  let opam_uses_dune' path =
    let base_name = OpamFilename.Base.of_string path in
    let path = OpamFilename.of_basename base_name in
    let filename = OpamFile.make path in
    match OpamFile.OPAM.read_opt filename with
    | None -> `Finished (Error (`Msg "file missing"))
    | Some opam -> dune_in_build_check opam.build
  in
  lint ~label:"opam uses dune" (fun () -> opam_uses_dune' path)

let all ~label checks = lint ~label (fun () -> `Recursive (Ok (), checks))

(* code that is exposed for tests but not part of the API *)
module Private = struct
  let check_version = check_version
  let upstream_version = upstream_version
end
