open Base
module Sexp = Sexplib.Sexp

type lint = (unit, [ `Msg of string ]) Result.t

type check = {
  label : string;
  f : unit -> [ `Finished of lint | `Recursive of lint * check list ];
}

let lint ~label f = { label; f }
let dune_n = Re.(seq [ str "+dune"; rep digit; eol ]) |> Re.compile

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
      | Some _name, Some version ->
          let check =
            lint ~label:"Version is correct" (fun () -> check_version version)
          in
          `Recursive (Ok (), [ check ])
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
end
